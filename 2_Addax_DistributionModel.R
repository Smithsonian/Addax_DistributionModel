## ----setup, include=FALSE----------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----Library, message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------
# Remove objects from memory
rm(list=ls())

# Load required libraries
library(arm)
library(DAAG)
library(dplyr)
library(leaflet)
library(lubridate)
library(pROC)
library(tmap)
library(usdm)
library(visreg)


## ----Load, eval = T----------------------------------------------------------------------------------------------------------------------------------
# Load the Data Frame and see that ndvi, rough, and Occ are added to your R environment
# The NDVI image is from November 2017 and will be used in our prediction
# Note: I converted the spatialpointsdataframe to a simple dataframe for the analyses
load(file="Data/Addax_Dataset.RData")

# Look at data
head(Occ)

# Or get fancy with how the data table is displayed.  R has many, many different options.
#library(DT)
#datatable(Occ, rownames = FALSE, filter="top", options = list(pageLength = 5, scrollX = T))


## ----Recode------------------------------------------------------------------------------------------------------------------------------------------
# Create month, year and season fields
Occ <- Occ %>% mutate(
  Month = month(Date),
  Year = year(Date),
  Season = case_when(
    Month >=3 & Month <= 6 ~ "Dry",
    Month >=7 & Month <=10 ~ "Wet",
    Month >=11 & Month <=12 ~ "Cold",
    TRUE ~ "Fix Problem"
  )
)

# Case_when is the same as:
#Occ$Season <- ifelse(Occ$Month >=3 & Occ$Month <=6, "Dry",
#                          ifelse(Occ$Month >=7 & Occ$Month <=10, "Wet",
#                                 ifelse(Occ$Month >=11 & Occ$Month <=12, "Cold","Fix Problem")
#                          ))

# We could easily include the following in the piping above.
# Recode Occurrence  
Occ$obsAddax <- ifelse(Occ$Addax > 0, 1, 0)
Occ$obsDorcas <- ifelse(Occ$Dorcas > 0, 1, 0)

# Add some summaries
Occ$Both <- Occ$obsAddax + Occ$obsDorcas
Occ$Both2 <- ifelse(Occ$Both > 1, 1, 0) # Both species present/occurred
Occ$One <- ifelse(Occ$Both > 0, 1, 0) # At least one species present/occurred

# Correct the data types
cols <- c("Cornul", "Stipa1", "Stipa2", "Season", "Year") 
Occ[cols] <- lapply(Occ[cols], factor) # You could change these individually, but this is more succinct Occ$Cornul <- factor(Occ$Cornul)
#str(Occ)


## ----Aggregate, warning=F, message=F-----------------------------------------------------------------------------------------------------------------
# Summarize
Occ.Summary <- Occ %>%
  group_by(Year, Month, Season) %>%
  summarize(PresAddax = sum(obsAddax),
            PresDorc = sum(obsDorcas),
            PrevAdd= round(sum(obsAddax)/length(obsAddax)*100,digits=1),
            PrevDorc = round(sum(obsDorcas)/length(obsAddax)*100,digits=1),
            One = round(sum(One)/length(obsAddax)*100,digits=1) ,
            Both = round(sum(Both2)/length(obsAddax)*100,digits=1)
            )

# Look at the result
Occ.Summary <- as.data.frame(Occ.Summary)
Occ.Summary

# If you want, write to a file
#write.csv(Occ.Summary, file="Addax_Dorcas_Prevalence.csv", quote = FALSE, row.names = FALSE)


## ----Plot--------------------------------------------------------------------------------------------------------------------------------------------
# Plot
plot(Occ$X,Occ$Y,xlab = "Easting", ylab = "Northing", main = "Plot Locations", frame = FALSE, pch = ".", col="red", cex = 5, asp=1)


## ----Loop Plot, eval=F, echo=F-----------------------------------------------------------------------------------------------------------------------
## Un.Date <- unique(Occ$YearMonth)
## 
## for(i in 1:length(Un.Date)){
##   #temp.df <- subset(Occ, YearMonth == Un.Date[i])
##   #plot(temp.df$X,temp.df$Y,xlab = "Easting", ylab = "Northing", main = Un.Date[i], frame = FALSE, pch = ".", col="red", cex = 5, asp=1)
## 
##   plot(Occ[Occ$YearMonth == Un.Date[i],"X"],Occ[Occ$YearMonth == Un.Date[i],"Y"],xlab = "Easting", ylab = "Northing", main = Un.Date[i], frame = FALSE, pch = ".", col="red", cex = 5, asp=1)
## }


## ----Scale, eval=T-----------------------------------------------------------------------------------------------------------------------------------
# Scale the continuous variables that we'll include in the modelling
Occ <- Occ %>% mutate(
  sHuman = as.numeric(scale(Human)), # This is a bit ugly, but if you don't specify as "as.numeric", the variables can be difficult to plot
  sndvi = as.numeric(scale(ndvi)),
  srough = as.numeric(scale(rough)),
  sDorcas = as.numeric(scale(Dorcas)),
  sAddax = as.numeric(scale(Addax))
)

# The default scale function simply does the following:
#CenterScale <- function(x){
#  (x-mean(x))/sd(x)
#}

#sndvi2 <- CenterScale(Occ$ndvi)

# These two scaled ndvi vectors should be exactly the same:
#cor(Occ$sndvi,sndvi2)
#summary(Occ$sndvi)
#summary(sndvi2)

# Let's also scale our raster layers by the mean and standard deviation at each location so we don't forget to later when making predictions from our model
s.ndvi <- (ndvi - mean(Occ$ndvi)) / sd(Occ$ndvi)
s.rough <- (rough - mean(Occ$rough)) / sd(Occ$rough)

par(mfrow=c(2,2))
# You should notice that the rasters look exactly the same, but their values have changed.  They are now on a much more similar scale.
plot(ndvi, main = "Non-Scaled NDVI");plot(s.ndvi, main = "Scaled NDVI");plot(rough, main = "Non-Scaled Rough");plot(s.rough, main = "Scaled Rough")
par(mfrow=c(1,1)) # Returning plotting window to normal


## ----Corr, eval=T------------------------------------------------------------------------------------------------------------------------------------
# Assess the continuous variables we'll include in our analysis
vifstep(Occ[,22:26]) # Including scaled Human Presence, NDVI, roughness, and Dorcas/Addax

# You could also use the cor function to investigate correlation.
# What is a reasonable correlation threshold to use |0.65|??
#cor(Occ[,22:26])


## ----Model, eval=T-----------------------------------------------------------------------------------------------------------------------------------
# Create a full model with all the variables you think are important predictors of addax occurrence
glm.Addax <- glm(obsAddax ~ rough + I(srough^2) + sndvi + I(sndvi^2) + sHuman + sDorcas + Stipa1 + Stipa2 + Cornul + Season + Year, 
                 data = Occ, 
                 family = binomial(link="logit"))
# Summarize result
summary(glm.Addax)

# Or print the confidence intervals
#confint(glm.Addax)


## ----Model Graph, eval=T-----------------------------------------------------------------------------------------------------------------------------
# Plot the coefficients
coefplot(glm.Addax,
        plot=TRUE,
        mar=c(1,4,5.1,2),
        intercept=FALSE,
        vertical=TRUE,
        main="",
        var.las=1,
        frame.plot=FALSE)

# Reset plotting
par(mar=c(5,4,4,2)+0.1) 

# Graph result for all variables
#visreg(glm.Addax, scale="response", ylab="Prob", partial=TRUE)

# Or just one variable at a time
par(mfrow=c(1,2))
visreg(glm.Addax,"srough",
       scale="response", 
       ylab="Probability of Occurrence", 
       xlab="Surface Roughness",
       partial=TRUE, 
       line=list(col="blue"), 
       fill=list(col="gray"),
       points=list(col="black",cex=0.25,pch=19),
       ylim=c(0,1))
visreg(glm.Addax,"sndvi",
       scale="response", 
       ylab="Probability of Occurrence",
       xlab="NDVI",
       partial=TRUE, 
       line=list(col="blue"), 
       fill=list(col="gray"),
       points=list(col="black",cex=0.25,pch=19),
       ylim=c(0,1))
par(mfrow=c(1,1))


## ----Validation,eval=T-------------------------------------------------------------------------------------------------------------------------------
# Evaluate deviance residuals
# No strong evidence of lack of fit.  Most residuals are around a value of 0.
devresid <- resid(glm.Addax, type = "deviance")
hist(devresid)

# Calculate AUC
predpr <- predict(glm.Addax, type=c("response"))
(roccurve <- roc(Occ$obsAddax ~ predpr))
plot(roccurve, main="AUC")

# Calculate cross-validation statistic
cv.binary(glm.Addax)


## ----ModelSub, eval=T--------------------------------------------------------------------------------------------------------------------------------
glm.Addax2 <- glm(obsAddax ~ srough + I(srough^2) + sndvi + I(sndvi^2), 
                  data = Occ, 
                  family = binomial(link="logit"))

# Summarize and print confidence intervals
summary(glm.Addax2)

# Calculate AUC
predpr <- predict(glm.Addax2, type=c("response"))
(roccurve <- roc(Occ$obsAddax ~ predpr))
plot(roccurve) # Not great

# Calculate cross-validation statistic
cv.binary(glm.Addax2)


## ----Model Predict, eval=T---------------------------------------------------------------------------------------------------------------------------
# We could physically calculate the prediction from the model coefficients:
#coef <- summary(glm.Addax2)
#coef <- coef$coefficients
#coef

#Addax.predict <- (exp(coef[1] + s.rough*coef[2] + s.rough^2*coef[3] + s.ndvi*coef[4] + s.ndvi^2*coef[5])/(1 + exp(coef[1] + s.rough*coef[2] + s.rough^2*coef[3] + s.ndvi*coef[4] + s.ndvi^2*coef[5])))

# Or and much easier, use Predict:
# Create quadratic raster layers to include in the rasterBrick
s.rough2 <- s.rough^2
s.ndvi2 <- s.ndvi^2

# Add to brick and rename layer names
satImage <- brick(s.rough, s.rough2, s.ndvi, s.ndvi2) # Use brick here, rather than stack to create a single, multi-layer file
names(satImage) <- c("srough","srough2", "sndvi", "sndvi2")

# Predict and export image to directory
Addax.predict <- predict(satImage, glm.Addax2, type="response", progress='text')

# Plot result
plot(Addax.predict) # Not a great prediction, with mostly low values, but it highlights some important aeras.


## ----Interactive, eval=T, warning=F, message=F-------------------------------------------------------------------------------------------------------
# Load ESRI imagery baselayer
tmap_mode("view")
tm_basemap("Esri.WorldImagery") +
  tm_shape(Addax.predict, name = "Addax prediction") +
  tm_raster(palette="-inferno", n=8, alpha=0.6, 
            title = "Predicted Addax Occurrence")

