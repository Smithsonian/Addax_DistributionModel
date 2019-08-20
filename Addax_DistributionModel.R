## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----Library,message=FALSE,warning=FALSE---------------------------------
library(usdm)
library(arm)
library(visreg)
library(pROC)
library(DAAG)
library(fields)
library(MuMIn)
library(gstat)
library(sp)
library(lubridate)
library(DT)
library(leaflet)

## ----Load, eval = T------------------------------------------------------
# Load SpatialPointsDataFrame.  Set working directory if necessary.
load(file="Addax_Dataset2.RData")
# Look at data
head(Sub.All2)

# Or get fancy with how the data table is displayed
#datatable(Sub.All2, rownames = FALSE, filter="top", options = list(pageLength = 5, scrollX = T))

## ----Aggregate, eval=T, echo=F-------------------------------------------
# Place data in a table to summarize results. 
Unique.ID <- unique(Sub.All2$YearMonth)

# Create matrix to hold everything
dat6 <- matrix(NA,nrow = length(Unique.ID),ncol = 9,dimnames = list(c(),c("YearMonth","Year","Season","PresAddax","PresDorc","PrevAdd","PrevDorc","One","Both")))

for (i in 1:length(Unique.ID)){
	temp <- subset(Sub.All2, YearMonth == Unique.ID[i])
		# Summarize dataset
		dat6[i,1] <- Unique.ID[i]
		dat6[i,2] <- as.character(unique(temp$Year))
		dat6[i,3] <- as.character(unique(temp$Season))
			obs.Add <- ifelse(temp$Addax > 0, 1, 0)
			obs.Dorc <- ifelse(temp$Dorcas > 0, 1, 0)
			Both <- obs.Add + obs.Dorc
			Both2 <- ifelse(Both > 1, 1, 0) # Vector where both present
			One <- ifelse(Both > 0, 1, 0) # At least one species present
		dat6[i,4] <- sum(obs.Add)
		dat6[i,5] <- sum(obs.Dorc)
		dat6[i,6] <- round(sum(obs.Add)/length(obs.Add)*100,digits=1)
		dat6[i,7] <- round(sum(obs.Dorc)/length(obs.Add)*100,digits=1)
		dat6[i,8] <- round(sum(One)/length(obs.Add)*100,digits=1)
		dat6[i,9] <- round(sum(Both2)/length(obs.Add)*100,digits=1)
}

# Look at result.  This should match Table 1 in Stabach et al. 2017.
dat6 <- as.data.frame(dat6)
dat6

# Write to file, if necessary
#write.csv(dat6,file="Addax_Dorcas_Prevalence.csv", quote = FALSE, row.names = FALSE)

## ----ScaleAnswer, eval=F, echo=F-----------------------------------------
## # Must specify x as the column header
## # Function will then center (subtract the mean) and scale (divide by the standard deviation) every value
## CenterScale <- function(x){
##   (x-mean(x))/sd(x)
## }
## 
## # Test both methods
## test <- as.numeric(scale(Sub.All2$TRI, center=TRUE))
## summary(test)
## test2 <- CenterScale(Sub.All2$TRI)
## summary(test2)

## ----Scale, eval=TRUE----------------------------------------------------
# Scale the parameters...but let's create a copy of the dataset first
Sub.Scale <- Sub.All2

# Use the help(scale) for more information on the function
Sub.Scale$shuman <- as.numeric(scale(Sub.Scale[,"Human"],center=TRUE))
Sub.Scale$sndvi <- as.numeric(scale(Sub.Scale[,"ndvi"],center=TRUE))
Sub.Scale$srough <- as.numeric(scale(Sub.Scale[,"ROUGH"],center=TRUE))
Sub.Scale$sDorcas <- as.numeric(scale(Sub.Scale[,"Dorcas"],center=TRUE))
Sub.Scale$sAddax <- as.numeric(scale(Sub.Scale[,"Addax"],center=TRUE))

## ----ScaleHold, eval=TRUE------------------------------------------------
# Create a blank matrix to hold the Mean and Standard Deviation of each continuous variable included the dataset. 
# These values are essential to rescale to original values
Scale.Val <- matrix(NA, 2, ncol(Sub.Scale[,c(15,17,8,12,7)]), dimnames=list(c("Mn","Sd"),c("Rough","NDVI","Dorcas","Human","Addax")))

# Calculate the mean and standard deviations.
Scale.Val[1,] <- apply(Sub.Scale[,c(15,17,8,12,7)],2,mean)
Scale.Val[2,] <- apply(Sub.Scale[,c(15,17,8,12,7)],2,sd)

# Look at values
Scale.Val

# Create a second object which summarize the data.  Here, I'm just specifying the column numbers.
data.scale <- scale(Sub.Scale[,c(15,17,8,12,7)],center=TRUE)
(summary.data <- apply(data.scale,2,summary))

## ----Corr, eval=T--------------------------------------------------------
# Group variables together in a dataframe
data.all <- as.data.frame(cbind(Sub.Scale$srough,Sub.Scale$sndvi,Sub.Scale$sDorcas,Sub.Scale$shuman,Sub.Scale$sAddax))

# Variance Inflation Analysis
(eval.vif <- vifstep(data.all))

# See also cor(data.all)

## ----ModelDregde, eval=F, echo=T-----------------------------------------
## options(na.action=na.fail)
## m1 <- dredge(glm.Addax)
## head(m1)

## ----Model, eval=T-------------------------------------------------------
# Create a full model with all the variables you think are important predictors of addax occurrence
glm.Addax <- glm(obsAddax ~ srough + I(srough^2) + sndvi + I(sndvi^2) + Human + obsDorc + Stipa1 + Stipa2 + Cornul + Season + Year, data = Sub.Scale, family = binomial(link="logit"))
# Summarize result
summary(glm.Addax)

# Or print the confidence intervals
#confint(glm.Addax)

## ----ModelGraph, eval=T--------------------------------------------------

# Graph result for surface roughness
visreg(glm.Addax,"srough",scale="response",ylab="Prob",partial=TRUE,line=list(col="blue"),fill=list(col="gray"),ylim=c(0,1))

# Plot the coefficients
coefplot(glm.Addax, plot=TRUE, mar=c(1,4,5.1,2), intercept=FALSE, vertical=TRUE, main="", var.las=1, frame.plot=FALSE)

## ----PlotAll, eval=F, echo=F---------------------------------------------
## visreg(glm.Addax, scale="response",partial=TRUE,line=list(col="blue"),fill=list(col="gray"),ylim=c(0,1))
## visreg(glm.Addax,"sndvi", scale="response",partial=TRUE,line=list(col="blue"),fill=list(col="gray"),ylim=c(0,1))

## ----PrettyPlot, eval=T--------------------------------------------------
# Graph the NDVI response
# ************************
# Since graphing NDVI, require the 2nd column of the minimum and maximum  values calculated above in the summary.data table
MinVal <- summary.data[1,2]
MaxVal <- summary.data[6,2]

    # Then, set the sequence in which to plot the values 
    divs <- 100
    x <- seq(MinVal, MaxVal, length.out=divs)
    x.unscale <- x*Scale.Val[2,2]+Scale.Val[1,2] # Need to keep track of the correct row and column.  Here, multiply by the SD and add the mean

# You can then update the unscaled values into the plot, by specifying the axis with these values.
visreg(glm.Addax,"sndvi",scale="response",ylab="Probability of Occurrence",xlab="NDVI",partial=TRUE, axes=FALSE, rug=0, ylim=c(0,1),line=list(col="black",lwd=2,lty=1),fill=list(col="grey"),points=list(col="black",cex=0.25,pch=19),frame=FALSE,main="Addax")

# Plot axes
axis(2,col="black",col.axis="black")
# Most complicated part......Are just substituting the transformed values for the original data.
axis(1,at=c(x[1],x[50],x[100]),lab=c(round(x.unscale[1],digits=2),round(x.unscale[50],digits=2),round(x.unscale[100],digits=2)),col="black",col.axis="black")

# That's nicer!

## ----PrettyPlotSrough, eval=F, echo=F------------------------------------
## # Really, all you need to do is recognize that you need to reference a different column in the summary.data and Scale.Val objects.
## # Change the column values below
## MinVal <- summary.data[1,1] # Note the change in column number
## MaxVal <- summary.data[6,1] # Note the change in column number
## 
##   # Then, set the sequence in which to plot the values
##   divs <- 100
##   x <- seq(MinVal, MaxVal, length.out=divs)
##   x.unscale <- x*Scale.Val[2,1]+Scale.Val[1,1] # Note the change in column number
## 
## # Now, you simply need to change the variable that you want to plot ('srough') and the x label (xlab)
## visreg(glm.Addax,"srough",scale="response",ylab="Probability of Occurrence",xlab="Surface Roughness",partial=TRUE, axes=FALSE, rug=0, ylim=c(0,1),line=list(col="black",lwd=2,lty=1),fill=list(col="grey"),points=list(col="black",cex=0.25,pch=19),frame=FALSE,main="Addax")
## 
## # Plot axes
## axis(2,col="black",col.axis="black")
## axis(1,at=c(x[1],x[50],x[100]),lab=c(round(x.unscale[1],digits=2),round(x.unscale[50],digits=2),round(x.unscale[100],digits=2)),col="black",col.axis="black")
## 
## # Now loop over both variables and plot
## # **************************************
## # **************************************
## 
## # Create variables that you want to print iteratively
## plot.variable <- c("srough","sndvi")
## main.label <- c("Roughness","NDVI")
## x.label <- c("Surface Rougness", "NDVI")
## 
## for(i in 1:length(plot.variable)){
## 
##   # Change the column number to [i]...this is what you will be looping through
##   MinVal <- summary.data[1,i]
##   MaxVal <- summary.data[6,i]
## 
##     # Then, set the sequence in which to plot the values
##     divs <- 100
##     x <- seq(MinVal, MaxVal, length.out=divs)
##     # Change the column to [i]
##     x.unscale <- x*Scale.Val[2,i]+Scale.Val[1,i]
## 
## # Now change the variables in the plot
## visreg(glm.Addax,plot.variable[i],scale="response",ylab="Probability of Occurrence",xlab=x.label[i],partial=TRUE, axes=FALSE, rug=0, ylim=c(0,1),line=list(col="black",lwd=2,lty=1),fill=list(col="grey"),points=list(col="black",cex=0.25,pch=19),frame=FALSE,main=main.label[i])
## 
## # Plot axes
## axis(2,col="black",col.axis="black")
## axis(1,at=c(x[1],x[50],x[100]),lab=c(round(x.unscale[1],digits=2),round(x.unscale[50],digits=2),round(x.unscale[100],digits=2)),col="black",col.axis="black")
## }

## ----Val, eval=T---------------------------------------------------------
# What's the AUC?
predpr <- predict(glm.Addax, type=c("response"))
(roccurve <- roc(Sub.Scale$obsAddax ~ predpr))
plot(roccurve)

## ----ModelSub, eval=T----------------------------------------------------
glm.Addax2 <- glm(obsAddax ~ srough + I(srough^2) + sndvi + I(sndvi^2), data = Sub.Scale, family = binomial(link="logit"))

# Summarize and print confidence intervals
summary(glm.Addax2)

# AUC
predpr <- predict(glm.Addax2, type=c("response"))
(roccurve <- roc(Sub.Scale$obsAddax ~ predpr))
plot(roccurve)

## ----Predict, eval=T-----------------------------------------------------
# Load NDVI data from flight survey date
# Note that this file has 250-meter resolution (MOD13Q1 data product)
# The SRTM data has 30-m resolution
# We need these data sources to have the same resolution in order to make the prediction. 

# Use the 2007 data from November for validation...matches the flight survey
ndvi <- raster("Data/MOD13Q1_Nov2017.tif")
# Convert raster to values to actual NDVI values
ndvi <- ndvi*0.0001

# Data needs to be summarized at 2.5km and scaled to match
# Create a focal grid....to match the resampling done at the survey points.....this just creates a matrix
# This is confusing, but it is just a weighted grid...to summary values within the grid
FoGrid <- focalWeight(ndvi,d=2500,type='circle')

# Now Summarize the NDVI within the focalWeight grid
ndvi2 <- focal(x=ndvi,w=FoGrid,fun=sum,na.rm=TRUE) # Need to use sum....because the focalWeight grid...creates a matrix of values that add to 1.  We want a summary of values within the focal grid

# Plot result
plot(ndvi2)

## ----SRTM, eval=T--------------------------------------------------------
# Create a different focalWeight grid because the cell resolutions are different (30 meters instead of 250 meters)
rough <- raster("Data/Rough_Sub.tif")

FoGrid1 <- focalWeight(rough,d=2500,type='circle')
rough2 <- focal(x=rough,w=FoGrid1,fun=sum,na.rm=TRUE)

# Plot layer
plot(rough2)

## ----Scale2, eval=T------------------------------------------------------
# Scale values. To back transform, you need to:x - mean(x) / sd(x))
Scale.Val
srough <- (rough2-Scale.Val[1,1])/Scale.Val[2,1]
sndvi <- (ndvi2-Scale.Val[1,2])/Scale.Val[2,2]

# Resample the grids so that they can be added together in the model.  
# This may take a long time if interpolating from 250 to 30 m
# Much quicker if going to 30 to 250 m
# Why would you want to do one resolution over the other?
#sndvi <- resample(sndvi,srough,method="bilinear") # 250 - 30 m (Slow)
srough <- resample(srough, sndvi, method="bilinear") # 30 - 250 m (Fast)

# Compare the resolutions 
compareRaster(srough,sndvi)

## ----FinalPredict, eval=T------------------------------------------------
# Summarize the model
summary(glm.Addax2)

# Manually:
# We could physically calculate the prediction from the model coefficients:
##coef <- summary(glm.Addax2)
##coef <- coef$coefficients

##Addax.predict <- (exp(coef[1] + rgh.scale*coef[2] + rgh.scale^2*coef[3] + ndvi.rsmp*coef[4] + ndvi.rsmp^2*coef[5])/(1 + exp(coef[1] + rgh.scale*coef[2] + rgh.scale^2*coef[3] + ndvi.rsmp*coef[4] + ndvi.rsmp^2*coef[5])))

# Using Predict:
# Create quadratic raster layers to include in the rasterBrick
srough2 <- srough^2
sndvi2 <- sndvi^2

# Add to brick and rename layer names
satImage <- brick(srough, srough2, sndvi, sndvi2)
names(satImage) <- c("srough","srough2", "sndvi", "sndvi2")

# Predict and export image to directory
Addax.predict <- predict(satImage, glm.Addax2, type="response", progress='text')

# Plot result
plot(Addax.predict)

# Or write the raster to the directory separately
#writeRaster(Addax.predict, 'Test.tif', format="GTiff", datatype = "INT1U", overwrite=TRUE)

## ----Threshold, eval=T---------------------------------------------------
# Create presence threshold
presenceThresh <- 0.13

# Threshold the result and plot
Addax.Thresh <- Addax.predict >= presenceThresh
plot(Addax.Thresh)

## ----Interactive, eval=T-------------------------------------------------
# Create color ramp
pal <- colorNumeric(rev(terrain.colors(10)), values(Addax.predict), na.color = "transparent")

# Reproject raster predict to Lat/Long
proj.info <- crs(Addax.predict)
proj.info.LongLat <- CRS("+proj=longlat +datum=WGS84")
Addax.predict.lat <- projectRaster(Addax.predict, crs=proj.info.LongLat) # Use default resolution

leaflet() %>%
  addTiles() %>%
  addRasterImage(Addax.predict, colors = pal, opacity = 0.9) %>%
  addLegend(pal = pal, values = values(Addax.predict),
    title = "Predicted Addax Occurrence")

