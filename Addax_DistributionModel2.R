## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----Library,message=FALSE,warning=FALSE---------------------------------
#install.packages("packagename")
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

## ---- eval = F-----------------------------------------------------------
## getwd()
## #setwd("D:/whatever")

## ----Load, eval = T------------------------------------------------------
# Load prepared data file. NDVI and surface roughness already extracted.
load(file="./Data/Addax_Dataset.RData")
# Look at data
head(Sub.All2)

## ----Process, eval = T---------------------------------------------------
# Create a Month Field
Sub.All2$Month <- as.numeric(strftime(Sub.All2$Date,format="%m",tz="GMT"))
# Add in the Season
Sub.All2$Season <- ifelse(Sub.All2$Month >=3 & Sub.All2$Month <=6, "Dry",
                          ifelse(Sub.All2$Month >=7 & Sub.All2$Month <=10, "Wet",
                                 ifelse(Sub.All2$Month >=11 & Sub.All2$Month <=12, "Cold","Fix Problem")))
# Make a dataframe (remove from spatial format)
Sub.All2 <- as.data.frame(Sub.All2)

# Re-code the occurrence records as presence/absence 
Sub.All2$obsAddax <- ifelse(Sub.All2$Addax > 0, 1, 0) 
Sub.All2$obsDorc <- ifelse(Sub.All2$Dorcas > 0, 1, 0)

# Make appropriate fields factors (presence/absence)
cols <- c("Cornul", "Stipa1", "Stipa2", "Season", "obsAddax", "obsDorc") 
Sub.All2[cols] <- lapply(Sub.All2[cols], factor)

# Include survey year as a factor in models. Not included above because I am changing the column name. 
Sub.All2$Year <- as.factor(year(Sub.All2$Date))

## ----Aggregate, eval=T---------------------------------------------------
# Aggregate
aggregate(as.character(Year) ~ Season, data = Sub.All2, unique)

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

		# Output each survey...plot only the first survey
		if(i==1){
		plot(temp$X,temp$Y,xlab="Easting",ylab="Northing",xlim=c(min(Sub.All2$X),max(Sub.All2$X)), ylim=c(min(Sub.All2$Y),max(Sub.All2$Y)), frame=FALSE, main=Unique.ID[i], asp=1)
}}
# Look at result.  This should match Table 1 in Stabach et al. 2017.
dat6 <- as.data.frame(dat6)
dat6
# Write to file if necessary
#write.csv(dat6,file="./Addax_Dorcas_Prevalence.csv", quote = FALSE, row.names = FALSE)

## ----Scale, eval=TRUE----------------------------------------------------
# Scale the parameters...Create a copy of the dataset
Sub.Scale <- Sub.All2

# Scale variables and append into original dataframe
# Check out the scale command to make sure you know what the scale function (with center = TRUE) is doing
Sub.Scale$shuman <- as.numeric(scale(Sub.Scale[,"Human"],center=TRUE))
Sub.Scale$sndvi <- as.numeric(scale(Sub.Scale[,"ndvi"],center=TRUE))
Sub.Scale$srough <- as.numeric(scale(Sub.Scale[,"ROUGH"],center=TRUE))
Sub.Scale$sDorcas <- as.numeric(scale(Sub.Scale[,"Dorcas"],center=TRUE))
Sub.Scale$sAddax <- as.numeric(scale(Sub.Scale[,"Addax"],center=TRUE))

# Create a blank matrix to hold the Mean and Standard Deviation of each continuous variable included the dataset. These values will be necessary to make the prediction map.
Scale.Val <- matrix(NA, 2, ncol(Sub.Scale[,c(15,17,8,12,7)]), dimnames=list(c("Mn","Sd"),c("Rough","NDVI","Dorcas","Human","Addax")))

# Calculate the mean and standard deviations.
Scale.Val[1,] <- apply(Sub.Scale[,c(15,17,8,12,7)],2,mean)
Scale.Val[2,] <- apply(Sub.Scale[,c(15,17,8,12,7)],2,sd)

# Look at values
Scale.Val

# Summarize the data.  Here, I'm just specifying the column numbers.
# How to determine which columns are being referenced? 
data.scale <- scale(Sub.Scale[,c(15,17,8,12,7)])
(summary.data <- apply(data.scale,2,summary))

## ----Corr, eval=T--------------------------------------------------------
# Group variables together in a dataframe
data.all <- as.data.frame(cbind(Sub.Scale$srough,Sub.Scale$sndvi,Sub.Scale$sDorcas,Sub.Scale$shuman,Sub.Scale$sAddax))

# Variance Inflation Analysis
(eval.vif <- vifstep(data.all))
# Or, use the cor function...will give essentially the same result in a different format
#cor(data.all)

## ----Model, eval=T-------------------------------------------------------
# Create a full model with all the variables you think are important predictors of addax occurrence
glm.Addax <- glm(obsAddax ~ srough + I(srough^2) + sndvi + I(sndvi^2) + Human + obsDorc + Stipa1 + Stipa2 + Cornul + Season + Year, data = Sub.Scale, family = binomial(link="logit"))
# Summarize result and look at the confidence intervals
summary(glm.Addax)
confint(glm.Addax)

# Graph results
visreg(glm.Addax,scale="response",ylab="Prob",partial=TRUE,line=list(col="blue"),fill=list(col="gray"),ylim=c(0,1))

# Plot the coefficients
coefplot(glm.Addax, plot=TRUE, mar=c(1,4,5.1,2), intercept=FALSE, vertical=TRUE, main="", var.las=1, frame.plot=FALSE)


## ----PlotBack, eval=T----------------------------------------------------
# Graph the NDVI response
  # First, extract the Min and Max values from the summary.data above
  MinVal <- summary.data[1,2]
  MaxVal <- summary.data[6,2]

	# Then, set the sequence in which to plot the values 
	divs <- 100
	x <- seq(MinVal, MaxVal, length.out=divs)
	x.unscale <- x*Scale.Val[2,2]+Scale.Val[1,2]

# In the visreg function, you then just need to specify which variable you want to plot.
# You can then update the unscaled values into the plot, by specifying the axis with these values.
visreg(glm.Addax,"sndvi",scale="response",ylab="Probability of Occurrence",xlab="NDVI",partial=TRUE, axes=FALSE, rug=0, ylim=c(0,1),line=list(col="black",lwd=2,lty=1),fill=list(col="grey"),points=list(col="black",cex=0.25,pch=19),frame=FALSE,main="Addax")
axis(2,col="black",col.axis="black")
axis(1,at=c(x[1],x[50],x[100]),lab=c(round(x.unscale[1],digits=2),round(x.unscale[50],digits=2),round(x.unscale[100],digits=2)),col="black",col.axis="black")

## ----PlotBack2, eval=T---------------------------------------------------
  # Surface Roughness

  # Really, all you need to do is recognize that you need to reference a different column in the summary.data and Scale.Val dataframes.
  # Change the column values below
  MinVal <- summary.data[1,1] # Note the change in column number
  MaxVal <- summary.data[6,1] # Note the change in column number

	# Then, set the sequence in which to plot the values 
	divs <- 100
	x <- seq(MinVal, MaxVal, length.out=divs)
	x.unscale <- x*Scale.Val[2,1]+Scale.Val[1,1] # Note the change in column number

# Now, you simply need to change the variable that you want to plot ('srough') and the x label (xlab)
visreg(glm.Addax,"srough",scale="response",ylab="Probability of Occurrence",xlab="Surface Roughness",partial=TRUE, axes=FALSE, rug=0, ylim=c(0,1),line=list(col="black",lwd=2,lty=1),fill=list(col="grey"),points=list(col="black",cex=0.25,pch=19),frame=FALSE,main="Addax")
axis(2,col="black",col.axis="black")
axis(1,at=c(x[1],x[50],x[100]),lab=c(round(x.unscale[1],digits=2),round(x.unscale[50],digits=2),round(x.unscale[100],digits=2)),col="black",col.axis="black")

## ----Val, eval=T---------------------------------------------------------
# What's the AUC?
predpr <- predict(glm.Addax, type=c("response"))
(roccurve <- roc(Sub.Scale$obsAddax ~ predpr))
plot(roccurve)

# What's the cross-validation statistic?
cv.binary(glm.Addax)

## ----ModelSub, eval=T----------------------------------------------------
glm.Addax2 <- glm(obsAddax ~ srough + I(srough^2) + sndvi + I(sndvi^2), data = Sub.Scale, family = binomial(link="logit"))

# Summarize and print confidence intervals
summary(glm.Addax2)
confint(glm.Addax2)

# AUC
predpr <- predict(glm.Addax2, type=c("response"))
(roccurve <- roc(Sub.Scale$obsAddax ~ predpr))
plot(roccurve)

# Cross-Validation
cv.binary(glm.Addax2)

## ----Predict, eval=T-----------------------------------------------------
# Load NDVI data from flight survey date
# Note that this file has 250-meter resolution (MOD13Q1 data product)
# The SRTM data has 30-m resolution
# We need these data sources to have the same resolution in order to make the prediction. Otherwise, we will get an error.
#setwd("D:/Jared/Work/R/SCBI/MODIS/Niger_Validation")

# Use the 2007 data from November for validation...matches the flight survey
ndvi <- raster(paste0(getwd(),"/Data/MOD13Q1_Nov2017.tif"))
# Convert raster to values to actual NDVI values
ndvi <- ndvi*0.0001

# Data needs to be summarized at 2.5km and scaled to match
# Create a focal grid....to match the resampling done at the survey points.....this just creates a matrix
# This is confusing, but it is a weighted grid...to summary values within the grid
FoGrid <- focalWeight(ndvi,d=2500,type='circle')
# Now Summarize the NDVI within the focalWeight grid
ndvi2 <- focal(x=ndvi,w=FoGrid,fun=sum,na.rm=TRUE) # Need to use sum....because the focalWeight grid...creates a matrix of values that add to 1.  We want a summary of values within the focal grid

# Plot the two results
par(mfrow=c(1,2))
plot(ndvi)
plot(ndvi2)

## ----SRTM, eval=T--------------------------------------------------------
# Create a different focalWeight grid because the cell resolutions are different (30 meters instead of 250 meters)
rough <- raster(paste0(getwd(),"/Data/Rough_Sub.tif"))

FoGrid1 <- focalWeight(rough,d=2500,type='circle')
rough2 <- focal(x=rough,w=FoGrid1,fun=sum,na.rm=TRUE)

# Plot to see the two raster layers
plot(rough)
plot(rough2)

## ----Scale2, eval=T------------------------------------------------------
# Scale values. To back transform, you need to:x - mean(x) / sd(x))
Scale.Val
rgh.scale <- (rough2-Scale.Val[1,1])/Scale.Val[2,1]
ndvi.scale <- (ndvi2-Scale.Val[1,2])/Scale.Val[2,2]

# Resample the grids so that they can be added together in the model.
# Resampling the 30m to 250m will result in a faster calculation.
# Resampling the 250m to 30m will keep the values
ndvi.rsmp <- resample(ndvi.scale,rgh.scale,method="bilinear")

# Compare the resolutions 
compareRaster(rgh.scale,ndvi.rsmp)

## ----FinalPredict, eval=T------------------------------------------------
# Summarize the model
summary(glm.Addax2)

coef <- summary(glm.Addax2)
coef <- coef$coefficients

Addax.predict <- (exp(coef[1] + rgh.scale*coef[2] + rgh.scale^2*coef[3] + ndvi.rsmp*coef[4] + ndvi.rsmp^2*coef[5])/
(1 + exp(coef[1] + rgh.scale*coef[2] + rgh.scale^2*coef[3] + ndvi.rsmp*coef[4] + ndvi.rsmp^2*coef[5])))

par(mfrow=c(1,1))
plot(Addax.predict)

# Write the raster to a directory
#writeRaster(Addax.Thresh, 'EditDirectory.tif', format="GTiff", overwrite=TRUE)

