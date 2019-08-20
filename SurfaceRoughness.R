## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----Lib, eval=T, warning=F, message=F-----------------------------------
# Remove everything in memory
rm(list=ls())

# Load raster and rgdal library
library(raster)
library(sp)
library(rgdal)
library(proj4)

## ----WDir, eval = F------------------------------------------------------
## getwd()

## ----Load, eval=T--------------------------------------------------------
# Load the 30-m SRTM dataset
srtm <- raster("Data/srtm_30m")

# Check resolution
res(srtm)

# The project of the file is already defined
srtm

# Plot the result
#plot(srtm)

## ----Spatial Data, eval=T------------------------------------------------

# Load the point data
Sub <- readOGR(dsn="Data", layer="Clipped_Add")
#plot(Sub,pch=15,cex=0.7,add=TRUE)
# Look at the file
head(Sub)
# Look at the parameters 
Sub

# What if you had a .csv file
# Load your csv, then make spatial
MyData <- read.csv(file="Data/Clipped_Add_csvFile.csv",header=TRUE,sep=",")
head(MyData)
# Make SpatialPointsFile
xy <- MyData[,c(5,6)]
MyData.Spatial <- SpatialPointsDataFrame(coords= xy, data = MyData, 
                                         proj4string = CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Look at the file
head(MyData.Spatial)
# Look at the parameters
MyData.Spatial

#plot(MyData.Spatial,pch=10,cex=0.5,col="red",add=TRUE)

## ----PlotSRTM, eval=T----------------------------------------------------
# Plot the result
plot(srtm)
plot(Sub,pch=15,cex=0.7,add=TRUE)
plot(MyData.Spatial,pch=10,cex=0.5,col="red",add=TRUE)

## ----Roughness, eval = T-------------------------------------------------
rough <- terrain(srtm,opt='roughness')
# Other options. Use help(terrain) for more information
#TRI <- terrain(srtm,opt='TRI')
#TPI <- terrain(srtm,opt='TPI')

plot(rough)
#plot(TRI)
#plot(TPI)

# Other variables that could be calculated
#slope <- terrain(srtm,opt='slope', neighbors=8)
#aspect <- terrain(srtm,opt='aspect', neighbors=8)
# Or:
#x <- terrain(srtm, opt=c('slope', 'aspect'), unit='degrees')
#plot(x)
#hill <- hillShade(slope,aspect,angle=45,direction=0)

# Plot the hillshade and overlay the elevation (alpha is the transparency)
#plot(hill,col=grey(0:100/100),legend=FALSE,main='Niger')
#plot(srtm,col=rainbow(25,alpha=0.35), add=TRUE)

## ----Extract, eval=T-----------------------------------------------------
# Extract SRTM values at the point locations
# Take the average roughness within a 2.5 km buffer
Sub$ROUGH <- extract(rough,Sub,method='simple',buffer=2500,fun=mean)
#Sub$TRI <- extract(TRI,Sub,method='simple',buffer=2500,fun=mean)
#Sub$TPI <- extract(TPI,Sub,method='simple',buffer=2500,fun=mean)

# How is this different than just extracting at the point?
Sub$RGH_PT <- extract(rough,Sub)

# Look at the data
head(Sub)

# You could do the same with the .csv file you loaded above
#MyData.Spatial$ROUGH <- extract(rough,MyData.Spatial,method='simple',buffer=2500,fun=mean)
#MyData.Spatial$RGH_PT <- extract(rough,MyData.Spatial)

# Look at the Data
#head(MyData.Spatial)

