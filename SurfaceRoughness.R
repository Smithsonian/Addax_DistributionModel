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

## ----Load, eval=T--------------------------------------------------------
# Load the 30-m SRTM dataset, located in the Data directory
srtm <- raster("Data/srtm_30m")

## ----Spatial Data, eval=T------------------------------------------------

# Load the point data
Sub <- readOGR(dsn="Data", layer="Clipped_Add")

# Look at the file
head(Sub)

## ----Questions, eval=F, echo=F-------------------------------------------
## # Answers to questions:
## # 1) Print the name of the file to the R console.  This will provide information on the attributes of the file.  If projected, these details will appear in the output.  Use compareCRS() to determine if the projections match
## srtm
## Sub
## compareCRS(srtm,Sub)
## # 2) For raster and vector files, the function differs
## help(st_transform)
## help(projectRaster)
## # 3) Plot your data.  R does not have projection on the fly capabilities, so if you data do not overlay, there is a problem.  See below for plotting of data.

## ----PlotSRTM, eval=T----------------------------------------------------
# Plot the result
plot(srtm)
plot(Sub,pch=15,cex=0.7,add=TRUE)

## ----Roughness, eval = T-------------------------------------------------
rough <- terrain(srtm,opt='roughness')
plot(rough)

## ----Extract, eval=T-----------------------------------------------------
# Extract SRTM mean values within a 2.5 km buffer of each point location.  
Sub$ROUGH <- extract(rough,Sub,method='simple',buffer=2500,fun=mean)

# How is this different than just extracting at the point?
# See `help(extract)` for details about function syntax.  
Sub$RGH_PT <- extract(rough,Sub)

# Look at the data
head(Sub)

