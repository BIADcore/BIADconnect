#--------------------------------------------------------------------------------------
# Requirements
# You must have previously added the .Rprofile to your R_USER folder, here -> path.expand('~/') 
# See the BIADwiki readme or BIADwiki for details about using the .Rprofile

# Load some required functions
source("https://raw.githubusercontent.com/BIADwiki/BIADwiki/main/R/functions.R")
#--------------------------------------------------------------------------------------
library(mapdata)
#--------------------------------------------------------------------------------------
# get all sites within a polygon
#--------------------------------------------------------------------------------------
x <- run.server.query("SELECT `Longitude`,`Latitude` FROM `BIAD`.`Sites`")
d <- data.in.polygon(data=x,kml.path='../tools/kml/square.kml')
#--------------------------------------------------------------------------------------
# look at the data
#--------------------------------------------------------------------------------------
xlim <- c(-10,50)
ylim <- c(40,60)
plot(NULL,xlim=xlim,ylim=ylim,frame.plot=F,axes=F, xlab='',ylab='',main='square')
map('world',xlim=xlim,ylim=ylim,col='grey90',add=T, fill=T, border='grey')
points(d$Longitude, d$Latitude, pch=16)
#--------------------------------------------------------------------------------------