#--------------------------------------------------------------------------------------
# Usual overheads
#--------------------------------------------------------------------------------------
library(mapdata)
source('functions.R')
source('.Rprofile')
#--------------------------------------------------------------------------------------
# get all sites within a polygon
#--------------------------------------------------------------------------------------
sql.command <- "SELECT `Longitude`,`Latitude` FROM `BIAD`.`Sites`"
x <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)
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