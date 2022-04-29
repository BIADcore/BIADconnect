#------------------------------------------------------------------
# overheads
#------------------------------------------------------------------
source('functions.R')
library(leaflet)
library(htmlwidgets)
library(webshot)
#------------------------------------------------------------------
sql.command <- "SELECT * FROM COREX.Sites"
d <- sql.wrapper(sql.command,user,password)
#------------------------------------------------------------------
# map limits
lng1 <- min(d$Longitude,na.rm=T)
lng2 <- max(d$Longitude, na.rm=T)
lat1 <- min(d$Latitude, na.rm=T)
lat2 <- max(d$Latitude, na.rm=T)
#------------------------------------------------------------------
map <- leaflet()
map <- fitBounds(map=map, lng1=lng1, lat1=lat1, lng2=lng2, lat2=lat2)
map <- addTiles(map)
map <- addCircles(map, lng=d$Longitude, lat=d$Latitude, radius = 1)
saveWidget(map, file="../tools/plots/map.html", selfcontained = FALSE)
webshot("../tools/plots/map.html", "../tools/plots/map.png")
#------------------------------------------------------------------




