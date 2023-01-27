#------------------------------------------------------------------
require(ggplot2)
require(sf)
require(rnaturalearth)
require(rnaturalearthdata)
require(maps)
require(mapdata)
require(svglite)
#-----------------------------------------------------------------
# overall sites
#-----------------------------------------------------------------
sql.command <- "SELECT `Longitude`,`Latitude` FROM `BIAD`.`Sites`"
d <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)

xmean <- mean(d$Longitude, na.rm=T)
ymean <- mean(d$Latitude, na.rm=T)

crs <- paste("+proj=ortho +lat_0=",ymean," + lon_0=",xmean,sep='')

points <- st_as_sf(d, coords = c('Longitude','Latitude'), crs=4326)
points <- st_transform(points, crs=crs)

world <- ne_countries(scale='medium',returnclass='sf')
world <- st_transform(world, crs=crs)

map <- ggplot() + 
geom_sf(data=world, color='grey90',fill='grey') +
geom_sf(data = points, color = 'firebrick', pch=20, size=2) +
coord_sf(xlim=st_bbox(points$geometry)[c('xmin','xmax')],ylim=st_bbox(points$geometry)[c('ymin','ymax')]) + 
theme(panel.background=element_rect(fill='steelblue2'))

ggsave(file = '../tools/plots/map.svg', plot=map, width = 20, height = 15 )
#-----------------------------------------------------------------
# aDNA
#-----------------------------------------------------------------
sql.command <- "SELECT `Sites`.`SiteID`,`Longitude`,`Latitude`,`aDNAID` FROM `Sites`
INNER JOIN `Phases` ON `Sites`.`SiteID`=`Phases`.`SiteID`
INNER JOIN `Graves` ON `Phases`.`PhaseID`=`Graves`.`PhaseID`
INNER JOIN `GraveIndividuals` ON `GraveIndividuals`.`GraveID`=`Graves`.`GraveID`
WHERE `GraveIndividuals`.`aDNAID` IS NOT NULL;"

d <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)
res <- summary.maker(d)
x <- res$summary$Longitude
y <- res$summary$Latitude

svg(file = '../tools/plots/map.aDNA.svg', width = 10, height = 5 )
plot(NULL,xlim=range(x),ylim=range(y),frame.plot=F,axes=F, xlab='',ylab='',main='Human aDNA')
map('world',xlim=range(x),ylim=range(y),col='grey90',add=T, fill=T, border='grey')
points(x, y, col=res$summary$col, pch=16,cex=0.8)
legend('topleft',res$legend,bty='n',col=res$cols,pch=16,cex=0.7)
dev.off()
#-----------------------------------------------------------------
# grave individuals
#-----------------------------------------------------------------
sql.command <- "SELECT `Sites`.`SiteID`,`Longitude`,`Latitude`,`IndividualID` FROM `Sites`
INNER JOIN `Phases` ON `Sites`.`SiteID`=`Phases`.`SiteID`
INNER JOIN `Graves` ON `Phases`.`PhaseID`=`Graves`.`PhaseID`
INNER JOIN `GraveIndividuals` ON `GraveIndividuals`.`GraveID`=`Graves`.`GraveID`"

d <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)
res <- summary.maker(d)
x <- res$summary$Longitude
y <- res$summary$Latitude

svg(file = '../tools/plots/map.graveindividuals.svg', width = 10, height = 5 )
plot(NULL,xlim=range(x),ylim=range(y),frame.plot=F,axes=F, xlab='',ylab='',main='Grave individuals')
map('world',xlim=range(x),ylim=range(y),col='grey90',add=T, fill=T, border='grey')
points(x, y, col=res$summary$col, pch=16,cex=0.8)
legend('topleft',res$legend,bty='n',col=res$cols,pch=16,cex=0.7)
dev.off()
#-----------------------------------------------------------------
