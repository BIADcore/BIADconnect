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
d <- query.database(user, password, 'biad', sql.command)

xmean <- mean(d$Longitude, na.rm=T)
ymean <- mean(d$Latitude, na.rm=T)
xlim <- range(d$Longitude)
ylim <- range(d$Latitude)

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

d <- query.database(user, password, 'biad', sql.command)
res <- summary.maker(d)
x <- res$summary$Longitude
y <- res$summary$Latitude

svg(file = '../tools/plots/map.aDNA.svg', width = 10, height = 5 )
plot(NULL,xlim=xlim,ylim=ylim,frame.plot=F,axes=F, xlab='',ylab='',main='Human aDNA')
map('world',xlim=xlim,ylim=ylim,col='grey90',add=T, fill=T, border='grey')
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

d <- query.database(user, password, 'biad', sql.command)
res <- summary.maker(d)
x <- res$summary$Longitude
y <- res$summary$Latitude

svg(file = '../tools/plots/map.graveindividuals.svg', width = 10, height = 5 )
plot(NULL,xlim=xlim,ylim=ylim,frame.plot=F,axes=F, xlab='',ylab='',main='Grave individuals')
map('world',xlim=xlim,ylim=ylim,col='grey90',add=T, fill=T, border='grey')
points(x, y, col=res$summary$col, pch=16,cex=0.8)
legend('topleft',res$legend,bty='n',col=res$cols,pch=16,cex=0.7)
dev.off()
#-----------------------------------------------------------------
# Faunal species
#-----------------------------------------------------------------
sql.command <- "SELECT `Sites`.`SiteID`,`Longitude`,`Latitude`,`FaunalSpeciesID` FROM `Sites`
INNER JOIN `Phases` ON `Sites`.`SiteID`=`Phases`.`SiteID`
INNER JOIN `FaunalSpecies` ON `Phases`.`PhaseID`=`FaunalSpecies`.`PhaseID`"

d <- query.database(user, password, 'biad', sql.command)
res <- summary.maker(d)
x <- res$summary$Longitude
y <- res$summary$Latitude

svg(file = '../tools/plots/map.faunalspecies.svg', width = 10, height = 5 )
plot(NULL,xlim=xlim,ylim=ylim,frame.plot=F,axes=F, xlab='',ylab='',main='Faunal Species')
map('world',xlim=xlim,ylim=ylim,col='grey90',add=T, fill=T, border='grey')
points(x, y, col=res$summary$col, pch=16,cex=0.8)
legend('topleft',res$legend,bty='n',col=res$cols,pch=16,cex=0.7)
dev.off()
#-----------------------------------------------------------------
# Botanical species
#-----------------------------------------------------------------
sql.command <- "SELECT `Sites`.`SiteID`,`Longitude`,`Latitude`,`SampleID` FROM `Sites`
INNER JOIN `Phases` ON `Sites`.`SiteID`=`Phases`.`SiteID`
INNER JOIN `ABotSamples` ON `Phases`.`PhaseID`=`ABotSamples`.`PhaseID`"

d <- query.database(user, password, 'biad', sql.command)
res <- summary.maker(d)
x <- res$summary$Longitude
y <- res$summary$Latitude

svg(file = '../tools/plots/map.botanicalspecies.svg', width = 10, height = 5 )
plot(NULL,xlim=xlim,ylim=ylim,frame.plot=F,axes=F, xlab='',ylab='',main='Botanical Species')
map('world',xlim=xlim,ylim=ylim,col='grey90',add=T, fill=T, border='grey')
points(x, y, col=res$summary$col, pch=16,cex=0.8)
legend('topleft',res$legend,bty='n',col=res$cols,pch=16,cex=0.7)
dev.off()
#-----------------------------------------------------------------
# C14
#-----------------------------------------------------------------
sql.command <- "SELECT `Sites`.`SiteID`,`Longitude`,`Latitude`,`C14ID` FROM `Sites`
INNER JOIN `C14Samples` ON `Sites`.`SiteID`=`C14Samples`.`SiteID`"

d <- query.database(user, password, 'biad', sql.command)
res <- summary.maker(d)
x <- res$summary$Longitude
y <- res$summary$Latitude

svg(file = '../tools/plots/map.C14.svg', width = 10, height = 5 )
plot(NULL,xlim=xlim,ylim=ylim,frame.plot=F,axes=F, xlab='',ylab='',main='Radiocarbon dates')
map('world',xlim=xlim,ylim=ylim,col='grey90',add=T, fill=T, border='grey')
points(x, y, col=res$summary$col, pch=16,cex=0.8)
legend('topleft',res$legend,bty='n',col=res$cols,pch=16,cex=0.7)
dev.off()
#-----------------------------------------------------------------
# human isotopes
#-----------------------------------------------------------------
sql.command <- "SELECT `Sites`.`SiteID`,`Longitude`,`Latitude`,`HumanIsoID` FROM `Sites`
INNER JOIN `Phases` ON `Sites`.`SiteID`=`Phases`.`SiteID`
INNER JOIN `Graves` ON `Phases`.`PhaseID`=`Graves`.`PhaseID`
INNER JOIN `GraveIndividuals` ON `GraveIndividuals`.`GraveID`=`Graves`.`GraveID`
INNER JOIN `HumanIsotopes` ON `HumanIsotopes`.`IndividualID`=`GraveIndividuals`.`IndividualID`"

d <- query.database(user, password, 'biad', sql.command)
res <- summary.maker(d)
x <- res$summary$Longitude
y <- res$summary$Latitude

svg(file = '../tools/plots/map.humanisotopes.svg', width = 10, height = 5 )
plot(NULL,xlim=xlim,ylim=ylim,frame.plot=F,axes=F, xlab='',ylab='',main='Human isotope samples')
map('world',xlim=xlim,ylim=ylim,col='grey90',add=T, fill=T, border='grey')
points(x, y, col=res$summary$col, pch=16,cex=0.8)
legend('topleft',res$legend,bty='n',col=res$cols,pch=16,cex=0.7)
dev.off()
#-----------------------------------------------------------------
# human strontium
#-----------------------------------------------------------------
sql.command <- "SELECT `Sites`.`SiteID`,`Longitude`,`Latitude`,`StrontiumID` FROM `Sites`
INNER JOIN `Phases` ON `Sites`.`SiteID`=`Phases`.`SiteID`
INNER JOIN `Graves` ON `Phases`.`PhaseID`=`Graves`.`PhaseID`
INNER JOIN `GraveIndividuals` ON `GraveIndividuals`.`GraveID`=`Graves`.`GraveID`
INNER JOIN `Strontium` ON `Strontium`.`IndividualID`=`GraveIndividuals`.`IndividualID`"

d <- query.database(user, password, 'biad', sql.command)
res <- summary.maker(d)
x <- res$summary$Longitude
y <- res$summary$Latitude

svg(file = '../tools/plots/map.humanstrontium.svg', width = 10, height = 5 )
plot(NULL,xlim=xlim,ylim=ylim,frame.plot=F,axes=F, xlab='',ylab='',main='Human strontium samples')
map('world',xlim=xlim,ylim=ylim,col='grey90',add=T, fill=T, border='grey')
points(x, y, col=res$summary$col, pch=16,cex=0.8)
legend('topleft',res$legend,bty='n',col=res$cols,pch=16,cex=0.7)
dev.off()
#-----------------------------------------------------------------
# Faunal strontium
#-----------------------------------------------------------------
sql.command <- "SELECT `Sites`.`SiteID`,`Longitude`,`Latitude`,`FaunIsoID` FROM `Sites`
INNER JOIN `Phases` ON `Sites`.`SiteID`=`Phases`.`SiteID`
INNER JOIN `FaunalIsotopes` ON `FaunalIsotopes`.`PhaseID`=`Phases`.`PhaseID`"

d <- query.database(user, password, 'biad', sql.command)
res <- summary.maker(d)
x <- res$summary$Longitude
y <- res$summary$Latitude

svg(file = '../tools/plots/map.faunalisotope.svg', width = 10, height = 5 )
plot(NULL,xlim=xlim,ylim=ylim,frame.plot=F,axes=F, xlab='',ylab='',main='Faunal Isotope samples')
map('world',xlim=xlim,ylim=ylim,col='grey90',add=T, fill=T, border='grey')
points(x, y, col=res$summary$col, pch=16,cex=0.8)
legend('topleft',res$legend,bty='n',col=res$cols,pch=16,cex=0.7)
dev.off()
#-----------------------------------------------------------------
# health
#-----------------------------------------------------------------
sql.command <- "SELECT `Sites`.`SiteID`,`Longitude`,`Latitude`,`Trait` FROM `Sites`
INNER JOIN `Phases` ON `Sites`.`SiteID`=`Phases`.`SiteID`
INNER JOIN `Graves` ON `Phases`.`PhaseID`=`Graves`.`PhaseID`
INNER JOIN `GraveIndividuals` ON `GraveIndividuals`.`GraveID`=`Graves`.`GraveID`
INNER JOIN `Health` ON `Health`.`IndividualID`=`GraveIndividuals`.`IndividualID`"

d <- query.database(user, password, 'biad', sql.command)
res <- summary.maker(d)
x <- res$summary$Longitude
y <- res$summary$Latitude

svg(file = '../tools/plots/map.health.svg', width = 10, height = 5 )
plot(NULL,xlim=xlim,ylim=ylim,frame.plot=F,axes=F, xlab='',ylab='',main='Human disease traits')
map('world',xlim=xlim,ylim=ylim,col='grey90',add=T, fill=T, border='grey')
points(x, y, col=res$summary$col, pch=16,cex=0.8)
legend('topleft',res$legend,bty='n',col=res$cols,pch=16,cex=0.7)
dev.off()
#-----------------------------------------------------------------
# material culture
#-----------------------------------------------------------------
sql.command <- "SELECT `Sites`.`SiteID`,`Longitude`,`Latitude`,`MaterialCultureID` FROM `Sites`
INNER JOIN `Phases` ON `Sites`.`SiteID`=`Phases`.`SiteID`
INNER JOIN `MaterialCulture` ON `Phases`.`PhaseID`=`MaterialCulture`.`PhaseID`"

d <- query.database(user, password, 'biad', sql.command)
res <- summary.maker(d)
x <- res$summary$Longitude
y <- res$summary$Latitude

svg(file = '../tools/plots/map.materialculture.svg', width = 10, height = 5 )
plot(NULL,xlim=xlim,ylim=ylim,frame.plot=F,axes=F, xlab='',ylab='',main='Material culture')
map('world',xlim=xlim,ylim=ylim,col='grey90',add=T, fill=T, border='grey')
points(x, y, col=res$summary$col, pch=16,cex=0.8)
legend('topleft',res$legend,bty='n',col=res$cols,pch=16,cex=0.7)
dev.off()
#-----------------------------------------------------------------