#--------------------------------------------------------------------------------------------------
# Needs fixing!! Then change to running daily
# Should output .svgs
# should then be embedded into BIADwiki plots page
# merges are throwing warnings as the queries are pulling all columns, so many are in common
#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
# Time dependent plots
#--------------------------------------------------------------------------------------------------
require(ggplot2)
require(sf)
require(rnaturalearth)
require(rnaturalearthdata)
require(maps)
require(mapdata)
require(svglite)
#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
conn <- init.conn()
sit <- query.database("SELECT * FROM `BIAD`.`Sites`", conn=conn)
pha <- query.database("SELECT * FROM `BIAD`.`Phases`", conn=conn)
d <- merge(pha,sit,by='SiteID')

d$reportedMidBP <- 1950 + (d$StartBCEreported + d$EndBCEreported)/2
d$date <- d$GMM
i <- !is.na(d$reportedMidBP)
d$date[i] <- d$reportedMidBP[i]

# remove those still with no chronology
d <- d[!is.na(d$date),]
#--------------------------------------------------------------------------------------------------
# design the universal limits
#--------------------------------------------------------------------------------------------------
xlim <- quantile(d$Longitude, prob=c(0.025,0.975))
ylim <- quantile(d$Latitude, prob=c(0.025,0.975))
N <- 10
zposts <- round(rev(quantile(d$date, prob=seq(0,1,length.out=N+1))),-2)

# plot sizes
pheight <- 8
pwidth <- pheight * round(diff(xlim)/diff(ylim),1)
#--------------------------------------------------------------------------------------------------
# common plot function
#--------------------------------------------------------------------------------------------------
common.plotter <- function(dd, tablename, pwidth, pheight, zposts){
	N <- length(zposts)-1
	svglite(file = paste('../tools/plots/time.phases',tablename,'svg',sep='.'), width = pwidth, height = pheight )
	par(mfrow=c(2,5))
	for(n in 1:N){
		i <- dd$date<=zposts[n] & dd$date>zposts[n+1]
		data <- dd[i,]
		main <- paste(n,': ',table,': ',zposts[n],' to ',zposts[n+1],' BP (',zposts[n]-zposts[n+1],' span)',sep='')
		plot(NULL,xlim=xlim,ylim=ylim,frame.plot=F,axes=F, xlab='',ylab='',main=main)
		map('world',xlim=xlim,ylim=ylim,col='grey90',add=T, fill=T, border='grey')
		points(data$Longitude, data$Latitude, col='steelblue', pch=16,cex=1)
		#legend('topleft',res$legend,bty='n',col=res$cols,pch=16,cex=0.7)
		}
	dev.off()
	}
#--------------------------------------------------------------------------------------------------
# all phases
#--------------------------------------------------------------------------------------------------
common.plotter(dd=d, tablename='all', pwidth=pwidth, pheight=pheight, zposts=zposts)
#--------------------------------------------------------------------------------------------------
# Tables directly linked to Phases
tables <- c('ABotIsotopes','ABotSamples','FaunalBiometrics','FaunalIsotopes','Graves','FaunalSpecies','MaterialCulture')
#--------------------------------------------------------------------------------------------------
for(t in 1:length(tables)){
	tmp <- query.database(paste("SELECT * FROM `BIAD`.`",tables[t],"`",sep=''), conn=conn)
	dd <- subset(d, PhaseID%in%unique(tmp$PhaseID))
	common.plotter(dd = dd, tablename=tables[t], pwidth=pwidth, pheight=pheight, zposts=zposts)
	}
#--------------------------------------------------------------------------------------------------
# More complicated relationships indirectly linked to Phases
#--------------------------------------------------------------------------------------------------
t1 <- query.database("SELECT * FROM `BIAD`.`FaunalIsotopes`", conn=conn)
t2 <- query.database("SELECT * FROM `BIAD`.`FaunalIsotopeSequences`", conn=conn)
t2 <- subset(t2, !is.na(SampleID))
tmp <- merge(t2, t1, by='SampleID') 
dd <- subset(d, PhaseID%in%unique(tmp$PhaseID))
common.plotter(dd = dd, tablename='FaunalIsotopeSequences', pwidth=pwidth, pheight=pheight, zposts=zposts)
#--------------------------------------------------------------------------------------------------
t1 <- query.database("SELECT * FROM `BIAD`.`Graves`", conn=conn)
t2 <- query.database("SELECT * FROM `BIAD`.`GraveIndividuals`", conn=conn)
t2 <- subset(t2, !is.na(GraveID))
tmp <- merge(t2, t1, by='GraveID')
dd <- subset(d, PhaseID%in%unique(tmp$PhaseID))
common.plotter(dd = dd, tablename='GraveIndividuals', pwidth=pwidth, pheight=pheight, zposts=zposts)
#--------------------------------------------------------------------------------------------------
t1 <- query.database("SELECT * FROM `BIAD`.`Graves`", conn=conn)
t2 <- query.database("SELECT * FROM `BIAD`.`GraveIndividuals`", conn=conn)
t3 <- query.database("SELECT * FROM `BIAD`.`HumanIsotopes`", conn=conn)
tmp <- merge(t3, t2, by='IndividualID')
tmp <- merge(tmp, t1, by='GraveID')
dd <- subset(d, PhaseID%in%unique(tmp$PhaseID))
common.plotter(dd = dd, tablename='HumanIsotopes', pwidth=pwidth, pheight=pheight, zposts=zposts)
#--------------------------------------------------------------------------------------------------
t1 <- query.database("SELECT * FROM `BIAD`.`Graves`", conn=conn)
t2 <- query.database("SELECT * FROM `BIAD`.`GraveIndividuals`", conn=conn)
t3 <- query.database("SELECT * FROM `BIAD`.`Rites`", conn=conn)
tmp <- merge(t3, t2, by='IndividualID')
tmp <- merge(tmp, t1, by='GraveID')
dd <- subset(d, PhaseID%in%unique(tmp$PhaseID))
common.plotter(dd = dd, tablename='Rites', pwidth=pwidth, pheight=pheight, zposts=zposts)
#--------------------------------------------------------------------------------------------------
t1 <- query.database("SELECT * FROM `BIAD`.`Graves`", conn=conn)
t2 <- query.database("SELECT * FROM `BIAD`.`GraveIndividuals`", conn=conn)
t3 <- query.database("SELECT * FROM `BIAD`.`Strontium`", conn=conn)
tmp <- merge(t3, t2, by='IndividualID')
tmp <- merge(tmp, t1, by='GraveID')
dd <- subset(d, PhaseID%in%unique(tmp$PhaseID))
common.plotter(dd = dd, tablename='Strontium', pwidth=pwidth, pheight=pheight, zposts=zposts)
#--------------------------------------------------------------------------------------------------
sql.command <- "SELECT `Graves`.`PhaseID` FROM `Graves`
INNER JOIN `GraveIndividuals` ON `GraveIndividuals`.`GraveID`=`Graves`.`GraveID`
INNER JOIN `Strontium` ON `Strontium`.`IndividualID`=`GraveIndividuals`.`IndividualID`
INNER JOIN `StrontiumSequences` ON `StrontiumSequences`.`StrontiumID`=`Strontium`.`StrontiumID`;"
tmp <- query.database(sql.command, conn=conn)
dd <- subset(d, PhaseID%in%unique(tmp$PhaseID))
common.plotter(dd = dd, tablename='StrontiumSequences', pwidth=pwidth, pheight=pheight, zposts=zposts)
#--------------------------------------------------------------------------------------------------
sql.command <- "SELECT `Sites`.`SiteID`,`Longitude`,`Latitude`,`aDNAID`,`Phases`.`PhaseID` FROM `Sites`
INNER JOIN `Phases` ON `Sites`.`SiteID`=`Phases`.`SiteID`
INNER JOIN `Graves` ON `Phases`.`PhaseID`=`Graves`.`PhaseID`
INNER JOIN `GraveIndividuals` ON `GraveIndividuals`.`GraveID`=`Graves`.`GraveID`
WHERE `GraveIndividuals`.`aDNAID` IS NOT NULL;"
tmp <- query.database(sql.command, conn=conn)
dd <- subset(d, PhaseID%in%unique(tmp$PhaseID))
common.plotter(dd = dd, tablename='aDNA', pwidth=pwidth, pheight=pheight, zposts=zposts)
#--------------------------------------------------------------------------------------
disconnect()
#--------------------------------------------------------------------------------------
