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
sit <- query.database(user, password, 'biad', "SELECT * FROM `BIAD`.`Sites`")
pha <- query.database(user, password, 'biad', "SELECT * FROM `BIAD`.`Phases`")
d <- merge(pha,sit,by='SiteID')
d$reportedMidBP <- 1950 + (d$StartBCEreported + d$EndBCEreported)/2
d$date <- d$gaussianModelMu
i <- !is.na(d$reportedMidBP)
d$date[i] <- d$reportedMidBP[i]

# remove those still with no chronology
d <- d[!is.na(d$date),]

# plot sizes
pwidth <- 50
pheight <- 20
#--------------------------------------------------------------------------------------------------
# design the universal limits
#--------------------------------------------------------------------------------------------------
xlim <- quantile(d$Longitude, prob=c(0.025,0.975))
ylim <- quantile(d$Latitude, prob=c(0.025,0.975))
N <- 10
zposts <- round(rev(quantile(d$date, prob=seq(0,1,length.out=N+1))),-2)
#--------------------------------------------------------------------------------------------------
# all phases
#--------------------------------------------------------------------------------------------------
table <- 'all'
svg(file = paste('../tools/plots/time.phases',table,'svg',sep='.'), width = pwidth, height = pheight )
par(mfrow=c(2,5))
for(n in 1:N){
	i <- d$date<=zposts[n] & d$date>zposts[n+1]
	data <- d[i,]
	main <- paste(n,': ',table,': ',zposts[n],' to ',zposts[n+1],'BP (',zposts[n]-zposts[n+1],' span)',sep='')
	plot(NULL,xlim=xlim,ylim=ylim,frame.plot=F,axes=F, xlab='',ylab='',main=main)
	map('world',xlim=xlim,ylim=ylim,col='grey90',add=T, fill=T, border='grey')
	points(data$Longitude, data$Latitude, col='steelblue', pch=16,cex=1)
	#legend('topleft',res$legend,bty='n',col=res$cols,pch=16,cex=0.7)
	}
dev.off()
#--------------------------------------------------------------------------------------------------
# Tables directly linked to Phases
tables <- c('ABotIsotopes','ABotSamples','FaunalBiometrics','FaunalIsotopes','Graves','FaunalSpecies','MaterialCulture')
#--------------------------------------------------------------------------------------------------
for(t in 1:length(tables)){

	tmp <- query.database(user, password, 'biad', paste("SELECT * FROM `BIAD`.`",tables[t],"`",sep=''))
	dd <- subset(d, PhaseID%in%unique(tmp$PhaseID))

	table <- tables[t]
	#svg(file = paste('../tools/plots/time.phases',table,'svg',sep='.'), width = pwidth, height = pheight )
	par(mfrow=c(2,5))
	for(n in 1:N){
		i <- dd$date<=zposts[n] & dd$date>zposts[n+1]
		data <- dd[i,]
		main <- paste(n,': ',table,': ',zposts[n],' to ',zposts[n+1],'BP (',zposts[n]-zposts[n+1],' span)',sep='')
		plot(NULL,xlim=xlim,ylim=ylim,frame.plot=F,axes=F, xlab='',ylab='',main=main)
		map('world',xlim=xlim,ylim=ylim,col='grey90',add=T, fill=T, border='grey')
		points(data$Longitude, data$Latitude, col='steelblue', pch=16,cex=1)
		#legend('topleft',res$legend,bty='n',col=res$cols,pch=16,cex=0.7)
		}
	#dev.off()
	}
#--------------------------------------------------------------------------------------------------
# More complicated relationships indirectly linked to Phases
#--------------------------------------------------------------------------------------------------
t1 <- query.database(user, password, 'biad', "SELECT * FROM `BIAD`.`FaunalIsotopes`")
t2 <- query.database(user, password, 'biad', "SELECT * FROM `BIAD`.`FaunalIsotopeSequences`")
t2 <- subset(t2, !is.na(SampleID))
tmp <- merge(t2, t1, by='SampleID') # this is fucked!! Foreign key is missing!!
dd <- subset(d, PhaseID%in%unique(tmp$PhaseID))

table <- 'FaunalIsotopeSequences'
svg(file = paste('../tools/plots/time.phases',table,'svg',sep='.'), width = pwidth, height = pheight )
par(mfrow=c(2,5))
for(n in 1:N){
	i <- dd$date<=zposts[n] & dd$date>zposts[n+1]
	data <- dd[i,]
	main <- paste(n,': ',table,': ',zposts[n],' to ',zposts[n+1],'BP (',zposts[n]-zposts[n+1],' span)',sep='')
	plot(NULL,xlim=xlim,ylim=ylim,frame.plot=F,axes=F, xlab='',ylab='',main=main)
	map('world',xlim=xlim,ylim=ylim,col='grey90',add=T, fill=T, border='grey')
	points(data$Longitude, data$Latitude, col='steelblue', pch=16,cex=1)
	#legend('topleft',res$legend,bty='n',col=res$cols,pch=16,cex=0.7)
	}
dev.off()
#--------------------------------------------------------------------------------------------------
t1 <- query.database(user, password, 'biad', "SELECT * FROM `BIAD`.`Graves`")
t2 <- query.database(user, password, 'biad', "SELECT * FROM `BIAD`.`GraveIndividuals`")
t2 <- subset(t2, !is.na(GraveID))
tmp <- merge(t2, t1, by='GraveID')
dd <- subset(d, PhaseID%in%unique(tmp$PhaseID))

table <- 'GraveIndividuals'
svg(file = paste('../tools/plots/time.phases',table,'svg',sep='.'), width = pwidth, height = pheight )
par(mfrow=c(2,5))
for(n in 1:N){
	i <- dd$date<=zposts[n] & dd$date>zposts[n+1]
	data <- dd[i,]
	main <- paste(n,': ',table,': ',zposts[n],' to ',zposts[n+1],'BP (',zposts[n]-zposts[n+1],' span)',sep='')
	plot(NULL,xlim=xlim,ylim=ylim,frame.plot=F,axes=F, xlab='',ylab='',main=main)
	map('world',xlim=xlim,ylim=ylim,col='grey90',add=T, fill=T, border='grey')
	points(data$Longitude, data$Latitude, col='steelblue', pch=16,cex=1)
	#legend('topleft',res$legend,bty='n',col=res$cols,pch=16,cex=0.7)
	}
dev.off()
#--------------------------------------------------------------------------------------------------
t1 <- query.database(user, password, 'biad', "SELECT * FROM `BIAD`.`Graves`")
t2 <- query.database(user, password, 'biad', "SELECT * FROM `BIAD`.`GraveIndividuals`")
t3 <- query.database(user, password, 'biad', "SELECT * FROM `BIAD`.`HumanIsotopes`")
tmp <- merge(t3, t2, by='IndividualID')
tmp <- merge(tmp, t1, by='GraveID')
dd <- subset(d, PhaseID%in%unique(tmp$PhaseID))

table <- 'HumanIsotopes'
svg(file = paste('../tools/plots/time.phases',table,'svg',sep='.'), width = pwidth, height = pheight )
par(mfrow=c(2,5))
for(n in 1:N){
	i <- dd$date<=zposts[n] & dd$date>zposts[n+1]
	data <- dd[i,]
	main <- paste(n,': ',table,': ',zposts[n],' to ',zposts[n+1],'BP (',zposts[n]-zposts[n+1],' span)',sep='')
	plot(NULL,xlim=xlim,ylim=ylim,frame.plot=F,axes=F, xlab='',ylab='',main=main)
	map('world',xlim=xlim,ylim=ylim,col='grey90',add=T, fill=T, border='grey')
	points(data$Longitude, data$Latitude, col='steelblue', pch=16,cex=1)
	#legend('topleft',res$legend,bty='n',col=res$cols,pch=16,cex=0.7)
	}
dev.off()
#--------------------------------------------------------------------------------------------------
t1 <- query.database(user, password, 'biad', "SELECT * FROM `BIAD`.`Graves`")
t2 <- query.database(user, password, 'biad', "SELECT * FROM `BIAD`.`GraveIndividuals`")
t3 <- query.database(user, password, 'biad', "SELECT * FROM `BIAD`.`Rites`")
tmp <- merge(t3, t2, by='IndividualID')
tmp <- merge(tmp, t1, by='GraveID')
dd <- subset(d, PhaseID%in%unique(tmp$PhaseID))

table <- 'Rites'
svg(file = paste('../tools/plots/time.phases',table,'svg',sep='.'), width = pwidth, height = pheight )
par(mfrow=c(2,5))
for(n in 1:N){
	i <- dd$date<=zposts[n] & dd$date>zposts[n+1]
	data <- dd[i,]
	main <- paste(n,': ',table,': ',zposts[n],' to ',zposts[n+1],'BP (',zposts[n]-zposts[n+1],' span)',sep='')
	plot(NULL,xlim=xlim,ylim=ylim,frame.plot=F,axes=F, xlab='',ylab='',main=main)
	map('world',xlim=xlim,ylim=ylim,col='grey90',add=T, fill=T, border='grey')
	points(data$Longitude, data$Latitude, col='steelblue', pch=16,cex=1)
	#legend('topleft',res$legend,bty='n',col=res$cols,pch=16,cex=0.7)
	}
dev.off()
#--------------------------------------------------------------------------------------------------
t1 <- query.database(user, password, 'biad', "SELECT * FROM `BIAD`.`Graves`")
t2 <- query.database(user, password, 'biad', "SELECT * FROM `BIAD`.`GraveIndividuals`")
t3 <- query.database(user, password, 'biad', "SELECT * FROM `BIAD`.`Strontium`")
tmp <- merge(t3, t2, by='IndividualID')
tmp <- merge(tmp, t1, by='GraveID')
dd <- subset(d, PhaseID%in%unique(tmp$PhaseID))

table <- 'Strontium'
svg(file = paste('../tools/plots/time.phases',table,'svg',sep='.'), width = pwidth, height = pheight )
par(mfrow=c(2,5))
for(n in 1:N){
	i <- dd$date<=zposts[n] & dd$date>zposts[n+1]
	data <- dd[i,]
	main <- paste(n,': ',table,': ',zposts[n],' to ',zposts[n+1],'BP (',zposts[n]-zposts[n+1],' span)',sep='')
	plot(NULL,xlim=xlim,ylim=ylim,frame.plot=F,axes=F, xlab='',ylab='',main=main)
	map('world',xlim=xlim,ylim=ylim,col='grey90',add=T, fill=T, border='grey')
	points(data$Longitude, data$Latitude, col='steelblue', pch=16,cex=1)
	#legend('topleft',res$legend,bty='n',col=res$cols,pch=16,cex=0.7)
	}
dev.off()
#--------------------------------------------------------------------------------------------------
t1 <- query.database(user, password, 'biad', "SELECT * FROM `BIAD`.`Graves`")
t2 <- query.database(user, password, 'biad', "SELECT * FROM `BIAD`.`GraveIndividuals`")
t3 <- query.database(user, password, 'biad', "SELECT * FROM `BIAD`.`Strontium`")
t4 <- query.database(user, password, 'biad', "SELECT * FROM `BIAD`.`StrontiumSequences`")
tmp <- merge(t4, t3, by='StrontiumID')
tmp <- merge(tmp, t2, by='IndividualID')
tmp <- merge(tmp, t1, by='GraveID')
dd <- subset(d, PhaseID%in%unique(tmp$PhaseID))

table <- 'StrontiumSequences'
svg(file = paste('../tools/plots/time.phases',table,'svg',sep='.'), width = pwidth, height = pheight )
par(mfrow=c(2,5))
for(n in 1:N){
	i <- dd$date<=zposts[n] & dd$date>zposts[n+1]
	data <- dd[i,]
	main <- paste(n,': ',table,': ',zposts[n],' to ',zposts[n+1],'BP (',zposts[n]-zposts[n+1],' span)',sep='')
	plot(NULL,xlim=xlim,ylim=ylim,frame.plot=F,axes=F, xlab='',ylab='',main=main)
	map('world',xlim=xlim,ylim=ylim,col='grey90',add=T, fill=T, border='grey')
	points(data$Longitude, data$Latitude, col='steelblue', pch=16,cex=1)
	#legend('topleft',res$legend,bty='n',col=res$cols,pch=16,cex=0.7)
	}
dev.off()
#--------------------------------------------------------------------------------------------------
sql.command <- "SELECT `Sites`.`SiteID`,`Longitude`,`Latitude`,`aDNAID` FROM `Sites`
INNER JOIN `Phases` ON `Sites`.`SiteID`=`Phases`.`SiteID`
INNER JOIN `Graves` ON `Phases`.`PhaseID`=`Graves`.`PhaseID`
INNER JOIN `GraveIndividuals` ON `GraveIndividuals`.`GraveID`=`Graves`.`GraveID`
WHERE `GraveIndividuals`.`aDNAID` IS NOT NULL;"

tmp <- query.database(user, password, 'biad', sql.command)
dd <- subset(d, PhaseID%in%unique(tmp$PhaseID))

table <- 'aDNA'
svg(file = paste('../tools/plots/time.phases',table,'svg',sep='.'), width = pwidth, height = pheight )
par(mfrow=c(2,5))
for(n in 1:N){
	i <- dd$date<=zposts[n] & dd$date>zposts[n+1]
	data <- dd[i,]
	main <- paste(n,': ',table,': ',zposts[n],' to ',zposts[n+1],'BP (',zposts[n]-zposts[n+1],' span)',sep='')
	plot(NULL,xlim=xlim,ylim=ylim,frame.plot=F,axes=F, xlab='',ylab='',main=main)
	map('world',xlim=xlim,ylim=ylim,col='grey90',add=T, fill=T, border='grey')
	points(data$Longitude, data$Latitude, col='steelblue', pch=16,cex=1)
	#legend('topleft',res$legend,bty='n',col=res$cols,pch=16,cex=0.7)
	}
dev.off()
#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

