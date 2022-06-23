#------------------------------------------------------------------
require(maptools)
require(mapdata)
#------------------------------------------------------------------
sql.command <- "SELECT * FROM BIAD.Sites"
d <- sql.wrapper(sql.command,user,password)
#------------------------------------------------------------------
xmn <- min(d$Longitude, na.rm=T)
xmx <- max(d$Longitude, na.rm=T)
ymn <- min(d$Latitude, na.rm=T)
ymx <- max(d$Latitude, na.rm=T)
#------------------------------------------------------------------
svg(file = '../tools/plots/map.svg', width = 16, height = 9 )
par(mar=c(0,0,0,0))
plot(NULL,xlim=c(xmn,xmx),ylim=c(ymn,ymx),bty='n',xaxs='i',yaxs='i',main='',xlab='',ylab='',xaxt='n',yaxt='n')
polygon(x=c(xmn,xmx,xmx,xmn),y=c(ymn,ymn,ymx,ymx),col='steelblue',border=NA)
map('world',xlim=c(xmn,xmx),ylim=c(ymn,ymx),col='grey',add=T, fill=T, border='grey90')
points(d$Longitude,d$Latitude, col='firebrick',pch=20, cex=1)
dev.off()
#------------------------------------------------------------------
	
	
	
	