#----------------------------------------------------------------------------------------------------------------------------------------------
# Osteometric age groupings 
#----------------------------------------------------------------------------------------------------------------------------------------------
label <- c('neonate','infant','subadult','adult: young','adult: mature','adult: elderly')
int <- c(5,10,20,30,25,10)
d <- data.frame(label,int)
d$end <- cumsum(d$int)/sum(d$int)
d$start <- d$end - d$int/sum(d$int)

y.inc <- 0.03
y.gap <- 0

group.label <- 
	c('neonate','infant','infant / subadult','infant / subadult / adult: (young)',
	'subadult','subadult / adult: (young)','subadult / adult: (young / mature)','subadult / adult: (young / mature / elderly)',
	'adult: young','adult: (young / mature)','adult: (young / mature / elderly)','adult: (mature)','adult: (mature / elderly)','adult: (elderly)')
group.start.i <- c(1,2,2,2,3,3,3,3,4,4,4,5,5,6)
group.end.i   <- c(1,2,3,4,3,4,5,6,4,5,6,5,6,6)
groups <- data.frame(label=group.label,start=d$start[group.start.i],end=d$end[group.end.i])
# groups <- groups[order(groups$start + groups$end),]
#----------------------------------------------------------------------------------------------------------------------------------------------
png('../tools/plots/age.groups.1.plot.png',width=1500,height=750)
par(mar=c(1,1,1,1))
plot(NULL, xlim=c(0,1),ylim=c(0,0.5),xlab='',ylab='',xaxt='n',yaxt='n',bty='n')

y.top <- 0.5
for(n in 1:nrow(d)){
	rect(d$start[n],y.top-y.inc,d$end[n],y.top,border='firebrick',lwd=2,col=scales::alpha('firebrick',alpha=0.1))
	text(x=(d$start[n]+d$end[n])/2,y=y.top - y.inc/2, d$label[n],cex=1, col='firebrick')
	}

y.bottom <- y.top - 2*y.inc
for(n in 1:nrow(groups)){
	y.top <- y.bottom - y.gap 
	y.bottom <- y.top - y.inc
	rect(groups$start[n],y.bottom,groups$end[n],y.top,border='steelblue',lwd=1,col=scales::alpha('steelblue',alpha=0.1))
	text(x=(groups$start[n]+groups$end[n])/2,y=y.top - y.inc/2, groups$label[n],cex=1, col='steelblue')
	}
dev.off()
#----------------------------------------------------------------------------------------------------------------------------------------------