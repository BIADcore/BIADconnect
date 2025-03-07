
#--------------------------------------------------------------------------------------
# Currently implemented:
# local phases inform on a prior
# include all phases with same culture/period as local, weighted by distance. 
# prioritise phases with no modelled chronology yet

# Still to implement:
# use log mean and log sigma, as neither can be negative
# add ellipsoid model
# increase resolution
# Upgrade distance weighting to include friction surface distance
# adjust for sequential phases
#--------------------------------------------------------------------------------------
library(ADMUR)
res <- 250
conn <- init.conn()
sit <- query.database(conn = conn, sql.command = "SELECT * FROM `Sites`;")
pha <- query.database(conn = conn, sql.command = "SELECT * FROM `Phases`;")
c14 <- query.database(conn = conn, sql.command = "SELECT `PhaseID`,`SiteID`,`C14.Age`,`C14.SD` FROM `C14Samples`;")
pha <- merge(pha,sit,by='SiteID', all.y=FALSE)
c14 <- subset(c14, !is.na(PhaseID))

# prioritise phases with no modelled chronology yet
priority <- rep(1,nrow(pha))
priority[is.na(pha$GMM)] <- 2
#--------------------------------------------------------------------------------------
N <- 800 #2000
mu.bw <- sigma.bw <- c()
for(n in 1:N){

	i <- sample(1:nrow(pha),size=1,prob=priority)
	phase <- pha[i,]

	# get other phases with same culture and period, and have a model estimate
	cultures <- phase[,c('Culture1','Culture2','Culture3')]
	cultures <- cultures[!is.na(cultures)]
	near.phases <- subset(pha, Culture1%in%cultures & Period%in%phase$Period & PhaseID!=phase$PhaseID)
	near.phases <- subset(near.phases, !is.na(GMM))
	if(nrow(near.phases)>0){
		near.phases$dist <- slc(x=phase$Longitude, y=phase$Latitude, ax=near.phases$Longitude, ay=near.phases$Latitude, input='deg') * 6378.1
		}

	# if almost no phases available, get phases with same period
	if(nrow(near.phases)==0){
		near.phases <- subset(pha, Period%in%phase$Period & PhaseID!=phase$PhaseID)
		near.phases <- subset(near.phases, !is.na(GMM))
		if(nrow(near.phases)>0){
			near.phases$dist <- slc(x=phase$Longitude, y=phase$Latitude, ax=near.phases$Longitude, ay=near.phases$Latitude, input='deg') * 6378.1
			}
		}

	# weighting by distance from target site using a Gaussian, requires a parameter
	if(nrow(near.phases)!=0){
		weights <- dnorm(near.phases$dist, 0 , 100)
		local.mu <- near.phases$GMM
		local.sigma <- near.phases$GMS
		i <- !is.na(local.mu) & weights!=0
		local.mu <- local.mu[i]
		local.sigma <- local.sigma[i]
		weights <- weights[i]
		weights <- weights/sum(weights)
		NL <- length(weights)
		}
	if(nrow(near.phases)==0)NL <- 0

	# get phase c14 dates
	d <- subset(c14, PhaseID==phase$PhaseID)
	data <- data.frame(age=d$C14.Age, sd=d$C14.SD)

	# remove outlier C14 dates retaining dates 3x the width of the 50% quantile
	if(nrow(data)>1){
		q <- quantile(data$age,prob=c(0.25,0.5,0.75))
		h <- (q[3]-q[1])*1.5
		r <- q[2] + c(-h,h)
		i <- data$age>r[1] & data$age<r[2]
		data <- data[i,]
		}		

	# Update chronology according to following rules. Note mu and sigma are independent in the prior. 

	if(nrow(data)==0 & NL==0){
		# no useful information. Ensure value is null
		mu <- 'NULL '
		sigma <- 'NULL '
		}

	if(nrow(data)==0 & NL>0){
		# no local 14C, so just use the weighted mean estimates from the local phases
		mu <- round(sum(local.mu * weights))
		sigma <- round(sum(local.sigma * weights))	
		}

	if(nrow(data)>0 & NL==0){
		# no local phases available, so use a uniform prior across a wider range than the 14c data, to account for low number of samples
		mu.range <- estimateDataDomain(data, calcurve=intcal20) + c(-1000,1000)
		sigma.range <- c(diff(mu.range)/10, diff(mu.range)/3)
		prior.matrix <- matrix(1/(res^2),res,res)
		row.names(prior.matrix) <- seq(min(mu.range),max(mu.range),length.out=res)
		colnames(prior.matrix) <- seq(min(sigma.range),max(sigma.range),length.out=res)
		mod.gaussian <- phaseModel(data, calcurve=intcal20, prior.matrix=prior.matrix, model='norm', plot = FALSE)
		mu <- mod.gaussian$mu
		sigma <- mod.gaussian$sigma
		}

	if(nrow(data)>0 & NL==1){
		# only one local phase available, bandwidth cannot be calculated automatically
		m1 <- estimateDataDomain(data, calcurve=intcal20) + c(-1000,1000)		
		m2 <- range(local.mu) 
		mu.range <- c(min(m1[1],m2[1]),max(m1[2],m2[2]))	
		s1 <- c(diff(m1)/10, diff(m2)/3)	
		s2 <- range(local.sigma)
		sigma.range <- c(min(s1[1],s2[1]),max(s1[2],s2[2]))
		d.mu <- density(local.mu,from=mu.range[1],to=mu.range[2],n=res, bw=100)
		d.sigma <- density(local.sigma,from=sigma.range[1],to=sigma.range[2],n=res, bw=25)			
		prior.matrix <- matrix(d.mu$y,res,res) * t(matrix(d.sigma$y,res,res))
		prior.matrix <- prior.matrix/sum(prior.matrix)
		row.names(prior.matrix) <- d.mu$x
		colnames(prior.matrix) <- d.sigma$x
		mod.gaussian <- phaseModel(data, calcurve=intcal20, prior.matrix=prior.matrix, model='norm', plot = FALSE)
		mu <- mod.gaussian$mu
		sigma <- mod.gaussian$sigma
		}

	if(nrow(data)>0 & NL>1){
		# bandwidth can be calculated automatically. Print, to assist choosing a bandwdith for previous codeblock		
		m1 <- estimateDataDomain(data, calcurve=intcal20)
		m2 <- range(local.mu) 
		mu.range <- c(min(m1[1],m2[1]),max(m1[2],m2[2])) + c(-1000,1000)
		s1 <- c(diff(m1)/10, diff(m2)/3)	
		s2 <- range(local.sigma)
		sigma.range <- c(min(s1[1],s2[1]),max(s1[2],s2[2]))
		d.mu <- density(local.mu,from=mu.range[1],to=mu.range[2],n=res, weights=weights)
		d.sigma <- density(local.sigma,from=sigma.range[1],to=sigma.range[2],n=res, weights=weights)
		mu.bw <- c(mu.bw,d.mu$bw)
		sigma.bw <- c(sigma.bw,d.sigma$bw)
		prior.matrix <- matrix(d.mu$y,res,res) * t(matrix(d.sigma$y,res,res))
		prior.matrix <- prior.matrix/sum(prior.matrix)
		row.names(prior.matrix) <- d.mu$x
		colnames(prior.matrix) <- d.sigma$x
		mod.gaussian <- phaseModel(data, calcurve=intcal20, prior.matrix=prior.matrix, model='norm', plot = FALSE)
		mu <- mod.gaussian$mu
		sigma <- mod.gaussian$sigma
		}

	sql.command <- paste("UPDATE `BIAD`.`Phases` SET `GMM`=",mu,", `GMS`=",sigma," WHERE `PhaseID`='",phase$PhaseID,"';",sep='')
	if(!is.nan(mu) & !is.nan(sigma))query.database(conn=conn,sql.command=sql.command)
	}
#--------------------------------------------------------------------------------------
print(paste('mu.bw mean:', round(mean(mu.bw,na.rm=T),1)))
print(paste('mu.bw SD:', round(sd(mu.bw,na.rm=T),1)))
print(paste('sigma.bw mean:', round(mean(sigma.bw,na.rm=T),1)))
print(paste('sigma.bw SD:', round(sd(sigma.bw,na.rm=T),1)))
#--------------------------------------------------------------------------------------
disconnect()
#--------------------------------------------------------------------------------------


