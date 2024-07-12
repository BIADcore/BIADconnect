
#--------------------------------------------------------------------------------------
# testing new approach
#--------------------------------------------------------------------------------------
library(ADMUR)

sit <- query.database(user, password, 'biad',"SELECT * FROM `Sites`;")
pha <- query.database(user, password, 'biad',"SELECT * FROM `Phases`;")
c14 <- query.database(user, password, 'biad',"SELECT `PhaseID`,`SiteID`,`C14.Age`,`C14.SD` FROM `C14Samples`;")
pha <- merge(pha,sit,by='SiteID', all.y=FALSE)
c14 <- subset(c14, !is.na(PhaseID))
#--------------------------------------------------------------------------------------
# estimate bandwidth for later



#--------------------------------------------------------------------------------------

# start with a uniform prior, using log mean and log sigma, as neither can be negative
		res <- 100
		mu <- seq(5,11,length.out=res)
		sigma <- seq(3,9,length.out=res)
		mu.prob <- rep(1/res,res)
		sigma.prob <- rep(1/res,res)

# extract info from local phases

	i <- sample(1:nrow(pha),size=1)
	phase <- pha[i,]

	# get other phases with same culture and period, within 100km
	cultures <- phase[,c('Culture1','Culture2','Culture3')]
	cultures <- cultures[!is.na(cultures)]
	near.phases <- subset(pha, Culture1%in%cultures & Period%in%phase$Period & PhaseID!=phase$PhaseID)
	if(nrow(near.phases)>0){
		near.phases$dist <- slc(x=phase$Longitude, y=phase$Latitude, ax=near.phases$Longitude, ay=near.phases$Latitude, input='deg') * 6378.1
		near.phases <- subset(near.phases,dist<100)
		}

	# if no other phases available, get phases with same period within 100km
	if(nrow(near.phases)==0){
		near.phases <- subset(pha, Period%in%phase$Period & PhaseID!=phase$PhaseID)
		if(nrow(near.phases)>0){
			near.phases$dist <- slc(x=phase$Longitude, y=phase$Latitude, ax=near.phases$Longitude, ay=near.phases$Latitude, input='deg') * 6378.1
			near.phases <- subset(near.phases,dist<100)
			}
		}

	local.mu <- near.phases$GMM
	local.mu <- local.mu[!is.na(local.mu)]
	local.sigma <- near.phases$GMS
	local.sigma <- local.sigma[!is.na(local.sigma)]
	
	# If there are any local estimates, use them to update prior non-parameterically, using kernel density
	# Note, mu and sigma are independent at this stage
	# If there is only one local phase, bandwidth cannot be calculated automatically from data, so for now use 0.1 for mu and sigma
#	local.mu <- c(5000, 5400, 7200)
#	local.sigma <- c(300, 400, 350)
	NL <- length(local.mu)

	# get phase c14 dates
	d <- subset(c14, PhaseID==phase$PhaseID)
	data <- data.frame(age=d$C14.Age, sd=d$C14.SD)

	if(nrow(data)==0 & NL<3){
		# Do not store posterior estimates if zero 14C dates AND less than 3 local phases
		mu.range <- NA
		}
	if(nrow(data)==0) & NL>=3){
		mu.range <- range(local.mu) + c(-0.5,0.5)
		sigma.range <- range(local.sigma) + c(-0.5,0.5)	
		d.mu <- density(log(local.mu),from=mu.range[1],to=mu.range[2],n=res)
		d.sigma <- density(log(local.mu),from=mu.sigma[1],to=mu.sigma[2],n=res)		
		}
	if(nrow(data)>0 & NL==0){
		mu.range <- log(estimateDataDomain(data, calcurve=intcal20)) + c(-0.5,0.5)
		}
	if(nrow(data)>0 & NL>1){
		m1 <- range(local.mu) 
		m2 <- log(estimateDataDomain(data, calcurve=intcal20))
		mu.range <- c(min(m1[1],m2[1]),max(m1[2],m2[2])) + c(-0.5,0.5)
		}
		

	if(NL==1)bw <- "nrd0"
	if(NL>1)bw <- 0.1
mu.range
		

	if(length(local.mu)==1)d.mu <- density(log(local.mu),bw=0.1)
	if(length(local.sigma)==1)d.sigma <- density(log(local.sigma),bw=0.1)
	if(length(local.mu)>1)d.mu <- density(log(local.mu))
	if(length(local.sigma)>1)d.sigma <- density(log(local.sigma))
	



###########################################################
# parameters domain and prior probabilities for phase modelled as Gaussian
# note, log parameters, as both mean and sig cannot be negative 

res <- 50

mu <- seq(5,11,length.out=res)
sigma <- seq(3,9,length.out=res)
mu.error <- seq(2,8,length.out=res)
sigma.error <- seq(2,8, length.out=res)

prior <- data.frame(mu=mu,
	mu.prob = rep(1,res)/res,
	sigma = sigma,
	sigma.prob = rep(1,res)/res)

# extract info from local phases

	i <- sample(1:nrow(pha),size=1)
	phase <- pha[i,]

	# get other phases with same culture and period, within 100km
	cultures <- phase[,c('Culture1','Culture2','Culture3')]
	cultures <- cultures[!is.na(cultures)]
	near.phases <- subset(pha, Culture1%in%cultures & Period%in%phase$Period & PhaseID!=phase$PhaseID)
	if(nrow(near.phases)>0){
		near.phases$dist <- slc(x=phase$Longitude, y=phase$Latitude, ax=near.phases$Longitude, ay=near.phases$Latitude, input='deg') * 6378.1
		near.phases <- subset(near.phases,dist<100)
		}

	# if no other phases available, get phases with same period within 100km
	if(nrow(near.phases)==0){
		near.phases <- subset(pha, Period%in%phase$Period & PhaseID!=phase$PhaseID)
		if(nrow(near.phases)>0){
			near.phases$dist <- slc(x=phase$Longitude, y=phase$Latitude, ax=near.phases$Longitude, ay=near.phases$Latitude, input='deg') * 6378.1
			near.phases <- subset(near.phases,dist<100)
			}
		}

	local.mu <- near.phases$GMM
	local.mu <- local.mu[!is.na(local.mu)]
	local.sigma <- near.phases$GMS
	local.sigma <- local.sigma[!is.na(local.sigma)]

	# likelihoods from local phases
	# i.e., if we have one local phase, we can only improve our prior a tiny bit




	# the problem here is the mean of local phases informs on the mean of the target phase
	# but the SD of local phases is irrelevant - each phase has a SD. This is not the uncertainty, but the genuine spread!
	# So maybe we should have independent parameters?
	# start with two independent priors...
	# if local phases are available, update prior, such that the uncertainty on mean and separately the sd are dealt with


	# log probabilities updated by local phases

	gaus.mean.lik <- matrix(0,res,res)
	gaus.sd.lik <- matrix(0,res,res)
	
	if(length(local.mu)>0){
		for(r in 1:res){
			for(c in 1:res){
				gaus.mean.lik[r,c] <- sum(dnorm(log(local.mu), gaus.mean.mu.vector[r], gaus.mean.sig.vector[c], log=TRUE),na.rm=TRUE)
				gaus.sd.lik[r,c] <- sum(dnorm(log(local.sig), gaus.sd.mu.vector[r], gaus.sd.sig.vector[c], log=TRUE),na.rm=TRUE)
				}
			}
		}

	# combine prior and likelihood, and normalise to posterior
	gaus.mean.posterior <- exp(log(gaus.mean.prior) + gaus.mean.lik)
	gaus.mean.posterior <- gaus.mean.posterior/sum(gaus.mean.posterior)
	gaus.sd.posterior <- exp(log(gaus.sd.prior) + gaus.sd.lik)
	gaus.sd.posterior <- gaus.sd.posterior/sum(gaus.sd.posterior)

	# posterior becomes new prior, to be updated by target phase data
	gaus.mean.prior <- gaus.mean.posterior
	gaus.sd.prior <- gaus.sd.posterior

	# get phase c14 dates
	d <- subset(c14, PhaseID==phase$PhaseID)
	data <- data.frame(age=d$C14.Age, sd=d$C14.SD)

	# generate the phase model
	# phaseModel needs rebuilding as there are two independent prior matrices: the mean and the sd
	mod.gaussian <- phaseModel(data, calcurve=intcal20, prior.matrix=prior.matrix.gaussian, model='norm', plot = FALSE)


	# should probably output marginal 99 CI, to better inform domain








##################

# create a prior probability surface
prior.matrix.initial <- matrix(1,200,200); prior.matrix.initial <- prior.matrix.initial/sum(prior.matrix.initial)

# gaussian
mu.range <- c(500,40000)
sigma.range <- c(10,1000)
prior.matrix.gaussian.initial <- prior.matrix.initial
row.names(prior.matrix.gaussian.initial) <- seq(min(mu.range),max(mu.range),length.out=nrow(prior.matrix.gaussian.initial))
colnames(prior.matrix.gaussian.initial) <- seq(min(sigma.range),max(sigma.range),length.out=ncol(prior.matrix.gaussian.initial))

# ellipsoid
min.range <- c(500,40000)
duration.range <- c(10,4000)
prior.matrix.ellipsoid.initial <- prior.matrix.initial
row.names(prior.matrix.ellipsoid.initial) <- seq(min(min.range),max(min.range),length.out=nrow(prior.matrix.ellipsoid.initial))
colnames(prior.matrix.ellipsoid.initial) <- seq(min(duration.range),max(duration.range),length.out=ncol(prior.matrix.ellipsoid.initial))

N <- 500
for(n in 1:N){

	i <- sample(1:nrow(pha),size=1)
	phase <- pha[i,]

	# get other phases with same culture and period, within 100km
	cultures <- phase[,c('Culture1','Culture2','Culture3')]
	cultures <- cultures[!is.na(cultures)]
	near.phases <- subset(pha, Culture1%in%cultures & Period%in%phase$Period & PhaseID!=phase$PhaseID)
	if(nrow(near.phases)>0){
		near.phases$dist <- slc(x=phase$Longitude, y=phase$Latitude, ax=near.phases$Longitude, ay=near.phases$Latitude, input='deg') * 6378.1
		near.phases <- subset(near.phases,dist<100)
		}

	# if no other phases available, get phases with same period within 100km
	if(nrow(near.phases)==0){
		near.phases <- subset(pha, Period%in%phase$Period & PhaseID!=phase$PhaseID)
		if(nrow(near.phases)>0){
			near.phases$dist <- slc(x=phase$Longitude, y=phase$Latitude, ax=near.phases$Longitude, ay=near.phases$Latitude, input='deg') * 6378.1
			near.phases <- subset(near.phases,dist<100)
			}
		}

	# reduce to informative phases (that have a posterior model already)
	already.gaussian <- gsub('.RData','',list.files(model.folder.gaussian))
	near.phases.gaussian <- near.phases[near.phases$PhaseID%in%already.gaussian,]

	already.ellipsoid <- gsub('.RData','',list.files(model.folder.ellipsoid))
	near.phases.ellipsoid <- near.phases[near.phases$PhaseID%in%already.ellipsoid,]

	# generate the new prior from the posteriors of the near phases
	# gaussian
	NP <- nrow(near.phases.gaussian)
	prior.matrix <- prior.matrix.gaussian.initial
	if(NP>0)for(np in 1:NP){
		load(paste(model.folder.gaussian,paste(near.phases.gaussian$PhaseID[np],'RData',sep='.'),sep='/'))
		if(!is.nan(sum(mod$posterior)))prior.matrix <- prior.matrix + mod$posterior
		}
	prior.matrix.gaussian <- prior.matrix/sum(prior.matrix)

	# ellipsoid
	NP <- nrow(near.phases.ellipsoid)
	prior.matrix <- prior.matrix.ellipsoid.initial
	if(NP>0)for(np in 1:NP){
		load(paste(model.folder.ellipsoid,paste(near.phases.ellipsoid$PhaseID[np],'RData',sep='.'),sep='/'))
		if(!is.nan(sum(mod$posterior)))prior.matrix <- prior.matrix + mod$posterior
		}
	prior.matrix.ellipsoid <- prior.matrix/sum(prior.matrix)

	# get phase c14 dates
	d <- subset(c14, PhaseID==phase$PhaseID)
	data <- data.frame(age=d$C14.Age, sd=d$C14.SD)

	# generate the phase model
	mod.gaussian <- phaseModel(data, calcurve=intcal20, prior.matrix=prior.matrix.gaussian, model='norm', plot = FALSE)
	mod.ellipsoid <- phaseModel(data, calcurve=intcal20, prior.matrix=prior.matrix.ellipsoid, model='ellipse', plot = FALSE)

	# save the models
	mod <- mod.gaussian
	save(mod, file=paste(model.folder.gaussian,paste(phase$PhaseID,'RData',sep='.'),sep='/'))
	mod <- mod.ellipsoid
	save(mod, file=paste(model.folder.ellipsoid,paste(phase$PhaseID,'RData',sep='.'),sep='/'))

	# add point estimates to the database if the posterior is reasonably tight	
	# gaussian
	cond <- max(mod.gaussian$posterior)/mean(mod.gaussian$posterior)>10
	if(cond){
		sql.command <- paste("UPDATE `BIAD`.`Phases` SET `gaussianModelMu`=",mod.gaussian$mu,", `gaussianModelSigma`=",mod.gaussian$sigma," WHERE `PhaseID`='",phase$PhaseID,"';",sep='')
		query.database(user, password, 'biad',sql.command)
		}

	# ellipsoid
	cond <- max(mod.ellipsoid$posterior)/mean(mod.ellipsoid$posterior)>10
	if(cond){
		sql.command <- paste("UPDATE `BIAD`.`Phases` SET `ellipsoidModelMin`=",mod.ellipsoid$min,", `ellipsoidModelDuration`=",mod.ellipsoid$duration," WHERE `PhaseID`='",phase$PhaseID,"';",sep='')
		query.database(user, password, 'biad',sql.command)
		}
	}
#-----------------------------------------------------------------------------------------


