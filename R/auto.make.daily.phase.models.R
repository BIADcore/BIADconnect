
#--------------------------------------------------------------------------------------
# testing new approach
# use log mean and log sigma, as neither can be negative
# add ellipsoid model
# slightly prioritise phases with no data yet
# increase resolution
#--------------------------------------------------------------------------------------
library(ADMUR)
res <- 200

sit <- query.database(user, password, 'biad',"SELECT * FROM `Sites`;")
pha <- query.database(user, password, 'biad',"SELECT * FROM `Phases`;")
c14 <- query.database(user, password, 'biad',"SELECT `PhaseID`,`SiteID`,`C14.Age`,`C14.SD` FROM `C14Samples`;")
pha <- merge(pha,sit,by='SiteID', all.y=FALSE)
c14 <- subset(c14, !is.na(PhaseID))
#--------------------------------------------------------------------------------------
N <- 1000
mu.bw <- sigma.bw <- c()
for(n in 1:N){

	i <- sample(1:nrow(pha),size=1)
	phase <- pha[i,]

	print(phase$PhaseID)

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
	NL <- length(local.mu)

	# get phase c14 dates
	d <- subset(c14, PhaseID==phase$PhaseID)
	data <- data.frame(age=d$C14.Age, sd=d$C14.SD)

	# Do not store posterior estimates if zero 14C dates AND less than 3 local phases
	# If there are any local estimates, use them to update prior non-parameterically (kernel density)
	# Bandwidth calculated automatically from the data, except if there is only one local phase
	# Note mu and sigma are independent in the prior. 

	if(nrow(data)==0 & NL>=3){
		# no local 14C, so just use the mean estimates from the local phases
		mu <- mean(local.mu)
		sigma <- mean(local.sigma)	
		sql.command <- paste("UPDATE `BIAD`.`Phases` SET `GMM`=",mu,", `GMS`=",sigma," WHERE `PhaseID`='",phase$PhaseID,"';",sep='')
		query.database(user, password, 'biad',sql.command)	
		}

	if(nrow(data)>0 & NL==0){
		# no local phases available, so use a uniform prior across a wider range than the 14c data, to account for low number of samples
		mu.range <- estimateDataDomain(data, calcurve=intcal20) + c(-500,500)
		sigma.range <- c(diff(mu.range)/10, diff(mu.range)/3)
		prior.matrix <- matrix(1/(res^2),res,res)
		row.names(prior.matrix) <- seq(min(mu.range),max(mu.range),length.out=res)
		colnames(prior.matrix) <- seq(min(sigma.range),max(sigma.range),length.out=res)
		mod.gaussian <- phaseModel(data, calcurve=intcal20, prior.matrix=prior.matrix, model='norm', plot = FALSE)
		mu <- mod.gaussian$mu
		sigma <- mod.gaussian$sigma
		sql.command <- paste("UPDATE `BIAD`.`Phases` SET `GMM`=",mu,", `GMS`=",sigma," WHERE `PhaseID`='",phase$PhaseID,"';",sep='')
		query.database(user, password, 'biad',sql.command)
		}

	if(nrow(data)>0 & NL==1){
		# only one local phase available, bandwidth cannot be calculated automatically
		m1 <- estimateDataDomain(data, calcurve=intcal20) + c(-500,500)		
		m2 <- range(local.mu) 
		mu.range <- c(min(m1[1],m2[1]),max(m1[2],m2[2]))	
		s1 <- c(diff(m1)/10, diff(m2)/3)	
		s2 <- range(local.sigma)
		sigma.range <- c(min(s1[1],s2[1]),max(s1[2],s2[2]))

		print(mu.range)
		print(sigma.range)
		
		d.mu <- density(local.mu,from=mu.range[1],to=mu.range[2],n=res, bw=100)
		d.sigma <- density(local.sigma,from=sigma.range[1],to=sigma.range[2],n=res, bw=30)			
		prior.matrix <- matrix(d.mu$y,res,res) * t(matrix(d.sigma$y,res,res))
		prior.matrix <- prior.matrix/sum(prior.matrix)
		row.names(prior.matrix) <- d.mu$x
		colnames(prior.matrix) <- d.sigma$x
		mod.gaussian <- phaseModel(data, calcurve=intcal20, prior.matrix=prior.matrix, model='norm', plot = FALSE)
		mu <- mod.gaussian$mu
		sigma <- mod.gaussian$sigma
		sql.command <- paste("UPDATE `BIAD`.`Phases` SET `GMM`=",mu,", `GMS`=",sigma," WHERE `PhaseID`='",phase$PhaseID,"';",sep='')
		query.database(user, password, 'biad',sql.command)
		}

	if(nrow(data)>0 & NL>1){
		# bandwidth be calculated automatically. Print, to assist choosing a bandwdith for previous codeblock		
		m1 <- estimateDataDomain(data, calcurve=intcal20)
		m2 <- range(local.mu) 
		mu.range <- c(min(m1[1],m2[1]),max(m1[2],m2[2])) + c(-500,500)
		s1 <- c(diff(m1)/10, diff(m2)/3)	
		s2 <- range(local.sigma)
		sigma.range <- c(min(s1[1],s2[1]),max(s1[2],s2[2]))


		print(mu.range)
		print(sigma.range)
		

		d.mu <- density(local.mu,from=mu.range[1],to=mu.range[2],n=res)
		d.sigma <- density(local.sigma,from=sigma.range[1],to=sigma.range[2],n=res)
		mu.bw <- c(mu.bw,d.mu$bw)
		sigma.bw <- c(sigma.bw,d.sigma$bw)
		prior.matrix <- matrix(d.mu$y,res,res) * t(matrix(d.sigma$y,res,res))
		prior.matrix <- prior.matrix/sum(prior.matrix)
		row.names(prior.matrix) <- d.mu$x
		colnames(prior.matrix) <- d.sigma$x
		mod.gaussian <- phaseModel(data, calcurve=intcal20, prior.matrix=prior.matrix, model='norm', plot = FALSE)
		mu <- mod.gaussian$mu
		sigma <- mod.gaussian$sigma
		sql.command <- paste("UPDATE `BIAD`.`Phases` SET `GMM`=",mu,", `GMS`=",sigma," WHERE `PhaseID`='",phase$PhaseID,"';",sep='')
		query.database(user, password, 'biad',sql.command)
		}
	}
print(mean(mu.bw,na.rm=T))
print(mean(sigma.bw,na.rm=T))




