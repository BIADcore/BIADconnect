
#--------------------------------------------------------------------------------------
# chronology on all phases
#--------------------------------------------------------------------------------------
# To do:
# Prioritise phases that haven't been done yet
# Change loop structure so it can process norm and ellipsoid models together
# Structure prior(s) in a Bayesian chain, using parameters derived empirically from the entire database
# Ensure resolution and range of prior is compatible with different resolution and range of previous posteriors

#--------------------------------------------------------------------------------------
model.folder.gaussian <- '../../phase model posteriors/gaussian'
model.folder.ellipsoid <- '../../phase model posteriors/ellipsoid'
library(ADMUR)
#--------------------------------------------------------------------------------------
sit <- query.database(user, password, 'biad',"SELECT * FROM `Sites`;")
pha <- query.database(user, password, 'biad',"SELECT * FROM `Phases`;")
c14 <- query.database(user, password, 'biad',"SELECT `PhaseID`,`SiteID`,`C14.Age`,`C14.SD` FROM `C14Samples`;")
pha <- merge(pha,sit,by='SiteID', all.y=FALSE)
c14 <- subset(c14, !is.na(PhaseID))

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

N <- 3000
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








