
#--------------------------------------------------------------------------------------
# chronology on all phases
#--------------------------------------------------------------------------------------
model.folder <- '../../phase model posteriors/gaussian'
library(ADMUR)
#--------------------------------------------------------------------------------------
sit <- query.database(user, password, 'biad',"SELECT * FROM `Sites`;")
pha <- query.database(user, password, 'biad',"SELECT * FROM `Phases`;")
c14 <- query.database(user, password, 'biad',"SELECT `PhaseID`,`SiteID`,`C14.Age`,`C14.SD` FROM `C14Samples`;")
pha <- merge(pha,sit,by='SiteID', all.y=FALSE)
c14 <- subset(c14, !is.na(PhaseID))

# create a prior probability surface
mu.range <- c(500,40000)
sigma.range <- c(10,1000)
prior.matrix <- matrix(1,200,200); prior.matrix <- prior.matrix/sum(prior.matrix)
row.names(prior.matrix) <- seq(min(mu.range),max(mu.range),length.out=nrow(prior.matrix))
colnames(prior.matrix) <- seq(min(sigma.range),max(sigma.range),length.out=ncol(prior.matrix))

# prioritise by those without a date estimate yet
# do later

N <- 5
for(n in 1:N){

	# pick a random phase
	# change to the prioritisation order
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

	# reduce to informative phases (that have a phase estimate already)
	near.phases <- subset(near.phases, !is.na(gaussianModelMu))

	# generate the new prior from the posteriors of the near phases
	NP <- nrow(near.phases)
	if(NP>0)for(np in 1:NP){
		load(paste(model.folder,paste(near.phases$PhaseID[np],'RData',sep='.'),sep='/'))
		prior.matrix <- prior.matrix + mod$posterior
		}
	prior.matrix <- prior.matrix/sum(prior.matrix)

	# get phase c14 dates
	d <- subset(c14, PhaseID==phase$PhaseID)
	data <- data.frame(age=d$C14.Age, sd=d$C14.SD)

	# generate the phase model
	mod <- phaseModel(data, calcurve=intcal20, prior.matrix, plot = FALSE)

	# save the model in folder 
	save(mod, file=paste(model.folder,paste(phase$PhaseID,'RData',sep='.'),sep='/'))

	# add point estimates to the database if the posterior is reasonably tight
	cond <- max(mod$posterior)/mean(mod$posterior)>10
	if(cond){
		sql.command <- paste("UPDATE `BIAD`.`Phases` SET `gaussianModelMu`=",mod$mean.mu,", `gaussianModelSigma`=",mod$mean.sigma," WHERE `PhaseID`='",phase$PhaseID,"';",sep='')
		query.database(user, password, 'biad',sql.command)
		}
	print(paste(phase$PhaseID, nrow(near.phases), n,'of',N))
	}
#-----------------------------------------------------------------------------------------
