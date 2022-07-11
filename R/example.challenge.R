#------------------------------------------------------------------
# Example R script for directly querying BIAD using the RMySQL package
#------------------------------------------------------------------
# First obtain the following objects from the BIAD administrator.
# Put them in a .Rprofile file, in the same folder that this script is in.
#------------------------------------------------------------------
source('functions.R')

sit <- sql.wrapper("SELECT * FROM `Sites`",user,password,hostname,hostuser,keypath,ssh)
pha <- sql.wrapper("SELECT * FROM `Phases`",user,password,hostname,hostuser,keypath,ssh)
c14 <- sql.wrapper("SELECT * FROM `C14Samples`",user,password,hostname,hostuser,keypath,ssh)
gra <- sql.wrapper("SELECT * FROM `Graves`",user,password,hostname,hostuser,keypath,ssh)

#------------------------------------------------------------------
# loop 
#------------------------------------------------------------------
N <- nrow(sit)
cults <- c14.count <- grave.individuals <- c()

for(n in 1:N){
	
	site <- sit$SiteID[n]
	
	# get c14
	c14.count[n] <- nrow(subset(c14, SiteID==site))
	
	# get cultures
	phase.info <- subset(pha, SiteID==site)
	cultures <- c(phase.info$Culture1, phase.info$Culture2, phase.info$Culture3)
	cultures <- cultures[!is.na(cultures)]
	cults[n] <- paste(cultures, collapse=';')

	# get grave info
	grave.info <- subset(gra, PhaseID%in%phase.info$PhaseID)
	grave.individuals[n] <- sum(grave.info$HumanMNI)
	}


df <- data.frame(SiteID=sit$SiteID, cultures=cults, indivs=grave.individuals)
#------------------------------------------------------------------



