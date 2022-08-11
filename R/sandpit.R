#-----------------------------------------------------------------------------------------
# general sandpit
#-----------------------------------------------------------------------------------------
source('functions.R')
#-----------------------------------------------------------------------------------------
# possibly amalgamate taxon tables?
#-----------------------------------------------------------------------------------------
fau <- sql.wrapper("SELECT * FROM BIAD.zoptions_FaunalTaxaList",user,password,hostname,hostuser,keypath,ssh)
abo <- sql.wrapper("SELECT * FROM BIAD.zoptions_ABotTaxaList",user,password,hostname,hostuser,keypath,ssh)

a <- fau$TaxonCode
b <- tolower(abo$TaxonCode)
same <- a[a%in%b]
same

subset(fau, TaxonCode%in%same)
subset(abo, TaxonCode%in%toupper(same))
#-----------------------------------------------------------------------------------------
# import problems
#-----------------------------------------------------------------------------------------
new <- read.csv('Isot.csv', encoding='UTF8')
new <- unique(new[,c('PhaseID','CitationID')])
c1 <- c2 <- c()
for(n in 1:nrow(new)){
	both <- strsplit(new$CitationID[n],split='; ')[[1]]
	c1[n] <- both[1]
	c2[n] <- both[2]
	}
new1 <- cbind(new$PhaseID,c1)
new2 <- cbind(new$PhaseID,c2)
comb <- rbind(new1, new2)
comb <- as.data.frame(comb)
comb <- subset(comb, !is.na(c1))
names(comb) <- c('PhaseID','CitationID')
phacit <- sql.wrapper("SELECT * FROM BIAD.PhaseCitation",user,password,hostname,hostuser,keypath,ssh)
phacit <- phacit[,c('PhaseID','CitationID')]

comb.str <- paste(comb[,1],comb[,2],sep='-')
phacit.str <- paste(phacit[,1],phacit[,2],sep='-')
add <- comb.str[!comb.str%in%phacit.str]
add <- as.data.frame(t(matrix(unlist(strsplit(add, split='-')),2,length(add))))
names(add) <- c('PhaseID','CitationID')
write.csv(add,file='toadd.csv',fileEncoding = "UTF-8",row.names=F)

pha <- sql.wrapper("SELECT * FROM BIAD.Phases",user,password,hostname,hostuser,keypath,ssh)
add$PhaseID[!add$PhaseID%in%pha$PhaseID]
#-----------------------------------------------------------------------------------------
abopha <- sql.wrapper("SELECT * FROM BIAD.ABotPhases",user,password,hostname,hostuser,keypath,ssh)
phacit <- sql.wrapper("SELECT * FROM BIAD.PhaseCitation",user,password,hostname,hostuser,keypath,ssh)
abosam <- sql.wrapper("SELECT * FROM BIAD.ABotSamples",user,password,hostname,hostuser,keypath,ssh)

abosam[!is.na(abosam$CitationID),]
#-----------------------------------------------------------------------------------------
gra <- sql.wrapper("SELECT * FROM BIAD.Graves",user,password,hostname,hostuser,keypath,ssh)
ind <- sql.wrapper("SELECT * FROM BIAD.GraveIndividuals",user,password,hostname,hostuser,keypath,ssh)
new <- read.csv('borić2013a_graveindividuals3.csv', encoding='UTF8')

short$IndividualName='aa'
write.csv(short,file='new.csv',fileEncoding = "UTF-8",row.names=F)
unique(new$GraveID[!new$GraveID%in%gra$GraveID])
nchar(new$CitationID)
unique(new$CitationID)
#-----------------------------------------------------------------------------------------
# merge FaunalBones and FaunalBiometrics ino a new FaunalBones
#-----------------------------------------------------------------------------------------
bo <- sql.wrapper("SELECT * FROM BIAD.FaunalBones",user,password,hostname,hostuser,keypath,ssh)
bi <- sql.wrapper("SELECT * FROM BIAD.FaunalBiometrics",user,password,hostname,hostuser,keypath,ssh)
both <- merge(bo,bi,by='BoneID')
newID <- both$MetricID+9000000
newID <- as.character(newID)
newID <- paste('M',substring(newID,2),sep='')
stamps <- data.frame(time_added=rep(NA,nrow(both)), user_added=rep(NA,nrow(both)), time_last_update=both$timestamp.x, user_last_update=both$userstamp.x)
castrate <- data.frame(Castrate=NA)
new <- cbind(data.frame(MetricID=newID),both[,c('BoneID','PhaseID','TaxonCode','Element','Sex')],castrate,both[,c('Measurement','Value')],stamps)

i <- new$Sex=='Undet'
new$Sex[i] <- 0.5; new$Castrate[i] <- NA
i <- new$Sex=='Castrate'
new$Sex[i] <- 0; new$Castrate[i] <- 1
i <- new$Sex=='Castrate?'
new$Sex[i] <- NA; new$Castrate[i] <- 0.5
i <- new$Sex=='Male'
new$Sex[i] <- 0; new$Castrate[i] <- 0
i <- new$Sex=='Male?'
new$Sex[i] <- 0.25; new$Castrate[i] <- NA
i <- new$Sex=='Female'
new$Sex[i] <- 1; new$Castrate[i] <- 0
i <- new$Sex=='Female?'
new$Sex[i] <- 0.75; new$Castrate[i] <- NA

sql <- c()
for(n in 1:nrow(new)){
	i <- !is.na(new[n,])
	names <- paste(names(new)[i],collapse="`, `")
	values <- paste(new[n,i], collapse="', '")
	sql[n] <- paste("INSERT INTO `BIAD`.`FaunalAdrian` (`",names, "`) VALUES ('", values, "')",sep="")
	}

sql.wrapper(sql,user,password,hostname,hostuser,keypath,ssh)

#-----------------------------------------------------------------------------------------
# explore pulling from backups
#-----------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------
