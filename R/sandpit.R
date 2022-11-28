#-----------------------------------------------------------------------------------------
# general sandpit
#-----------------------------------------------------------------------------------------
source('.Rprofile')
source('functions.R')

#-----------------------------------------------------------------------------------------
# old bug with phase citations and phase types
#-----------------------------------------------------------------------------------------
pha <- sql.wrapper("SELECT * FROM BIAD.Phases",user,password,hostname,hostuser,keypath,ssh)
pha.typ <- sql.wrapper("SELECT PhaseID, Type FROM BIAD.PhaseTypes",user,password,hostname,hostuser,keypath,ssh)

phases <- unique(pha.typ$PhaseID)
types <- c()
N <- length(phases)
for(n in 1:N){
	sub <- subset(pha.typ, PhaseID==phases[n])
	types[n] <- paste(sub$Type, collapse='; ')
	}
d <- data.frame(PhaseID=phases,types=types)

d1 <- merge(pha,d,by='PhaseID')

bad.i <- which(d1$Type!=d1$types)
d1[bad.i[1:5],c('PhaseID','Type','types')]

check.phases <- d1[bad.i,]$PhaseID

text <- c()
for(n in 1:length(check.phases)){
	text[n] <- paste("UPDATE `BIAD`.`Phases` SET `StartBCEestimated`=NULL WHERE `PhaseID`='",check.phases[n],"';",sep='')
	}

sql.wrapper(text,user,password,hostname,hostuser,keypath,ssh)
#-----------------------------------------------------------------------------------------
# possibly amalgamate taxon tables?
#-----------------------------------------------------------------------------------------
fau <- sql.wrapper("SELECT * FROM BIAD.zoptions_FaunalTaxaList",user,password,hostname,hostuser,keypath,ssh)
abo <- sql.wrapper("SELECT * FROM BIAD.zoptions_ABotTaxaList",user,password,hostname,hostuser,keypath,ssh)
####
a <- fau$TaxonCode
b <- tolower(abo$TaxonCode)
same <- a[a%in%b]
same

subset(fau, TaxonCode%in%same)
subset(abo, TaxonCode%in%toupper(same))

#-----------------------------------------------------------------------------------------
# Jan
#-----------------------------------------------------------------------------------------
d <- read.csv('test.csv')
d <- as.matrix(d)
biad <- d[,c(1,3)]
czech <- d[,c(2,4)]
match <- !is.na(biad) &  !is.na(czech) & biad==czech
biad.value <- !is.na(biad)
czech.value <- !is.na(czech)

keep.either <- match
keep.biad <- !match & !czech.value
keep.czech <- !match & !biad.value
flag <- !match & biad.value & czech.value

new.table <- biad
new.table[keep.either] <- biad[keep.either]
new.table[keep.biad] <- biad[keep.biad]
new.table[keep.czech] <- czech[keep.czech]
new.table[flag] <- paste(biad[flag],'::OR::',czech[flag])

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
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
