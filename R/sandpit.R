#-----------------------------------------------------------------------------------------
# general sandpit
#-----------------------------------------------------------------------------------------
source('.Rprofile')
source('functions.R')
#-----------------------------------------------------------------------------------------
# check whats going on with taxon codes for the materialculture
#-----------------------------------------------------------------------------------------
tax <- sql.wrapper("SELECT * FROM `BIAD`.`zoptions_TaxaList`",user,password,hostname,hostuser,keypath,ssh)
mt <- sql.wrapper("SELECT * FROM `BIAD`.`MaterialCulture`",user,password,hostname,hostuser,keypath,ssh)
x <- unique(mt$TaxonCode)
table(mt$TaxonCode)
bad <- x[!x%in%tax$TaxonCode]
bad

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
# merge faunal taxa and abot taxa into a single table
#-----------------------------------------------------------------------------------------
ft <- sql.wrapper("SELECT * FROM `BIAD`.`zoptions_FaunalTaxaList`",user,password,hostname,hostuser,keypath,ssh)
at <- sql.wrapper("SELECT * FROM `BIAD`.`zoptions_ABotTaxaList`",user,password,hostname,hostuser,keypath,ssh)
names(at)[5] <- 'WildDomesticStatus'
ft <- ft[,1:8]
ft <- cbind(ft, data.frame(Kingdom='Animal'))
at <- cbind(at, data.frame(Kingdom ='Plant'))
require(dplyr)
both <- bind_rows(ft,at)
table(both$WildDomesticStatus)
write.csv(both,file='toadd.csv',fileEncoding = "UTF-8",row.names=F, na='\\N')

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------


##################################


#----------------------------------------------------------------------------------------------------
get.child.tables <- function(foreign, table){
	res <- subset(foreign, REFERENCED_TABLE_NAME==table)
return(res)}
#----------------------------------------------------------------------------------------------------
get.child.data <- function(table, values){

	sql.command <- "SELECT * FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE WHERE CONSTRAINT_SCHEMA='BIAD'"
	keys <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)
	primary <- subset(keys, CONSTRAINT_NAME=='PRIMARY')
	foreign <- keys[grepl('^FK_',keys$CONSTRAINT_NAME) & keys$ORDINAL_POSITION==1,]

	child.tables <- get.child.tables(foreign, table)

	res <- list()
	N <- nrow(child.tables)
	for(n in 1:N){
		t <- child.tables$TABLE_NAME[n]
		c <- child.tables$COLUMN_NAME[n]
		value.command <- paste(c,"=",paste("'",values,"'",sep=''), collapse=" OR ")
		sql.command <- paste("SELECT * FROM `BIAD`.`",t,"` WHERE ",value.command, sep='')
		data <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)
		if(!is.null(data))res[[t]] <- data
		}
return(res)}
#----------------------------------------------------------------------------------------------------
table='Phases'
column = 'PhaseID'
values=c('NITR1', 'NITR2')
dd <- get.child.data(table, values)



get.all.data <- function(table, column, values){

	# blank list
	data <- list()

	# current table data 
	value.command <- paste(column,"=",paste("'",values,"'",sep=''), collapse=" OR ")
	sql.command <- paste("SELECT * FROM `BIAD`.`",table,"` WHERE ",value.command, sep='')
	data[[table]] <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)
	done <- table

	# child data
	for(n in 1:length(done)){
		child.tables <- get.child.tables(foreign, done[n])

	children <- get.child.data(table, values)
	data[[value]] <- children



	# grandchild data
	names(children)
	for(n in 1:length(children)){
		child <- names(children)[n]
		child.tables <- get.child.tables(foreign, child)
		for(c in 1:length(child.tables)){
			child.data <- get.child.data(child.tables$TABLE_NAME[c], value
str(data)
str(child)
#-----------------------------------------------------------------------------------------
child <- get.child.data(table='Phases', column='PhaseID', value='NITR1')
child <- get.child.data(table='Sites', value='S10567')

names(qq[[names(qq)]])
qq[['a']]


qqq$


#-----------------------------------------------------------------------------------------
library(data.tree)

acme <- Node$new("Acme Inc.")
  accounting <- acme$AddChild("Accounting")
    software <- accounting$AddChild("New Software")
    standards <- accounting$AddChild("New Accounting Standards")
  research <- acme$AddChild("Research")
    newProductLine <- research$AddChild("New Product Line")
    newLabs <- research$AddChild("New Labs")
  it <- acme$AddChild("IT")
    outsource <- it$AddChild("Outsource")
    agile <- it$AddChild("Go agile")
    goToR <- it$AddChild("Switch to R")

print(acme)
#-----------------------------------------------------------------------------------------
