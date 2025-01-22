#------------------------------------------------------------------
# Private stuff, this script is not pushed to git, so can be lost if recloned
#------------------------------------------------------------------
source("https://raw.githubusercontent.com/BIADwiki/BIADwiki/main/R/functions.R")
conn  <- init.conn()
#------------------------------------------------------------------
# Extract any citation value from EuroevolFull
#------------------------------------------------------------------
q1 <- query.database(sql.command = "SELECT `DatingNotes`,`PhaseCode`,`SiteID` FROM `EUROEVOLfull`.`ABotPhases` WHERE `DatingNotes` IS NOT NULL;", conn = conn)
q2 <- query.database(sql.command = "SELECT * FROM `EUROEVOLfull`.`FaunalSites`;", conn = conn)
q3 <- query.database(sql.command = "SELECT `ReferenceID`,`SiteID` FROM `EUROEVOLfull`.`CommonSiteReferences`;", conn = conn)
q4 <- query.database(sql.command = "SELECT * FROM `EUROEVOLfull`.`CommonReferences`;", conn = conn)
q5 <- query.database(sql.command = "SELECT `SiteID`,`SiteName` FROM `EUROEVOLfull`.`CommonSites`;", conn = conn)
disconnect()
#------------------------------------------------------------------
# various processing
#------------------------------------------------------------------
q2$Notes <- gsub('\t','',q2$Notes)
q2$Notes <- gsub('\v','',q2$Notes)

key <- q4$'Custom 1'
q4 <- q4[,!'Custom'%in%names(q4)]
N <- nrow(q4)
string <- character(N)
for(n in 1:N)string[n] <- paste(q4[n,],collapse=' ')
string <- gsub('NA ','',string)
q4 <- data.frame(key,string)

SiteID <- unique(q1$SiteID)
N <- length(SiteID)
dating.notes <- character(N)
for(n in 1:N)dating.notes[n] <- paste(subset(q1,SiteID==SiteID[n])$DatingNotes, collapse='.')
q1 <- data.frame(SiteID,dating.notes)
#------------------------------------------------------------------
# various merges
#------------------------------------------------------------------
q3.4 <- merge(q3,q4,by.x='ReferenceID', by.y='key')[,c('SiteID','string')]
q1.3.4 <- merge(q1,q3.4, by='SiteID', all=TRUE)
q1.2.3.4 <- merge(q1.3.4,q2,by='SiteID', all=TRUE)
raw <- merge(q1.2.3.4, q5, by='SiteID', all.x=TRUE, all.y=FALSE)
raw <- raw[,names(raw)!='PhaseCode']
raw <- raw[,c(1,5,3,4,2)]
names(raw) <- c('SiteID','sitename.euroevol','notes.1','notes.2','notes.3')
data <- raw
#------------------------------------------------------------------
data$SiteID <- gsub('S','S0',data$SiteID)
for(c in 1:ncol(data))data[is.na(data[,c]),c] <- 'NULL'
write.csv(data,file='tmp.csv',row.names=FALSE, fileEncoding='utf-8')
#------------------------------------------------------------------
for(c in 1:ncol(data))print(max(nchar(data[,c])))


