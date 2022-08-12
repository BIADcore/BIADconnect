#-----------------------------------------------------------------------------------------
# Pull table information from backups
# Do not attempt to run this script from anywhere other than the server!!
# Not only are the file paths specific to the server, but a remote machine will take forever!
#-----------------------------------------------------------------------------------------
source('functions.R')
source('.Rprofile')
#-----------------------------------------------------------------------------------------
file <- "/Users/admin/dropbox/MySQLbackups/BIAD/monthly/BIADbackup20220701055503.sql"
back <- get.tables.from.backup(file)
names(back)
#-----------------------------------------------------------------------------------------
current <- sql.wrapper("SELECT * FROM BIAD.Sites",user,password,hostname,hostuser,keypath,ssh)
old <- back$Sites
both <- merge(old[,c('SiteID','timestamp','userstamp')],current[,c('SiteID','time_added','user_added')], by='SiteID')
both[is.na(both)] <- 'NA'
bad <- both[both$user_added!=both$userstamp,]
sql.time <- sql.user <- c()
for(n in 1:nrow(bad)){
	sql.time[n] <- paste("UPDATE `BIAD`.`Sites` SET `time_added` =  '",bad$timestamp[n],"' WHERE `SiteID` = '", bad$SiteID[n], "';", sep='')
	sql.user[n] <- paste("UPDATE `BIAD`.`Sites` SET `user_added` =  '",bad$userstamp[n],"' WHERE `SiteID` = '", bad$SiteID[n], "';", sep='')
	}
sql.wrapper(sql.time,user,password,hostname,hostuser,keypath,ssh)
sql.wrapper(sql.user,user,password,hostname,hostuser,keypath,ssh)
#-----------------------------------------------------------------------------------------
current <- sql.wrapper("SELECT * FROM BIAD.Phases",user,password,hostname,hostuser,keypath,ssh)
old <- back$Phases
both <- merge(old[,c('PhaseID','timestamp','userstamp')],current[,c('PhaseID','time_added','user_added')], by='PhaseID')
both[is.na(both)] <- 'NA'
bad <- both[both$user_added!=both$userstamp,]
sql.time <- sql.user <- c()
for(n in 1:nrow(bad)){
	sql.time[n] <- paste("UPDATE `BIAD`.`Phases` SET `time_added` =  '",bad$timestamp[n],"' WHERE `PhaseID` = '", bad$PhaseID[n], "';", sep='')
	sql.user[n] <- paste("UPDATE `BIAD`.`Phases` SET `user_added` =  '",bad$userstamp[n],"' WHERE `PhaseID` = '", bad$PhaseID[n], "';", sep='')
	}
sql.wrapper(sql.time,user,password,hostname,hostuser,keypath,ssh)
sql.wrapper(sql.user,user,password,hostname,hostuser,keypath,ssh)
#-----------------------------------------------------------------------------------------
current <- sql.wrapper("SELECT * FROM BIAD.ABotIsotopes",user,password,hostname,hostuser,keypath,ssh)
old <- back$ABotIsotopes
both <- merge(old[,c('ABotIsoID','time_added','user_added')],current[,c('ABotIsoID','time_added','user_added')], by='ABotIsoID')
both[is.na(both)] <- 'NA'
bad <- both[both$user_added.x!=both$user_added.y,]
#-----------------------------------------------------------------------------------------
current <- sql.wrapper("SELECT * FROM BIAD.ABotPhases",user,password,hostname,hostuser,keypath,ssh)
old <- back$ABotPhases
both <- merge(old[,c('PhaseID','timestamp','userstamp')],current[,c('PhaseID','time_added','user_added')], by='PhaseID')
both[is.na(both)] <- 'NA'
bad <- both[both$user_added!=both$userstamp,]
sql.time <- sql.user <- c()
for(n in 1:nrow(bad)){
	sql.time[n] <- paste("UPDATE `BIAD`.`ABotPhases` SET `time_added` =  '",bad$timestamp[n],"' WHERE `PhaseID` = '", bad$PhaseID[n], "';", sep='')
	sql.user[n] <- paste("UPDATE `BIAD`.`ABotPhases` SET `user_added` =  '",bad$userstamp[n],"' WHERE `PhaseID` = '", bad$PhaseID[n], "';", sep='')
	}
sql.wrapper(sql.time,user,password,hostname,hostuser,keypath,ssh)
sql.wrapper(sql.user,user,password,hostname,hostuser,keypath,ssh)
#-----------------------------------------------------------------------------------------
current <- sql.wrapper("SELECT * FROM BIAD.ABotSamples",user,password,hostname,hostuser,keypath,ssh)
old <- back$ABotSamples
both <- merge(old[,c('SampleID','time_added','user_added')],current[,c('SampleID','time_added','user_added')], by='SampleID')
both[is.na(both)] <- 'NA'
#-----------------------------------------------------------------------------------------
current <- sql.wrapper("SELECT * FROM BIAD.C14Samples",user,password,hostname,hostuser,keypath,ssh)
old <- back$C14Samples
both <- merge(old[,c('C14ID','timestamp','userstamp')],current[,c('C14ID','time_added','user_added')], by='C14ID')
both[is.na(both)] <- 'NA'
bad <- both[both$user_added!=both$userstamp,]
sql.time <- sql.user <- c()
for(n in 1:nrow(bad)){
	sql.time[n] <- paste("UPDATE `BIAD`.`C14Samples` SET `time_added` =  '",bad$timestamp[n],"' WHERE `C14ID` = '", bad$C14ID[n], "';", sep='')
	sql.user[n] <- paste("UPDATE `BIAD`.`C14Samples` SET `user_added` =  '",bad$userstamp[n],"' WHERE `C14ID` = '", bad$C14ID[n], "';", sep='')
	}
sql.wrapper(sql.time,user,password,hostname,hostuser,keypath,ssh)
sql.wrapper(sql.user,user,password,hostname,hostuser,keypath,ssh)
#-----------------------------------------------------------------------------------------
current <- sql.wrapper("SELECT * FROM BIAD.Citations",user,password,hostname,hostuser,keypath,ssh)
old <- back$Citations
both <- merge(old[,c('CitationID','time_added','user_added')],current[,c('CitationID','time_added','user_added')], by='CitationID')
both[is.na(both)] <- 'NA'
bad <- both[both$user_added!=both$userstamp,]
#-----------------------------------------------------------------------------------------
current <- sql.wrapper("SELECT * FROM BIAD.FaunalBiometrics",user,password,hostname,hostuser,keypath,ssh)
old <- back$FaunalBiometrics
x <-paste('M',(old$MetricID+100000),sep='')
x <- gsub('M1','M0',x)
old$MetricID <- x
both <- merge(old[,c('MetricID','timestamp','userstamp')],current[,c('MetricID','time_added','user_added')], by='MetricID')
both[is.na(both)] <- 'NA'
bad <- both[both$user_added!=both$userstamp,]
#-----------------------------------------------------------------------------------------
current <- sql.wrapper("SELECT * FROM BIAD.FaunalIsotopes",user,password,hostname,hostuser,keypath,ssh)
old <- back$FaunalIsotopes
both <- merge(old[,c('FaunIsoID','time_added','user_added')],current[,c('FaunIsoID','time_added','user_added')], by='FaunIsoID')
both[is.na(both)] <- 'NA'
bad <- both[both$user_added!=both$userstamp,]
#-----------------------------------------------------------------------------------------
current <- sql.wrapper("SELECT * FROM BIAD.FaunalSpecies",user,password,hostname,hostuser,keypath,ssh)
old <- back$FaunalSpecies
both <- merge(old[,c('FaunalSpeciesID','timestamp','userstamp')],current[,c('FaunalSpeciesID','time_added','user_added')], by='FaunalSpeciesID')
both[is.na(both)] <- 'NA'
bad <- both[both$user_added!=both$userstamp,]
#-----------------------------------------------------------------------------------------

