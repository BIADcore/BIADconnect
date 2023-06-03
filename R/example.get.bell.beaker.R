#--------------------------------------------------------------------------------------
# Usual overheads
#--------------------------------------------------------------------------------------
source('functions.R')
# source('.Rprofile') # should already have loaded if you open R from this script.
#--------------------------------------------------------------------------------------
# get all C14 dates associated with bell beaker
#--------------------------------------------------------------------------------------
sql.command <- "SELECT `C14ID`,`C14Samples`.`PhaseID`,`C14Samples`.`SiteID`,`C14Samples`.`Period`,`C14.Age`,`C14.SD`,`LabID`,`Culture1` FROM `C14Samples` INNER JOIN `Phases` ON `C14Samples`.`PhaseID`=`Phases`.`PhaseID` WHERE `Culture1`='Bell Beaker'"
x <- run.server.query(sql.command, user, password, hostuser, hostname, pempath)
#--------------------------------------------------------------------------------------
# look at the data
#--------------------------------------------------------------------------------------
head(x)