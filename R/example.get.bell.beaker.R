#--------------------------------------------------------------------------------------
# Usual overheads
#--------------------------------------------------------------------------------------
source('../github/BIADwiki/R/functions.R')
source('../github/BIADwiki/R/.Rprofile')
#--------------------------------------------------------------------------------------
# get all C14 dates associated with bell beaker
#--------------------------------------------------------------------------------------
sql.command <- "SELECT `C14ID`,`C14Samples`.`PhaseID`,`C14Samples`.`SiteID`,`C14Samples`.`Period`,`C14.Age`,`C14.SD`,`LabID`,`Culture1` FROM `C14Samples` INNER JOIN `Phases` ON `C14Samples`.`PhaseID`=`Phases`.`PhaseID` WHERE `Culture1`='Bell Beaker'"
x <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)
#--------------------------------------------------------------------------------------
# look at the data
#--------------------------------------------------------------------------------------
head(x)