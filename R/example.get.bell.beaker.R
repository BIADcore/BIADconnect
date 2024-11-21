#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
# get all C14 dates associated with bell beaker
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
# Requirements first read:
# https://biadwiki.org/en/connectR
#--------------------------------------------------------------------------------------
source("https://raw.githubusercontent.com/BIADwiki/BIADwiki/main/R/functions.R")
open.tunnel()
sql.command <- "SELECT `C14ID`,`C14Samples`.`PhaseID`,`C14Samples`.`SiteID`,`C14Samples`.`Period`,`C14.Age`,`C14.SD`,`LabID`,`Culture1` FROM `C14Samples` INNER JOIN `Phases` ON `C14Samples`.`PhaseID`=`Phases`.`PhaseID` WHERE `Culture1`='Bell Beaker'"
x <- query.database(sql.command)
close.tunnel()
#--------------------------------------------------------------------------------------
# look at the data
head(x)
#--------------------------------------------------------------------------------------