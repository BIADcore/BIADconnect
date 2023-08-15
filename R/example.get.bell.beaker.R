#--------------------------------------------------------------------------------------
# Requirements
# You must have previously added the .Rprofile to your R_USER folder, here -> path.expand('~/') 
# See the BIADwiki readme or BIADwiki for details about using the .Rprofile

# Load some required functions
source("https://raw.githubusercontent.com/BIADwiki/BIADwiki/main/R/functions.R")
#--------------------------------------------------------------------------------------
# get all C14 dates associated with bell beaker
#--------------------------------------------------------------------------------------
sql.command <- "SELECT `C14ID`,`C14Samples`.`PhaseID`,`C14Samples`.`SiteID`,`C14Samples`.`Period`,`C14.Age`,`C14.SD`,`LabID`,`Culture1` FROM `C14Samples` INNER JOIN `Phases` ON `C14Samples`.`PhaseID`=`Phases`.`PhaseID` WHERE `Culture1`='Bell Beaker'"
x <- run.server.query(sql.command)
#--------------------------------------------------------------------------------------
# look at the data
head(x)
#--------------------------------------------------------------------------------------