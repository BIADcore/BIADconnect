#------------------------------------------------------------------
# Example R script for directly querying BIAD using the RMySQL package
#------------------------------------------------------------------
# First obtain the following objects from the BIAD administrator.
# Either keep them at the beginning of each script, or shove them into a .Rprofile file

user <- '???'
password <- '???'
hostname <- '???'
hostuser <- '???'
keypath <- '???'
ssh <- TRUE

#------------------------------------------------------------------
# helper functions in the functions.R script take care of various problems:
#------------------------------------------------------------------
source('functions.R')
sql.command <- "SELECT SiteName, Sites.SiteID, Phases.PhaseID, Graves.GraveID, GraveIndividuals.IndividualID, GraveIndividuals.IndividualName, Sex, AgeCategorical, GraveIndividuals.CitationID
FROM BIAD.Sites
LEFT JOIN BIAD.Phases ON Sites.SiteID = Phases.SiteID
LEFT JOIN BIAD.Graves ON Phases.PhaseID = Graves.PhaseID
LEFT JOIN BIAD.GraveIndividuals ON Graves.GraveID = GraveIndividuals.GraveID
WHERE IndividualID IS NOT NULL
ORDER BY 'IndividualID';"
query <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)
#------------------------------------------------------------------
# The object 'query' can now be inspected
#------------------------------------------------------------------
head(query)
table(query$AgeCategorical)
plot(table(query$AgeCategorical))

#-----------------------------------------------------------------