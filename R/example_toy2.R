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
sql.command1 <- "SELECT SiteName, SiteID FROM BIAD.Sites"
query1 <- sql.wrapper(sql.command1,user,password,hostname,hostuser,keypath,ssh)

sql.command2 <- "SELECT SiteID, PhaseID FROM BIAD.Phases"
query2 <- sql.wrapper(sql.command2,user,password,hostname,hostuser,keypath,ssh)

sql.command3 <- "SELECT PhaseID, GraveID FROM BIAD.Graves"
query3 <- sql.wrapper(sql.command3,user,password,hostname,hostuser,keypath,ssh)

query_combined1 <- merge(query1, query2, by.x = "SiteID", by.y = "SiteID")
query_combined <- merge(query_combined1, query3, by.x = "PhaseID", by.y = "PhaseID")

sql.command4 <- "SELECT SiteName, SiteID FROM BIAD.Sites"
query4 <- sql.wrapper(sql.command4,user,password,hostname,hostuser,keypath,ssh)

#------------------------------------------------------------------
# The object 'query' can now be inspected
#------------------------------------------------------------------
head(query_combined)
table(query_combined$GraveID)
plot(table(query_combined$GraveID))

#-----------------------------------------------------------------