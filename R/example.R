#------------------------------------------------------------------
# Example R script for directly querying BIAD using the RMySQL package
#------------------------------------------------------------------
# First obtain the following objects from the BIAD administrator.
# Put them in a .Rprofile file, in the same folder that this script is in.

user <- '???'
password <- '???'
hostname <- '???'
hostuser <- '???'
keypath <- '???'
ssh <- TRUE
#------------------------------------------------------------------
source('functions.R')
#------------------------------------------------------------------
# Example 1
#------------------------------------------------------------------
sql.command <- "SELECT * FROM Sites"
query <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)
#------------------------------------------------------------------
# The object 'query' can now be inspected
#------------------------------------------------------------------
head(query)
table(query$Country)
plot(table(query$Country))
#-----------------------------------------------------------------
#Example 2: Joins done in MySQL
#-----------------------------------------------------------------
sql.command <- "SELECT `Sites`.`SiteName`, `Sites`.`SiteID`, `Phases`.`PhaseID`, `Graves`.`GraveID`, `GraveIndividuals`.`IndividualID`,  `GraveIndividuals`.`Sex`, `GraveIndividuals`.`AgeCategorical`
FROM `Sites`
LEFT JOIN `Phases` ON `Sites`.`SiteID` = `Phases`.`SiteID`
LEFT JOIN `Graves` ON `Phases`.`PhaseID` = `Graves`.`PhaseID`
LEFT JOIN `GraveIndividuals` ON `Graves`.`GraveID` = `GraveIndividuals`.`GraveID`
WHERE `AgeCategorical` = 'infant'
ORDER BY `IndividualID`"
query <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)
#-----------------------------------------------------------------
#Example 3: Exactly the same output, but joins done in R
#-----------------------------------------------------------------
sql.command1 <- "SELECT `SiteName`, `SiteID` FROM `Sites`"
query1 <- sql.wrapper(sql.command1,user,password,hostname,hostuser,keypath,ssh)

sql.command2 <- "SELECT `SiteID`, `PhaseID` FROM `Phases`"
query2 <- sql.wrapper(sql.command2,user,password,hostname,hostuser,keypath,ssh)

sql.command3 <- "SELECT `PhaseID`, `GraveID` FROM `Graves`"
query3 <- sql.wrapper(sql.command3,user,password,hostname,hostuser,keypath,ssh)

sql.command4 <- "SELECT `GraveID`, `IndividualID`, `Sex`, `AgeCategorical` FROM `GraveIndividuals`"
query4 <- sql.wrapper(sql.command4,user,password,hostname,hostuser,keypath,ssh)

query <- merge(query1, query2, by = "SiteID")
query <- merge(query, query3, by = "PhaseID")
query <- merge(query, query4, by = "GraveID")
query <- subset(query, AgeCategorical=='infant')
query <- subset(query, AgeCategorical=='infant')
query <- query[order(query$IndividualID),]
#-----------------------------------------------------------------


