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
# Example 2: Joins done in MySQL
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
# Example 3: Exactly the same output, but joins done in R
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
query <- merge(query,  query3, by = "PhaseID")
query <- merge(query,  query4, by = "GraveID")
query <- subset(query, AgeCategorical=='infant')
query <- subset(query, AgeCategorical=='infant')
query <- query[order(query$IndividualID),]
#------------------------------------------------------------------
# Example 4: quantifying PhaseType entries in BIAD using the RMySQL package
#------------------------------------------------------------------
sql.command1 <- "SELECT * FROM PhaseTypes"
query <- sql.wrapper(sql.command1,user,password,hostname,hostuser,keypath,ssh)
query
type.count <- sort(table(query$Type), decreasing = T)
View(type.count)
sum(type.count)
#------------------------------------------------------------------
# Example 5: updating the database directly
#------------------------------------------------------------------
new <- data.frame(names=sample(c('andy','bob','chris','dave')), id=1:4)
sql.command <- c()
for(n in 1:4){
	sql.command[n] <- paste("UPDATE `BIAD`.`zprivate_encoding` SET `latin`='",new$names[n],"' WHERE `ID`='",new$id[n],"'",sep="")
	}
sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)
#------------------------------------------------------------------
# Example 6: check if LabIDs exist in the C14Samples table
#------------------------------------------------------------------
source('functions.R')
sql.command <- "SELECT LabID FROM C14Samples"
query <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)
test <- merge(query, test, by = "LabID")
#------------------------------------------------------------------
# Example 7: use GraveID's to extract associated metadata for the C14Samples
#------------------------------------------------------------------
source('functions.R')
sql.command <- "SELECT GraveID, Graves.PhaseID, SiteID, Period FROM Graves JOIN Phases ON Graves.PhaseID = Phases.PhaseID GROUP BY GraveID ORDER BY GraveID"
query <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)
test <- merge(query, test, by = "GraveID")
write.csv(test, "metadata.csv")
#------------------------------------------------------------------
# Example 8: use IndividualID's to import ItemIDs
#------------------------------------------------------------------
source('functions.R')
sql.command <- "SELECT * FROM GraveIndividuals"
query <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)
test <- merge(query, test, by = "IndividualID")
test
write.csv(test, "metadata.csv")