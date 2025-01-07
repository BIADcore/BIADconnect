#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
# Example R script for querying BIAD
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
# Requirements first read:
# https://biadwiki.org/en/connectR
# ensure you have opened a tunnel first (e.g. putty)
#--------------------------------------------------------------------------------------
source("https://raw.githubusercontent.com/BIADwiki/BIADwiki/main/R/functions.R")
conn  <-  init.conn()
#--------------------------------------------------------------------------------------
# Example 1
#--------------------------------------------------------------------------------------
sql.command <- "SELECT * FROM `Sites`"
query <- query.database(sql.command, conn=conn)

sql.command <- "SELECT `TABLE_NAME` FROM `information_schema`.`TABLES` WHERE TABLE_SCHEMA='biad' AND `TABLE_TYPE`='BASE TABLE';"
tables <- query.database(sql.command = sql.command, conn=conn)

#--------------------------------------------------------------------------------------
# The object 'query' can now be inspected
#--------------------------------------------------------------------------------------
head(query)
table(query$Country)
plot(log(table(query$Country)),las=2)

#--------------------------------------------------------------------------------------
# Example 2: Joins done in MySQL
#--------------------------------------------------------------------------------------
sql.command <- "SELECT `Sites`.`SiteName`, `Sites`.`SiteID`, `Phases`.`PhaseID`, `Graves`.`GraveID`, `GraveIndividuals`.`IndividualID`,  `GraveIndividuals`.`Sex`, `GraveIndividuals`.`AgeCategorical`
FROM `Sites`
LEFT JOIN `Phases` ON `Sites`.`SiteID` = `Phases`.`SiteID`
LEFT JOIN `Graves` ON `Phases`.`PhaseID` = `Graves`.`PhaseID`
LEFT JOIN `GraveIndividuals` ON `Graves`.`GraveID` = `GraveIndividuals`.`GraveID`
WHERE `AgeCategorical` = 'infant'
ORDER BY `IndividualID`"
query <- query.database(sql.command, conn=conn)
head(query)
#--------------------------------------------------------------------------------------
# Example 3: Exactly the same output, but joins done in R
#--------------------------------------------------------------------------------------
query1 <- query.database("SELECT `SiteName`, `SiteID` FROM `Sites`", conn=conn)
query2 <- query.database("SELECT `SiteID`, `PhaseID` FROM `Phases`", conn=conn)
query3 <- query.database("SELECT `PhaseID`, `GraveID` FROM `Graves`", conn=conn)
query4 <- query.database("SELECT `GraveID`, `IndividualID`, `Sex`, `AgeCategorical` FROM `GraveIndividuals`", conn=conn)

query <- merge(query1, query2, by = "SiteID")
query <- merge(query,  query3, by = "PhaseID")
query <- merge(query,  query4, by = "GraveID")
query <- subset(query, AgeCategorical=='infant')
query <- query[order(query$IndividualID),]
head(query)
#--------------------------------------------------------------------------------------
# Example 4: quantifying PhaseType entries in BIAD 
#--------------------------------------------------------------------------------------
query <- query.database("SELECT * FROM `PhaseTypes`", conn=conn)
type.count <- sort(table(query$Type), decreasing = TRUE)
View(type.count)
sum(type.count)

#--------------------------------------------------------------------------------------
# Example 5: updating the database directly
#--------------------------------------------------------------------------------------
old <- query.database("SELECT * FROM `zprivate_encoding`", conn=conn)
new <- paste('sausage', date())

sql.command <- c()
for(n in 1:nrow(old)){
	sql.command[n] <- paste("UPDATE `BIAD`.`zprivate_encoding` SET `notes`='",new,"' WHERE `ID`='",old$ID[n],"'",sep="")
	}
query.database(sql.command, conn=conn)

#--------------------------------------------------------------------------------------
# Example 6: get LabIDs from C14Samples table
#--------------------------------------------------------------------------------------
query <- query.database("SELECT LabID FROM C14Samples", conn=conn)
head(query)
#--------------------------------------------------------------------------------------
# Example 7: use GraveID's to extract associated metadata for the C14Samples
#--------------------------------------------------------------------------------------
sql.command <- "SELECT GraveID, Graves.PhaseID, SiteID, Period FROM Graves JOIN Phases ON Graves.PhaseID = Phases.PhaseID GROUP BY GraveID ORDER BY GraveID"
query <- query.database(sql.command, conn=conn)

#--------------------------------------------------------------------------------------
# Example 8: get c14 for a culture and country
#--------------------------------------------------------------------------------------
query1 <- query.database("SELECT * FROM `Sites` WHERE `Country` = 'Hungary'", conn=conn)
query2 <- query.database("SELECT * FROM `Phases` WHERE `Culture1` ='Bell Beaker'", conn=conn)
query3 <- query.database("SELECT * FROM `C14Samples`", conn=conn)

m1 <- merge(query1, query2, by = "SiteID")
m2 <- merge(m1, query3, by = "PhaseID")

head(m2)

#--------------------------------------------------------------------------------------
# Example 9: country-based distribution of aDNA samples per country
#--------------------------------------------------------------------------------------
sql.command <- "SELECT Country, Count(aDNAID) AS Frequency FROM Sites LEFT JOIN Phases on Sites.SiteID = Phases.SiteID LEFT JOIN Graves ON Phases.PhaseID = Graves.PhaseID LEFT JOIN GraveIndividuals ON Graves.GraveID = GraveIndividuals.GraveID WHERE aDNAID IS NOT NULL GROUP BY Country;"
query <- query.database(sql.command, conn=conn)
library(ggplot2)
ggplot(query, aes(x = Country, y = Frequency)) + 
	geom_bar(stat="identity") +
	labs(title = "aDNA samples in BIAD [n=2045") +
	coord_flip()
#--------------------------------------------------------------------------------------
disconnect()
#--------------------------------------------------------------------------------------

