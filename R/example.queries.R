#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
# Example R script for querying BIAD
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
# Requirements first read:
# https://biadwiki.org/en/connectR
#--------------------------------------------------------------------------------------
source("https://raw.githubusercontent.com/BIADwiki/BIADwiki/main/R/functions.R")
#--------------------------------------------------------------------------------------
# Example 1
#--------------------------------------------------------------------------------------
sql.command <- "SELECT * FROM `Sites`"
query <- query.database(sql.command)

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
query <- query.database(sql.command)
head(query)
#--------------------------------------------------------------------------------------
# Example 3: Exactly the same output, but joins done in R
#--------------------------------------------------------------------------------------
query1 <- query.database("SELECT `SiteName`, `SiteID` FROM `Sites`")
query2 <- query.database("SELECT `SiteID`, `PhaseID` FROM `Phases`")
query3 <- query.database("SELECT `PhaseID`, `GraveID` FROM `Graves`")
query4 <- query.database("SELECT `GraveID`, `IndividualID`, `Sex`, `AgeCategorical` FROM `GraveIndividuals`")

query <- merge(query1, query2, by = "SiteID")
query <- merge(query,  query3, by = "PhaseID")
query <- merge(query,  query4, by = "GraveID")
query <- subset(query, AgeCategorical=='infant')
query <- query[order(query$IndividualID),]
head(query)
#--------------------------------------------------------------------------------------
# Example 4: quantifying PhaseType entries in BIAD 
#--------------------------------------------------------------------------------------
query <- query.database("SELECT * FROM `PhaseTypes`")
type.count <- sort(table(query$Type), decreasing = T)
View(type.count)
sum(type.count)

#--------------------------------------------------------------------------------------
# Example 5: updating the database directly
#--------------------------------------------------------------------------------------
old <- query.database("SELECT * FROM `zprivate_encoding`")
new <- paste('sausage', date())

sql.command <- c()
for(n in 1:nrow(old)){
	sql.command[n] <- paste("UPDATE `BIAD`.`zprivate_encoding` SET `notes`='",new,"' WHERE `ID`='",old$ID[n],"'",sep="")
	}
query.database(sql.command)

#--------------------------------------------------------------------------------------
# Example 6: get LabIDs from C14Samples table
#--------------------------------------------------------------------------------------
query <- query.database("SELECT LabID FROM C14Samples")
head(query)
#--------------------------------------------------------------------------------------
# Example 7: use GraveID's to extract associated metadata for the C14Samples
#--------------------------------------------------------------------------------------
sql.command <- "SELECT GraveID, Graves.PhaseID, SiteID, Period FROM Graves JOIN Phases ON Graves.PhaseID = Phases.PhaseID GROUP BY GraveID ORDER BY GraveID"
query <- query.database(sql.command)

#--------------------------------------------------------------------------------------
# Example 8: get c14 for a culture and country
#--------------------------------------------------------------------------------------
query1 <- query.database("SELECT * FROM `Sites` WHERE `Country` = 'Hungary'")
query2 <- query.database("SELECT * FROM `Phases` WHERE `Culture1` ='Bell Beaker'")
query3 <- query.database("SELECT * FROM `C14Samples`")

m1 <- merge(query1, query2, by = "SiteID")
m2 <- merge(m1, query3, by = "PhaseID")

head(m2)

#--------------------------------------------------------------------------------------
# Example 9: country-based distribution of aDNA samples per country
#--------------------------------------------------------------------------------------
query <- query.database("SELECT Country, Count(aDNAID) AS Frequency FROM Sites LEFT JOIN Phases on Sites.SiteID = Phases.SiteID LEFT JOIN Graves ON Phases.PhaseID = Graves.PhaseID LEFT JOIN GraveIndividuals ON Graves.GraveID = GraveIndividuals.GraveID WHERE aDNAID IS NOT NULL GROUP BY Country;")
library(ggplot2)
ggplot(query, aes(x = Country, y = Frequency)) +
  geom_bar(stat="identity") +
  labs(title = "aDNA samples in BIAD [n=2045") +
  coord_flip()
#--------------------------------------------------------------------------------------


