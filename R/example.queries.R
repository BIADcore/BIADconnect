#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
# Example R script for querying BIAD using the RMySQL package
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
# Requirements
# You must have previously added the .Rprofile to your R_USER folder, here -> path.expand('~/') 
# See the BIADwiki readme or BIADwiki for details about using the .Rprofile

# Load some required functions
source("https://raw.githubusercontent.com/BIADwiki/BIADwiki/main/R/functions.R")
#--------------------------------------------------------------------------------------
# Example 1
#--------------------------------------------------------------------------------------
sql.command <- "SELECT * FROM `Sites`"
query <- run.server.query(sql.command)

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
query <- run.server.query(sql.command)

#--------------------------------------------------------------------------------------
# Example 3: Exactly the same output, but joins done in R
#--------------------------------------------------------------------------------------
query1 <- run.server.query("SELECT `SiteName`, `SiteID` FROM `Sites`")
query2 <- run.server.query("SELECT `SiteID`, `PhaseID` FROM `Phases`")
query3 <- run.server.query("SELECT `PhaseID`, `GraveID` FROM `Graves`")
query4 <- run.server.query("SELECT `GraveID`, `IndividualID`, `Sex`, `AgeCategorical` FROM `GraveIndividuals`")

query <- merge(query1, query2, by = "SiteID")
query <- merge(query,  query3, by = "PhaseID")
query <- merge(query,  query4, by = "GraveID")
query <- subset(query, AgeCategorical=='infant')
query <- query[order(query$IndividualID),]

#--------------------------------------------------------------------------------------
# Example 4: quantifying PhaseType entries in BIAD 
#--------------------------------------------------------------------------------------
query <- run.server.query("SELECT * FROM `PhaseTypes`")
type.count <- sort(table(query$Type), decreasing = T)
View(type.count)
sum(type.count)

#--------------------------------------------------------------------------------------
# Example 5: updating the database directly
#--------------------------------------------------------------------------------------
old <- run.server.query("SELECT * FROM `zprivate_encoding`")
new <- paste('sausage', date())

sql.command <- c()
for(n in 1:nrow(old)){
	sql.command[n] <- paste("UPDATE `BIAD`.`zprivate_encoding` SET `notes`='",new,"' WHERE `ID`='",old$ID[n],"'",sep="")
	}
run.server.query(sql.command)

#--------------------------------------------------------------------------------------
# Example 6: get LabIDs from C14Samples table
#--------------------------------------------------------------------------------------
query <- run.server.query("SELECT LabID FROM C14Samples")
#--------------------------------------------------------------------------------------
# Example 7: use GraveID's to extract associated metadata for the C14Samples
#--------------------------------------------------------------------------------------
sql.command <- "SELECT GraveID, Graves.PhaseID, SiteID, Period FROM Graves JOIN Phases ON Graves.PhaseID = Phases.PhaseID GROUP BY GraveID ORDER BY GraveID"
query <- run.server.query(sql.command)
#--------------------------------------------------------------------------------------
# Example 8: check temporal distribution of radiocarbon dating by country
#--------------------------------------------------------------------------------------
library(ggridges)
library(ggplot2)
sql.command <- "SELECT `C14ID`, `C14.Age` AS Age, `C14.SD`, `Country` FROM `C14Samples`
LEFT JOIN `Sites` ON `C14Samples`.`SiteID` = `Sites`.`SiteID`"
query <- run.server.query(sql.command)

a <- ggplot(query, aes(Country)) +
  geom_bar() +
  coord_flip() +
  theme_minimal()



b <- ggplot(query, aes(x = Age, y = Country)) +
  geom_density_ridges2(
    jittered_points = TRUE,
    position = position_points_jitter(width = 0.05, height = 0),
    point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.7,
    quantile_lines = TRUE,
    scale = 0.9) +
  scale_x_continuous(limits = c(0, 10000),
                     breaks = c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000, 11000, 12000)) +
  ylab("") +
  xlab("uncal BP") +
  theme_minimal() + 
  theme(legend.position = "none")

a
b
#--------------------------------------------------------------------------------------

