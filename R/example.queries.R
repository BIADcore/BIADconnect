#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
# Example R script for directly querying BIAD using the RMySQL package
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
# Requirements
# 1. Ensure R is in the same working directory as this file. Use getwd() or list.files() to check
# 2. Ensure you have your .Rprofile file in the same folder that this script is in. See the github readme for details.
#--------------------------------------------------------------------------------------
# Overheads
source('.Rprofile') # should already have loaded if you open R from this script.
source('functions.R')
#--------------------------------------------------------------------------------------
# Example 1
#--------------------------------------------------------------------------------------
sql.command <- "SELECT * FROM `Sites`"
query <- run.server.query(sql.command, user, password, hostuser, hostname, pempath)

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
query <- run.server.query(sql.command, user, password, hostuser, hostname, pempath)

#--------------------------------------------------------------------------------------
# Example 3: Exactly the same output, but joins done in R
#--------------------------------------------------------------------------------------
sql.command1 <- "SELECT `SiteName`, `SiteID` FROM `Sites`"
query1 <- run.server.query(sql.command1, user, password, hostuser, hostname, pempath)

sql.command2 <- "SELECT `SiteID`, `PhaseID` FROM `Phases`"
query2 <- run.server.query(sql.command2, user, password, hostuser, hostname, pempath)

sql.command3 <- "SELECT `PhaseID`, `GraveID` FROM `Graves`"
query3 <- run.server.query(sql.command3, user, password, hostuser, hostname, pempath)

sql.command4 <- "SELECT `GraveID`, `IndividualID`, `Sex`, `AgeCategorical` FROM `GraveIndividuals`"
query4 <- run.server.query(sql.command4, user, password, hostuser, hostname, pempath)

query <- merge(query1, query2, by = "SiteID")
query <- merge(query,  query3, by = "PhaseID")
query <- merge(query,  query4, by = "GraveID")
query <- subset(query, AgeCategorical=='infant')
query <- query[order(query$IndividualID),]

#--------------------------------------------------------------------------------------
# Example 4: quantifying PhaseType entries in BIAD 
#--------------------------------------------------------------------------------------
sql.command <- "SELECT * FROM `PhaseTypes`"
query <- run.server.query(sql.command, user, password, hostuser, hostname, pempath)
type.count <- sort(table(query$Type), decreasing = T)
View(type.count)
sum(type.count)

#--------------------------------------------------------------------------------------
# Example 5: updating the database directly
#--------------------------------------------------------------------------------------
sql.command <- "SELECT * FROM `zprivate_encoding`"
old <- run.server.query(sql.command, user, password, hostuser, hostname, pempath)
new <- paste('sausage', date())

sql.command <- c()
for(n in 1:nrow(old)){
	sql.command[n] <- paste("UPDATE `BIAD`.`zprivate_encoding` SET `notes`='",new,"' WHERE `ID`='",old$ID[n],"'",sep="")
	}
run.server.query(sql.command, user, password, hostuser, hostname, pempath)

#--------------------------------------------------------------------------------------
# Example 6: get LabIDs from C14Samples table
#--------------------------------------------------------------------------------------
sql.command <- "SELECT LabID FROM C14Samples"
query <- run.server.query(sql.command, user, password, hostuser, hostname, pempath)


#--------------------------------------------------------------------------------------
# Example 7: use GraveID's to extract associated metadata for the C14Samples
#--------------------------------------------------------------------------------------
sql.command <- "SELECT GraveID, Graves.PhaseID, SiteID, Period FROM Graves JOIN Phases ON Graves.PhaseID = Phases.PhaseID GROUP BY GraveID ORDER BY GraveID"
query <- run.server.query(sql.command, user, password, hostuser, hostname, pempath)

#--------------------------------------------------------------------------------------
# Example 8: using an object from a different database to query BIAD
#--------------------------------------------------------------------------------------
sql.command1 <- "SELECT * FROM `Sites`"
query1 <- run.server.query(sql.command1, user, password, hostuser, hostname, pempath)
head(query1)
RISE_query <- read.csv("C:/Users/rstan/OneDrive/post-doc/2022_COREX/RISE database/RISE_database.csv")
head(RISE_query)
RISE_query_sitename <- RISE_query$RISE_Sitename
RISE_query_sitename
test <- query1[query1$SiteName %in% c(RISE_query_sitename), ]
View(test)
sql.command2 <- "SELECT * FROM `Phases`"
query2 <- run.server.query(sq2.command, user, password, hostuser, hostname, pempath)
head(query2)
complete_query <- merge(test,  query2, by = "SiteID")
View(complete_query)

#--------------------------------------------------------------------------------------
# Example 9: check whether sites from a supplementary fall within the COREX spatial extent
#--------------------------------------------------------------------------------------
COREX <- st_read("../tools/shapes/COREX.shp")
COREX <- st_transform(COREX, crs = 4326)
points <- st_as_sf(, coords = c('Longitude','Latitude'), crs=4326) #insert file name you are checking
test <- st_join(, COREX) #insert file name you are checking
write.csv(test, file = "C:/Users/rstan/OneDrive/post-doc/2022_COREX/1_spatial_analysis//good_points.csv")

#--------------------------------------------------------------------------------------
# Example 10: check temporal distribution of radiocarbon dating by country
#--------------------------------------------------------------------------------------
sql.command <- "SELECT `C14ID`, `C14.Age` AS Age, `C14.SD`, `Country` FROM `C14Samples`
LEFT JOIN `Sites` ON `C14Samples`.`SiteID` = `Sites`.`SiteID`"
query <- run.server.query(sql.command, user, password, hostuser, hostname, pempath)
query <- na.omit(query) #remove NA's

a <- ggplot(query, aes(Country)) +
  geom_bar() +
  coord_flip() +
  theme_minimal()

library(ggridges)
library(ggplot2)

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

library(gridExtra)

#--------------------------------------------------------------------------------------
# Example 11: return all GraveIndividuals with ItemIDs and Phase based on a set of selected ID numbers
#--------------------------------------------------------------------------------------
sql.command <- "SELECT GraveIndividuals.IndividualID, GraveIndividuals.GraveID, GraveIndividuals.IndividualName, GraveIndividuals.ItemID, Graves.PhaseID
FROM GraveIndividuals
JOIN Graves ON GraveIndividuals.GraveID = Graves.GraveID"
query <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)
library(readr)
selected_ID_number <- read_csv("saag_material.csv")
query$GraveID %in% selected_ID_number$GraveID # specify which column of the query is supposed to be checked against the object with selected columns
test <- subset(query, GraveID %in% selected_ID_number$GraveID) # save an object with all the results which fulfill the specific condition
View(test) # inspect the final object
