#-----------------------------------------------------------------------------------------
# Some summary data fro Magnus regarding current Sweden data
#-----------------------------------------------------------------------------------------
source('functions.R')
#-----------------------------------------------------------------------------------------
# Firstly, we want to query for Swedish sites in the database.
sql.command <- "SELECT SiteID, SiteName, Longitude, Latitude
FROM Sites
WHERE Country = 'Sweden'
ORDER BY 'SiteName'"
query <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)
head(query)

# Secondly, we want to query this information for phases and associated finds.
sql.command <- "SELECT Sites.SiteID, SiteName, Phases.PhaseID, Phases.Period, COUNT(C14Samples.C14ID)
FROM Phases
LEFT JOIN Sites ON Phases.SiteID = Sites.SiteID
LEFT JOIN C14Samples ON Phases.PhaseID = C14Samples.PhaseID
WHERE Phases.PhaseID IS NOT NULL AND Country = 'Sweden'
GROUP BY Phases.PhaseID"
c14 <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)
head(c14)
View(c14)

sql.command <- "SELECT Phases.PhaseID, COUNT(GraveIndividuals.IndividualID)
FROM Phases
LEFT JOIN Sites ON Phases.SiteID = Sites.SiteID
LEFT JOIN `Graves` ON `Phases`.`PhaseID` = `Graves`.`PhaseID`
LEFT JOIN `GraveIndividuals` ON `Graves`.`GraveID` = `GraveIndividuals`.`GraveID`
WHERE `Phases`.`PhaseID` IS NOT NULL AND `Country` = 'Sweden'
GROUP BY Phases.PhaseID"
indiv <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)
head(indiv)
View(indiv)
c14indiv <- merge(c14, indiv, by.x = "PhaseID", by.y = "PhaseID")
View(c14indiv)

sql.command <- "SELECT Phases.PhaseID, Count(`ABotSamples`.`SampleID`)
FROM Phases
LEFT JOIN Sites ON Phases.SiteID = Sites.SiteID
LEFT JOIN `ABotSamples` ON `Phases`.`PhaseID` = `ABotSamples`.`PhaseID`
WHERE `Phases`.`PhaseID` IS NOT NULL AND `Country` = 'Sweden'
GROUP BY Phases.PhaseID"
abot <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)
head(abot)
View(abot)
c14indivabot <- merge(c14indiv, abot, by.x = "PhaseID", by.y = "PhaseID")
View(abot)

sql.command <- "SELECT Phases.PhaseID, Count(`FaunalBones`.`BoneID`)
FROM Phases
LEFT JOIN Sites ON Phases.SiteID = Sites.SiteID
LEFT JOIN `FaunalBones` ON `Phases`.`PhaseID` = `FaunalBones`.`PhaseID`
WHERE `Phases`.`PhaseID` IS NOT NULL AND `Country` = 'Sweden'
GROUP BY Phases.PhaseID"
faun <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)
head(faun)
View(faun)
c14indivabotfaun <- merge(c14indivabot, abot, by.x = "PhaseID", by.y = "PhaseID")
View(c14indivabotfaun)
#merged query results

#Rquery - general
source('functions.R')
sit <- sql.wrapper("SELECT * FROM `Sites`",user,password,hostname,hostuser,keypath,ssh)
pha <- sql.wrapper("SELECT * FROM `Phases`",user,password,hostname,hostuser,keypath,ssh)
c14 <- sql.wrapper("SELECT * FROM `C14Samples`",user,password,hostname,hostuser,keypath,ssh)
gra <- sql.wrapper("SELECT * FROM `Graves`",user,password,hostname,hostuser,keypath,ssh)
abo <- sql.wrapper("SELECT * FROM `ABotSamples`",user,password,hostname,hostuser,keypath,ssh)
zoo <- sql.wrapper("SELECT * FROM `FaunalSpecies`",user,password,hostname,hostuser,keypath,ssh)

#------------------------------------------------------------------
#loop 
#------------------------------------------------------------------
c14data <- c14.count <- cults <- grave.individuals <- abo.sample <- zoo.count <- c()
sweden <- subset(sit, Country == 'Sweden')
N <- nrow(sweden)

for(n in 1:N){
  
  site <- sweden$SiteID[n]
  
  #get c14
  c14.count[n] <- nrow(subset(c14, SiteID==site))
  
  #get cultures
  phase.info <- subset(pha, SiteID==site)
  cultures <- c(phase.info$Culture1, phase.info$Culture2, phase.info$Culture3)
  cultures <- cultures[!is.na(cultures)]
  cults[n] <- paste(cultures, collapse=';')
  
  #get grave info
  grave.info <- subset(gra, PhaseID%in%phase.info$PhaseID)
  grave.individuals[n] <- sum(grave.info$HumanMNI)
  
  #get abot info
  abo.info <- subset(abo, PhaseID%in%phase.info$PhaseID)
  abo.sample[n] <- length(unique(abo.info$TaxonCode))
  
  #get zooarch info
  zoo.info <- subset(zoo, PhaseID%in%phase.info$PhaseID)
  zoo.count[n] <- length(unique(zoo.info$TaxonCode))
  
}

df <- data.frame(SiteID = sweden$SiteID, SiteName = sweden$SiteName, lat = sweden$Latitude, lon = sweden$Longitude, cultures = cults, indivs = grave.individuals, c14count = c14.count, abo.sum = abo.sample, zoocount = zoo.count)
View(df)
write.csv(df, file = "sweden_sumary.csv", row.names = F)
