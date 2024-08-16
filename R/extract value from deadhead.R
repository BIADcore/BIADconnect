#--------------------------------------------------------------------------------------
# Requirements
# You must have previously added the .Rprofile to your R_USER folder, here -> path.expand('~/') 
# See the BIADwiki readme or BIADwiki for details about using the .Rprofile

# Load some required functions
source("https://raw.githubusercontent.com/BIADwiki/BIADwiki/main/R/functions.R")
library(dplyr)
#--------------------------------------------------------------------------------------
c14 <- run.server.query("SELECT * FROM `C14Samples`;")
pha <- run.server.query("SELECT * FROM `Phases`;")
sit <- run.server.query("SELECT * FROM `Sites`;")
dead <- run.server.query("SELECT * FROM `zprivate_deadhead`;")

c14 <- c14[,c('C14ID','LabID','PhaseID','SiteID','Period','C14.Age','C14.SD','δ13C','δ13C.SD','C/N_collagen','Material','CitationID')]
pha <- pha[,c('PhaseID','SiteID','Culture1','Culture2','Culture3','Period')]
sit <- sit[,c('SiteID','SiteName','Latitude','Longitude','Country')]

m1 <- merge(c14, sit, by='SiteID')
biad <- merge(m1, pha, by='PhaseID', all.x=TRUE)
# note, .y NAs are for 14C dates with no phase, so the .y table is the phase table info

#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
# Match by labcode, explore importing 'Material'
d <- merge(c14,dead,by='LabID',both=FALSE)
d <- subset(d, is.na(Material.x) & !is.na(Material.y))
sort(table(d$Material.y),decreasing=T)

sort(table(c14$Material),decreasing=T)
sort(table(dead$Material),decreasing=T)

sum(sort(table(dead$Material),decreasing=T)<5)
sum(sort(table(dead$Material),decreasing=T)>50)
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
# Extract the Materials and equivalent columns with frequencies from the different tables
dead <- run.server.query("SELECT DISTINCT Material AS Material, COUNT(Material) AS n FROM zprivate_deadhead GROUP BY Material;")
c14 <-  run.server.query("SELECT DISTINCT Material AS Material, COUNT(Material) AS n FROM C14Samples GROUP BY Material;")
matcul <-  run.server.query("SELECT DISTINCT Material AS Material, COUNT(Material) AS n FROM MaterialCulture GROUP BY Material;")
stron <- run.server.query("SELECT DISTINCT Material AS Material, COUNT(Material) AS n FROM Strontium GROUP BY Material;")
fauni <- run.server.query("SELECT DISTINCT Material AS Material, COUNT(Material) AS n FROM FaunalIsotopes GROUP BY Material;")
human <- run.server.query("SELECT DISTINCT Material AS Material, COUNT(Material) AS n FROM HumanIsotopes GROUP BY Material;")
aboti <- run.server.query("SELECT DISTINCT anatomy AS Material, COUNT(anatomy) AS n FROM ABotIsotopes GROUP BY anatomy;")
abots1 <- run.server.query("SELECT DISTINCT Anatomy1 AS Material, COUNT(Anatomy1) AS n FROM ABotSamples GROUP BY Anatomy1")
abots2 <- run.server.query("SELECT DISTINCT Anatomy2 AS Material, COUNT(Anatomy2) AS n FROM ABotSamples GROUP BY Anatomy2")
abots3 <- run.server.query("SELECT DISTINCT Anatomy3 AS Material, COUNT(Anatomy3) AS n FROM ABotSamples GROUP BY Anatomy3")
abots4 <- run.server.query("SELECT DISTINCT Anatomy4 AS Material, COUNT(Anatomy4) AS n FROM ABotSamples GROUP BY Anatomy4")

# Store the results as a single table
materials <- do.call("rbind", list(dead, c14, matcul, stron, fauni, human, aboti, abots1, abots2, abots3, abots4))

# merge identical variables
materials <- materials %>%
  group_by(Material) %>%
  summarise(sum(n))
