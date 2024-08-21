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
tax <- run.server.query("SELECT * FROM `zoptions_TaxaList`;")

c14 <- c14[,c('C14ID','LabID','PhaseID','SiteID','Period','C14.Age','C14.SD','δ13C','δ13C.SD','C/N_collagen','Material','CitationID','TaxonCode')]
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
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
# start inputting deadhead data into c14 table
#--------------------------------------------------------------------------------------
# 1. Lowest hanging fruit: matches on labcode AND c14 date AND SD
all <- merge(biad, dead, by='LabID', all=FALSE)
all <- all[all$C14.Age==all$CRA,]
all <- all[all$C14.SD==all$Error,]

# Periods
# needs some thought later, as many deadhead samples have culture and period info where BIAD does not, but ideally should be assigned to the phase
period <- all[,c('LabID','Period.x','Period.y','CulturalPeriod')]
period <- period[!is.na(period$CulturalPeriod) & is.na(period$Period.x) & is.na(period$Period.y),]


# δ13C. Many have a decimal place in the wrong place
delta <- all[,c('LabID','δ13C','DC13')]
delta <- delta[!is.na(delta$DC13) & is.na(delta$δ13C),]
bad <- delta$DC13<(-99)
delta$DC13[bad] <- delta$DC13[bad]/1000
text <- c()
for(n in 1:nrow(delta))text[n] <- paste("UPDATE `BIAD`.`C14Samples` SET `δ13C`=",delta$DC13[n]," WHERE  `LabID`='",delta$LabID[n],"';",sep='')
# run.server.query(text)

# Flags can be inherited, but needs thought. circa 15% have one

# OthMeasures can be inherited, only 6% have one, various including human age and sex, and other FC14 values??

# DateMethod
dm <- all[,c('LabID','DateMethod')]
dm <- dm[!is.na(dm$DateMethod),]
dm$DateMethod[dm$DateMethod=='WMD'] <- NA
dm$DateMethod[dm$DateMethod=='AWM'] <- NA
dm$DateMethod[dm$DateMethod=='LSC (HP);LSC'] <- 'LSC (HP)'
dm <- dm[!is.na(dm$DateMethod),]
text <- c()
for(n in 1:nrow(dm))text[n] <- paste("UPDATE `BIAD`.`C14Samples` SET `Method`='",dm$DateMethod[n],"' WHERE  `LabID`='",dm$LabID[n],"';",sep='')
# run.server.query(text)

# Species
sp <- all[,c('LabID','TaxonCode','Species')]
sp <- sp[!is.na(sp$Species) & is.na(sp$TaxonCode),]
sp$Species[sp$Species=='Homo sapiens?'] <- 'Homo sapiens'


# testing
test <- data.frame(Species=unique(sp$Species))
mm <- merge(test, tax, by.x='Species', by.y='FullNameOfTaxon', all=FALSE)
bad <- sp$Species[!sp$Species%in%mm$Species]





