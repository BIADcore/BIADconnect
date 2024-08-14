
#--------------------------------------------------------------------------------------
# Requirements
# You must have previously added the .Rprofile to your R_USER folder, here -> path.expand('~/') 
# See the BIADwiki readme or BIADwiki for details about using the .Rprofile

# Load some required functions
source("https://raw.githubusercontent.com/BIADwiki/BIADwiki/main/R/functions.R")
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
# explore matching LabID between biad and deadhead
d <- merge(c14,dead,by='LabID',both=FALSE)
d <- subset(d, is.na(Material.x) & !is.na(Material.y))
sort(table(d$Material.y),decreasing=T)

sort(table(c14$Material),decreasing=T)
#--------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
