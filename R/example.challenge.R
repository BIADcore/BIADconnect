#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
# Example R script to test if R connection to BIAD is working
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
# Requirements
# You must have previously added the .Rprofile to your R_USER folder, here -> path.expand('~/') 
# See the BIADwiki readme or BIADwiki for details about using the .Rprofile

# Load some required functions
source("https://raw.githubusercontent.com/BIADwiki/BIADwiki/main/R/functions.R")
#--------------------------------------------------------------------------------------
# Pull some data from BIAD
query <- run.server.query("SELECT * FROM `Sites`")

# Do something trivial
plot(table(query$Country),las=2)
#------------------------------------------------------------------
