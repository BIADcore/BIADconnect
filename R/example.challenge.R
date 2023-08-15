#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
# Example R script to test if R connection to BIAD is working
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
# Requirements
# You must have previously (once only) added various objects to your system environmental variables
# See the BIADwiki readme or BIADwiki for details about using Sys.setenv()
#--------------------------------------------------------------------------------------
# Overheads
source("https://raw.githubusercontent.com/BIADwiki/BIADwiki/main/R/functions.R")
#--------------------------------------------------------------------------------------
# Pull some data from BIAD
sql.command <- "SELECT * FROM `Sites`"
query <- run.server.query(sql.command)
#------------------------------------------------------------------
# Do something trivial
#------------------------------------------------------------------
plot(table(query$Country),las=2)
#------------------------------------------------------------------
