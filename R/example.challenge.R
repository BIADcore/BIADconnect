#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
# Example R script to test if R connection to BIAD is working
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
# Requirements first read:
# https://biadwiki.org/en/connectR
#--------------------------------------------------------------------------------------
source("https://raw.githubusercontent.com/BIADwiki/BIADwiki/main/R/functions.R")
open.tunnel()
query <- query.database("SELECT * FROM `Sites`")
plot(table(query$Country),las=2)
close.tunnel()
#------------------------------------------------------------------

