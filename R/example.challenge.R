#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
# Example R script to test if R connection to BIAD is working
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
# Requirements
# 1. Ensure R is in the same working directory as this file. Use getwd() or list.files() to check
# 2. Ensure you have your .Rprofile file in the same folder that this script is in. See the github readme for details.
#--------------------------------------------------------------------------------------
# Overheads
# source('.Rprofile') # should already have loaded if you open R from this script.
source('functions.R')
#--------------------------------------------------------------------------------------
# Pull some data from BIAD
sql.command <- "SELECT * FROM `Sites`"
query <- run.server.query(sql.command, user, password, hostuser, hostname, pempath)
#------------------------------------------------------------------
# Do something trivial
#------------------------------------------------------------------
plot(table(query$Country),las=2)
#------------------------------------------------------------------


