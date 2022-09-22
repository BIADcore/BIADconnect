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
# Pull some data from BIAD
sit <- sql.wrapper("SELECT * FROM `Sites`",user,password,hostname,hostuser,keypath,ssh)
#------------------------------------------------------------------
# Do something trivial
#------------------------------------------------------------------
plot(sit$Longitude,sit$Latitude)
#------------------------------------------------------------------


