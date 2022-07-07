#------------------------------------------------------------------
# Example R script for directly querying BIAD using the RMySQL package
#------------------------------------------------------------------
# First obtain the following objects from the BIAD administrator.
# Put them in a .Rprofile file, in the same folder that this script is in.

user <- '???'
password <- '???'
hostname <- '???'
hostuser <- '???'
keypath <- '???'
ssh <- TRUE

#------------------------------------------------------------------
# helper functions in the functions.R script take care of various problems:
#------------------------------------------------------------------
source('functions.R')
sql.command <- "SELECT * FROM BIAD.Sites"
query <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)
#------------------------------------------------------------------
# The object 'query' can now be inspected
#------------------------------------------------------------------
head(query)
table(query$Country)
plot(table(query$Country))

#-----------------------------------------------------------------