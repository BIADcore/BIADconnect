#------------------------------------------------------------------
# Example R script for directly querying BIAD using the RMySQL package
#------------------------------------------------------------------
# First obtain the following objects from the BIAD administrator.
# Either keep them at the beginning of each script, or shove them into a .Rprofile file

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
#  Alternatively, without the helper functions:
#------------------------------------------------------------------
require(RMySQL)

# open new ssh tunnel for R
open.ssh.tunnel <- paste("plink -ssh ",hostuser,"@",hostname," -i ",keypath," -N -L 3306:",hostname,":3306",sep='')
shell(open.ssh.tunnel, wait=FALSE)	

# connect to BIAD
drv <- dbDriver("MySQL")
con <- dbConnect(drv,host = "127.0.0.1", user=user, pass=password)

# query BIAD
sql.command <- "SELECT * FROM BIAD.Sites" 
con <- dbConnect(drv,host = "127.0.0.1", user=user, pass=password)
dbSendQuery(con,"SET NAMES 'utf8'")
res <- dbSendQuery(con,sql.command)
query <- fetch(res, n= -1)
dbDisconnect(con)

#------------------------------------------------------------------
# Either way, the object 'query' can now be inspected
#------------------------------------------------------------------
head(query)
table(query$Country)
plot(table(query$Country))

#-----------------------------------------------------------------