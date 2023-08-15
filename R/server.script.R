user <- ''
password <- ''
hostuser <- ''
source('https://raw.githubusercontent.com/BIADwiki/BIADwiki/main/R/functions.R')
sql.command <- c("SELECT * FROM `Sites`")
query <- query.database(user, password, sql.command)
save(query, file='tmp.RData')
