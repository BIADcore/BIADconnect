#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
# get all C14 dates associated with bell beaker
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
# Requirements first read:
# https://biadwiki.org/en/connectR
# ensure you have opened a tunnel first (e.g. putty)
#--------------------------------------------------------------------------------------
conn  <-  init.conn()
#--------------------------------------------------------------------------------------
x <- query.database(sql.command)
head(x)
#--------------------------------------------------------------------------------------
disconnect()
#--------------------------------------------------------------------------------------
