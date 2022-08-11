#-----------------------------------------------------------------------------------------
# Pull table information from backups
# Do not attempt to run this script from anywhere other than the server!!
# Not only are the file paths specific to the server, but a remote machine will take forever!
#-----------------------------------------------------------------------------------------
file <- "/Users/admin/dropbox/MySQLbackups/BIAD/monthly/BIADbackup20220701055503.sql"
#-----------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
q <- get.tables.from.backup(file)
#-----------------------------------------------------------------------------------------
