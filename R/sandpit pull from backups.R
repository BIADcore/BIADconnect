#-----------------------------------------------------------------------------------------
# Pull table information from backups
# Do not attempt to run this script from anywhere other than the server!!
# Not only are the file paths specific to the server, but a remote machine will take forever!
#-----------------------------------------------------------------------------------------
file <- "/Users/admin/dropbox/MySQLbackups/BIAD/monthly/BIADbackup20220701055503.sql"
#-----------------------------------------------------------------------------------------
raw <- readLines(file)

# get a summary of each table

start.posts <- grep('CREATE TABLE', raw)
end.posts <- grep('Dumping data for table', raw)

n <- 1

# for a particular table, get the primary key and stamp info
table.info <- raw[start.posts[n]:end.posts[n]]
primary.key.row <- grep('PRIMARY KEY', table.info)
primary.key.line <- table.info[primary.key.row]
primary.key <- regmatches(primary.key.line, gregexpr("(?<=\`)(.*?)(?=\`)", primary.key.line, perl=T))[[1]]
row.info <- table.info[2:(primary.key.row -1)]
column.names <- unlist(regmatches(row.info, gregexpr("(?<=\`)(.*?)(?=\`)", row.info, perl=T)))
table.name <- regmatches(table.info[1], gregexpr("(?<=\`)(.*?)(?=\`)", table.info[1], perl=T))[[1]]

# convert to a table
d <- raw[end.posts[n]+5]
d <- gsub(paste("INSERT INTO `",table.name,"` VALUES (",sep=''),"", d, fixed=TRUE)
d <- substr(d,1,nchar(d)-2)
d <- strsplit(d, split='),(', fixed=T)[[1]]
data <- read.table(text=d,sep=',', col.names=column.names, encoding = "UTF-8")
#-----------------------------------------------------------------------------------------

