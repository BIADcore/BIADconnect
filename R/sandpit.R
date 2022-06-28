#-----------------------------------------------------------------------------------------
# general sandpit
#-----------------------------------------------------------------------------------------
source('functions.R')
#-----------------------------------------------------------------------------------------
# Convert skeleton parts to a zoptions look up
#-----------------------------------------------------------------------------------------
map <- read.csv('mapping final.csv')
map[map==''] <- '\\N'
d <- sql.wrapper(sql.command = "SELECT SkeletonPart, SkeletonSide FROM BIAD.FaunalIsotopesCopyAdrian",user,password,hostname,hostuser,keypath,ssh)
N <- nrow(map)

sql.side <- c()
for(n in 1:N){
	sql.side[n] <- paste("UPDATE BIAD.FaunalIsotopesCopyAdrian SET SkeletonSide='",map$symmetry[n],"' WHERE SkeletonPart='",map$oldpart[n],"';",sep='')
	}


# map everything with a loop
oldpart <- sort(unique(d$SkeletonPart))
N <- length(oldpart)
df <- data.frame(oldpart=oldpart,newpart=NA, side=NA)
#write.csv(df, file='mapping.csv',row.names=F)

sql.command <- c()
sql.command[1] <- "UPDATE BIAD.FaunalIsotopesCopyAT SET SkeletonSide='Left' WHERE SkeletonPart='radius L';"
sql.command[2] <- "UPDATE BIAD.FaunalIsotopesCopyAT SET SkeletonPart='radius' WHERE SkeletonPart='radius L';"
sql.command[3] <- "UPDATE BIAD.FaunalIsotopesCopyAT SET SkeletonSide='Left' WHERE SkeletonPart='metacarpal L';"
sql.command[4] <- "UPDATE BIAD.FaunalIsotopesCopyAT SET SkeletonPart='metacarpal' WHERE SkeletonPart='metacarpal L';"
sql.command[5] <- "UPDATE BIAD.FaunalIsotopesCopyAT SET SkeletonSide='Left' WHERE SkeletonPart='tibia L';"
sql.command[6] <- "UPDATE BIAD.FaunalIsotopesCopyAT SET SkeletonPart='tibia' WHERE SkeletonPart='tibia L';"

sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)

