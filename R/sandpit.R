#-----------------------------------------------------------------------------------------
# general sandpit
#-----------------------------------------------------------------------------------------
source('functions.R')
#-----------------------------------------------------------------------------------------
# Convert skeleton parts to a zoptions look up
#-----------------------------------------------------------------------------------------

d <- sql.wrapper(sql.command = "SELECT SkeletonPart, SkeletonSide FROM BIAD.FaunalIsotopesCopyAT",user,password)

table(d$SkeletonPart)

sql.command <- c()
sql.command[1] <- "UPDATE BIAD.FaunalIsotopesCopyAT SET SkeletonSide='Left' WHERE SkeletonPart='radius L';"
sql.command[2] <- "UPDATE BIAD.FaunalIsotopesCopyAT SET SkeletonPart='radius' WHERE SkeletonPart='radius L';"
sql.command[3] <- "UPDATE BIAD.FaunalIsotopesCopyAT SET SkeletonSide='Left' WHERE SkeletonPart='metacarpal L';"
sql.command[4] <- "UPDATE BIAD.FaunalIsotopesCopyAT SET SkeletonPart='metacarpal' WHERE SkeletonPart='metacarpal L';"
sql.command[5] <- "UPDATE BIAD.FaunalIsotopesCopyAT SET SkeletonSide='Left' WHERE SkeletonPart='tibia L';"
sql.command[6] <- "UPDATE BIAD.FaunalIsotopesCopyAT SET SkeletonPart='tibia' WHERE SkeletonPart='tibia L';"

sql.wrapper(sql.command,user,password)

