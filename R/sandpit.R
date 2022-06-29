#-----------------------------------------------------------------------------------------
# general sandpit
#-----------------------------------------------------------------------------------------
source('functions.R')
#-----------------------------------------------------------------------------------------
# Convert skeleton parts to a zoptions look up
#-----------------------------------------------------------------------------------------
map <- read.csv('map.csv')

d <- sql.wrapper(sql.command = "SELECT Element FROM BIAD.FaunalBones",user,password,hostname,hostuser,keypath,ssh)

sql <- c()
N <- nrow(map)
for(n in 1:N){
	sql[n] <- paste("UPDATE BIAD.FaunalBones SET Element='",map$new[n],"' WHERE Element='",map$old[n],"';",sep='')
	}

sql.wrapper(sql,user,password,hostname,hostuser,keypath,ssh)
#-----------------------------------------------------------------------------------------
d1 <- sql.wrapper(sql.command = "SELECT Element FROM BIAD.zoptions_SkeletalElements",user,password,hostname,hostuser,keypath,ssh)

a <- d1[,1]
b <- map$new
b[!b%in%a]

df <- data.frame(element=b[!b%in%a])
write.csv(df,file='new.csv', row.names=F)