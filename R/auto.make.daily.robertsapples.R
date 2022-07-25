
sql.command <- "SELECT * FROM BIAD.Sites"
d <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)
d <- d[1:5,]
d[is.na(d)] <- '\\N'
d$timestamp <- '\\N'
d$userstamp <- '\\N'

library(gridExtra)
png('../tools/plots/apples.png',width=1600, height=400)
grid.table(d)
dev.off()
