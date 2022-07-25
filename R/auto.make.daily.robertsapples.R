
sql.command <- "SELECT * FROM BIAD.Sites"
d <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)
d <- d[1:5,]
d[is.na(d)] <- '\\N'
d$timestamp <- '\\N'
d$userstamp <- '\\N'

library(gridExtra)


mytheme <- ttheme_default(base_size=20)

png('../tools/plots/apples.png',width=2000, height=400)
grid.table(d, theme=mytheme)
dev.off()
