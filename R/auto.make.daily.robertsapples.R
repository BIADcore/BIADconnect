
sql.command <- "SELECT * FROM BIAD.Sites"
d <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)
d <- d[1:5,]
d[is.na(d)] <- '\\N'
d$timestamp <- '\\N'
d$userstamp <- '\\N'

library(gridExtra)


mytheme <- ttheme_default(base_size=25)

png('../tools/plots/apples.png',width=1200, height=350)
grid.table(d, theme=mytheme)
dev.off()
