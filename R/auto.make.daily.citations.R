source('functions.R')
sql.command <- "SELECT * FROM BIAD.Citations"
d <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)
d <- d[1:5,]
d[is.na(d)] <- '\\N'
d$timestamp <- '\\N'
d$userstamp <- '\\N'

library(gridExtra)

png('../tools/plots/citations.png',width=4000, height=500)
grid.table(d, theme=ttheme_minimal())
dev.off()


# mytheme <- ttheme_default(base_size=25)