
sql.command <- "SELECT * FROM BIAD.Sites"
d <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)
d <- d[1:5,]
d[is.na(d)] <- '\\N'
d$timestamp <- '\\N'
d$userstamp <- '\\N'

library(gridExtra)

mytheme <- gridExtra::ttheme_default(
    core = list(fg_params=list(cex = 1.5)),
    colhead = list(fg_params=list(cex = 1.5)),
    rowhead = list(fg_params=list(cex = 1.5)))

png('../tools/plots/apples.png',width=1600, height=400)
grid.table(d, theme=mytheme)
dev.off()
