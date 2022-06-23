#-----------------------------------------------------------------------------------------
# Pull table summaries from the database, and update to Gists
#-----------------------------------------------------------------------------------------
sql.command <- "SELECT * FROM INFORMATION_SCHEMA.TABLES WHERE table_schema='BIAD'"
d <- sql.wrapper(sql.command,user,password)	

sql.command <- "SELECT * FROM INFORMATION_SCHEMA.COLUMNS WHERE table_schema='BIAD';"	
d.cols <- sql.wrapper(sql.command,user,password)	
#-----------------------------------------------------------------------------------------
# Pull out just the main tables
all <- d$TABLE_NAME
zprivate <- all[grepl('zprivate', all)]
zoptions <- all[grepl('zoptions', all)]
copy <- all[grepl('_copy', all)]
standard <- all[!all%in%c(zprivate,zoptions,copy)]

x <- subset(d, TABLE_NAME%in%standard)
x <- subset(x, TABLE_ROWS>100)

file = '../../Gists/summary_stats/row_counts/row_counts.md'
create.markdown.for.table.content(x, d.cols, file)
#-----------------------------------------------------------------------------------------
