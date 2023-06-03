#-----------------------------------------------------------------------------------------
# Pull table summaries from the database, and update to Gists
#-----------------------------------------------------------------------------------------
sql.command <- "SELECT * FROM INFORMATION_SCHEMA.TABLES WHERE table_schema='BIAD'"
d <- query.database(user, password, sql.command)

sql.command <- "SELECT * FROM INFORMATION_SCHEMA.COLUMNS WHERE table_schema='BIAD';"	
d.cols <- query.database(user, password, sql.command)	
#-----------------------------------------------------------------------------------------
all <- d$TABLE_NAME
zprivate <- all[grepl('zprivate', all)]
zoptions <- all[grepl('zoptions', all)]
copy <- all[grepl('_copy', all)]
standard <- all[!all%in%c(zprivate,zoptions,copy)]
lookup <- zoptions[!zoptions%in%copy]

standard <- subset(d, TABLE_NAME%in%standard)
standard <- subset(standard, TABLE_ROWS>10)

lookup <- subset(d, TABLE_NAME%in%lookup)
lookup <- subset(standard, TABLE_ROWS>1)

create.markdown.for.table.content(standard, d.cols, file = '../../Gists/summary_stats/row_counts/row_counts.md')
create.markdown.for.table.content(lookup, d.cols, file = '../../Gists/summary_stats/row_counts/row_counts.md')
#-----------------------------------------------------------------------------------------