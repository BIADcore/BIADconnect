#-----------------------------------------------------------------------------------------
# Generate a summary of row counts etc
#-----------------------------------------------------------------------------------------
sql.command <- "SELECT * FROM INFORMATION_SCHEMA.TABLES WHERE table_schema='BIAD'"
d <- query.database(sql.command = sql.command, conn=conn)

sql.command <- "SELECT * FROM INFORMATION_SCHEMA.COLUMNS WHERE table_schema='BIAD';"	
d.cols <- query.database(sql.command = sql.command, conn=conn)
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
lookup <- subset(lookup, TABLE_ROWS>1)
#-----------------------------------------------------------------------------------------
create.svg.for.table.content(x=standard, d.cols, file='../tools/plots/row_counts.svg')
#-----------------------------------------------------------------------------------------
