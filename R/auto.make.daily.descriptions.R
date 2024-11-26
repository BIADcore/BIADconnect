
#-----------------------------------------------------------------------------------------
# Pull table summaries from the database, and update to Gists
#-----------------------------------------------------------------------------------------

# Pull all table meta data
conn <- init.conn()
sql.command <- "SELECT * FROM INFORMATION_SCHEMA.TABLES WHERE table_schema='BIAD';"	
d.tables <- query.database(sql.command = sql.command, conn=conn)
	
sql.command <- "SELECT * FROM INFORMATION_SCHEMA.COLUMNS WHERE table_schema='BIAD';"	
d.cols <- query.database(sql.command = sql.command, conn=conn)

# Group tables to put into different parts of the wiki:
all <- d.tables$TABLE_NAME
zprivate <- all[grepl('zprivate', all)]
zoptions <- all[grepl('zoptions', all)]
temp <- all[grepl('_copy|_update', all)]
standard <- all[!all%in%c(zprivate,zoptions,temp)]

# construct a single markdown file for all standard tables and zoptions
create.markdown.for.several.tables(d.tables, d.cols, table.names = standard, file = '../../Gists/table_comments/standard/standard.md')
create.markdown.for.several.tables(d.tables, d.cols, table.names = zoptions, file = '../../Gists/table_comments/zoptions/zoptions.md')
disconnect()
#-----------------------------------------------------------------------------------------
