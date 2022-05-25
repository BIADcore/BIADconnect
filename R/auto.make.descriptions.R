
#-----------------------------------------------------------------------------------------
# Pull table summaries from the database, and update to Gists
#-----------------------------------------------------------------------------------------

# Pull all table meta data

sql.command <- "SELECT * FROM INFORMATION_SCHEMA.TABLES WHERE table_schema='COREX';"	
d.tables <- sql.wrapper(sql.command,user,password)	
	
sql.command <- "SELECT * FROM INFORMATION_SCHEMA.COLUMNS WHERE table_schema='COREX';"	
d.cols <- sql.wrapper(sql.command,user,password)	

# Group tables to put into different parts of the wiki:
all <- d.tables$TABLE_NAME
zprivate <- all[grepl('zprivate', all)]
zoptions <- all[grepl('zoptions', all)]
copy <- all[grepl('_copy', all)]
standard <- all[!all%in%c(zprivate,zoptions,copy)]

# construct a single markdown file for all standard tables and zoptions
create.markdown.for.several.tables(d.tables, d.cols, table.names = standard, file = '../../Gists/table_comments/standard/standard.md')
create.markdown.for.several.tables(d.tables, d.cols, table.names = zoptions, file = '../../Gists/table_comments/zoptions/zoptions.md')
#-----------------------------------------------------------------------------------------
