
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
ztemp <- all[grepl('ztemp', all)]
zoptions <- all[grepl('zoptions', all)]
copy <- all[grepl('_copy', all)]
standard <- all[!all%in%c(ztemp,zoptions,copy)]

# construct a single markdown file for all standard tables
text <- c()
for(n in 1:length(standard)){
	table.name <- standard[n]
	table.text <- create.markdown.for.single.table(d.tables, d.cols, table.name)	
	text <- c(text, table.text)
	}

writeLines(text, con='../../Gists/table_comments/standard/standard.md')
#-----------------------------------------------------------------------------------------



