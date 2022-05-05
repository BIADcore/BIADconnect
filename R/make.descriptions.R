
#------------------------------------------------------------------
# Pull table summaries from the dtabase, and update to Gists
#------------------------------------------------------------------

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
table.name <- standard[1]

table.comment <- subset(d.tables, TABLE_NAME==table.name)$TABLE_COMMENT
col.names <- subset(d.cols, TABLE_NAME==table.name)$COLUMN_NAME
col.comments <- subset(d.cols, TABLE_NAME==table.name)$COLUMN_COMMENT

text <- paste('#', table.name)
text <- c(text, table.comment)
for(n in 1:length(col.names)){
	text <- c(text, paste('###', col.names[n]))
	text <- c(text, col.comments[n])
	}
text <- c(text, '***)

writeLines(text, con='../../Gists/table_comments/standard/standard.md')