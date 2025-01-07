#-----------------------------------------------------------------------------------------
# Generate a summary of row counts etc
#-----------------------------------------------------------------------------------------
conn  <-  init.conn()

sql.command <- "SELECT * FROM INFORMATION_SCHEMA.TABLES WHERE table_schema='BIAD'"
d.tables <- query.database(sql.command = sql.command, conn=conn)

sql.command <- "SELECT * FROM INFORMATION_SCHEMA.COLUMNS WHERE table_schema='BIAD';"	
d.cols <- query.database(sql.command = sql.command, conn=conn)
#-----------------------------------------------------------------------------------------
# get rid of common meta
#-----------------------------------------------------------------------------------------
d.cols <- d.cols[!grepl('time_added|user_added|time_last_update|user_last_update',d.cols$COLUMN_NAME),]
#-----------------------------------------------------------------------------------------
# Group tables to put into different types
#-----------------------------------------------------------------------------------------
all <- d.tables$TABLE_NAME
all <- all[!grepl('_copy|_update', all)]
zprivate <- all[grepl('zprivate', all)]
zoptions <- all[grepl('zoptions', all)]
standard <- all[!all%in%c(zprivate,zoptions)]
#-----------------------------------------------------------------------------------------
# summary data
#-----------------------------------------------------------------------------------------
standard.table.data <- subset(d.tables, TABLE_NAME%in%standard)
standard.table.data  <- subset(standard.table.data, TABLE_ROWS>10)

zoptions.table.data  <- subset(d.tables, TABLE_NAME%in%zoptions)
zoptions.table.data  <- subset(zoptions.table.data, TABLE_ROWS>2)

standard.column.data <- subset(d.cols, TABLE_NAME%in%standard.table.data$TABLE_NAME)[,c('TABLE_NAME','COLUMN_NAME','DATA_TYPE','COLUMN_COMMENT')]
zoptions.column.data <- subset(d.cols, TABLE_NAME%in%zoptions.table.data$TABLE_NAME)[,c('TABLE_NAME','COLUMN_NAME','DATA_TYPE','COLUMN_COMMENT')]
#-----------------------------------------------------------------------------------------
# create various summary table .html
#-----------------------------------------------------------------------------------------
create.html.for.table.comments(table.data=standard.table.data, column.data=standard.column.data, file='../tools/table_comments/row_counts.html')
create.html.for.row.comments(table.data=standard.table.data, column.data=standard.column.data, file='../tools/table_comments/table_summary.html')
create.html.for.row.comments(table.data=zoptions.table.data, column.data=zoptions.column.data, file='../tools/table_comments/zoptions_summary.html')
#--------------------------------------------------------------------------------------
disconnect()
#--------------------------------------------------------------------------------------



