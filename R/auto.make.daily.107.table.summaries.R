conn  <-  init.conn()
#-----------------------------------------------------------------------------------------
# Generate a summary of row counts etc
#-----------------------------------------------------------------------------------------
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
# create various summary table .svg
#-----------------------------------------------------------------------------------------
create.svg.for.table.content(table.data=standard.table.data, column.data=standard.column.data, file='../tools/plots/row_counts.svg')
create.svg.for.row.content(table.data=standard.table.data, column.data=standard.column.data, file='../tools/plots/table_summary.svg')
#-----------------------------------------------------------------------------------------
disconnect()




	
