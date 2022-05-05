#--------------------------------------------------------------------------------------------------
encoder <- function(df){
	if(nrow(df)==0) return(NULL)
	if(nrow(df)>0){
		for(n in 1:ncol(df))
		if(class(df[,n])=="character")df[,n] <- iconv(df[,n],"utf8","utf8")
		return(df)	
		}
	}
#--------------------------------------------------------------------------------------------------
sql.wrapper <- function(sql.command,user,password){
	require(RMySQL)
	require(odbc)
	drv <- dbDriver("MySQL")
	
	# connect locally to the database
	con <- dbConnect(drv,host = "127.0.0.1", user=user, pass=password)
	dbSendQuery(con,"SET NAMES 'utf8'")

	# query the database and tidy
	res <- suppressWarnings(dbSendQuery(con,sql.command))
	query <- fetch(res, n= -1)
	query <- encoder(query)

	# close the connection to the database
	suppressWarnings(dbDisconnect(con))
return(query)}
#--------------------------------------------------------------------------------------------------
create.markdown.for.single.table <- function(d.tables, d.cols, table.name){
	
	table.comment <- subset(d.tables, TABLE_NAME==table.name)$TABLE_COMMENT
	col.names <- subset(d.cols, TABLE_NAME==table.name)$COLUMN_NAME
	col.comments <- subset(d.cols, TABLE_NAME==table.name)$COLUMN_COMMENT

	text <- paste('#', table.name)
	text <- c(text, table.comment)
	for(n in 1:length(col.names)){
		text <- c(text, paste('###', col.names[n]))
		text <- c(text, col.comments[n])
		}
	text <- c(text, '***')
return(text)}
#--------------------------------------------------------------------------------------------------
create.markdown.for.several.tables <- function(d.tables, d.cols, table.names, file){
	
	text <- c()
	for(n in 1:length(table.names)){
		table.name <- table.names[n]
		table.text <- create.markdown.for.single.table(d.tables, d.cols, table.name)	
		text <- c(text, table.text)
		}

	writeLines(text, con=file)
return(NULL)}
#--------------------------------------------------------------------------------------------------