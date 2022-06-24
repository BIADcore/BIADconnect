#--------------------------------------------------------------------------------------------------
encoder <- function(df){
	if(nrow(df)==0) return(NULL)
	if(nrow(df)>0){
		for(n in 1:ncol(df))
		if(class(df[,n])=="character")df[,n] <- iconv(df[,n],"utf8","utf8")
		return(df)	
		}
	}
#-----------------------------------------------------------------------------------------------
get.plink.pid <- function(){
	tasklist <- shell(cmd='tasklist /nh /fo "csv" /fi "imagename eq plink.exe"',intern=T)
	pid <- c()
	for(n in 1:length(tasklist)){
		pid[n] <- as.numeric(strsplit(tasklist[n],split='\",\"')[[1]][2])
		}
return(pid)}
#--------------------------------------------------------------------------------------------------
sql.wrapper <- function(sql.command,user,password,hostname,hostuser,keypath,ssh){
	require(RMySQL)
	require(odbc)
	drv <- dbDriver("MySQL")
	
	# only needed if connecting externally
	if(ssh){

		# check if plink already has some tunnels open
		pid.current <- get.plink.pid()

		# open new ssh tunnel for R
		open.ssh.tunnel <- paste("plink -ssh ",hostuser,"@",hostname," -i",keypath,"-N -L 3306:",hostname,":3306")
		shell(open.ssh.tunnel, wait=FALSE)	

		# get pid for this tunnel
		pid.all <- get.plink.pid()
		pid.remove <- pid.all[!pid.all%in%pid.current]
		}

	# connect locally to the database
	con <- dbConnect(drv,host = "127.0.0.1", user=user, pass=password)
	dbSendQuery(con,"SET NAMES 'utf8'")

	# query the database and tidy
	for(n in 1:length(sql.command)) res <- suppressWarnings(dbSendQuery(con,sql.command[n]))
	query <- fetch(res, n= -1)
	query <- encoder(query)

	# close the connection to the database
	suppressWarnings(dbDisconnect(con))

	# close this tunnel
	if(ssh){
		close.ssh.tunnel <- paste('taskkill /f /fi "pid eq ',pid.remove,'"',sep='')
		shell(close.ssh.tunnel)
		}

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
	writeLines(text, con=file, useBytes = TRUE )
return(NULL)}
#--------------------------------------------------------------------------------------------------
create.markdown.for.table.content <- function(x, d.cols, file){
       
	text <- '| Table | Number of rows | Number of columns | Column names |'
	text <- c(text,'| ----------- | ----------- | ----------- | ----------- |')

	for(n in 1:nrow(x)){
		cols <- subset(d.cols, TABLE_NAME==x$TABLE_NAME[n])
		colnames <- paste(cols$COLUMN_NAME,collapse=', ')
		txt <- paste('| ',x$TABLE_NAME[n],' | ',x$TABLE_ROWS[n],' | ',nrow(cols),' | ', colnames,' | ', sep='')
		text <- c(text,txt)
		}
	text <- c(text, '***')
	writeLines(text, con=file, useBytes = TRUE )
return(NULL)}
#--------------------------------------------------------------------------------------------------