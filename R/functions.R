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
get.plink.pids <- function(){

	system <- .Platform$OS.type

	if(system=='windows'){
		tasklist <- system('tasklist /nh /fo "csv" /fi "imagename eq plink.exe"',intern=T)
		pid <- c()
		N <- length(tasklist)
		if(N>0){
			for(n in 1:N){
				pid[n] <- as.numeric(strsplit(tasklist[n],split='\",\"')[[1]][2])
				}
			}
		}
	if(system=='unix'){
		tasklist <- suppressWarnings(system("lsof -i -n | egrep 'ssh'",intern=T))
		pid <- c()
		N <- length(tasklist)
		if(N>0){
			for(n in 1:N){
				x <- strsplit(tasklist[n],split=' ')[[1]]	
				x <- x[x!='']
				pid[n] <- as.numeric(x[2])
				}
			}
		}
		pid <- unique(pid)
return(pid)}
#--------------------------------------------------------------------------------------------------
open.ssh.tunnel <- function(hostuser, hostname, keypath){

	system <- .Platform$OS.type
	if(system=='windows'){
		cmd <- paste("plink -ssh ",hostuser,"@",hostname," -i ",keypath," -N -L 3306:",hostname,":3306",sep='')
		system(cmd, wait=FALSE)
		}
	if(system=='unix'){
		cmd <- paste("ssh ",hostuser,"@",hostname," -i ",keypath," -N -L 3306:",hostname,":3306",sep='')
		system(cmd,wait=FALSE)
		}
return(NULL)}
#--------------------------------------------------------------------------------------------------
close.ssh.tunnel <- function(pid){

	system <- .Platform$OS.type
	if(system=='windows'){
		cmd <- paste('taskkill /f /fi "pid eq ',pid,'"',sep='')
		system(cmd, wait=FALSE)
		}
	if(system=='unix'){
#		cmd <- paste("kill -9 ",pid,sep='')
		cmd <- 'kill -9 ssh'
#		cmd <- kill -9 $(lsof -ti :$3306)
		system(cmd,wait=FALSE)
		}



return(NULL)}
#--------------------------------------------------------------------------------------------------
query.database <- function(user, password, sql.command){
	require(RMySQL)
	require(odbc)
	drv <- dbDriver("MySQL")

	# close any connections to the database
	cons <- dbListConnections(MySQL())
	for(con in cons)dbDisconnect(con)

	# connect locally to the database
  con <- dbConnect(drv, user=user, pass=password, dbname='BIAD', host = "127.0.0.1", port=3306)
	# con <- dbConnect(drv, user=user, pass=password, dbname='BIAD', host = "localhost", port=3306)
	dbSendQuery(con,"SET NAMES 'utf8'")

	# query the database and tidy
	for(n in 1:length(sql.command)) res <- suppressWarnings(dbSendQuery(con,sql.command[n]))
	query <- fetch(res, n= -1)
	query <- encoder(query)

	# close any connections to the database
	cons <- dbListConnections(MySQL())
	for(con in cons)dbDisconnect(con)
return(query)}
#--------------------------------------------------------------------------------------------------
sql.wrapper <- function(sql.command,user,password,hostname,hostuser,keypath,ssh){

	if(ssh) pids.before <- get.plink.pids()
	if(ssh) suppressWarnings(open.ssh.tunnel(hostuser, hostname, keypath))
	Sys.sleep(1)
	query <- suppressWarnings(query.database(user, password, sql.command))
	if(ssh) pids.after <- get.plink.pids()
	if(ssh) pid <- pids.after[!pids.after%in%pids.before]
	if(ssh) close.ssh.tunnel(pid)

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
#-----------------------------------------------------------------------------------------
get.tables.from.backup <- function(file){
	tables <- list()
	
	raw <- readLines(file)
	start.posts <- grep('CREATE TABLE', raw)
	end.posts <- grep('Dumping data for table', raw)
	
	N <- length(start.posts)
	i <- 0
	for(n in 1:N){		
		table.info <- raw[start.posts[n]:end.posts[n]]
		table.name <- regmatches(table.info[1], gregexpr("(?<=\`)(.*?)(?=\`)", table.info[1], perl=T))[[1]]
		key.row <- grep('KEY|CONSTRAINT|ENGINE', table.info)[1]
		column.info <- table.info[2:(key.row -1)]
		column.names <- unlist(regmatches(column.info, gregexpr("(?<=\`)(.*?)(?=\`)", column.info, perl=T)))
		
		# just keep main tables
		if(!grepl('zoptions|zprivate',table.name)){
			d <- raw[grep(paste("INSERT INTO `",table.name,"` VALUES ",sep=''),raw)]
			if(length(d)>0){
				d <- gsub(paste("INSERT INTO `",table.name,"` VALUES (",sep=''),"", d, fixed=TRUE)
				d <- substr(d,1,nchar(d)-2)
				d <- gsub("\\'","´", d, fixed=TRUE)
				d <- strsplit(d, split='),(', fixed=T)[[1]]
				data <- read.table(text=d,sep=',', col.names=column.names, encoding = "UTF-8",stringsAsFactors=F)
				data[data=='NULL'] <- NA
				i <- i + 1
				tables[[i]] <- data
				names(tables)[i] <- table.name	
				}
			}
		}
return(tables)}
#--------------------------------------------------------------------------------------------------