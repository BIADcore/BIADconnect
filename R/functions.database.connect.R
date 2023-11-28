#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
# generic functions to query any database hosted at macelab
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
run.server.query <- function(sql.command){	

	# create 'server.script.R' to be run on server
	text <- c(
		paste0("user <- '",user,"'"),
		paste0("password <- '",password,"'"),
		paste0("hostuser <- '",hostuser,"'"),
		paste0("dbname <- '",dbname,"'"),
		"source('https://raw.githubusercontent.com/BIADwiki/BIADwiki/main/R/functions.database.connect.R')",
		paste('sql.command <- c("',paste(sql.command,collapse='","'),'")',sep=''),
		"query <- query.database(user, password, dbname, sql.command)",
		"save(query, file='tmp.RData')"
		)
	writeLines(text,con= 'server.script.R')

	query <- run.server.query.inner(user, password, hostuser, hostname, pempath)
return(query)}
#----------------------------------------------------------------------------------------------------
run.server.query.inner <- function(user, password, hostuser, hostname, pempath){ 
	require(ssh)
	tmp.path <- paste("tmp/tmp",runif(1),sep='')

	# create bash commands to be run on server
	commands <- c(
		paste("cd",tmp.path),
		"/Library/Frameworks/R.framework/Resources/bin/R CMD BATCH --no-save server.script.R tmp.Rout",
		"cd .."
		)

	# ssh onto server, copy required files to server, tell server to run R, copy results back to local 
	session <- ssh_connect(host=paste(hostuser,"@",hostname,sep=''), keyfile=pempath)
	ssh_exec_wait(session, command = paste("mkdir",tmp.path))
	scp_upload(session, files = "server.script.R" , to = tmp.path, verbose=FALSE)
#	unlink('server.script.R')
	ssh_exec_wait(session, command = commands)
	RData <- paste(tmp.path,"tmp.RData",sep="/")
	scp_download(session, files = RData, to = getwd(), verbose=FALSE)
	cond <- file.exists("tmp.RData")
	if(cond){
		load('tmp.RData')
		unlink('tmp.RData')
		}
	if(!cond){
		query <- NULL
		warning('sql command failed')
		}
	ssh_exec_wait(session, command = paste("rm -r",tmp.path))
	ssh_disconnect(session)

return(query)}
#--------------------------------------------------------------------------------------------------
query.database.inner <- function(user, password, dbname, sql.command){
	require(RMySQL)
	drv <- dbDriver("MySQL")

	# close any connections to the database
	cons <- dbListConnections(MySQL())
	for(con in cons)dbDisconnect(con)

	# connect locally to the database
	con <- dbConnect(drv, user=user, pass=password, dbname=dbname, host = "127.0.0.1", port=3306)
	dbSendStatement(con,"SET NAMES 'utf8'")

	# query the database and tidy
	for(n in 1:length(sql.command)) res <- dbSendStatement(con,sql.command[n])
	query <- fetch(res, n= -1)
	query <- encoder(query)

	# close any connections to the database
	cons <- dbListConnections(MySQL())
	for(con in cons)dbDisconnect(con)
return(query)}
#--------------------------------------------------------------------------------------------------
query.database <- function(user, password, dbname, sql.command){
	query <- suppressWarnings(query.database.inner(user, password, dbname, sql.command))
return(query)}
#--------------------------------------------------------------------------------------------------
encoder <- function(df){
	if(nrow(df)==0) return(NULL)
	names(df) <- iconv(names(df),from="UTF-8",to="UTF-8")
	if(nrow(df)>0){
		for(n in 1:ncol(df))
		if(class(df[,n])=="character"){
			df[,n] <- iconv(df[,n],from="UTF-8",to="UTF-8")
			}
		return(df)	
		}
	}
#--------------------------------------------------------------------------------------------------