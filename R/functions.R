
#--------------------------------------------------------------------------------------------------
sql.wrapper <- function(sql.command,user,password,hostname,hostuser,keypath,ssh){

	if(!ssh){
		query <- suppressWarnings(query.database(user, password, sql.command))
		}

	if(ssh){
		system <- .Platform$OS.type

		if(system=='windows'){
			cmd <- paste("plink -ssh ",hostuser,"@",hostname," -i ",keypath," -N -L 3306:",hostname,":3306",sep='')
			system(cmd, wait=FALSE)
			query <- suppressWarnings(query.database(user, password, sql.command))
			system("taskkill /F /IM ssh-agent.exe /T", wait=FALSE)
			}

		if(system=='unix'){
			cmd <- paste("ssh ",hostuser,"@",hostname," -i ",keypath," -N -L 3306:",hostname,":3306",sep='')
			system(cmd, wait=FALSE)
			Sys.sleep(1)
			query <- suppressWarnings(query.database(user, password, sql.command))
			system("killall ssh",wait=FALSE)
			}
		}
return(query)}
#--------------------------------------------------------------------------------------------------
sql.wrapper.new <- function(sql.command, user, password, hostname, hostuser, pempath){

	require(ssh)
	session <- ssh_connect(host=paste(hostuser,"@",hostname,sep=''), keyfile=pempath)
	query <- suppressWarnings(query.database(user, password, sql.command))
	ssh_disconnect(session)

	sql.command <- "SELECT * FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE WHERE CONSTRAINT_SCHEMA='BIAD'"
	keys <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)

return(query)}
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
#	dbSendQuery(con,"SET NAMES 'utf8'")
	dbSendStatement(con,"SET NAMES 'utf8'")

	# query the database and tidy
#	for(n in 1:length(sql.command)) res <- suppressWarnings(dbSendQuery(con,sql.command[n]))
	for(n in 1:length(sql.command)) res <- suppressWarnings(dbSendStatement(con,sql.command[n]))
	query <- fetch(res, n= -1)
	query <- encoder(query)

	# close any connections to the database
	cons <- dbListConnections(MySQL())
	for(con in cons)dbDisconnect(con)
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
#----------------------------------------------------------------------------------------------------
get.child.relationships <- function(keys, table.name, primary.value, user, password, hostname, hostuser, keypath, ssh){
	primary.data <- get.table.data(keys, table.name, primary.value, user, password, hostname, hostuser, keypath, ssh)
	primary.column <- get.primary.column.from.table(keys, table.name)
	res <- subset(keys, REFERENCED_COLUMN_NAME==primary.column & REFERENCED_TABLE_NAME==table.name)
return(res)}
#----------------------------------------------------------------------------------------------------
get.parent.relationships <- function(keys, table.name, primary.value, user, password, hostname, hostuser, keypath, ssh){
	primary.data <- get.table.data(keys, table.name, primary.value, user, password, hostname, hostuser, keypath, ssh)
	primary.column <- get.primary.column.from.table(keys, table.name)
	res <- subset(keys, TABLE_NAME==table.name & grepl('FK_',CONSTRAINT_NAME))
return(res)}
#----------------------------------------------------------------------------------------------------
get.primary.column.from.table <- function(keys, table.name){
	x <- subset(keys, TABLE_NAME == table.name & CONSTRAINT_NAME %in% c('unique','PRIMARY'))$COLUMN_NAME
	column <- x[duplicated(x)]
	if(length(column)==0)column <- NA
	if(length(column)>1)stop('unclear which column to use')	
return(column)}
#----------------------------------------------------------------------------------------------------
get.table.data <- function(keys, table.name, primary.value, user, password, hostname, hostuser, keypath, ssh){

	if(length(primary.value)!=1)stop('provide a single primary value')
	primary.column <- get.primary.column.from.table(keys, table.name)
	sql.command <- paste("SELECT * FROM `BIAD`.`",table.name,"` WHERE ",primary.column," IN ('",primary.value,"')", sep='')
	data <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)
	data <- remove.blank.columns.from.table(data)
return(data)}
#----------------------------------------------------------------------------------------------------
decendants <- function(keys, table.name, primary.value, user, password, hostname, hostuser, keypath, ssh){

	if(is.null(primary.value))return(NULL)
	relationships <- get.child.relationships(keys, table.name, primary.value, user, password, hostname, hostuser, keypath, ssh)
	child.tables <- relationships$TABLE_NAME
	child.columns <- relationships$COLUMN_NAME

	res <- list()
	N <- length(child.tables)
	if(N==0)return(NULL)
	for(n in 1:N){
		child.table <- child.tables[n]
		child.column <- child.columns[n]
		sql.command <- paste("SELECT * FROM `BIAD`.`",child.table,"` WHERE ",child.column," = '",primary.value,"'", sep='')
		data <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)
		data <- remove.blank.columns.from.table(data)
		res[[child.table]]$data <- data
		}
	if(length(res)==0)res <- NULL

return(res)}
#----------------------------------------------------------------------------------------------------
ancestors <- function(keys, table.name, primary.value, user, password, hostname, hostuser, keypath, ssh){

	if(is.null(primary.value))return(NULL)
	relationships <- get.parent.relationships(keys, table.name, primary.value, user, password, hostname, hostuser, keypath, ssh)
	
	# whether or not to include zoptions parents? ... subset(relationships, !grepl('zoptions_',REFERENCED_TABLE_NAME))
	parent.tables <- relationships$REFERENCED_TABLE_NAME
	parent.columns <- relationships$REFERENCED_COLUMN_NAME
	child.columns <- relationships$COLUMN_NAME
	
	table.data <- get.table.data(keys, table.name, primary.value, user, password, hostname, hostuser, keypath, ssh)
	
	res <- list()
	N <- length(parent.tables)
	if(N==0)return(NULL)
	for(n in 1:N){
		parent.table <- parent.tables[n]
		parent.column <- parent.columns[n]
		child.column <- child.columns[n]
		

		# get parent data
		if(child.column %in% names(table.data)){
			values <- table.data[child.column]
			values <- values[!is.na(values)]
			values <- paste(values, collapse="','")
			sql.command <- paste("SELECT * FROM `BIAD`.`",parent.table,"` WHERE ",parent.column," IN ('",values,"')", sep='')		
			data <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)
			data <- remove.blank.columns.from.table(data)
			res[[parent.table]]$data <- data	
			}	
		}

	if(length(res)==0)res <- NULL

return(res)}
#----------------------------------------------------------------------------------------------------
wrapper <- function(keys, table.data, fnc, user, password, hostname, hostuser, keypath, ssh){
	rel.data <- list()
	N <- length(table.data)
	for(n in 1:N){
		rel <- table.data[n]	
		table.name <- names(rel)
		col <- get.primary.column.from.table(keys, table.name=table.name)
		rel.values <- rel[[table.name]]$data[[col]]
		for(rel in rel.values){

			x <- fnc(keys, table.name, rel, user, password, hostname, hostuser, keypath, ssh) 
			rel.data[[table.name]][[rel]] <- x
			}
		}
	if(length(rel.data)==0)return(NULL)
return(rel.data)}
#----------------------------------------------------------------------------------------------------
remove.blank.columns.from.table <- function(table){
	if(is.null(table))return(table)
	tb <- table
	keep.i <- colSums(!is.na(tb))!=0
	tb <- tb[,keep.i,drop=F]
return(tb)}
#----------------------------------------------------------------------------------------------------
get.related.data <- function(table.name, primary.value, fnc, user, password, hostname, hostuser, keypath, ssh){

	sql.command <- "SELECT * FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE WHERE CONSTRAINT_SCHEMA='BIAD'"
	keys <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)

	# table data
	all.data <- list()
	table.data <- get.table.data(keys, table.name, primary.value, user, password, hostname, hostuser, keypath, ssh) 
	if(is.null(table.data))return(NULL)
	all.data[[table.name]]$data <- table.data
	
	# relative level 0 data
	x.data <- all.data[table.name]
	x.sub <- wrapper(keys, table.data=x.data, fnc, user, password, hostname, hostuser, keypath, ssh)
	if(!is.null(x.sub))all.data[table.name] <- Map(c, x.data,x.sub)

	# relative level 1 data
	rel.1.names <- names(all.data[[table.name]][[primary.value]])	
	rel.1.names <- rel.1.names[rel.1.names!='data']	
	for(rel.1.name in rel.1.names){
		x.data <- all.data[[table.name]][[primary.value]][rel.1.name]
		x.sub <- wrapper(keys, x.data, fnc, user, password, hostname, hostuser, keypath, ssh)
		if(!is.null(x.sub))all.data[[table.name]][[primary.value]][rel.1.name] <- Map(c, x.data,x.sub)
		}

	# relative level 2 data
	rel.1.names <- names(all.data[[table.name]][[primary.value]])	
	rel.1.names <- rel.1.names[rel.1.names!='data']	
	for(rel.1.name in rel.1.names){
		rel.2.names <- names(all.data[[table.name]][[primary.value]][[rel.1.name]])
		rel.2.names <- rel.2.names[rel.2.names!='data']	
		for(rel.2.name in rel.2.names){
			x.data <- all.data[[table.name]][[primary.value]][[rel.1.name]][rel.2.name]
			x.sub <- wrapper(keys, x.data, fnc, user, password, hostname, hostuser, keypath, ssh)
			if(!is.null(x.sub))all.data[[table.name]][[primary.value]][[rel.1.name]][rel.2.name] <- Map(c, x.data,x.sub)
			}
		}

	# relative level 3 data
	rel.1.names <- names(all.data[[table.name]][[primary.value]])	
	rel.1.names <- rel.1.names[rel.1.names!='data']	
	for(rel.1.name in rel.1.names){
		rel.2.names <- names(all.data[[table.name]][[primary.value]][[rel.1.name]])
		rel.2.names <- rel.2.names[rel.2.names!='data']	
		for(rel.2.name in rel.2.names){
			rel.3.names <- names(all.data[[table.name]][[primary.value]][[rel.1.name]][[rel.2.name]])
			rel.3.names <- rel.3.names[rel.3.names!='data']	
			for(rel.3.name in rel.3.names){	
				x.data <- all.data[[table.name]][[primary.value]][[rel.1.name]][[rel.2.name]][rel.3.name]
				x.sub <- wrapper(keys, x.data, fnc, user, password, hostname, hostuser, keypath, ssh)
				if(!is.null(x.sub))all.data[[table.name]][[primary.value]][[rel.1.name]][[rel.2.name]][rel.3.name] <- Map(c, x.data,x.sub)
				}
			}	
		}

return(all.data)}
#----------------------------------------------------------------------------------------------------
run.server.searcher <- function(table.name,primary.value){
	
	text <- c(
		"source('tmp/functions.R')",
		"source('tmp/.Rprofile')",
		"ssh <- FALSE",
		paste("table.name <- '",table.name,"'",sep=''),
		paste("primary.value <- '",primary.value,"'",sep=''),
		"down <- get.related.data(table.name, primary.value, fnc = decendants, user, password, hostname, hostuser, keypath, ssh)",
		"up <- get.related.data(table.name, primary.value, fnc = ancestors, user, password, hostname, hostuser, keypath, ssh)",
		"save(up,down, file='tmp/tmp.RData')"
		)

	commands <- c(
		"cd BIAD/R",
		"/Library/Frameworks/R.framework/Resources/bin/R CMD BATCH --no-save tmp/server.script.R tmp/tmp.Rout"
		)

	tmp.path <- "BIAD/R/tmp"
	writeLines(text,con= 'server.script.R')
	session <- ssh_connect(host=paste(hostuser,"@",hostname,sep=''), keyfile=pempath)
	ssh_exec_wait(session, command = paste("mkdir",tmp.path))
	scp_upload(session, files = "functions.R" , to = tmp.path, verbose=FALSE)
	scp_upload(session, files = ".Rprofile" , to = tmp.path, verbose=FALSE)
	scp_upload(session, files = "server.script.R" , to = tmp.path, verbose=FALSE)
	ssh_exec_wait(session, command = commands)
	scp_download(session, files = paste(tmp.path,"tmp.RData",sep="/"), to = getwd(), verbose=FALSE)
	ssh_exec_wait(session, command = paste("rm -r",tmp.path))
	ssh_disconnect(session)

	load('tmp.RData')
	unlink('tmp.RData')
	unlink('server.script.R')

return(list(down=down,up=up))}
#--------------------------------------------------------------------------------------------------
database.relationship.plotter <- function(d.tables, include.look.ups=TRUE){

	sql.command <- "SELECT * FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE WHERE REFERENCED_TABLE_SCHEMA = 'BIAD'"
	d <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)

	d <- subset(d, TABLE_NAME%in%strsplit(d.tables,split='; ')[[1]])
	if(!include.look.ups){
		d <- subset(d, REFERENCED_TABLE_NAME%in%strsplit(d.tables,split='; ')[[1]])
		d <- subset(d,!grepl('zoptions', REFERENCED_TABLE_NAME))
		}
	z.tables <- d$REFERENCED_TABLE_NAME[grep('zoptions',d$REFERENCED_TABLE_NAME)]

	# convert foreign keys into a suitable format for DiagrammeR
	edges <- paste(d$REFERENCED_TABLE_NAME, d$TABLE_NAME, sep=' -> ')
	edges <- paste('edge [color = dimgray]', edges, collapse='\n ')

	data.tables <- paste("
  		node [shape = circle,
		style = filled,
		fillcolor = orange,
		fixedsize = true,
		width = 2.2,
		fontsize = 15]", d.tables, sep='\n ')

	look.ups <- paste("
		node [shape = box,
		style = filled,
		fillcolor = lightblue,
		fixedsize = true,
		width = 3.0,
		fontsize = 15]", z.tables, sep='\n ')

	subgraph <- "
	subgraph cluster {
	node [shape = circle,
 		style = filled,
 		fillcolor = orange,
 		fixedsize = true,
		width = 1,
		fontsize = 10]
		DataTable
		node [shape = box,
		style = filled,
		fillcolor = lightblue,
		fixedsize = true,
		width = 1,
		fontsize = 10]
	LookUpTable}"

	if(!include.look.ups)subgraph <- ""

	diagram <- paste("digraph {", data.tables, look.ups, edges, subgraph, "}")
	image <- DiagrammeR::grViz(diagram)
return(image)}
#--------------------------------------------------------------------------------------------------
generate.colours.for.a.numeric <- function(x){
	if(max(x,na.rm=T)<1.1)res <- generate.colours.for.zero.to.one(x)
	if(max(x,na.rm=T)>5)res <- generate.colours.for.integers(x)
return(res)}
#--------------------------------------------------------------------------------------------------
generate.colours.for.zero.to.one <- function(x){
	posts <- round(quantile(x,na.rm=T),3)
	N <- length(posts)-1
	key <- code <- col <- c()
	for(n in 1:N){
		lower <- posts[n]
		upper <- posts[n+1]
		key[n] <- paste(lower,upper,sep='=>')
		i <- x>=lower & x<upper
		code[i] <- n
		}
	cols <- colorRampPalette(c("red", "blue"))(N)
	for(n in 1:(N))col[code==n] <- cols[n]
	legend <- data.frame(key=key,col=cols)
return(list(col=col,legend=legend))}
#--------------------------------------------------------------------------------------------------
generate.colours.for.integers <- function(x){
	posts <- floor(quantile(x[!x%in%c(0,1,2)],na.rm=T))	
	N <- length(posts)-1
	posts[N+1] <- posts[N+1]+1
	key <- code <- col <- c()
	for(n in 1:N){
		lower <- posts[n]
		upper <- posts[n+1]
		key[n] <- paste(lower,upper,sep='=>')
		i <- x>=lower & x<upper
		code[i] <- n+2
		}
	code[x==1] <- 1
	code[x==2] <- 2
	cols <- colorRampPalette(c("red", "blue"))(N+2)
	for(n in 1:(N+2))col[code==n] <- cols[n]
	legend <- data.frame(key=c(1,2,key),col=cols)
return(list(col=col,legend=legend))}
#--------------------------------------------------------------------------------------------------
summary.maker <- function(d){

	x <- as.data.frame(table(d$SiteID)); names(x) <- c('SiteID','count')
	x <- merge(x,unique(d[,1:3]),by='SiteID')
	x$code[x$count==1] <- 1
	x$code[x$count==2] <- 2
	posts <- floor(unique(quantile(x$count[!x$count%in%c(1,2)])))
	N <- length(posts)-1
	posts[N+1] <- posts[N+1]+1
	key <- c()
	for(n in 1:N){
		lower <- posts[n]
		upper <- posts[n+1]
		key[n] <- paste(lower,upper,sep='=>')
		i <- x$count>=lower & x$count<upper
		x$code[i] <- n+2
		}
	cols <- colorRampPalette(c("red", "blue"))(N+2)
	for(n in 1:(N+2))x$col[x$code==n] <- cols[n]
	legend <- c(1,2,key)
return(list(summary=x,cols=cols,legend=legend))}
#--------------------------------------------------------------------------------------------------
make.trigger <- function(table,columns,type){
	triggername <- paste('auto_trigger_padding_',type,'_',table,sep='')
	t1 <- paste('CREATE DEFINER=`Rscripts`@`%` TRIGGER `',triggername,'` BEFORE ',type,' ON `',table,'` FOR EACH ROW BEGIN',sep='')
	t2 <- paste("SET NEW.`",columns,"` = TRIM(REPLACE(REPLACE(REPLACE(NEW.`",columns,"`, '\r', ' '), '\n', ' '), '\t', ' '));",sep='')
	t3 <- 'END'
	txt <- c(t1,t2,t3)
	txt <- paste(txt,collapse=' ')
return(txt)}
#--------------------------------------------------------------------------------------------------
make.all.triggers <- function(x){
	txt <- c()
	tables <- unique(x$TABLE_NAME)
	N <- length(tables)
	if(N==0)return(NULL)
	if(N>0){
		for(n in 1:N){
			table <- tables[n]
			columns <- subset(x, TABLE_NAME==table)$COLUMN_NAME
			inserts <- make.trigger(table,columns,type = 'INSERT')
			updates <- make.trigger(table,columns,type = 'UPDATE')
			txt <- c(txt,inserts,updates)
			}
		}
return(txt)}
#--------------------------------------------------------------------------------------------------


