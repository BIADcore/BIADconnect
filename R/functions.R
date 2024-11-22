#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
# various functions and objects for BIAD
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
source("https://raw.githubusercontent.com/BIADwiki/BIADwiki/main/R/functions.database.connect.R")
source("https://raw.githubusercontent.com/AdrianTimpson/snippets/main/R/functions.R")
#----------------------------------------------------------------------------------------------------
# searhes for all directly related data
run.searcher <- function(table.name, primary.value, conn = NULL, db.credential = NULL, direction = NULL){
	if(is.null(conn)) conn <- check.conn() # due to the way these functions are handled by this searcher, we can't avoid passing the connecter explictly, so if it hasn't been done then we should create one.
	if(is.null(direction))  directions  <- list(down = 'decendants', up = 'ancestors')
	else if(direction == "down") directions  <- list(down = 'decendants')
	else if(direction == "up") directions  <- list(up = 'ancestors')
	lapply(directions,function(fn)  get.related.data(table.name, primary.value, fnc = get(fn) , conn = conn , db.credential = db.credential))
	}
#----------------------------------------------------------------------------------------------------
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
get.child.relationships <- function(keys, table.name, primary.value, conn = NULL, db.credentials = NULL){
	primary.data <- get.table.data(keys, table.name, primary.value, conn = conn, db.credentials = db.credentials)
	primary.column <- get.primary.column.from.table(keys, table.name, conn = conn, db.credentials = db.credentials)
	res <- subset(keys, REFERENCED_COLUMN_NAME==primary.column & REFERENCED_TABLE_NAME==table.name)
return(res)}
#----------------------------------------------------------------------------------------------------
get.parent.relationships <- function(keys, table.name, primary.value, conn = NULL, db.credentials = NULL){
	primary.data <- get.table.data(keys, table.name, primary.value, conn = conn, db.credentials = db.credentials)
	primary.column <- get.primary.column.from.table(keys, table.name, conn = conn, db.credentials = db.credentials)
	res <- subset(keys, TABLE_NAME==table.name & grepl('FK_',CONSTRAINT_NAME))
return(res)}
#----------------------------------------------------------------------------------------------------
get.primary.column.from.table <- function(keys = NULL, table.name, conn = NULL, db.credentials = NULL){                                                                                                                 
    if(is.null(keys))keys <- get.keys(conn = conn, db.credentials = db.credentials ) 
	x <- subset(keys, TABLE_NAME == table.name & CONSTRAINT_NAME %in% c('unique','PRIMARY'))$COLUMN_NAME
	column <- x[duplicated(x)]
	if(length(column)==0)column <- NA
	if(length(column)>1)stop('unclear which column to use')	
return(column)}
#----------------------------------------------------------------------------------------------------
#' Retrieve Table Entries from Database
#'
#' This function queries a database table to retrieve infor about one or multiple entries in the database 
#'
#' @param keys A vector of key names used to determine the primary column of the table.
#' @param table.name A string specifying the name of the table from which to retrieve data.
#' @param primary.value A value or a vector of values that are used to filter the rows in the table based on the primary column.
#' @param conn A database connection object to be used for the query. If NULL, db.credentials should be provided.
#' @param db.credentials Credentials required to establish a database connection, used when conn is NULL.
#' @param na.rm A logical value indicating whether to remove columns with all NA values from the result. The default is TRUE.
#'
#' @return A data frame containing the queried data, potentially with NA columns removed.
#'
#' @export
get.table.data <- function(keys = NULL, table.name = NULL, primary.value = NULL, conn = NULL, db.credentials = NULL, na.rm = TRUE){
	primary.column <- get.primary.column.from.table(keys, table.name)
    if(length(primary.value) == 1) matchexp <- paste0(" = '",primary.value,"'")
    if(length(primary.value) > 1) matchexp <- paste0(" IN ('",paste0(primary.value,collapse=","),"')")
    sql.command <- paste0("SELECT * FROM `BIAD`.`",table.name,"` WHERE ",primary.column, matchexp)
	data <- query.database(sql.command = sql.command, conn = conn,db.credentials = db.credentials)
	if(na.rm) data <- remove.blank.columns.from.table(data)
return(data)}
#----------------------------------------------------------------------------------------------------
decendants <- function(keys, table.name, primary.value, conn = NULL, db.credentials = NULL){

	if(is.null(primary.value))return(NULL)
	relationships <- get.child.relationships(keys, table.name, primary.value, conn, db.credentials)
	child.tables <- relationships$TABLE_NAME
	child.columns <- relationships$COLUMN_NAME

	res <- list()
	N <- length(child.tables)
	if(N==0)return(NULL)
	for(n in 1:N){
		child.table <- child.tables[n]
		child.column <- child.columns[n]
		sql.command <- paste("SELECT * FROM `BIAD`.`",child.table,"` WHERE ",child.column," = '",primary.value,"'", sep='')
		data <- query.database(conn = conn, db.credentials = db.credentials, sql.command = sql.command)
		data <- remove.blank.columns.from.table(data)
		res[[child.table]]$data <- data
		}
	if(length(res)==0)res <- NULL

return(res)}
#----------------------------------------------------------------------------------------------------
ancestors <- function(keys, table.name, primary.value, conn = NULL, db.credentials = NULL){

	if(is.null(primary.value))return(NULL)
	relationships <- get.parent.relationships(keys, table.name, primary.value, conn = conn, db.credentials = db.credentials)
	
	# whether or not to include zoptions parents? ... subset(relationships, !grepl('zoptions_',REFERENCED_TABLE_NAME))
	parent.tables <- relationships$REFERENCED_TABLE_NAME
	parent.columns <- relationships$REFERENCED_COLUMN_NAME
	child.columns <- relationships$COLUMN_NAME
	
	table.data <- get.table.data(keys, table.name, primary.value, conn, db.credentials)
	
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
			data <- query.database(conn = conn,db.credentials = db.credentials, sql.command = sql.command)
			data <- remove.blank.columns.from.table(data)
			res[[parent.table]]$data <- data	
			}	
		}

	if(length(res)==0)res <- NULL

return(res)}
#----------------------------------------------------------------------------------------------------
wrapper <- function(keys, table.data, fnc, conn = NULL, db.credentials = NULL){
	rel.data <- list()
	N <- length(table.data)
	for(n in 1:N){
		rel <- table.data[n]	
		table.name <- names(rel)
		col <- get.primary.column.from.table(keys, table.name=table.name, conn = conn, db.credentials = db.credentials)
		rel.values <- rel[[table.name]]$data[[col]]
		for(rel in rel.values){

			x <- fnc(keys, table.name, rel, conn = conn , db.credentials = db.credentials) 
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
get.related.data <- function(table.name, primary.value, fnc, conn = NULL, db.credentials = NULL){

	keys <- get.keys(conn = conn, db.credentials = db.credentials)

	# table data
	all.data <- list()
	table.data <- get.table.data(keys, table.name, primary.value, conn, db.credentials) 
	if(is.null(table.data))return(NULL)
	all.data[[table.name]]$data <- table.data
	
	# relative level 0 data
	x.data <- all.data[table.name]
	x.sub <- wrapper(keys, table.data=x.data, fnc, conn = conn, db.credentials = db.credentials)
	if(!is.null(x.sub))all.data[table.name] <- Map(c, x.data,x.sub)

	# relative level 1 data
	rel.1.names <- names(all.data[[table.name]][[primary.value]])	
	rel.1.names <- rel.1.names[rel.1.names!='data']	
	for(rel.1.name in rel.1.names){
		x.data <- all.data[[table.name]][[primary.value]][rel.1.name]
		x.sub <- wrapper(keys, x.data, fnc, conn = conn, db.credentials = db.credentials)
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
			x.sub <- wrapper(keys, x.data, fnc, conn, db.credentials)
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
				x.sub <- wrapper(keys, x.data, fnc, conn, db.credentials)
				if(!is.null(x.sub))all.data[[table.name]][[primary.value]][[rel.1.name]][[rel.2.name]][rel.3.name] <- Map(c, x.data,x.sub)
				}
			}	
		}

	# relative level 4 data
	rel.1.names <- names(all.data[[table.name]][[primary.value]])	
	rel.1.names <- rel.1.names[rel.1.names!='data']	
	for(rel.1.name in rel.1.names){
		rel.2.names <- names(all.data[[table.name]][[primary.value]][[rel.1.name]])
		rel.2.names <- rel.2.names[rel.2.names!='data']	
		for(rel.2.name in rel.2.names){
			rel.3.names <- names(all.data[[table.name]][[primary.value]][[rel.1.name]][[rel.2.name]])
			rel.3.names <- rel.3.names[rel.3.names!='data']	
			for(rel.3.name in rel.3.names){
				rel.4.names <- names(all.data[[table.name]][[primary.value]][[rel.1.name]][[rel.2.name]][[rel.3.name]])
				rel.4.names <- rel.4.names[rel.4.names!='data']	
				for(rel.4.name in rel.4.names){
					x.data <- all.data[[table.name]][[primary.value]][[rel.1.name]][[rel.2.name]][[rel.3.name]][rel.4.name]
					x.sub <- wrapper(keys, x.data, fnc, conn, db.credentials)
					if(!is.null(x.sub))all.data[[table.name]][[primary.value]][[rel.1.name]][[rel.2.name]][[rel.3.name]][rel.4.name] <- Map(c, x.data,x.sub)
					}
				}
			}	
		}

	# relative level 5 data
	rel.1.names <- names(all.data[[table.name]][[primary.value]])	
	rel.1.names <- rel.1.names[rel.1.names!='data']	
	for(rel.1.name in rel.1.names){
		rel.2.names <- names(all.data[[table.name]][[primary.value]][[rel.1.name]])
		rel.2.names <- rel.2.names[rel.2.names!='data']	
		for(rel.2.name in rel.2.names){
			rel.3.names <- names(all.data[[table.name]][[primary.value]][[rel.1.name]][[rel.2.name]])
			rel.3.names <- rel.3.names[rel.3.names!='data']	
			for(rel.3.name in rel.3.names){
				rel.4.names <- names(all.data[[table.name]][[primary.value]][[rel.1.name]][[rel.2.name]][[rel.3.name]])
				rel.4.names <- rel.4.names[rel.4.names!='data']	
				for(rel.4.name in rel.4.names){
					rel.5.names <- names(all.data[[table.name]][[primary.value]][[rel.1.name]][[rel.2.name]][[rel.3.name]][[rel.4.name]])
					rel.5.names <- rel.5.names[rel.5.names!='data']	
					for(rel.5.name in rel.5.names){
						x.data <- all.data[[table.name]][[primary.value]][[rel.1.name]][[rel.2.name]][[rel.3.name]][[rel.4.name]][rel.5.name]
						x.sub <- wrapper(keys, x.data, fnc, conn = conn , db.credentials = db.credentials)
						if(!is.null(x.sub))all.data[[table.name]][[primary.value]][[rel.1.name]][[rel.2.name]][[rel.3.name]][[rel.4.name]][rel.5.name] <- Map(c, x.data,x.sub)
						}
					}
				}
			}	
		}

return(all.data)}
#----------------------------------------------------------------------------------------------------
database.relationship.plotter <- function(d.tables, include.look.ups=TRUE, conn = NULL, db.credentials = NULL){

	require(DiagrammeR)

	sql.command <- "SELECT * FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE WHERE REFERENCED_TABLE_SCHEMA = 'BIAD'"
	d <- query.database(conn = conn, db.credentials = db.credentials, sql.command = sql.command)
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
	image <- DiagrammeR::grViz(diagram, engine='neato')
return(image)}
#--------------------------------------------------------------------------------------------------
make.autopad.trigger <- function(table, columns, type, prefix){
        triggername <- paste(prefix, type,'_',table,sep='')
        t1 <- paste('CREATE DEFINER=`Rscripts`@`%` TRIGGER `',triggername,'` BEFORE ',type,' ON `',table,'` FOR EACH ROW BEGIN',sep='')
        t2 <- paste("SET NEW.`",columns,"` = TRIM(REPLACE(REPLACE(REPLACE(NEW.`",columns,"`, '\r', ' '), '\n', ' '), '\t', ' '));",sep='')
        t3 <- 'END'
        txt <- c(t1,t2,t3)
        txt <- paste(txt,collapse=' ')
return(txt)}
#--------------------------------------------------------------------------------------------------
make.stamp.trigger <- function(table, columns, type, prefix){
        triggername <- paste(prefix, type,'_',table,sep='')
        t1 <- paste('CREATE DEFINER=`Rscripts`@`%` TRIGGER `',triggername,'` BEFORE ',type,' ON `',table,'` FOR EACH ROW BEGIN',sep='')
        if(type=='INSERT')t2 <- c('SET NEW.user_added = SYSTEM_USER();','SET NEW.time_added = CURRENT_TIMESTAMP;','SET NEW.user_last_update = SYSTEM_USER();','SET NEW.time_last_update = CURRENT_TIMESTAMP;')
        if(type=='UPDATE')t2 <- c('SET NEW.user_last_update = SYSTEM_USER();','SET NEW.time_last_update = CURRENT_TIMESTAMP;')
        t3 <- 'END'
        txt <- c(t1,t2,t3)
        txt <- paste(txt,collapse=' ')
return(txt)}
#--------------------------------------------------------------------------------------------------
make.all.triggers <- function(x, prefix, trigger){
        txt <- c()
        tables <- unique(x$TABLE_NAME)
        N <- length(tables)
        if(N==0)return(NULL)
        if(N>0){
                for(n in 1:N){
                        table <- tables[n]
                        columns <- subset(x, TABLE_NAME==table)$COLUMN_NAME
                        inserts <- trigger(table,columns,type = 'INSERT', prefix)
                        updates <- trigger(table,columns,type = 'UPDATE', prefix)
                        txt <- c(txt,inserts,updates)
                        }
                }
return(txt)}
#--------------------------------------------------------------------------------------------------
#' Retrieve Relatives from Database Table
#'
#' This function generates trees of ancestor or descendant records related to a specific entries a database table.
#'
#' @param table.name A string specifying the name of the table where the entry is.
#' @param primary.value The primary key value used to find the entry in the database.
#' @param directions A character vector indicating direction(s) for retrieving the data related to the entry 
#' Available options are "up" for ancestors and "down" for descendants. Default is both (`directions=c("up", "down")`).
#' @param conn A database connection object. Default is `NULL`.
#' @param db.credentials parameter for manual setup of database credentials. Default is `NULL`.
#'
#' @return A list containing a root element with one branch with all the data associated with the specific entry and two other branches storing trees as nested list with all related entries.
#' @export
#'
get.relatives <- function(table.name, primary.value, directions = c("up","down"), conn = NULL, db.credentials = NULL,zoption=FALSE){
    stopifnot(directions %in% c("up","down"))
    if(is.null(conn)) conn  <- check.conn()
    
    keys  <- get.keys(conn)
    dir.functions = c("up"=get.ancestors,"down"=get.decendants)
    names(directions)=directions
    trees=lapply(directions,function(dir)dir.functions[[dir]](keys=keys, table.name=table.name, primary.value = primary.value, conn = conn, db.credentials = db.credentials))
    root=list() #root is here for esthetic trees root -> 'S01200' followd by three branches: data up and down
    root[[primary.value]]=c(list(data=get.table.data(keys=keys, table.name, primary.value, conn, db.credentials,na.rm = F)),trees)
    return(root)
}

#--------------------------------------------------------------------------------------------------
#' Retrieve Descendant Records from Database
#'
#' This function retrieves all descendant records related to a specified primary value in a database table.
#'
#' @param keys A data frame containing database information, including relationships between tables (obtained via `get.keys`)
#' @param table.name A string specifying the name of the table from which to start retrieving descendant records.
#' @param primary.value The primary key value from which to find descendant records. 
#' @param conn A database connection object. 
#' @param db.credentials manual database credentials. 
#'
#' @return A nested list containing data frames of descendant records for each related table.
#' @export
#'
get.decendants <- function(keys, table.name, primary.value, conn = NULL, db.credentials = NULL){

    if(is.null(primary.value) || primary.value == ""  )return(NULL)

    primary.column <- get.primary.column.from.table(keys, table.name)
    relative.info  <- subset(keys, REFERENCED_COLUMN_NAME==primary.column & REFERENCED_TABLE_NAME==table.name)
    if(nrow(relative.info) == 0) return(NULL)
    
    relative.tables <- relative.info$TABLE_NAME #table using the key
    relative.columns <- relative.info$COLUMN_NAME #name of column using the key
    res <- list()
    for(n in 1:length(relative.tables)){
        rt <- relative.tables[n]
        rc <- relative.columns[n]
        sql.command <- paste("SELECT * FROM `BIAD`.`",rt,"` WHERE ",rc," = '",primary.value,"'", sep='')
        data <- query.database(conn = conn, db.credentials = db.credentials, sql.command = sql.command)
        if(length(data)>0){
            relative.key  <- get.primary.column.from.table(keys, rt)
            res[[rt]]=list()
            res[[rt]][["data"]]  <- data
            for(rv in data[[relative.key]]){
                res[[rt]][[as.character(rv)]] <-  get.decendants(keys = keys,table.name = rt,primary.value = rv,conn = conn, db.credentials = db.credentials)
            }
        }
    }
    return(res)
}

#--------------------------------------------------------------------------------------------------
#' Retrieve Ancestor Records from Database
#'
#' This function retrieves all ancestor records related to a specified primary value in a database table.
#'
#' @param keys A data frame containing database information, including relationships between tables (obtained via `get.keys`)
#' @param table.name A string specifying the name of the table from which to start retrieving descendant records.
#' @param primary.value The primary key value from which to find descendant records. 
#' @param conn A database connection object. 
#' @param db.credentials manual database credentials. 
#'
#' @return A nested list containing data frames of descendant records for each related table.
#' @export
#'
get.ancestors <- function(keys, table.name, primary.value, conn = NULL, db.credentials = NULL, orig.table = NULL , zoption = FALSE){

    relative.info  <- subset(keys, TABLE_NAME==table.name & grepl('FK_',CONSTRAINT_NAME))
    #if(!zoption) relative.info  <- subset(relative.info, !grepl('zoptions_',REFERENCED_TABLE_NAME))

    if(is.null(orig.table)) orig.table <- get.table.data(keys, table.name, primary.value, conn, db.credentials,na.rm = F) 

    if(nrow(relative.info) == 0) return(orig.table)
    
    relative.tables <- relative.info$REFERENCED_TABLE_NAME #table using the key
    relative.columns <- relative.info$REFERENCED_COLUMN_NAME #name of column using the key
    orig.column.alt <- relative.info$COLUMN_NAME #name of column using the key

    res <- list()
    for(n in 1:length(relative.tables)){
        rt <- relative.tables[n]
        rc <- relative.columns[n]
        rv.c <- orig.column.alt[n] #column where the reference value is stored
        if(rv.c %in% names(orig.table)){
            values <- unique(unlist(na.omit(orig.table[rv.c])))
            if(length(values) > 0){
                if(length(values) == 1) matchexp <- paste0(" = '",values,"'")
                if(length(values) > 1) matchexp <- paste0(" IN ('",paste0(values,collapse=","),"')")
                sql.command <- paste0("SELECT * FROM `BIAD`.`",rt,"` WHERE ",rc,matchexp)
                data <- query.database(conn = conn, db.credentials = db.credentials, sql.command = sql.command)
                if(length(data)>0){
                    relative.key  <- get.primary.column.from.table(keys, rt)
                    res[[rt]]=list()
                    res[[rt]][["data"]]  <- data
                    for(rv in data[[relative.key]]){
                        res[[rt]][[as.character(rv)]] <- get.ancestors(keys = keys,table.name = rt,primary.value = rv,conn = conn, db.credentials = db.credentials, orig.table = data)
                    }
                }
            }
        }
    }
    return(res)
}

#--------------------------------------------------------------------------------------------------
#' Retrieve BIAD size
#'
#' This function retrieves the sizes of BIAD, to help figuring out which dockers to use
#'
#' @param conn A database connection object. Default is `NULL`.
#' @param db.credential manually pass database credentials. Default is `NULL`.
#' @param db name of the database to be returned
#'
#' @return A data frame with the database sizes in gigabytes.
#' @export
getSize <- function(conn = NULL, db.credential = NULL, db = 'BIAD'){
    sql.command='SELECT table_schema AS "Database", (SUM(data_length)+SUM(index_length)) / 1024 / 1024 / 1024 AS "Size (GB)" FROM information_schema.TABLES GROUP BY table_schema'
    size <- query.database(sql.command,conn)
    size[which( size[,1] == db),]
}



get.keys <- function(conn = NULL, db.credentials = NULL){
	sql.command <- "SELECT * FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE WHERE CONSTRAINT_SCHEMA='BIAD'"
	keys <- query.database(conn = conn, db.credentials = db.credentials, sql.command = sql.command)
}


