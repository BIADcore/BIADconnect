#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
# Generic functions for connecting to and interacting with BIAD
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#' Get Primary Key Column from Table
#'
#' This function retrieves the name of the primary key column from a specified table in the database.
#' It first checks for any unique or primary constraints, and then determines which column serves as
#' the primary key. If there is ambiguity in determining the primary key, the function will stop with an error.
#'
#' @param keys A data frame containing key information for database tables. If `NULL`, keys are retrieved from the database using the `get.keys` function.
#' @param table.name A character string specifying the name of the table for which the primary key column is being queried.
#' @param conn A DBI connection object. If `NULL`, the function will attempt to check or initialize a  connection using the provided or default `db.credentials`.
#' @param db.credentials A list containing database connection details, including user, password, host, and port. If `NULL`, credentials are fetched using `get.credentials`.
#'
#' @return A character string representing the name of the primary key column. If no primary key is found, `NA` is returned. If multiple potential primary keys are found, the function stops with an error.
#'
#' @export
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
#' This function queries a database table to retrieve information about one or multiple entries in the database 
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
	primary.value  <- DBI::dbQuoteString(ANSI(),as.character(primary.value)) #Sanitize strings
	if(length(primary.value) == 1) matchexp <- paste0(" = ",primary.value)
	if(length(primary.value) > 1) matchexp <- paste0(" IN (",paste0(primary.value,collapse=","),")")
	sql.command <- paste0("SELECT * FROM `BIAD`.`",table.name,"` WHERE ",primary.column, matchexp)
	data <- query.database(sql.command = sql.command, conn = conn,db.credentials = db.credentials)
	if(na.rm) data <- remove.blank.columns.from.table(data)
return(data)}
#----------------------------------------------------------------------------------------------------
# internal helper function
remove.blank.columns.from.table <- function(table){
	if(is.null(table))return(table)
	tb <- table
	keep.i <- colSums(!is.na(tb))!=0
	tb <- tb[,keep.i,drop=F]
return(tb)}
#----------------------------------------------------------------------------------------------------
#' @export
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
get.relatives <- function(table.name, primary.value, directions = c("up","down"), conn = NULL, db.credentials = NULL, zoption=FALSE){
    stopifnot(directions %in% c("up","down"))
    if(is.null(conn)) conn  <- check.conn()
    
    keys  <- get.keys(conn)
    dir.functions = c("up"=get.ancestors,"down"=get.decendants)
    names(directions)=directions
    trees=lapply(directions,function(dir)dir.functions[[dir]](keys=keys, table.name=table.name, primary.value = primary.value, conn = conn, db.credentials = db.credentials))
    root=list() #root is here to create 'esthetic' tree roots. Trees will start as root -> 'S01200' followed by three branches: data (the data available at the root), up (ancestors in the trees) and down (child in the tree)
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
        if(is.numeric(primary.value))primary.value  <- as.character(primary.value)
        primary.value  <- DBI::dbQuoteString(ANSI(),primary.value) #Sanitize primary values
        sql.command <- paste("SELECT * FROM `BIAD`.`",rt,"` WHERE ",rc," = ",primary.value, sep='')
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
            if(is.numeric(values))values  <- as.character(values)
            values  <- DBI::dbQuoteString(ANSI(),values) #Sanitize strings
            if(length(values) > 0){
                if(length(values) == 1) matchexp <- paste0(" = ",values)
                if(length(values) > 1) matchexp <- paste0(" IN (",paste0(values,collapse=","),")")
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
#--------------------------------------------------------------------------------------------------
get.keys <- function(conn = NULL, db.credentials = NULL){
	sql.command <- "SELECT * FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE WHERE CONSTRAINT_SCHEMA='BIAD'"
	keys <- query.database(conn = conn, db.credentials = db.credentials, sql.command = sql.command)
    return(keys)
}
#--------------------------------------------------------------------------------------------------
#' Get Elements from a Tree Structure
#'
#' This function extracts from a tree, as created by the function \code{get.relatives}, 
#' all nodes/leaves that have names corresponding to the specified element.
#' trees should be as named list of list like list(a=1,b=list(a=2,b=3,c=4),d=list(e=1,f=4))
#' Based on the answers here: https://stackoverflow.com/questions/64578972/pull-all-elements-with-specific-name-from-a-nested-list/79168230#79168230
#'
#' @param x A list representing the tree structure.
#' @param element A character string specifying the name of elements to extract from the tree.
#' 
#' @return A list containing all elements from the tree that have names matching the specified element.
#'
#' @examples
#' \dontrun{
#' tree <- list(a = 1, b = list(a = 2, c = 3), d = 4)
#' get_elements(tree, "a")
#' # Expected output: list(1, 2)
#' }
get_elements <- function(x, element) {
	newlist=list()
	for(elt in names(x)){
		if(elt == element) newlist=append(newlist,x[[elt]])
		else if(is.list(x[[elt]])) newlist=append(newlist,get_elements(x[[elt]],element) )
	}
	return(newlist)
}
#--------------------------------------------------------------------------------------------------
#' Shortest Distance Calculation Using Spherical Law of Cosines
#'
#' This function calculates the shortest distance between a single point \code{(x, y)} and a set of points \code{(ax, ay)}
#' using the spherical law of cosines. The distances are returned in radians and can be converted to kilometers by
#' multiplying by the Earth's radius (6378.1 km).
#'
#' @param x A numeric value representing the longitude of the single point in radians or degrees.
#' @param y A numeric value representing the latitude of the single point in radians or degrees.
#' @param ax A numeric vector representing the longitudes of the point set in radians or degrees.
#' @param ay A numeric vector representing the latitudes of the point set in radians or degrees.
#' @param input A character string indicating the input unit. It can be either \code{'rad'} for radians 
#'        (default) or \code{'deg'} for degrees.
#'
#' @return A numeric vector of distances in radians from the single point \code{(x, y)} to each of the points in \code{(ax, ay)}.
#'
#' 
#' @export
slc <- function(x,y,ax,ay,input='rad'){
	# inputs required in rad or deg
	# calculate shortest distance between a single point (x,y) and all points(xa,ya) using spherical law of cosines
	# returns distances in radians, therefore only needs multiplying by radius of earth 6378.1 to convert to km
	if(length(ax)!=length(ay))stop('ax must be same length as ay')
	if(length(ax)==0)return(0); if(length(ax)>0){
	if(input=='deg'){x <- x*pi/180; y <- y*pi/180; ax <- ax*pi/180; ay <- ay*pi/180;}
	step.1 = sin(y) * sin (ay) + cos(y) * cos(ay) * cos(ax - x)
	
	# floating point bullshit, as sometimes 1 is greater than 1 (Rinferno!) 
	step.1[step.1>1]=1
	step.1[step.1<(-1)]=-1
	dist <- acos(step.1)	
return(dist)}}
#--------------------------------------------------------------------------------------------------
#' Summary Maker for Site Data
#'
#' @param  d dataframe of site, with SiteID
#'
#' @return a smumarry
#'
#' @export
summary_maker <- function(d){
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
    return(list(summary=x,cols=cols,legend=legend))
}
#----------------------------------------------------------------------------------------------------

