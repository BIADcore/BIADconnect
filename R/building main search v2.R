#----------------------------------------------------------------------------------------------------
source('.Rprofile')
source('functions.R')
#----------------------------------------------------------------------------------------------------
# needs to be placed inside a larger wrapper that only opens the connection once, and closes it once, for speed.
#----------------------------------------------------------------------------------------------------
get.child.relationships <- function(keys, table.name, primary.value){
	primary.data <- get.table.data(keys, table.name, primary.value)
	primary.column <- get.primary.column.from.table(keys, table.name)
	res <- subset(keys, REFERENCED_COLUMN_NAME==primary.column & REFERENCED_TABLE_NAME==table.name)
return(res)}
#----------------------------------------------------------------------------------------------------
get.parent.relationships <- function(keys, table.name, primary.value){
	primary.data <- get.table.data(keys, table.name, primary.value)
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
get.table.data <- function(keys, table.name, primary.value){

	if(length(primary.value)!=1)stop('provide a single primary value')
	primary.column <- get.primary.column.from.table(keys, table.name)
	sql.command <- paste("SELECT * FROM `BIAD`.`",table.name,"` WHERE ",primary.column," IN ('",primary.value,"')", sep='')
	data <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)
	data <- remove.blank.columns.from.table(data)
return(data)}
#----------------------------------------------------------------------------------------------------
get.child.data <- function(keys, table.name, primary.value){

	if(is.null(primary.value))return(NULL)
	relationships <- get.child.relationships(keys, table.name, primary.value)
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
get.parent.data <- function(keys, table.name, primary.value){

	if(is.null(primary.value))return(NULL)
	relationships <- get.parent.relationships(keys, table.name, primary.value)
	
	# whether or not to include zoptions parents? ... subset(relationships, !grepl('zoptions_',REFERENCED_TABLE_NAME))
	parent.tables <- relationships$REFERENCED_TABLE_NAME
	parent.columns <- relationships$REFERENCED_COLUMN_NAME
	child.columns <- relationships$COLUMN_NAME
	
	res <- list()
	N <- length(parent.tables)
	if(N==0)return(NULL)
	for(n in 1:N){
		parent.table <- parent.tables[n]
		parent.column <- parent.columns[n]
		child.column <- child.columns[n]
		

		# get parent data
		values <- table.data[[table.name]][['data']][[child.column]]
		values <- values[!is.na(values)]
		values <- paste(values, collapse="','")
		sql.command <- paste("SELECT * FROM `BIAD`.`",parent.table,"` WHERE ",parent.column," IN ('",values,"')", sep='')		

		data <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)
		data <- remove.blank.columns.from.table(data)
		res[[parent.table]]$data <- data		
		}

	if(length(res)==0)res <- NULL

return(res)}
#----------------------------------------------------------------------------------------------------
child.data.wrapper <- function(keys, table.data){
	
	child.data <- list()
	N <- length(table.data)
	for(n in 1:N){
		child <- table.data[n]	
		table.name <- names(child)
		col <- get.primary.column.from.table(keys, table.name=table.name)
		child.values <- child[[table.name]]$data[[col]]
		for(child in child.values){
			x <- get.child.data(keys, table.name, child) 
			child.data[[table.name]][[child]] <- x
			}
		}
	if(length(child.data)==0)return(NULL)
return(child.data)}
#----------------------------------------------------------------------------------------------------
remove.blank.columns.from.table <- function(table){
	if(is.null(table))return(table)
	tb <- table
	keep.i <- colSums(!is.na(tb))!=0
	tb <- tb[,keep.i,drop=F]
return(tb)}
#----------------------------------------------------------------------------------------------------
get.all.data <- function(table.name, primary.value){

	sql.command <- "SELECT * FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE WHERE CONSTRAINT_SCHEMA='BIAD'"
	keys <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)


	# table data
	all.data <- list()
	all.data[[table.name]]$data <- get.table.data(keys, table.name, primary.value) 
	
	# child data
	x.data <- all.data[table.name]
	x.sub <- child.data.wrapper(keys, x.data)
	if(!is.null(x.sub))all.data[table.name] <- Map(c, x.data,x.sub)

	# grand child data
	child.names <- names(all.data[[table.name]][[primary.value]])	
	child.names <- child.names[child.names!='data']	
	for(child.name in child.names){
		x.data <- all.data[[table.name]][[primary.value]][child.name]
		x.sub <- child.data.wrapper(keys, x.data)
		if(!is.null(x.sub))all.data[[table.name]][[primary.value]][child.name] <- Map(c, x.data,x.sub)
		}

	# great grand child data
	child.names <- names(all.data[[table.name]][[primary.value]])	
	child.names <- child.names[child.names!='data']	
	for(child.name in child.names){
		grand.child.names <- names(all.data[[table.name]][[primary.value]][[child.name]])
		grand.child.names <- grand.child.names[grand.child.names!='data']	
		for(grand.child.name in grand.child.names){
			x.data <- all.data[[table.name]][[primary.value]][[child.name]][grand.child.name]
			x.sub <- child.data.wrapper(keys, x.data)
			if(!is.null(x.sub))all.data[[table.name]][[primary.value]][[child.name]][grand.child.name] <- Map(c, x.data,x.sub)
			}
		}

	# great great grand child data
	child.names <- names(all.data[[table.name]][[primary.value]])	
	child.names <- child.names[child.names!='data']	
	for(child.name in child.names){
		grand.child.names <- names(all.data[[table.name]][[primary.value]][[child.name]])
		grand.child.names <- grand.child.names[grand.child.names!='data']	
		for(grand.child.name in grand.child.names){
			great.grand.child.names <- names(all.data[[table.name]][[primary.value]][[child.name]][[grand.child.name]])
			great.grand.child.names <- great.grand.child.names[great.grand.child.names!='data']	
			for(great.grand.child.name in great.grand.child.names){	
				x.data <- all.data[[table.name]][[primary.value]][[child.name]][[grand.child.name]][great.grand.child.name]
				x.sub <- child.data.wrapper(keys, x.data)
				if(!is.null(x.sub))all.data[[table.name]][[primary.value]][[child.name]][[grand.child.name]][great.grand.child.name] <- Map(c, x.data,x.sub)
				}
			}	
		}

	# parent data
	get.parent.data(keys, table.name, primary.value)

	# child data
	x.data <- all.data[table.name]
	x.sub <- child.data.wrapper(keys, x.data)
	if(!is.null(x.sub))all.data[table.name] <- Map(c, x.data,x.sub)
	
#--------------------------
library(data.tree)
plot(FromListSimple(all.data))


#----------------------------------------------------------------------------------------------------
table.name <- 'Phases'
primary.value <-  'TEG31'
res <- get.all.data(table.name, primary.value)

table.name <- 'Sites'
primary.value <-  'S09209'

table.name <- 'Phases'
primary.value <-  'NITR2'
res <- get.all.data(table.name, primary.value)

table.name <- 'Phases'
primary.value <-  'SCHEV4'
res <- get.all.data(table.name, primary.value)

table.name <- 'Sites'
primary.value <- 'S10386'
res <- get.all.data(table.name, primary.value)

table.name <- 'Sites'
primary.value <- 'S10191'
res <- get.all.data(table.name, primary.value)



#----------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------



