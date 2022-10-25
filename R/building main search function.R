#----------------------------------------------------------------------------------------------------
source('.Rprofile')
source('functions.R')
#----------------------------------------------------------------------------------------------------
# needs to be placed inside a larger wrapper that only opens the connection once, and closes it once, for speed.
#----------------------------------------------------------------------------------------------------
get.child.relationships <- function(foreign, table.name){
	res <- subset(foreign, REFERENCED_TABLE_NAME==table.name)
return(res)}
#----------------------------------------------------------------------------------------------------
get.parent.relationships <- function(foreign, table.name){
	res <- subset(foreign, TABLE_NAME==table.name)
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
	res <- list()
	res[[primary.value]][[table.name]] <- data
return(res)}
#----------------------------------------------------------------------------------------------------
get.child.data <- function(keys, table.data){

	table.name <- names(table.data)
	child.relationships <- get.child.relationships(keys, table.name)
	child.column <- get.primary.column.from.table(keys, table.name)
	children <- table.data[[table.name]][[child.column]]
	res <- list()
	N <- nrow(child.relationships)
	if(N==0)return(NULL)
	for(child in children){
		for(n in 1:N){
			t <- child.relationships$TABLE_NAME[n]
			c <- child.relationships$COLUMN_NAME[n]
			sql.command <- paste("SELECT * FROM `BIAD`.`",t,"` WHERE ",c," = '",child,"'", sep='')
			data <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)
			data <- remove.blank.columns.from.table(data)
			res[[child]][[t]] <- data
			}
		}
	if(length(res)==0)res <- NULL

return(res)}
#----------------------------------------------------------------------------------------------------
get.parent.data <- function(foreign, table.data){

	table <- names(table.data)
	parent.relationships <- get.parent.relationships(foreign, table)
	res <- list()
	N <- nrow(parent.relationships)
	if(N==0)return(NULL)
	for(n in 1:N){
		tr <- parent.relationships$REFERENCED_TABLE_NAME[n]
		cr <- parent.relationships$REFERENCED_COLUMN_NAME[n]
		c <- parent.relationships$COLUMN_NAME[n]

		# get parent data
		values <- table.data[[table]][[c]]
		values <- values[!is.na(values)]
		values <- paste(values, collapse="','")
		data <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)
		data <- query.database(user, password, sql.command)
		data <- remove.blank.columns.from.table(data)
		if(!is.null(data))res[[c]] <- data
		}

	if(length(res)==0)res <- NULL

return(res)}
#----------------------------------------------------------------------------------------------------
child.data.wrapper <- function(keys, table.data){
	
	child.data <- list()
	children <- names(table.data)
	for(child in children){
		tables <- names(table.data[[child]])
		for(table in tables){
			data <- get.child.data(keys, table.data[[child]][table])
			child.data <- c(child.data,data)
			}
		}
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

	# store table, child data, parent data, grandchild data etc
	table.data <- get.table.data(keys, table.name, primary.value) 
	child.data <- child.data.wrapper(keys, table.data)
	grand.child.data <- child.data.wrapper(keys, child.data)
	great.grand.child.data <- child.data.wrapper(keys, grand.child.data)
	great.great.grand.child.data <- child.data.wrapper(keys, great.grand.child.data)
	great.great.great.grand.child.data <- child.data.wrapper(keys, great.great.grand.child.data)

#	parent.data <- get.parent.data(   , table.data)
test <- c(table.data,child.data,grand.child.data,great.grand.child.data,great.great.grand.child.data)
return(test)}
#----------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------
table.name <- 'Phases'
primary.value <-  'TEG31'
res <- get.all.data(table.name, primary.value)

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


names(res)

#----------------------------------------------------------------------------------------------------

get.primary.column.from.table(keys, names(res[[3]]))

unlist(res)

	

#####################
	# blank lists
	level.0 <- level.down.1 <- level.down.2 <- level.down.3 <- level.up.1 <- level.up.2 <- list()

	# level.0 data 
	value.command <- paste(column," IN ('",paste(values, collapse="','"),"')",sep='')
	sql.command <- paste("SELECT * FROM `BIAD`.`",table,"` WHERE ",value.command, sep='')
	level.0[[table]] <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)

	# level.1.down
	children.data <- get.child.data(foreign, table, values)
	children.relationships <- get.child.relationships(foreign, table)		
	children.relationships <- subset(children.relationships, TABLE_NAME%in%names(children.data))
	N <- nrow(children.relationships)
	if(N>0){
		for(n in 1:N){
				table <- children.relationships$TABLE_NAME[n]
				level.down.1[[table]] <- get.child.data(foreign, table, values)
				}
			}




	# grandchild data
	names(children)
	for(n in 1:length(children)){
		child <- names(children)[n]
		child.tables <- get.child.tables(foreign, child)
		for(c in 1:length(child.tables)){
			child.data <- get.child.data(child.tables$TABLE_NAME[c], value
str(data)
str(child)
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------

