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
return(data)}
#----------------------------------------------------------------------------------------------------
get.child.data <- function(keys, table.name, primary.value){

	# distinguish between:
	# table : the data.frame of 'table.name', limited to entries matching 'primary.value'
	# child.table : obvious
	# child.value: the single value in current table that subsets to the next child table.

	if(is.null(primary.value))return(NULL)
	primary.data <- get.table.data(keys, table.name, primary.value)
	relationships <- get.child.relationships(keys, table.name)
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
	if(length(child.data)==0)return(
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
	table.data <- list()
	table.data[[table.name]]$data <- get.table.data(keys, table.name, primary.value) 

	# child data
	child.data <- child.data.wrapper(keys, table.data[1])
	xx <- Map(c,table.data,child.data)

	# grand child data
	grand.child.data <- c()
	N <- length(child.data[[1]][[1]])
	for(n in 1:N){
		grand.child.data[[n]] <- child.data.wrapper(keys, child.data[[1]][[1]][n])
		}

#table.data = child.data[[1]][[1]][n]

########

#	parent.data <- get.parent.data(   , table.data)
test <- c(table.data,child.data,grand.child.data,great.grand.child.data,great.great.grand.child.data)
return(test)}
#----------------------------------------------------------------------------------------------------
str(table.data)
str(child.data)
rbind(table.data,child.data)
names(table.data)
names(child.data)
#----------------------------------------------------------------------------------------------------
ggc <- list(S10386 = grand.child.data)
all <- Map(c, table.data, child.data, ggc)
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



td <- get.table.data(keys, table.name, primary.value)
cd <- get.child.data(keys, table.name, primary.value)
gcd <- get.child.data(keys, names(cd)[1], 'OZER1')
#----------------------------------------------------------------------------------------------------
str(res)

a <- data.frame(dog=1:3,cat=5:7)
all <- list()
all$b <- a
all$c <- a
all$d$e <- a
all$d$f <- a


b <- list(a,a)
c <- list(a,a,a)
d <- list(b,c)

names(res[[1]][[1]])
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
# excecuting remotely on server
#-----------------------------------------------------------------------------------------
https://ryanhafen.com/blog/rmote/
rmote::start_rmote()

install.packages("rmote", repos = c(
  CRAN = "http://cran.rstudio.com",
  tessera = "http://packages.tessera.io"))



library(remoter)
remoter::client("ec2-1-2-3-4.compute-1.amazonaws.com", port=56789)

cmd <- paste("plink -ssh ",hostuser,"@",hostname," -i ",keypath," -N -L 3306:",hostname,":3306",sep='')
system(cmd, wait=FALSE)
remoter::client("localhost", port=3306)

ssh user@my.remote.machine -L 55556:localhost:55555 -N
remoter::client("localhost", port=55556)
#-----------------------------------------------------------------------------------------

