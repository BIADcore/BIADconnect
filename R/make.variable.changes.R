
#--------------------------------------------------------------------------------------------------------------------
source('functions.R')

mapping <- sql.wrapper("SELECT * FROM BIAD.zprivate_mapping",user,password,hostname,hostuser,keypath,ssh)
keys <- sql.wrapper("SELECT * FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE WHERE REFERENCED_TABLE_SCHEMA = 'BIAD'",user,password,hostname,hostuser,keypath,ssh)
#--------------------------------------------------------------------------------------------------------------------
for(n in 1:nrow(mapping)){
	fk <- subset(keys, TABLE_NAME==mapping$Table[n] & COLUMN_NAME==mapping$Column[n])
	upstream <- subset(keys, COLUMN_NAME==mapping$Column[n])
	from.variable <- mapping$From[n]
	to.variable <- mapping$To[n]
	zoption.column <- fk$REFERENCED_COLUMN_NAME
	zoption.table <- fk$REFERENCED_TABLE_NAME
	existing.variables <- sql.wrapper(paste("SELECT ",zoption.column," FROM BIAD.",zoption.table,sep=''),user,password,hostname,hostuser,keypath,ssh)[,1]

	# add new variable to zoptions table, if it isnt already there.
	if(!to.variable%in%existing.variables){
		sql.command <- paste("INSERT INTO `BIAD`.`",zoption.table,"` (`",zoption.column,"`) VALUES ('",to.variable,"')",sep='')
		sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)
		}

	# change the variable in the specified table
	sql.command <- paste("UPDATE `BIAD`.`",mapping$Table[n],"` SET `",mapping$Column[n],"`='",to.variable,"' WHERE `",mapping$Column[n],"`='",from.variable,"'",sep='')
	sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)

	# remove old variable, if it is not required by any other upstream table
	all.upstream <- c()
	for(i in 1:nrow(upstream)){
		upstream.variables <- unique(sql.wrapper(paste("SELECT ",upstream$COLUMN_NAME[i]," FROM BIAD.",upstream$TABLE_NAME[i],sep=''),user,password,hostname,hostuser,keypath,ssh)[,1])
		all.upstream <- c(all.upstream, upstream.variables)
		}
	all.upstream <- sort(unique(all.upstream))
	if(!from.variable%in%all.upstream){
		sql.command <- paste("DELETE FROM `BIAD`.`",zoption.table,"` WHERE `",zoption.column,"`= '",from.variable,"'",sep='')
		sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)
		}
	}

#--------------------------------------------------------------------------------------------------------------------

