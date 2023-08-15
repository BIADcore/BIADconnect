
#--------------------------------------------------------------------------------------------------------------------
# Requirements
# You must have previously added the .Rprofile to your R_USER folder, here -> path.expand('~/') 
# See the BIADwiki readme or BIADwiki for details about using the .Rprofile

# Load some required functions
source("https://raw.githubusercontent.com/BIADwiki/BIADwiki/main/R/functions.R")
#--------------------------------------------------------------------------------------
mapping <- run.server.query("SELECT * FROM BIAD.zprivate_mapping")
keys <- run.server.query("SELECT * FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE WHERE REFERENCED_TABLE_SCHEMA = 'BIAD'")
#--------------------------------------------------------------------------------------------------------------------
for(n in 1:nrow(mapping)){
	fk <- subset(keys, TABLE_NAME==mapping$Table[n] & COLUMN_NAME==mapping$Column[n])
	upstream <- subset(keys, COLUMN_NAME==mapping$Column[n])
	from.variable <- mapping$From[n]
	to.variable <- mapping$To[n]
	zoption.column <- fk$REFERENCED_COLUMN_NAME
	zoption.table <- fk$REFERENCED_TABLE_NAME
	existing.variables <- run.server.query(paste("SELECT ",zoption.column," FROM BIAD.",zoption.table,sep=''))[,1]

	# add new variable to zoptions table, if it isnt already there.
	if(!to.variable%in%existing.variables){
		sql.command <- paste("INSERT INTO `BIAD`.`",zoption.table,"` (`",zoption.column,"`) VALUES ('",to.variable,"')",sep='')
		run.server.query(sql.command)
		}

	# change the variable in the specified table
	sql.command <- paste("UPDATE `BIAD`.`",mapping$Table[n],"` SET `",mapping$Column[n],"`='",to.variable,"' WHERE `",mapping$Column[n],"`='",from.variable,"'",sep='')
	run.server.query(sql.command)

	# remove old variable, if it is not required by any other upstream table
	all.upstream <- c()
	for(i in 1:nrow(upstream)){
		upstream.variables <- unique(run.server.query(paste("SELECT ",upstream$COLUMN_NAME[i]," FROM BIAD.",upstream$TABLE_NAME[i],sep=''))[,1])
		all.upstream <- c(all.upstream, upstream.variables)
		}
	all.upstream <- sort(unique(all.upstream))
	if(!from.variable%in%all.upstream){
		sql.command <- paste("DELETE FROM `BIAD`.`",zoption.table,"` WHERE `",zoption.column,"`= '",from.variable,"'",sep='')
		run.server.query(sql.command)
		}
	}

#--------------------------------------------------------------------------------------------------------------------

