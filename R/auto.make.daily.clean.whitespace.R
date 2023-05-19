#--------------------------------------------------------------------------------------------------------------
# It is always possible for some crap to sneak into the database,
# For example, a column that allows a VARCHAR (such as a notes field) could be handed a blank ('') instead of a NULL
#--------------------------------------------------------------------------------------------------------------
tables <- sql.wrapper("SELECT `TABLE_NAME` FROM `information_schema`.`TABLES` WHERE TABLE_SCHEMA='biad' AND `TABLE_TYPE`='BASE TABLE';",user,password,hostname,hostuser,keypath,ssh)
tables <- tables$TABLE_NAME
#--------------------------------------------------------------------------------------------------------------
# replace any blank entries with NULL
#--------------------------------------------------------------------------------------------------------------
sql.commands <- c()
for(n in 1:length(tables)){

	d <- sql.wrapper(paste("SELECT * FROM `BIAD`.`",tables[n],"`",sep=''),user,password,hostname,hostuser,keypath,ssh)

	C <- ncol(d)
	for(c in 1:C){
		raw <- d[,c]
		bad <- which(raw=='')
		if(length(bad)>0){
			sql.command <- paste("UPDATE `BIAD`.`",tables[n],"` SET `",names(d)[c],"`=NULL WHERE `",names(d)[c],"`=''",sep='')
			sql.commands <- c(sql.commands,sql.command)
			}
		}
	}
if(!is.null(sql.commands))sql.wrapper(sql.commands,user,password,hostname,hostuser,keypath,ssh)
#--------------------------------------------------------------------------------------------------------------
# remove any leading or trailing whitespace, or tabs, carriage returns or new lines
#--------------------------------------------------------------------------------------------------------------
sql.commands <- c()
for(n in 1:length(tables)){

	d <- sql.wrapper(paste("SELECT * FROM `BIAD`.`",tables[n],"`",sep=''),user,password,hostname,hostuser,keypath,ssh)

	C <- ncol(d)
	for(c in 1:C){
		raw <- d[,c]
		clean <- gsub('\t|\n|\r',' ',trimws(raw))
		bad <- which(raw!=clean)
		if(length(bad)>0){
			for(b in 1:length(bad)){
				to <- clean[bad[b]]
				from <- raw[bad[b]]
				sql.command <- paste("UPDATE `BIAD`.`",tables[n],"` SET `",names(d)[c],"`=\"",to,"\" WHERE  `",names(d)[c],"`=\"",from,"\"",sep='')
				sql.commands <- c(sql.commands,sql.command)
				}
			}
		}
	}
sql.commands <- unique(sql.commands)
if(!is.null(sql.commands))sql.wrapper(sql.commands,user,password,hostname,hostuser,keypath,ssh)
#--------------------------------------------------------------------------------------------------------------