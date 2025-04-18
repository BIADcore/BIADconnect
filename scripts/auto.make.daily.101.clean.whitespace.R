#--------------------------------------------------------------------------------------------------------------
# It is always possible for some crap to sneak into the database,
# For example, a column that allows a VARCHAR (such as a notes field) could be handed a blank ('') instead of a NULL
#--------------------------------------------------------------------------------------------------------------
conn <- init.conn()
sql.command <- "SELECT `TABLE_NAME` FROM `information_schema`.`TABLES` WHERE TABLE_SCHEMA='biad' AND `TABLE_TYPE`='BASE TABLE';"
tables <- query.database(sql.command = sql.command, conn=conn)
tables <- tables$TABLE_NAME
#--------------------------------------------------------------------------------------------------------------
# replace any blank entries with NULL
#--------------------------------------------------------------------------------------------------------------
for(n in 1:length(tables)){

	sql.command <- paste("SELECT * FROM `BIAD`.`",tables[n],"`",sep='')
    d <- query.database(sql.command = sql.command, conn=conn)
    prog  <-  paste0("checking table: ", n, "/", length(tables), " [", tables[n], "]")
    cat('\r',sprintf("%-*s", 80, prog), sep="")
	if(!is.null(d)){
		C <- ncol(d)
		for(c in 1:C){
			raw <- d[,c]
			bad <- which(raw=='')
			if(length(bad)>0){
				sql.command <- paste("UPDATE `BIAD`.`",tables[n],"` SET `",names(d)[c],"`=NULL WHERE `",names(d)[c],"`=''",sep='')
                print("cleaning:",sql.command)
                query.database(sql.command = sql.command, conn = conn)
				}
			}
			clean <- gsub('\t|\n|\r',' ',trimws(raw))
			bad <- which(raw!=clean)
			if(length(bad)>0){
				for(b in 1:length(bad)){
					to <- clean[bad[b]]
					from <- raw[bad[b]]
					sql.command <- paste("UPDATE `BIAD`.`",tables[n],"` SET `",names(d)[c],"`=\"",to,"\" WHERE  `",names(d)[c],"`=\"",from,"\"",sep='')
                    print("cleaning:",sql.command)
                    query.database(sql.command = sql.command, conn = conn)
                }
            }
		}
	}
print("done")
#--------------------------------------------------------------------------------------
disconnect()
#--------------------------------------------------------------------------------------