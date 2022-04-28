#--------------------------------------------------------------------------------------------------
encoder <- function(df){
	if(nrow(df)==0) return(NULL)
	if(nrow(df)>0){
		for(n in 1:ncol(df))
		if(class(df[,n])=="character")df[,n] <- iconv(df[,n],"utf8","utf8")
		return(df)	
		}
	}
#--------------------------------------------------------------------------------------------------
sql.wrapper <- function(sql.command,user,password){
	require(RMySQL)
	require(odbc)
	drv <- dbDriver("MySQL")
	
	# connect locally to the database
	con <- dbConnect(drv,host = "127.0.0.1", user=user, pass=password)
	dbSendQuery(con,"SET NAMES 'utf8'")

	# query the database and tidy
	res <- suppressWarnings(dbSendQuery(con,sql.command))
	query <- fetch(res, n= -1)
	query <- encoder(query)

	# close the connection to the database
	suppressWarnings(dbDisconnect(con))
return(query)}
#--------------------------------------------------------------------------------------------------
