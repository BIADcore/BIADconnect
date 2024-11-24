#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
# generic functions to query any database hosted at macelab
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
query.database <- function(sql.command, conn=NULL, db.credentials=NULL, wait = 0){
	require(DBI)
	conn <- check.conn(conn = conn, db.credentials = db.credentials) #this doesn't return anything but modify conn if need, if not, nothing happen
	if(is.null(conn))conn  <- get("conn", envir = .GlobalEnv)
	for(n in 1:length(sql.command)){
 		if(wait>0)Sys.sleep(wait)
		res <- tryCatch(suppressWarnings(DBI::dbSendStatement(conn,sql.command[n])),
			error = function(e){
				print(e)
				disco <- disconnect()
				conn <- init.conn(db.credentials=db.credentials)
				assign("conn", conn, envir = .GlobalEnv)
				stop("error while sending command: ",sql.command[n], "\n Starting a new connection: you will need to re-run your last command.")
				}
			)
		query <- fetch(res, n= -1)	
		DBI::dbClearResult(res)
		}
	query <- encoder(query)
return(query)}
#--------------------------------------------------------------------------------------------------
encoder <- function(df){
	if(nrow(df)==0) return(NULL)
	names(df) <- iconv(names(df),from="UTF-8",to="UTF-8")
	char <- sapply(df,class) == 'character'
	df[,char] <- apply(df[,char,drop=F],2,iconv,from="UTF-8",to="UTF-8")
return(df)}	
#--------------------------------------------------------------------------------------------------
#' Initialize Database Connection
#'
#' This function initializes a connection to the BIAD database using the provided
#' credentials. If no credentials are supplied, it attempts to retrieve them from
#' environment variables that should be stored in `~/.Renviron`
#'
#' @param db.credentials A list containing database connection details. The list 
#' should include `user`, `password`, `host`, and `port`. If `NULL`, defaults 
#' to fetching these values from environment variables. You can store these in
#' `~/.Renviron` or export them in your environment using your favorite method
#' (ie: $export host='127.0.0.1')

#' @return A DBI connection object to the MySQL database.
init.conn <- function(db.credentials=NULL){
    require(RMySQL)
    require(DBI)
    if(length(DBI::dbListConnections(DBI::dbDriver("MySQL")))!=0) disconnect()
    if(is.null(db.credentials)){
        
        db.credentials <- get.credentials()
    }
    if (all(sapply(db.credentials, function(cred) is.null(cred) || is.na(cred) || cred == ""))) {
        if (exists("user", envir = .GlobalEnv) && exists("password", envir = .GlobalEnv)) {
            warning("It seems that you are still using credentials set in .Rprofile; please use environment variables  ~/.Renviron or you ~/.bashrc.\n\r",
                    "Your ~/.Renviron should be like:\n",
                    "\t BIAD_DB_USER=\"your username\"\n",
                    "\t BIAD_DB_PASS=\"your password\"\n",
                    "\t BIAD_DB_HOST=127.0.0.1 \n",
                    "\t BIAD_DB_PORT=3306 #or something different if you specified a different port\n",
                    "  or add:  export BIAD_DB_XXX=XXX to your .bashrc")
            db.credentials$BIAD_DB_USER <- get("user", envir = .GlobalEnv)
            db.credentials$BIAD_DB_PASS <- get("password", envir = .GlobalEnv)
            db.credentials$BIAD_DB_HOST <- "127.0.0.1"
            db.credentials$BIAD_DB_PORT <- 3306
        } 
    }
    missing_vars <- names(db.credentials)[sapply(db.credentials, function(x) is.null(x) || is.na(x) || x == "")]
    if (length(missing_vars) > 0) 
        warning("Missing: ", paste(missing_vars, collapse = ", "), ". You may want to check your ~/.Renviron file and reload R, or manually provide db.credentials as a list to init.conn.")
    
    conn <- tryCatch(
            DBI::dbConnect(drv=DBI::dbDriver("MySQL"), user=db.credentials$BIAD_DB_USER, pass=db.credentials$BIAD_DB_PASS, dbname="BIAD", host = db.credentials$BIAD_DB_HOST, port=db.credentials$BIAD_DB_PORT) ,
		error=function(e){
			message("Couldn't initialise connection with the database, dbConnect returned error: ")
			message(e)
			message("Check your db.credentials below:")
			na <- sapply(names(db.credentials),function(nc)message(nc,": ", ifelse(nc=="BIAD_DB_PASS",msp(db.credentials[[nc]]),db.credentials[[nc]])))
			message("You probably haven't opened an SSH tunnel")
			message("Try running: open.tunnel()")
			stop("DBConnection fail")
   			}
		)
	DBI::dbSendQuery(conn, 'set character set "utf8"')
	DBI::dbSendQuery(conn, 'SET NAMES utf8')
    	return(conn)	
	}
#--------------------------------------------------------------------------------------------------
#' msp(Mask Password)
#' This function masks a given password by replacing all but the first and last character with asterisks.
#' @param password A character string representing the password to be masked.
#' @return A character string with the masked password.
msp <- function(password) {
    if(length(password)<=0)return(NULL)
    maskp <- strsplit(password, "")[[1]]
    paste0(maskp[1], paste0(rep("*", length(maskp) - 2), collapse = ""), maskp[length(maskp)])
}
#--------------------------------------------------------------------------------------------------
disconnect <- function(drv="MySQL"){
    require(RMySQL)
    require(DBI)
    sapply(DBI::dbListConnections(DBI::dbDriver(drv)),DBI::dbDisconnect)
}
#--------------------------------------------------------------------------------------------------
check.conn <- function(conn = NULL, db.credentials=NULL){
	require(DBI)
	if(is.null(conn) || !tryCatch(DBI::dbIsValid(conn),error=function(err)FALSE) ){ #check if no connector has been provided, or if the connector doesnt work
	if(exists("conn", envir = .GlobalEnv))conn <- get("conn", envir = .GlobalEnv) #check if a connector already exist at global level
	if(is.null(conn) || !tryCatch(DBI::dbIsValid(conn),error=function(err)FALSE) ){
		# print("the global connector is not good, delete and retry ")
		disco <- disconnect()
		conn <- init.conn(db.credentials=db.credentials)
		assign("conn",conn,envir = .GlobalEnv)
		}
	}
return(conn)}
#----------------------------------------------------------------------------------------------------
#' Retrieve Credentials from Environment Variables
#'
#' This function fetches database credentials from specified environment variables.
#'
#' @return A list containing the database user, password, host, and port.
#' @export
get.credentials  <-  functions(){
    list(
         BIAD_DB_USER=Sys.getenv("BIAD_DB_USER"),
         BIAD_DB_PASS=Sys.getenv("BIAD_DB_PASS"),
         BIAD_DB_HOST=Sys.getenv("BIAD_DB_HOST"),
         BIAD_DB_PORT=as.numeric(Sys.getenv("BIAD_DB_PORT"))
    )
}
