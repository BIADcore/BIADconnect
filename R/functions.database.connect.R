#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
# generic functions to query any database on the server
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#' Query Database
#'
#' This function sends a SQL command to query BIAD 
#' It handles SQL command execution, error management, and automatic reconnection if the 
#' connection fails.
#'
#' @param sql.command A character vector containing SQL queries to be executed.
#' @param conn A DBI connection object. If `NULL`, it will check or initialize a connection 
#' from the global environment.
#' @param db.credentials A list of database credentials including user, password, host, and port. 
#' If `NULL`, credentials are fetched from environment variables using `get.credentials`.
#' @param wait A numeric value specifying the time in seconds to wait between each SQL command execution. Default is 0.
#'
#' @return The function returns the fetched query results as a dataframe.
#' @examples
#' \dontrun{
#'   # Example of connecting and querying
#'   credentials <- list(
#'     BIAD_DB_USER = "yourusername",
#'     BIAD_DB_PASS = "yourpassword",
#'     BIAD_DB_HOST = "host",
#'     BIAD_DB_PORT = 3306
#'   )
#'   
#'   result <- query.database(
#'     sql.command=c("SELECT * FROM table"),
#'     db.credentials=credentials
#'   )
#' }
#'
#' @export
query.database <- function(sql.command, conn=NULL, db.credentials=NULL, wait = 0){
	require(DBI)
	conn <- check.conn(conn = conn, db.credentials = db.credentials) #this doesn't return anything but modify conn if need, if not, nothing happen
	if(is.null(conn))conn  <- get("conn", envir = .GlobalEnv)
	for(n in 1:length(sql.command)){
 		if(wait>0)Sys.sleep(wait)
		res <- tryCatch(suppressWarnings(DBI::dbSendStatement(conn,sql.command[n])),
			error = function(e){
                    if(grepl("statement",e$message)){
                        stop("Problement in SQL statement: ",sql.command[n], "\n (original error: \"",e$message,"\", call: \"",e$call,"\"")
                    }
                    if(grepl("connection",e$message)){
                        disco <- disconnect()
                        conn <- init.conn(db.credentials=db.credentials)
                        assign("conn", conn, envir = .GlobalEnv)
                        stop("error while sending command: ",sql.command[n], "\n Starting a new connection: you will need to re-run your last command.\n (original error: \"",e$message,"\", call: \"",e$call,"\"")
                    }
                    print(e)
				}
			)
		query <- DBI::fetch(res, n= -1)	
		DBI::dbClearResult(res)
		}
	query <- encoder(query)
return(query)}
#--------------------------------------------------------------------------------------------------
# internal helper function
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
#' environment variables that should be stored in `~/.Renviron`. For more info see
#' \code{\link{https://biadwiki.org/en/connectR}} 
#'
#' @param db.credentials A list containing database connection details. The list 
#' should include `BIAD_DB_USER`, `BIAD_DB_PASS`, `BIAD_DB_HOST`, and `BIAD_DB_USER`. If `NULL`, defaults 
#' to fetching these values from environment variables. You can store these in
#' `~/.Renviron` or export them in your environment using your favorite method
#' (ie: $export BIAD_DB_HOST='127.0.0.1')
#'
#' @return A DBI connection object to the MySQL database.
#' @examples
#' \dontrun{
#'   # Example of connecting and querying
#'   conn  <- init.conn(
#'       db.credentials = list(
#'           BIAD_DB_USER = "yourusername",
#'           BIAD_DB_PASS = "yourpassword",
#'           BIAD_DB_HOST = "hostname",
#'           BIAD_DB_PORT = 3306
#'       )
#'   )
#'  }
#'   result <- query.database(
#'     sql.command=c("SELECT * FROM table"),
#'     conn=conn
#'   )
#' @export
init.conn <- function(db.credentials=NULL, dbname="BIAD"){
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
            DBI::dbConnect(drv=DBI::dbDriver("MySQL"), user=db.credentials$BIAD_DB_USER, pass=db.credentials$BIAD_DB_PASS, dbname=dbname, host = db.credentials$BIAD_DB_HOST, port=db.credentials$BIAD_DB_PORT) ,
		error=function(e){
			message("Couldn't initialise connection with the database, dbConnect returned error: ")
			message(e)
			message("Check your db.credentials below:")
			na <- sapply(names(db.credentials),function(nc)message(nc,": ", ifelse(nc=="BIAD_DB_PASS",msp(db.credentials[[nc]]),db.credentials[[nc]])))
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
    if(nchar(password)<2)return("")
    maskp <- strsplit(password, "")[[1]]
    paste0(maskp[1], paste0(rep("*", length(maskp) - 2), collapse = ""), maskp[length(maskp)])
}
#--------------------------------------------------------------------------------------------------
#' Disconnect from Database
#'
#' This function disconnects all active connections to the specified database driver. By default, it 
#' disconnects all MySQL database connections. It requires the RMySQL and DBI packages to manage 
#' database connections.
#'
#' @param drv A character string specifying the database driver to disconnect. Default is `"MySQL"`.
#'
#' @return None. All connections using the specified driver are closed.
#'
#' @examples
#' \dontrun{
#'   # Disconnect all MySQL connections
#'   disconnect()
#'   
#'   # Disconnect all connections of a specific driver (e.g., PostgreSQL)
#'   disconnect(drv = "PostgreSQL")
#' }
#'
#' @export
disconnect <- function(drv="MySQL"){
    require(RMySQL)
    require(DBI)
    sapply(DBI::dbListConnections(DBI::dbDriver(drv)),DBI::dbDisconnect)
}
#--------------------------------------------------------------------------------------------------
#' Check and Validate Database Connection
#'
#' This function verifies the validity of a given database connection. If the connection is not valid 
#' or not provided, it attempts to retrieve an existing connection from the global environment. If no 
#' valid connection is found, it initializes a new connection using the provided database credentials.
#'
#' @param conn A DBI connection object. If `NULL`, the function attempts to find a valid connection 
#' in the global environment or initialize a new connection.
#' @param db.credentials A list of database credentials including user, password, host, and port. 
#' If `NULL`, credentials are fetched using `get.credentials`.
#'
#' @return A valid DBI connection object.
#'
#' @examples
#' \dontrun{
#'   # Check an existing connection or create a new one
#'   credentials <- list(
#'     BIAD_DB_USER = "yourusername",
#'     BIAD_DB_PASS = "yourpassword",
#'     BIAD_DB_HOST = "127.0.0.1",
#'     BIAD_DB_PORT = 3306
#'   )
#'   
#'   conn <- check.conn(conn = NULL, db.credentials = credentials)
#' }
#'
#' @export
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
get.credentials  <-  function(){
    list(
         BIAD_DB_USER=Sys.getenv("BIAD_DB_USER"),
         BIAD_DB_PASS=Sys.getenv("BIAD_DB_PASS"),
         BIAD_DB_HOST=Sys.getenv("BIAD_DB_HOST"),
         BIAD_DB_PORT=as.numeric(Sys.getenv("BIAD_DB_PORT"))
    )
}
#----------------------------------------------------------------------------------------------------
