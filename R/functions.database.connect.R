#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
# generic functions to query any database hosted at macelab
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
run.server.query <- function(sql.command,db.credentials=NULL, hostuser=NULL, hostname=NULL, pempath=NULL){	

	# create 'server.script.R' to be run on server
	text <- c(
		"source('functions.database.connect.R')",
		"source('functions.R')",
		paste('sql.command <- c("',paste(sql.command,collapse='","'),'")',sep=''),
        "conn <- init.conn()", ##credendtial will be passed through env variable so they don't need to be written anywhere
		"query <- query.database(conn, sql.command)",
		"save(query, file='tmp.RData')",
		"DBI::dbDisconnect(conn)"
		)
	writeLines(text,con= 'server.script.R')

	query <- run.server.query.inner(db.credentials=db.credentials, hostuser=hostuser, hostname=hostname, pempath=pempath)
return(query)}
#----------------------------------------------------------------------------------------------------
run.server.query.inner <- function(db.credentials=NULL, hostuser=NULL, hostname=NULL, pempath=NULL){ 
	require(ssh)

    hostuser <- Sys.getenv("BIAD_SSH_USER")
    hostname <- Sys.getenv("BIAD_SSH_HOST")
    pempath <- Sys.getenv("BIAD_SSH_PEM")
    if (any(c(hostuser, hostname, pempath) == "") || any(is.na(c(hostuser, hostname, pempath)))) {
        if (exists("hostuser", envir = .GlobalEnv) && exists("hostname", envir = .GlobalEnv) && exists("pempath", envir = .GlobalEnv)) {
            warning(
                "seems like SSH connection details are still set from global environment (using .Rprofile) \n",
                "To avoid this warning in the future, please set the SSH connection details via environment variables in your .Renviron or .bashrc file:\n",
                "- BIAD_SSH_USER=your_ssh_username\n",
                "- BIAD_SSH_HOST=your_ssh_hostname\n",
                "- BIAD_SSH_PEM=path_to_your_pem_file\n"
            )
        hostuser <- get("hostuser", envir = .GlobalEnv)
        hostname <- get("hostname", envir = .GlobalEnv)
        pempath <- get("pempath", envir = .GlobalEnv)
        } else {
            stop("Error: Missing details for SSH connection and no global environment values found.")
        }
    }

    if(is.na(Sys.getenv("BIAD_DB_USER")) || is.null(Sys.getenv("BIAD_DB_USER")) || is.null(Sys.getenv("BIAD_DB_USER"))){
        error("as probably only you adrian and I are using this ssh feature i won't do fancy test, but you need to put usernames and passwords in environment variable (via ~/.Renviron or export VAR=dsadsa")
    }
    env_vars <- paste0("BIAD_DB_USER=\"",Sys.getenv("BIAD_DB_USER"),"\" ",
                       "BIAD_DB_PASS=\"",Sys.getenv("BIAD_DB_PASS"),"\" ",
                       "BIAD_DB_PORT=",3306," ",  
                       "BIAD_DB_HOST=","\"127.0.0.1\"")

	# create bash commands to be run on server
    tmp.path <- tempfile(pattern = "tmpdir")

	# ssh onto server, copy required files to server, tell server to run R, copy results back to local 
	session <- ssh::ssh_connect(host=paste(hostuser,"@",hostname,sep=''), keyfile=pempath)
	ssh::ssh_exec_wait(session, command = paste("mkdir -p",tmp.path))
	ssh::scp_upload(session, files = "server.script.R" , to = tmp.path, verbose=FALSE)
    ## --- this should disapear when BIADwiki becomes a packages as we'll load the package instead of sourcing things
	ssh::scp_upload(session, files = "functions.R" , to = tmp.path, verbose=FALSE)
	ssh::scp_upload(session, files = "functions.database.connect.R" , to = tmp.path, verbose=FALSE)
	unlink('server.script.R')
	ssh::ssh_exec_wait(session, command = commands)
    res <- c("tmp.RData","tmp.Rout")
	res <- sapply(res,function(fn)paste(tmp.path,fn,sep="/"))
	dl  <- sapply(res,function(fn)ssh::scp_download(session, files = fn, to = getwd(), verbose=FALSE))
    if(file.exists("tmp.RData")){
        load('tmp.RData')
        unlink(c('tmp.RData','tmp.Rout'))
    }
    else{
        query <- NULL
        na <- sapply(readLines("tmp.Rout"),function(i)cat(i,"\n"))
        unlink('tmp.Rout')
        warning('sql command failed')
    }
    ssh::ssh_exec_wait(session, command = paste("rm -r",tmp.path))
	ssh::ssh_disconnect(session)
return(query)}

#--------------------------------------------------------------------------------------------------

run.server.query.inner.alt <- function(scriptname){ 
	commands <- c(
		paste("cd",tmp.path),
		paste("/Library/Frameworks/R.framework/Resources/bin/R CMD BATCH --no-save",scriptname," > tmp.Rout"),
		"cd .."
		)
    tmp.path <- tempfile(pattern = "tmpdir")
    linkcred="-i ${BIAD_SSH_PEM}"
    host="${BIAD_SSH_USER}@${BIAD_SSH_HOST}" #we rely again on the ENV var
    system(paste("ssh", linkcred, host, shQuote(paste("mkdir -p", tmp.path))))
    sourcefold=here::here("R") #link to the source files
    filestosend <- paste(scriptname,file.path(sourcefold,"function*.R")) #send the script and all source file
    system(paste("scp", linkcred,filestosend, paste0(host,":",tmp.path,"/")))
    res <- c("tmp.RData","tmp.Rout")
	res <- sapply(res,function(fn)paste(tmp.path,fn,sep="/"))
	ssh::ssh_exec_wait(session, command = commands)
    system(paste("ssh", linkcred, host, commands ))
    dl  <- sapply(res,function(fn)system(paste("scp", linkcred, paste0(host,":",fn),".")))
    if(file.exists("tmp.RData")){
        load('tmp.RData')
        unlink(c('tmp.RData','tmp.Rout'))
    }
    else{
        query <- NULL
        na <- sapply(readLines("tmp.Rout"),function(i)cat(i,"\n"))
        unlink('tmp.Rout')
        warning('sql command failed')
    }
}
#--------------------------------------------------------------------------------------------------
query.database <- function(sql.command, conn=NULL, db.credentials=NULL){
    if(is.null(conn) || !DBI::dbIsValid(conn) ){ #check if no connector has been provided, or if the connector doesnt work
        #print("no connector provided, creating one here connecting ")
        if(exists("conn", envir = .GlobalEnv))conn <- get("conn", envir = .GlobalEnv) #check if a connector already exist at global level
        if(is.null(conn) || !DBI::dbIsValid(conn) ){
            #print("the global connector is not good, delete and retry ")
            disco <- disconnect()
            conn <- init.conn(db.credentials=db.credentials)
            assign("conn",conn,envir = .GlobalEnv)
        }
        #else{ print("connector exist at global, continue with it")}
    }
    #else{ print("connector provided")}
	for(n in 1:length(sql.command)) {
        res <- tryCatch(suppressWarnings(DBI::dbSendStatement(conn,sql.command[n])),
                    error=function(e){
                        print(e)
						disco <- disconnect()
						conn <- init.conn(db.credentials=db.credentials)
						assign("conn",conn,envir = .GlobalEnv)
                        stop("error while sending command: ",sql.command[n], "\n Starting a new connection: you will need to re-run your last command.")
                    })
    }
	query <- fetch(res, n= -1)
    DBI::dbClearResult(res)
	query <- encoder(query)
    return(query)
}
#--------------------------------------------------------------------------------------------------
encoder <- function(df){
    if(nrow(df)==0) return(NULL)
    names(df) <- iconv(names(df),from="UTF-8",to="UTF-8")
    char <- sapply(df,class) == 'character'
    df[,char] <- apply(df[,char,drop=F],2,iconv,from="UTF-8",to="UTF-8")
    return(df)	
}
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
#' @examples
#' \dontrun{
#' # Using environment variables:
#' conn <- init.conn()
#'
#' # Using explicit credentials:
#' db.credentials <- list(
#'   user = "my_user",
#'   password = "my_password",
#'   host = "localhost",
#'   port = 3306
#' )
#' conn <- init.conn(db.credentials)
#' }
init.conn <- function(db.credentials=NULL){
    require(RMySQL)
    if(is.null(db.credentials)){
        db.credentials <- list(
            BIAD_DB_USER=Sys.getenv("BIAD_DB_USER"),
            BIAD_DB_PASS=Sys.getenv("BIAD_DB_PASS"),
            BIAD_DB_HOST=Sys.getenv("BIAD_DB_HOST"),
            BIAD_DB_PORT=as.numeric(Sys.getenv("BIAD_DB_PORT"))
        )
    }
    if (all(sapply(db.credentials, function(cred) is.null(cred) || is.na(cred) || cred == ""))) {
        if (exists("user", envir = .GlobalEnv) && exists("password", envir = .GlobalEnv)) {
            warning("It seems that you are still using credentials set in .Rprofile; we will slowly move to using environment variables that you will set in your ~/.Renviron or you ~/.bashrc.\n\r",
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
    
    conn <-  tryCatch(
            DBI::dbConnect(drv=DBI::dbDriver("MySQL"), user=db.credentials$BIAD_DB_USER, pass=db.credentials$BIAD_DB_PASS, dbname="biad", host = db.credentials$BIAD_DB_HOST, port=db.credentials$BIAD_DB_PORT) ,
        error=function(e){
            message("Couldn't initialise connection with the database, dbConnect returned error: ", e)
            message("Check your db.credentials below:")
            na <- sapply(names(db.credentials),function(nc)message(nc,": ", ifelse(nc=="password",msp(db.credentials[[nc]]),db.credentials[[nc]])))
            message("Note: you can only connect to the dataset through ssh ; so you may want to check you're ssh tunel (or any plugin you may use to do so) is working (cf:https://biadwiki.org/en/Connect)")
            stop("DBConnection fail")
    })
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

disconnect <- function(drv="MySQL"){
    require(RMySQL)
    sapply(DBI::dbListConnections(DBI::dbDriver(drv)),DBI::dbDisconnect)
}
#--------------------------------------------------------------------------------------------------
