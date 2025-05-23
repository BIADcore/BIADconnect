% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.database.connect.R
\name{query.database}
\alias{query.database}
\title{Query Database}
\usage{
query.database(sql.command, conn = NULL, db.credentials = NULL, wait = 0)
}
\arguments{
\item{sql.command}{A character vector containing SQL queries to be executed.}

\item{conn}{A DBI connection object. If \code{NULL}, it will check or initialize a connection
from the global environment.}

\item{db.credentials}{A list of database credentials including user, password, host, and port.
If \code{NULL}, credentials are fetched from environment variables using \code{get.credentials}.}

\item{wait}{A numeric value specifying the time in seconds to wait between each SQL command execution. Default is 0.}
}
\value{
The function returns the fetched query results as a dataframe.
}
\description{
This function sends a SQL command to query BIAD
It handles SQL command execution, error management, and automatic reconnection if the
connection fails.
}
\examples{
\dontrun{
  # Example of connecting and querying
  credentials <- list(
    BIAD_DB_USER = "yourusername",
    BIAD_DB_PASS = "yourpassword",
    BIAD_DB_HOST = "host",
    BIAD_DB_PORT = 3306
  )
  
  result <- query.database(
    sql.command=c("SELECT * FROM table"),
    db.credentials=credentials
  )
}

}
