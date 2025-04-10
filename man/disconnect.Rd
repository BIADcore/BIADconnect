% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.database.connect.R
\name{disconnect}
\alias{disconnect}
\title{Disconnect from Database}
\usage{
disconnect(drv = "MySQL")
}
\arguments{
\item{drv}{A character string specifying the database driver to disconnect. Default is \code{"MySQL"}.}
}
\value{
None. All connections using the specified driver are closed.
}
\description{
This function disconnects all active connections to the specified database driver. By default, it
disconnects all MySQL database connections. It requires the RMySQL and DBI packages to manage
database connections.
}
\examples{
\dontrun{
  # Disconnect all MySQL connections
  disconnect()
  
  # Disconnect all connections of a specific driver (e.g., PostgreSQL)
  disconnect(drv = "PostgreSQL")
}

}
