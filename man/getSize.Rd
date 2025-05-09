% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{getSize}
\alias{getSize}
\title{Retrieve BIAD size}
\usage{
getSize(conn = NULL, db.credential = NULL, db = "BIAD")
}
\arguments{
\item{conn}{A database connection object. Default is \code{NULL}.}

\item{db.credential}{manually pass database credentials. Default is \code{NULL}.}

\item{db}{name of the database to be returned}
}
\value{
A data frame with the database sizes in gigabytes.
}
\description{
This function retrieves the sizes of BIAD, to help figuring out which dockers to use
}
