\name{checkInternet}
\alias{checkInternet}
\title{
Checking internet connectivity for functions with remote call
}
\description{
This is for testing and reporting url of remote servers for graceful fail in functions which use remote call
}
\usage{
checkInternet(dbase_file)
}

\arguments{
  \item{dbase_file}{
url address of the remote server
}
}
\details{
It carries out three tests on the given url: 1- if the url fetch times out , 2- if there is no internet connection, and 3- if there is bad path name and passes a message when their is failure.
}
\value{
a response message (Time out or error 404 or No internet connection) or data.frame of available data
}
\author{
Christian Thine Omuto
}

\examples{
xx="https://docs.google.com/spreadsheets/d/e"
checkInternet(xx)
}
