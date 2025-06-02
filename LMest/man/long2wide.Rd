\name{long2wide}
\alias{long2wide}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{From data in the long format to data in the wide format}
\description{Function that transforms data in the long format to data in the wide format.}
\usage{
long2wide(data, nameid, namet, colx, coly, aggr = T, full = 999)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{matrix of data}
  \item{nameid}{name of the id column}
  \item{namet}{name of the t column}
  \item{colx}{vector of the names of the columns of the covariates}
  \item{coly}{vector of the names of the columns of the responses}
  \item{aggr}{if wide aggregated format is required}
  \item{full}{number to use for missing data}
}
\value{
  \item{listid}{list of id for every unit}
  \item{listt}{list of the time occasions}
  \item{data_wide}{data in wide format}
  \item{XX}{array of the covariates}
  \item{YY}{array of the responses}
  \item{freq}{vector of the corresponding frequencies}
}

\author{Francesco Bartolucci, Silvia Pandolfi, University of Perugia (IT), http://www.stat.unipg.it/bartolucci}

\examples{
# Example based on criminal data
# load criminal data
data(data_criminal_sim)
# consider only the first 1000 records to shorten time
out <- long2wide(data_criminal_sim[1:1000,], "id", "time", "sex",
	c("y1","y2","y3","y4","y5","y6","y7","y8","y9","y10"), aggr = TRUE, full = 999)
}
