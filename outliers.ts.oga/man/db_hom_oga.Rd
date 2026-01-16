\name{db_hom_oga}
\alias{db_hom_oga}
\title{Detecting and cleaning outliers in a homogeneous time series database with OGA}
\description{Detects and cleans Additive Outliers (AOs) and Level Shifts (LSs) in time series that form a homogeneous database, i.e. all series are defined similarly, have the same length and the same frequency. The function runs in parallel on the computer cores.}
\usage{db_hom_oga(Y,s=NULL)}
\arguments{
  \item{Y}{The database, a \code{matrix} of size \code{Txp}, where \code{T} is the time series length and \code{p} is the number of series.}
  \item{s}{Optional, the time series frequency, i.e., the number of observations per unit of time (\code{s=1} for non-seasonal, \code{s=4} for quarterly, \code{s=7} for weekly, \code{s=12} for monthly, \code{s=24} for daily, \code{s=52} for yearly, or \code{s=60} for hourly). If the value of \code{s} is not given, the value \code{s=1} is taken.}
}
\details{The function applies the \code{single_oga} function to each of the time series that make up the database to detect outlier effects and clean the series of such effects. This process is run in parallel on the computer cores, which saves a lot of computational cost. The function provides a \code{matrix} with the original series cleaned from the effect of the AOs and LSs, in addition to the location, size and t-statistic corresponding to each of them.}
\value{
  \item{n_AOs}{A \code{vector} with the number of AOs detected in each series of the database.}
  \item{n_LSs}{A \code{vector} with the number of LSs detected in each series of the database.}
  \item{AOs}{A \code{list} with the AOs detected in each series of the database.}
  \item{LSs}{A \code{list} with the LSs detected in each series of the database.}
  \item{Y_clean}{The cleaned database, a \code{matrix} of size \code{Txp}.}
  \item{result}{A message indicating when the procedure has worked correctly or the problem encountered if the procedure stops.}
}
\references{Galeano, P., Peña, D. and Tsay, R. S. (2025). Efficient outlier detection for large time series databases. Working paper, Universidad Carlos III de Madrid.}
\author{Pedro Galeano.}
\note{The computational cost depends on the size of the database and the level of contamination of the series. Note that the function may take several minutes if the database contains hundred of series with thousands of observations.}
\seealso{\code{\link{single_oga}}; \code{\link{db_het_oga}}.}
\examples{
\donttest{
# Load FREDMDApril19 dataset from the SLBDD package
data("FREDMDApril19",package="SLBDD")

# Define frequency s
s <- 12

# Apply the procedure to the first 10 time series in FREDMDApril19
Y <- FREDMDApril19[,1:10]
out_db_hom_oga <- db_hom_oga(Y,s=s)
}
}
