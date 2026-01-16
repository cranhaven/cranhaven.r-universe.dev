\name{db_het_oga}
\alias{db_het_oga}
\title{Detecting and cleaning outliers in a heterogeneous time series database with OGA}
\description{Detects and cleans Additive Outliers (AOs) and Level Shifts (LSs) in time series that form a heterogeneous database, i.e. the series may have different definitions, sample sizes and/or frequencies. The function runs in parallel on the computer cores.}
\usage{db_het_oga(Y)}
\arguments{
  \item{Y}{The database, a \code{list} of \code{p} \code{ts} objects with possibly different lengths and/or frequencies. It is assumed that each time series has its frequency defined in its \code{ts} object.}
}
\details{The function applies the \code{single_oga} function to each of the time series that make up the database to detect outlier effects and clean the series of such effects. This process is run in parallel on the computer cores, which saves a lot of computational cost. The function provides a \code{list} of \code{ts} objects with the original series cleaned from the effect of the AOs and LSs, in addition to the location, size and t-statistic corresponding to each of them.}
\value{
  \item{n_AOs}{A \code{vector} with the number of AOs detected in each series of the database.}
  \item{n_LSs}{A \code{vector} with the number of LSs detected in each series of the database.}
  \item{AOs}{A \code{list} with the AOs detected in each series of the database.}
  \item{LSs}{A \code{list} with the LSs detected in each series of the database.}
  \item{Y_clean}{The cleaned database, a \code{list} of \code{p} cleaned time series.}
  \item{result}{A message indicating when the procedure has worked correctly or the problem encountered if the procedure stops.}
}
\references{Galeano, P., Peña, D. and Tsay, R. S. (2025). Efficient outlier detection for large time series databases. Working paper, Universidad Carlos III de Madrid.}
\author{Pedro Galeano.}
\note{The computational cost depends on the size of the database and the level of contamination of the series. Note that the function may take several minutes if the database contains hundred of series with thousands of observations.}
\seealso{\code{\link{single_oga}}; \code{\link{db_hom_oga}}.}
\examples{
\donttest{
# Load FREDMDApril19 dataset from the SLBDD package
data("FREDMDApril19",package="SLBDD")

# Define frequency s, the same for all series
s <- 12

# Define a list with the first 10 time series with frequency s
X <- FREDMDApril19[,1:10]
Y <- vector(mode='list',length=ncol(X))
for (k in 1:ncol(X)){Y[[k]] <- ts(X[,k],frequency=s)}

# Apply the function to Y
out_db_het_oga <- db_het_oga(Y)
}
}
