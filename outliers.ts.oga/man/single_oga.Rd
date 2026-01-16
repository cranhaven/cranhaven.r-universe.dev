\name{single_oga}
\alias{single_oga}
\title{Detect and clean outlying effects in a single time series with OGA}
\description{Algorithm for detecting and cleaning additive outliers and level shifts in a single time series with an Orthogonal Greedy Algorithm (OGA).}
\usage{single_oga(yt,s=NULL)}
\arguments{
  \item{yt}{A numeric \code{vector} or a \code{ts} object.}
  \item{s}{Optional, the time series frequency, i.e., the number of observations per unit of time (\code{s=1} for non-seasonal, \code{s=4} for quarterly, \code{s=7} for weekly, \code{s=12} for monthly, \code{s=24} for daily, \code{s=52} for yearly, or \code{s=60} for hourly). If \code{yt} is of format \code{ts}, the value of the frequency in \code{yt} is taken. If not and the value of \code{s} is not given, the value \code{s=1} is also taken.}
}
\details{The program detects and cleans a time series from the effect of Additive Outliers (AOs) and Level Shifts (LSs). For this purpose, the procedure proposed in the paper 'Efficient outlier detection in heterogeneous time series databases' by Galeano, Peña and Tsay (2024) is used. The procedure can be divided into three automatic steps. The initial step involves fitting a sufficiently high-order AR model to \code{yt} using robust regression to obtain an AR representation and a residual series. Then, an Orthogonal Greedy Algorithm (OGA) procedure is applied to the residual series to identify a set of potential AOs and LSs and to remove their effects from \code{yt}. The identified set of outlying effects is referred to as the first set of potential outliers. The second step is to identify and fit an ARIMA or SARIMA model, depending on whether seasonality is detected, to the outlier-adjusted series of the first step and to obtain a new residual series. The OGA procedure is then applied to this new residual series to identify a new set of potential AOs and LSs, if any. The detected outlying effects form the second set of potential outliers. The third step involves combining the potential outliers identified in the first and second steps to remove any redundancies so as to obtain a final set of potential AOs and LSs, and fitting an ARIMA (or SARIMA) model jointly with the final set of potential outliers. Then, any negligible outlying effects, if any, are removed. Finally, any detected AOs and LSs are removed from the observed time series \code{yt} to produce an outlier-free time series.}
\value{
  \item{yt_clean}{A \code{ts} object with the cleaned time series after removing the effects of the outliers in the observed time series.}
  \item{aos}{A \code{matrix} with the Additive Outliers (AOs) detected including location, size and t-test. If \code{NULL}, no AOs have been found in the series.}
  \item{lss}{A \code{matrix} with the Level Shifts (LSs) detected including location, size and t-test. If \code{NULL}, no LSs have been found in the series.}
}
\references{Galeano, P., Peña, D. and Tsay, R. S. (2025). Efficient outlier detection for large time series databases. Working paper, Universidad Carlos III de Madrid.}
\author{Pedro Galeano.}
\seealso{\code{\link{db_hom_oga}}; \code{\link{db_het_oga}}.}
\examples{

## Load FREDMDApril19 dataset from the SLBDD package
data("FREDMDApril19",package="SLBDD")
Y <- FREDMDApril19

## Define time series yt and frequency s
yt <- Y[,1]
s <- 12

## Apply the function to yt
out_single_oga <- single_oga(yt,s=s)

}
