\name{summary_stat}
\alias{summary_stat}
\title{
Returns summary statistics of Financial variables}
\description{
The function allows to obtain summary statistics for financial variables. Mean and standard deviation are computed by default. If requested spatial autocorrelation is computed through the Moran test.}
\usage{
summary_stat(
data,
corr = FALSE,
variable,
d1,
d2,
plot = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Dataset of class 'data.frame'. Specify the dataset obtained from the retrieving and the \code{merge_data} functions.}

\item{corr}{
Logical.  By default set to \code{FALSE}. If spatial autocorrelation is of interest set to \code{TRUE}. To compute the spatial autocorrelation point geometries are required.}

\item{variable}{
Character. Specify the name of the variable for which to obtain the spatial autocorrelation.}

\item{d1}{See \code{\link[spdep]{dnearneigh}} function for details.}
\item{d2}{See \code{\link[spdep]{dnearneigh}} function for details.}

\item{plot}{Logical. By default set to \code{FALSE}. If Moran plot for spatial autocorrelation is of interest set the argument to \code{TRUE}.}
}

\value{
A list with class list containing the following components:
\item{variable }{the names of the financial variables}
\item{mean}{the value of the mean for each financial variable}
\item{sd}{the value of the standard deviation for each financial variables}
\item{moran.test}{list containing the results of the \code{moran.test} function}
}
\references{
  Moran, Patrick AP (1950). "A test for the serial independence of residuals." Biometrika, 37(1/2), 178–181. JSTOR.
}
\author{
Lorena Ricciotti}


