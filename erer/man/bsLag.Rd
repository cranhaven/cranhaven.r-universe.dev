\name{bsLag}
\alias{bsLag}
\title{Lagged Time Series}
\description{Generate a set of lagged time series for time series data.}

\usage{
bsLag(h, lag, prefix = "", var.name, suffix = ".t_", 
    include.orig = TRUE, by.lag = FALSE, ...)
}
\arguments{
  \item{h}{time series data}
  \item{lag}{number of lags}
  \item{prefix}{prefix for the name of lagged time series.}
  \item{var.name}{varible name of the lagged time series.}
  \item{suffix}{suffix of the name of lagged time series.}
  \item{include.orig}{logical value (default of TRUE) of whether to include the original series (i.e., lag zero) in the final output.}
  \item{by.lag}{logical value (default of FALSE) of whether to order the column by variable (FALSE) or by lag (TRUE).}
  \item{\dots}{additional arguments to be passed.}
}
\details{
The input data can be a single time series or a set of multiple time series data. The output is a set of lagged time series with the specified lag dimension. All the series are aligned with the shortest window so the loss of observations is equal to \code{lag}. The original series (e.g., without lag but just loss of beginning observations) can be included or excluded by setting the logical value of \code{include.org}. 

The name of the output data is composed of four parts: \code{prefix}, \code{var.name}, \code{suffix}, and an index number of \code{lag}. Users can control the first three parts only because the lag number is added automatically.  \code{prefix} and \code{suffix} can be fixed for all the output series. \code{var.name} provides some flexibility when \code{bsLag} is used within a function and the variable name is unknown \emph{a priori}.

The column of the output can be ordered either by the variable name (e.g., diff.GA.t_0, diff.GA.t_1, diff.ND.t_0, diff.ND.t_1), or by the lag order ((e.g., diff.GA.t_0, diff.ND.t_0, diff.GA.t_1, diff.ND.t_1).
}
\value{Return a multiple time series object.}
\author{Changyou Sun (\email{cs258@msstate.edu})}
\examples{
# simple example
h1 <- ts(data=cbind(1:24), start=c(2001, 1), frequency=12)
h2 <- ts(data=cbind(1:24, 25:48), start=c(2001, 1), frequency=12)
h3 <- ts(data=cbind(1:4, 5:8, 9:12), start=c(2001, 1), frequency=4)
colnames(h2) <- c("aa", "bb")
colnames(h3) <- c("cc", "dd", "ee")
h1; h2; h3

bsLag(h=h1, lag=0, prefix="", suffix=".t_")
bsLag(h=h1, lag=2, prefix="price.", var.name="fl", suffix=".t_")
bsLag(h=h1, lag=2, prefix="price.", var.name="fl", suffix=".t_", by.lag=TRUE)
bsLag(h=h1, lag=23, prefix="price.", suffix=".t_", include.orig=FALSE)

bsLag(h=h2, lag=4, prefix="", suffix=".t_", include.orig = TRUE)
bsLag(h=h2, lag=4, prefix="", suffix=".t_", include.orig = FALSE)
bsLag(h=h2, lag=4, prefix="", suffix=".t_", include.orig = FALSE, by.lag=TRUE)
bsLag(h=h2, lag=0, prefix="", var.name=c("nc", "sc"), suffix=".t_")

bsLag(h=h3, lag=2, prefix="", suffix=".t_", include.orig=FALSE)
bsLag(h=h3, lag=1, prefix="", var.name=c("nd", "sd", "mi"), suffix=".lag.")
bsLag(h=h3, lag=2, prefix="NY.", suffix=".t_", by.lag=TRUE)
bsLag(h=h3, lag=3, prefix="NY.", suffix=".t_", include.orig=FALSE)

# with real data
data(daBedRaw)
small <- daBedRaw[, c("vCN", "qCN")]
(lag.small <- bsLag(h=small, lag=4))
colnames(lag.small)

resid <- residuals(lm(qCN ~ vCN, data = small))
res <- ts(resid, start=start(small), end=end(small),
    frequency=tsp(small)[3])
lag.res <- bsLag(h=res, lag=2, prefix="resid.", var.name="china")
str(lag.res)
head(lag.res)
tail(lag.res)
}

\keyword{manip}