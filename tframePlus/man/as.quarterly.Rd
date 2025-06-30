\name{as.quarterly}
\alias{as.weekly}
\alias{as.quarterly}
\alias{as.annually}

\title{Aggregate Time Series}
\description{Convert series to quarterly or annual.}

\usage{
    as.quarterly(x, FUN=sum, na.rm=FALSE, ...)
    as.annually(x, FUN=sum, na.rm=FALSE, ...)
    as.weekly(x, FUN=sum, na.rm=FALSE, foldFrom=end(x), periodicity = 7)
}

\arguments{
    \item{x}{a tframed object. (Only montly is currently working)}
    \item{FUN}{the function to use for aggregating.}
    \item{na.rm}{Logical indicating if \code{NA} should be removed from 
       the beginning and end of a series}
    \item{foldFrom}{a date which is used to determine the end of weeks.}
    \item{periodicity}{the number of periods in a week.}
    \item{...}{additional arguments passed to \code{aggregate}}
}
\value{time series}
\details{
  Functions \code{as.quarterly} and \code{as.annually} uses aggregate, but 
  shifts the data to match usual economic and
  financial quarters or years (whereas aggregate simply groups together a number
  of periods corresponding to the new frequency starting with the first 
  observation). These functions should give the same result as
  aggregate if the data starts and ends on quarter (annual) boundaries. 
  If the data does not start and end on quarter (annual) boundaries 
  then \code{NA} will be put in where 
  data is incomplete, and the quarter (year) removed if \code{na.rm=TRUE}.

  Monthly to annual gives the aggregate by converting first to quarterly, 
  which is not exactly correct.

  To weekly by \code{periodicity} groupings from \code{foldFrom}.
  Partial weeks in the beginning or end are padded with \code{NA} and
  dropped if \code{na.rm=FALSE}. 
  (as.weekly has been tested only with daily 7 day weeks.) 
}
\seealso{
    \code{\link[tframe]{tfwindow}}
    \code{\link[stats]{aggregate}}
}
\examples{
  z <- ts(1:10, start = c(1999,2), frequency=4)
  as.annually(z) 
  as.annually(z, na.rm=TRUE)

  z <- ts(1:30, start = c(1999,2), frequency=12)
  as.annually(z) 
  as.annually(z, na.rm=TRUE)
  as.quarterly(z) 
  as.quarterly(z, na.rm=TRUE)

}
\keyword{utilities}
\keyword{ts}
