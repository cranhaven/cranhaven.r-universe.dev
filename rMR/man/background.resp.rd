\name{background.resp}
\alias{background.resp}

\title{
Determine the Background Respiration in a Respirometer
}
\description{
Takes user-defined start and end times to calculate the backround respiration rate in a respirometer. 
}
\usage{
background.resp(data, DO.var.name,
time.var.name = "std.time", 
start.time, end.time, col.vec = c("black","red"),...)
}
\arguments{
  \item{data}{
\code{data.frame} object to be used, best if formatted by \code{get.witrox.data()}. 
}
  \item{DO.var.name}{
Column name of DO variable, formatted as a character string.
}
  \item{time.var.name}{
Column name of time variable as character string. Time column must be formatted as default class for datetime: \code{class = "POSIXct" "POSIXt"}, \code{strptime} format = \code{"\%Y-\%m-\%d \%H:\%M:\%S"}.
}
  \item{start.time}{
Input start time as character string of \code{strptime} format = \code{"\%Y-\%m-\%d \%H:\%M:\%S"}.
}
  \item{end.time}{
Input endtime as character string of \code{strptime} format = \code{"\%Y-\%m-\%d \%H:\%M:\%S"}.
}
  \item{col.vec}{
Specifies colors on plot in the following order: 1) scatterplot points, 2) regression color.
}
  \item{\dots}{
Passes on arguments to internal functions.
}
}
\value{
Returns an object of method \code{biglm}. The slope of this funtion is the metabolic rate in input units/(default time).
}
\references{
Thomas Lumley (2013). biglm: bounded memory linear and generalized linear models. R package version 0.9-1. \url{https://CRAN.R-project.org/package=biglm}.

}
\author{
Tyler L. Moulton
}

\seealso{
 \code{\link{as.POSIXct}},
 \code{\link{strptime}},
 \code{\link{biglm}}
}
\examples{
##load data##
data(fishMR)

## create time variable in POSIXct format ##
fishMR$std.time <- as.POSIXct(fishMR$Date.time,
                    format = "\%d/\%m/\%Y \%I:\%M:\%S \%p")

bgd.resp <- 
background.resp(fishMR, "DO.mgL", 
           start.time = "2015-07-02 16:05:00",
           end.time = "2015-07-02 16:35:00")
}


