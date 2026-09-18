\name{createLocalList}
\alias{createLocalList}

\title{Precomputes quantities for hilde and improveSmallScales}
\description{Allows to precompute quantities that can be passed to \code{\link{hilde}} and \code{\link{improveSmallScales}}, all arguments have to coincide with the corresponding ones in those function calls. Doing so saves run time if those functions are called multiple times (with the same arguments) as then the quantities have to be computed only once.}

\usage{
createLocalList(filter, method = c("2Param", "LR"),
               lengths = if (method == "LR") 1:20 else 1:65)
}

\arguments{
\item{filter}{an object of class \code{\link{lowpassFilter}} giving the used analogue lowpass filter}
\item{method}{the testing method for short events, \code{"2Param"} allows for heterogeneous noise, \code{"LR"} assumes homogeneous noise}
\item{lengths}{a vector of integers giving the lengths on which tests will be performed to detect short events, should be chosen such that events on larger scales are already detected by the previous \code{\link{jsmurf}} step}
}

\value{
An object of class \code{"localList"}, contains computed quantities used by \code{\link{hilde}} and \code{\link{improveSmallScales}}
}

\seealso{\code{\link{hilde}}, \code{\link{improveSmallScales}}, \code{\link{lowpassFilter}}}

\examples{
# the used filter of the gramicidin A recordings given by gramA
filter <- lowpassFilter(type = "bessel", param = list(pole = 4L, cutoff = 1e3 / 1e4),
                        sr = 1e4)

\donttest{
localList <- createLocalList(filter = filter, method = "LR")

# idealisation by HILDE using localList
# this call requires a Monte-Carlo simulation
# and therefore might last a few minutes,
# progress of the Monte-Carlo simulation is reported
idealisation <- hilde(gramA, filter = filter, family = "jsmurfPS", method = "LR",
                      startTime = 9, messages = 10, localList = localList)

identical(hilde(gramA, filter = filter, family = "jsmurfPS", method = "LR",
                startTime = 9), idealisation)

# HILDE allowing heterogeneous noise, for only few lengths
localList <- createLocalList(filter = filter, method = "2Param", lengths = c(3, 10, 25))

# localList has to be computed with the same filter, method and lengths
hilde(gramA, filter = filter, family = "hjsmurf", method = "2Param",
      startTime = 9, messages = 10, r = 100,
      lengths = c(3, 10, 25), localList = localList)
# r = 100 is used to reduce its run time,
# this is okay for illustration purposes, but for precise results
# a larger number of Monte-Carlo simulations is recommend
}}

\keyword{nonparametric}
