\name{rm.all}
\alias{rm.all}
\title{Remove All Objects}
\usage{
rm.all(keep = NULL)
}
\arguments{
\item{keep}{a vector of strings specifying object names to be kept in environment, optional, if omitted then all objects in global environment are removed.}
}
\value{
None.
}
\description{
Removes all objects from global environment, except those that are specified by argument \code{keep}.
}
\examples{
# No example available to avoid possibly unwanted object deletion in user environment.
}
\concept{.GlobalEnv}
\concept{delete}
\concept{environment}
\concept{objects}
\concept{remove}
