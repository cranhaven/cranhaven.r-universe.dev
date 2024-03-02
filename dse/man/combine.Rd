\name{combine}
\alias{combine}
\alias{combine.default}

\title{Combine two objects.}
\description{This is a generic method to
combine two objects of the same class to make a single object of that class.
}
\usage{
    combine(e1, e2)
    \method{combine}{default}(e1, e2)
    }
\arguments{
    \item{e1, e2}{TSdata objects.}
}
\value{An object of the same class as the argument but containing both e1 and e2.}
\seealso{
tbind, combine.TSdata, combine.forecastCov 
}
\examples{
data("eg1.DSE.data.diff", package="dse")
data("eg1.DSE.data", package="dse") 
new.data.set <- combine(eg1.DSE.data.diff, eg1.DSE.data) 
}
\concept{DSE}
\keyword{ts}

