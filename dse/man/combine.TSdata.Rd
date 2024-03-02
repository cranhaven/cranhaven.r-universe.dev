\name{combine.TSdata}
\alias{combine.TSdata}
\title{Combine series from two TSdata objects.}
\description{Combine series from two TSdata objects.}
\usage{
    \method{combine}{TSdata}(e1, e2)
    }
\arguments{
    \item{e1, e2}{TSdata objects.}
}
\value{
An object of class TSdata which includes series from both e1 and e2.
}
\seealso{
tbind 
}
\examples{
data("eg1.DSE.data.diff", package="dse")
data("eg1.DSE.data", package="dse") 
new.data.set <- combine(eg1.DSE.data.diff, eg1.DSE.data) 
}
\concept{DSE}
\keyword{ts}


