\name{agg.region}
\alias{agg.region}

\title{Aggregate Regions}
\description{
\code{agg.sector} takes specified regions and creates a "new" joint region. This produces a new \code{InputOutput} object. Note the Leontief Inverse and Ghoshian Inverse are elements. All regions must have exactly the same sectors. See \code{\link{locate.mismatch}}.

Caution: Inverting large matrices will take a long time. R does a computation roughly every 8e-10 second. The number of computations per matrix inversion is n^3 where n is the dimension of the square matrix. For n = 5000 it should take 100 seconds.
}

\usage{
agg.region(io, regions, newname = "newname")
}
\arguments{
\item{io}{An \code{InputOutput} class object from \code{\link{as.inputoutput}}}
  \item{regions}{Character. Specific regions to be aggregated. Can either be a character that exactly matches the name of the region in \code{RS_label} or the number of the region in the order it appears in \code{RS_label}. May also be \code{'all'} to select all regions.}
  \item{newname}{Character. The name to give to the new aggregated region.}
}


\details{
Creates an aggregation matrix similar to that of \code{\link{agg.sector}}. See  Blair and Miller 2009 for more details.
}

\value{A new \code{InputOutput} object is created. See \code{\link{as.inputoutput}}.}
\references{
Blair, P.D. and Miller, R.E. (2009). "Input-Output Analysis: Foundations and Extensions". Cambridge University Press

Nazara, Suahasil & Guo, Dong & Hewings, Geoffrey J.D., & Dridi, Chokri, 2003. "PyIO. Input-Output Analysis with Python". REAL Discussion Paper 03-t-23. University of Illinois at Urbana-Champaign. (\url{http://www.real.illinois.edu/d-paper/03/03-t-23.pdf})
}
\author{John J. P. Wade, Ignacio Sarmiento-Barbieri}




\seealso{
\code{\link{as.inputoutput}}, \code{\link{locate.mismatch}}, \code{\link{agg.region}}
}
\examples{
data(toy.IO)
class(toy.IO)
agg.region(toy.IO, regions = c(1,2), newname = "Magic")

}
