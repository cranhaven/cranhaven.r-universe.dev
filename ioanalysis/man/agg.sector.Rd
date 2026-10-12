\name{agg.sector}
\alias{agg.sector}

\title{Aggregate Sectors}
\description{\code{agg.sector} takes specified sectors and creates a "new" joint sector. This produces a new \code{InputOutput} object. Note the Leontief Inverse and Ghoshian Inverse are elements. There is deliberately no warning if the sector does not occur in all regions. See \code{\link{locate.mismatch}}.

Caution: Inverting large matrices will take a long time. R does a computation roughly every 8e-10 second. The number of computations per matrix inversion is n^3 where n is the dimension of the square matrix. For n = 5000 it should take 100 seconds.}
\usage{
agg.sector(io, sectors, newname = "newname")
}

\arguments{
  \item{io}{An \code{InputOutput} class object from \code{\link{as.inputoutput}}}
  \item{sectors}{Character. Specific sectors to be aggregated. Can either be a character that exactly matches the name of the sector in \code{RS_label} or the number of the sector in the order it appears in \code{RS_label}. May also be \code{'all'} to select all sectors.}
  \item{newname}{Character. The name to give to the new aggregated sector.}
}


\details{Creates the aggregation matrix to pre (and/or post when appropriate) to aggregate the matrices in the \code{InputOutput} object. Say you have 1 region with n sectors and you wish to aggregate sectors i and i+1. A diagonal matrix is converted into a n-1xn matrix where rows i and i+1 are additively combined together. This matrix is then used to create new aggregated tables. The "new" sector is then stored in location i. See  Blair and Miller 2009 for more details.}


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
newIO <- agg.sector(toy.IO, sectors = c(1,2), newname = "Party.Supplies")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
