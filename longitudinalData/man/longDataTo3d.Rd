\name{longDataTo3d}
\alias{longDataTo3d}
\title{ ~ Function: longDataTo3d ~}
\description{
Build a object \code{\linkS4class{LongData3d}} from an object
\code{\linkS4class{LongData}}. The resulting object has a single
variable-trajectory stored in a array.
}
\usage{
longDataTo3d(xLongData)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xLongData}{\code{[LongData]}: structure containning a variable-trajectory.}
}

\details{
  Build a object \code{\linkS4class{LongData3d}} from an object
  \code{\linkS4class{LongData}}. The resulting object has a single
  variable-trajectory stored in a array.
}
\value{
  An object of class \code{\linkS4class{LongData3d}}.
}


\section{Author}{
Christophe Genolini\cr
1. UMR U1027, INSERM, Université Paul Sabatier / Toulouse III / France\cr
2. CeRSME, EA 2931, UFR STAPS, Université de Paris Ouest-Nanterre-La Défense / Nanterre / France
}

\references{
  [1] C. Genolini and B. Falissard\cr
  "KmL: k-means for longitudinal data"\cr
  Computational Statistics, vol 25(2), pp 317-328, 2010\cr

  [2] C. Genolini and B. Falissard\cr
  "KmL: A package to cluster longitudinal data"\cr
  Computer Methods and Programs in Biomedicine, 104, pp e112-121, 2011\cr
}

\seealso{\linkS4class{LongData}}

\examples{
### Creation of single variable-trajectory
mat <- matrix(c(1,NA,3,2,3,6,1,8,10,1,NA,1,2,NA,3,2,3,2),6,3)
(ldSingle <- longData(mat))

### Extension to joint trajectories
(ldHyp <- longDataTo3d(ldSingle))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{package}
\keyword{cluster}
\keyword{methods}



