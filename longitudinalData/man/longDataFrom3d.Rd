\name{longDataFrom3d}
\alias{longDataFrom3d}
\title{ ~ Function: longDataFrom3d ~}
\description{
Extract a single variable-trajectory from an object
\code{\linkS4class{LongData}} that contain some joint-trajectories.
}
\usage{
longDataFrom3d(xLongData3d,variable)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xLongData3d}{\code{[LongData3d]}: structure containning some joint-trajectories.}
  \item{variable}{\code{[character]}: either the name of one of the
    variable of \code{xLongData3d}, or its number.}
}

\details{
  Extract a single variable-trajectory from an object
  \code{\linkS4class{LongData3d}} that contain some join-trajectories.
}
\value{
  An object of class \code{\linkS4class{LongData}}.
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
### Creation of joint-trajectories
mat <- array(c(1,NA,3,2,3,6,1,8,10,1,NA,1,2,NA,3,2,3,2),dim=c(3,3,2))
(ldJoint <- longData3d(mat,varNames=c("Hyp","Som")))

### Extraction of the first variable-trajectory
(ldHyp <- longDataFrom3d(ldJoint,variable="Hyp"))

### Extraction of the second variable-trajectory
(ldSom <- longDataFrom3d(ldJoint,variable="Som"))

### Extraction of the second variable-trajectory, using number
(ldSom <- longDataFrom3d(ldJoint,variable=2))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{package}
\keyword{cluster}
\keyword{methods}



