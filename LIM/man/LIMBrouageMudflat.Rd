\name{LIMBrouageMudflat}
\docType{data}
\alias{LIMBrouageMudflat}
\title{
  Linear inverse model specification for the Intertidal mudflat food web
  on the Atlantic coast of France
}

\description{
  Linear inverse model specification for the Intertidal mudflat food web
  on the Atlantic coast of France as in Leguerrier et al., 2003.

  The foodweb comprises 16 functional compartments and 3 external
  compartments, connected with 95 flows.

  Units of the flows are g C/m2/year

  The linear inverse model LIMBrouageMudflat is generated from the file
  BrouageMudflat.input which can be found in subdirectory
  \code{/examples/FoodWeb} of the package directory

  In this subdirectory you will find many foodweb example input files

  These files can be read using \code{\link{Read}} and their output
  processed by \code{\link{Setup}} which will produce a linear inverse
  problem specification similar to LIMBrouageMudflat
}

\usage{
data(LIMBrouageMudflat)
}

\format{
  a list of matrices, vectors, names and values that specify the linear
  inverse model problem.

  see the return value of \code{\link{Setup}} for more information about
  this list

  A more complete description of this structures is in vignette("LIM")
}

\author{
  Karline Soetaert <karline.soetaert@nioz.nl>
  Dick van Oevelen <dick.vanoevelen@nioz.nl>
}
\examples{
Brouage <- Flowmatrix(LIMBrouageMudflat)
plotweb(Brouage, main = "Brouage mudflat food web", sub = "gC/m2/yr")
# Some ranges are infinite ->marked with "*
Plotranges(LIMBrouageMudflat, lab.cex = 0.7, sub = "*=unbounded",
           xlab = "gC/m2/year", main = "Brouage mudflat, Flowranges")
Plotranges(LIMBrouageMudflat, type = "V", lab.cex = 0.7, 
           sub = "*=unbounded",
           xlab = "gC/m2/year",main="Brouage mudflat, Variable ranges")
}

\references{
  Leguerrier, D., Niquil, N., Boileau, N., Rzeznik, J., Sauriau, P.G.,
  Le Moine, O., Bacher, C., 2003.
  Numerical analysis of the food web of an intertidal mudflat ecosystem
  on the Atlantic coast of France. Marine Ecology Progress Series 246, 17-37.
}

\seealso{
  browseURL(paste(system.file(package="LIM"), "/doc/examples/Foodweb/", sep=""))

  contains "BrouageMudflat.input", the input file; read this with \code{Setup}


  \code{\link{LIMTakapoto}}, \code{\link{LIMRigaSummer}} and many others
}

\keyword{datasets}

