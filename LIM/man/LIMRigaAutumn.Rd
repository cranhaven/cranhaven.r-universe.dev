\name{LIMRigaAutumn}
\docType{data}
\alias{LIMRigaAutumn}
\title{
  Linear inverse model specification for the Gulf of Riga *autumn*
  planktonic food web
}

\description{
  Linear inverse model specification for the Gulf of Riga planktonic
  food web in *autumn* as in Donali et al. (1999).

  The Gulf of Riga is a highly eutrophic system in the Baltic Sea.

  The foodweb comprises 7 functional compartments and two external compartments,
  connected with 26 flows.  Units of the flows are mg C/m3/day

  The linear inverse model LIMRigaAutumn is generated from the file
  RigaAutumn.input which can be found in subdirectory \code{/examples/FoodWeb}
  of the package directory

  In this subdirectory you will find many foodweb example input files

  These files can be read using \code{\link{Read}} and their output
  processed by \code{\link{Setup}} which will produce a linear inverse
  problem specification similar to LIMRigaAutumn
}

\usage{
data(LIMRigaAutumn)
}

\format{
  a list of matrices, vectors, names and values that specify the
  linear inverse model problem.

  see the return value of \code{\link{Setup}} for more information
  about this list

  A more complete description of this structures is in vignette("LIM")
}

\author{
  Karline Soetaert <karline.soetaert@nioz.nl>

  Dick van Oevelen<dick.vanoevelen@nioz.nl>
}

\examples{
rigaAutumn <- Flowmatrix(LIMRigaAutumn)
plotweb(rigaAutumn, main = "Gulf of Riga planktonic food web, autumn",
        sub = "mgC/m3/day")
# ranges of flows
Plotranges(LIMRigaAutumn, lab.cex = 0.7, xlab = "mgC/m3/d",
        main = "Gulf of Riga planktonic food web, autumn, Flowranges")
# ranges of variables
Plotranges(LIMRigaAutumn, type="V", lab.cex = 0.7, xlab = "mgC/m3/d",
        main = "Gulf of Riga planktonic food web, autumn, variables")
}

\references{
  Donali, E., Olli, K., Heiskanen, A.S., Andersen, T., 1999. Carbon flow
  patterns in the planktonic food web of the Gulf of Riga, the Baltic Sea:
  a reconstruction by the inverse method. Journal of Marine Systems 23,
  251..268.
}

\seealso{
  browseURL(paste(system.file(package="LIM"), "/doc/examples/Foodweb/", sep=""))

  contains "RigaAutumn.input", the input file; read this with \code{Setup}

  \code{\link{LIMTakapoto}}, \code{\link{LIMRigaSummer}},
  \code{\link{LIMRigaSpring}} and many others
}

\keyword{datasets}

