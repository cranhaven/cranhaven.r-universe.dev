\name{LIMRigaSpring}
\docType{data}
\alias{LIMRigaSpring}
\title{
  Linear inverse model specification for the Gulf of Riga *spring*
  planktonic food web.
}
\description{
  Linear inverse model specification for the Gulf of Riga planktonic
  food web in *spring* as in Donali et al. (1999).

  The Gulf of Riga is a highly eutrophic system in the Baltic Sea.

  The foodweb comprises 7 functional compartments and two external
  compartments, connected with 26 flows.

  Units of the flows are mg C/m3/day

  The linear inverse model LIMRigaSpring is generated from the file
  RigaSpring.input which can be found in subdirectory \code{/examples/FoodWeb}
  of the package directory

  In this subdirectory you will find many foodweb example input files

  These files can be read using \code{\link{Read}} and their output
  processed by \code{\link{Setup}} which will produce a linear inverse
  problem specification similar to LIMRigaSpring.
}

\usage{
data(LIMRigaSpring)
}

\format{
  a list of matrices, vectors, names and values that specify the linear
  inverse model problem.

  see the return value of \code{\link{Setup}} for more information
  about this list

  A more complete description of this structures is in vignette("LIM")
}

\author{
  Karline Soetaert <karline.soetaert@nioz.nl>

  Dick van Oevelen<dick.vanoevelen@nioz.nl>
}

\examples{
rigaSpring <- Flowmatrix(LIMRigaSpring)
plotweb(rigaSpring, main = "Gulf of Riga planktonic food web, spring",
        sub = "mgC/m3/day")
Plotranges(LIMRigaSpring, lab.cex = 0.7,
        main = "Gulf of Riga planktonic food web, spring, Flowranges")
Plotranges(LIMRigaSpring, type = "V", lab.cex = 0.7,
        main = "Gulf of Riga planktonic food web, spring, Variable ranges")
}

\references{
  Donali, E., Olli, K., Heiskanen, A.S., Andersen, T., 1999. Carbon flow
  patterns in the planktonic food web of the Gulf of Riga, the Baltic Sea:
  a reconstruction by the inverse method. Journal of Marine Systems 23,
  251..268.
}
\seealso{
  browseURL(paste(system.file(package="LIM"), "/doc/examples/Foodweb/", sep=""))

  contains "RigaSpring.input", the input file; read this with \code{Setup}

  \code{\link{LIMTakapoto}},\code{\link{LIMRigaAutumn}},
  \code{\link{LIMRigaAutumn}} and many others
}
\keyword{datasets}

