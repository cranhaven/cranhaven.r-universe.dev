\name{LIMEverglades}
\docType{data}
\alias{LIMEverglades}
\title{
  Linear inverse model specification for the herpetological food web
  of the Everglades
}

\description{
  Linear inverse model specification for the herpetological wet prairie
  example from the everglades.
  
  as described in Diffendorfer et al., 2001
 

  The everglades are a freshwater wetland in Florida, USA.

  The model contains 9 functional compartments and 3 external compartments,
  connected with 402 flows.

  Units of the flows are gram wet weight / Ha / year

  The linear inverse model LIMEverglades is generated from the file
  Everglades.input which can be found in subdirectory \code{/examples/FoodWeb}
  of the package directory

  In this subdirectory you will find many foodweb example input files

  These files can be read using \code{\link{Read}} and their output
  processed by \code{\link{Setup}} which will produce a linear inverse
  problem specification similar to LIMEverglades
}

\usage{
data(LIMEverglades)
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

# Cannot be solved, but the least squares solution is found
Flows <- Lsei(LIMEverglades, parsimonious = TRUE)
Everglades <- Flowmatrix(LIMEverglades)
plotweb(Everglades, main = "Everglades Herpetological Wet Prairie model",
        sub = "g WW/Ha/Yr", lab.size = 0.8)
}

\references{
  Diffendorfer, J.E., Richards, P.M., Dalrymple, G.H., DeAngelis, D.L., 2001.
  Applying Linear Programming to estimate fluxes in ecosystems or food webs:
  an example from the herpetological assemblage of the freshwater Everglades.
  Ecol. Model. 144, 99-120.
}

\seealso{
  browseURL(paste(system.file(package="LIM"), "/doc/examples/Foodweb/", sep=""))

  contains "Everglades.input", the input file; read this with \code{Setup}

  \code{\link{LIMTakapoto}}, \code{\link{LIMRigaSummer}} and many others
}

\keyword{datasets}

