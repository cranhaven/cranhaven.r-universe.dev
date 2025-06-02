\name{LIMCaliforniaSediment}
\docType{data}
\alias{LIMCaliforniaSediment}
\title{
  Linear inverse model specification for the Santa Monica Basin
  sediment food web
}
\description{
  Linear inverse model specification for the Santa Monica Basin
  (California) sediment food web  as in Eldridge and Jackson (1993).

  The Santa Monica Basin is a hypoxic-anoxic basin located near California.

  The model contains both chemical and biological species.

  The foodweb comprises 7 functional compartments and five external
  compartments, connected with 32 flows.

  Units of the flows are mg /m2/day

  The linear inverse model LIMCaliforniaSediment is generated from the file
  \file{CaliforniaSediment.input} which can be found in
  subdirectory \code{/examples/FoodWeb} of the package directory

  In this subdirectory you will find many foodweb example input files

  These files can be read using \code{\link{Read}} and their output
  processed by \code{\link{Setup}} which will produce a linear inverse
  problem specification similar to LIMCaliforniaSediment
}

\usage{
data(LIMCaliforniaSediment)
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
CaliforniaSediment <- Flowmatrix(LIMCaliforniaSediment)
plotweb(CaliforniaSediment, main = "Santa Monica Basin Benthic web",
        sub = "mgN/m2/day", lab.size = 0.8)
\dontrun{
xr <- LIMCaliforniaSediment$NUnknowns
i1 <- 1:(xr/2)
i2 <- (xr/2+1):xr
Plotranges(LIMCaliforniaSediment, index = i1, lab.cex = 0.7,
        sub = "*=unbounded",
        main = "Santa Monica Basin Benthic web, Flowranges - part1")
Plotranges(LIMCaliforniaSediment, index = i2, lab.cex = 0.7,
        sub = "*=unbounded",
        main = "Santa Monica Basin Benthic web, Flowranges - part2")}
}

\references{
  Eldridge, P.M., Jackson, G.A., 1993. Benthic trophic dynamics in
  California coastal basin and continental slope communities inferred
  using inverse analysis. Marine Ecology Progress Series 99, 115-135.
}

\seealso{
  browseURL(paste(system.file(package="LIM"), "/doc/examples/Foodweb/", sep=""))

  contains "CaliforniaSediment.input", the input file; read this with \code{Setup}


  \code{\link{LIMTakapoto}}, \code{\link{LIMRigaSummer}} and many others
}

\keyword{datasets}

