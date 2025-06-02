\name{LIMScheldtIntertidal}
\docType{data}
\alias{LIMScheldtIntertidal}
\title{
  Linear inverse model specification for the Schelde Intertidal flat food web
}

\description{
  Linear inverse model specification for the Westerschelde Intertidal
  flat food web in June

  as in Van Oevelen et al. (2006).

  The Westerschelde is a highly eutrophic estuary in the Netherlands.
  
  The food web model was created for the intertidal flat called the
  "Molenplaat", site 2.

  It is the basic input model.

  The foodweb comprises 7 functional compartments and five external
  compartments, connected with 32 flows.

  Units of the flows are mg C/m2/day

  The linear inverse model LIMScheldtIntertidal is generated from the file
  \code{ScheldtIntertidal.input }
  which can be found in subdirectory \code{/examples/FoodWeb} of the
  package directory

  In this subdirectory you will find many foodweb example input files

  These files can be read using \code{\link{Read}} and their output
  processed by \code{\link{Setup}} which will produce a linear inverse
  problem specification similar to LIMScheldtIntertidal
}

\usage{
data(LIMScheldtIntertidal)
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
  Dick van Oevelen<dick.vanoevelen@nioz.nl>
}

\examples{
ScheldtIntertidal <- Flowmatrix(LIMScheldtIntertidal)
plotweb(ScheldtIntertidal, main = "Scheldt intertidal flat food web",
        sub = "mgC/m2/day")
Plotranges(LIMScheldtIntertidal, lab.cex = 0.7,
        main = "Scheldt intertidal flat food web, Flowranges")
Plotranges(LIMScheldtIntertidal, type = "V", lab.cex = 0.7,
        main = "Scheldt intertidal flat food web, Variable ranges")
}

\references{
  Van Oevelen, D., Soetaert, K., Middelburg, J.J., Herman, P.M.J., Moodley,
  L., Hamels, I., Moens, T., Heip, C.H.R., 2006b.
  Carbon flows through a benthic food web: Integrating biomass, isotope
  and tracer data. J. Mar. Res. 64, 1-30.
}

\seealso{
  browseURL(paste(system.file(package="LIM"), "/doc/examples/Foodweb/", sep=""))

  contains "ScheldtIntertidal.input", the input file; read this with \code{Setup}


  \code{\link{LIMTakapoto}}, \code{\link{LIMRigaSummer}} and many others
}

\keyword{datasets}

