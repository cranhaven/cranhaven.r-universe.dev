\name{tess2spat}
\alias{tess2spat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert Voronoi tessellation tiles to a shapefile.
}
\description{
Convert Voronoi tessellation tiles to a shapefile.
}
\usage{
tess2spat(obj, idvec=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
Voronoi tessellation object created through function \code{deldir}.
}
  \item{idvec}{
Optional vector of ids for output shapefile polygons.
}
}

\value{
Object of class \code{SpatialPolygons}.
}
\author{
	Samuel Ackerman
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{

library(deldir)
library(sp)

vortess <- deldir(x=runif(8), y=runif(8), plotit=FALSE, suppressMsge=TRUE)
old_pars <- par(mfcol=par()$mfcol)

par(mfcol=c(1,2))
deldir::plot.deldir(vortess, wlines="tess", xlim=c(0,1), ylim=c(0,1))
vortess_shape <- tess2spat(obj=vortess)
plot(vortess_shape)
par(old_pars)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ spatial }% use one of  RShowDoc("KEYWORDS")