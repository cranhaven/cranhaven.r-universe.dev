\name{vecplot}
\alias{vecplot}
\title{
VEC plot function (to use in conjunction with Importance function).
}
\description{
VEC plot function (to use in conjunction with Importance function).
}
\usage{
vecplot(I, graph = "VEC", leg = NULL, xval = 1, sort = FALSE, data = NULL, 
digits = c(1, 1), TC = 1, intbar = NULL, lty = 1, pch = 19, col = NULL, 
datacol = NULL, main = "", main2 = "", Grid = 0, 
xlab = "", ylab = "", zlab = "", 
levels = NULL, levels2 = NULL, showlevels = FALSE, 
screen = list(z = 40, x = -60), zoom = 1, cex = 1)
}
\arguments{
  \item{I}{the output list of the \code{\link{Importance}} function.
}
  \item{graph}{type of VEC graph:
\itemize{
         \item \code{VEC} -- 1-D VEC curve; 
         \item \code{VECB} -- 1-D VEC curve with box plots (only valid for SA methods: "DSA", "MSA");
         \item \code{VEC3} -- 2-D VEC surface;
         \item \code{VECC} -- 2-D VEC contour; 
}
}
  \item{leg}{see \code{\link{mgraph}}}
  \item{xval}{the attribute input index (e.g. 1), only used if \code{graph="VEC"} or (\code{graph="VEC3" or "VECC"} 
and \code{length(interactions)=1}, see \code{\link{Importance}}). if a vector, then several VEC curves are plotted (in this case, x-axis is scaled).}
  \item{sort}{if factor inputs are sorted:
\itemize{
         \item \code{increasing} -- sorts the first attribute (if factor) according to the response values, increasing order; 
         \item \code{decreasing} -- similar to \code{increasing} but uses reverse order; 
         \item \code{TRUE} -- similar to \code{increasing}; 
         \item \code{increasing2} -- sorts the second attribute (for \code{graph="VEC3" or "VECC"}, if factor, according to the response values), increasing order; 
         \item \code{decreasing2} -- similar to \code{increasing2} but uses reverse order; 
         \item \code{FALSE} -- no sort is used; 
}
}
  \item{data}{see \code{\link{mgraph}}}
  \item{digits}{see \code{\link{mgraph}}}
  \item{TC}{see \code{\link{mgraph}}}
  \item{intbar}{see \code{\link{mgraph}}}
  \item{lty}{see \code{\link{mgraph}}}
  \item{pch}{point type for the \code{graph="VEC"} curve, can be a vector if there are several VEC curve plots}
  \item{col}{color (e.g. "black", "grayrange", "white")}
  \item{datacol}{color of the data histogram for \code{graph="VEC"}}
  \item{main}{see \code{\link{mgraph}}}
  \item{main2}{key title for \code{graph="VECC"}}
  \item{Grid}{see \code{\link{mgraph}}}
  \item{xlab}{x-axis label}
  \item{ylab}{y-axis label}
  \item{zlab}{z-axis label}
  \item{levels}{if x1 is factor you can choose the order of the levels to this argument}
  \item{levels2}{if x2 is factor you can choose the order of the levels to this argument}
  \item{showlevels}{if you want to show the factor levels in x1 or x2 axis in \code{graph="VEC3"}:
  \itemize{
            \item \code{FALSE} or \code{TRUE} -- do not (do) show the levels in x1, x2 and z axis for factor variables;
            \item vector with 3 logical values -- if you want to show the levels in each of the x1, x2 or z axis for factor variables (e.g. \code{c(FALSE,FALSE,TRUE)} only shows for z-axis).
          }
                   }
  \item{screen}{select the perspective angle of the \code{VEC3} graph:
  \itemize{
           \item \code{x} -- assumes \code{list(z=0,x=-90,y=0)};
           \item \code{X} -- assumes \code{list(x=-75)};
           \item \code{y} -- assumes \code{list(z=0,x=-90,y=-90)};
           \item \code{Y} -- assumes \code{list(z=10,x=-90,y=-90)};
           \item \code{z} -- assumes \code{list(z=0,x=0,y=0)};
           \item \code{xy} -- assumes \code{list(z=10,x=-90,y=-45)};
           \item else you need to specify a list with z, x an y angles, see \code{\link[lattice]{wireframe}}
          }
}
  \item{zoom}{zoom of the wireframe (\code{graph="VEC3"})}
  \item{cex}{label font size}
}
\details{
For examples and references check: \code{\link{Importance}}
}
\value{
A VEC curve/surface/contour plot.
}
\author{
Paulo Cortez \url{http://www3.dsi.uminho.pt/pcortez/}
}
\seealso{
\code{\link{Importance}}
}
\references{
\itemize{
\item To cite the Importance function or sensitivity analysis method, please use:\cr
\cr
P. Cortez and M.J. Embrechts.\cr
Using Sensitivity Analysis and Visualization Techniques to Open Black Box Data Mining Models.\cr
In Information Sciences, Elsevier, 225:1-17, March 2013.\cr
\cr
\doi{10.1016/j.ins.2012.10.039}\cr

}
}


\keyword{aplot}
\keyword{classif}
\keyword{regression}
\keyword{neural}
\keyword{nonlinear}
