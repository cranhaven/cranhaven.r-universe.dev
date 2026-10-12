\name{heatmap.io}
\alias{heatmap.io}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Heatmap Visualization}

\description{
A visualization tool for matrices belonging to an input-output system.
}
\usage{
heatmap.io(obj, RS_label = NULL, regions_x = 'all', sectors_x = 'all', 
           regions_y = 'all', sectors_y = 'all',
           ES_x = NULL, ES_y = NULL, FUN = NULL, low = NULL, high = NULL,
           min = NA, max = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{The object you wish to create a heat map for that corresponds to the RS_label}
  \item{RS_label}{The RS_label located in an \code{InputOutput} object. See \code{\link{as.inputoutput}}}
  \item{regions_x}{The regions you wish to plot on the x-axis. This can either be the numerical order the regions occur or the name of the regions. The default is \code{'all'}.}
  \item{sectors_x}{The sectors you wish to plot on the x-axis. This can either be the numerical order the sectors occur or the name of the sectors. The default is \code{'all'}.}
  \item{regions_y}{The regions you wish to plot on the y-axis. This can either be the numerical order the regions occur or the name of the regions. The default is \code{'all'}.}
  \item{sectors_y}{The sectors you wish to plot on the y-axis. This can either be the numerical order the sectors occur or the name of the sectors. The default is \code{'all'}.}
  \item{ES_x}{Instead of specifying regions and sectors individually, you can use an \code{EasySelect} object (see \code{\link{easy.select}}). If supplied, the \code{regions} and \code{sectors} are overridden. }
  \item{ES_y}{See ES_x  }
  \item{FUN}{The transformation of the elements in \code{obj} such as \code{log()}}
  \item{low}{The color of the low values. Default is \code{"yellow"}. }
  \item{high}{The color of the high values. Default is \code{"blue"}. }
  \item{min}{The minimum value for the color legend. Default of NA == min(obj). Both min and max must be provided to change default.}
  \item{max}{The maximum value for the color legend. Default of NA == max(obj). Both min and max must be provided to change default.}
}
\details{
\code{heatmap.io} uses ggplot2::geom_tiles() to create the visualization of the object.
}
\author{John J. P. Wade}

\note{
The coloring follows the temperatures of stars!
}


\examples{
data(toy.IO)
class(toy.IO)

RS_label = toy.IO$RS_label
obj = toy.IO$L
heatmap.io(obj, RS_label, FUN = log, max = 3)

cuberoot = function(x){x^(1/3)}
heatmap.io(obj, RS_label, FUN = cuberoot)

# Total field of influence
fit = f.influence.total(toy.IO)
heatmap.io(fit, RS_label, sectors_x = c(1,3,4,5), regions_y = c(2), sectors = 1:3)

data(toy.ES)
ES2 = matrix(c(1,5,6,8,9))
class(ES2) = 'EasySelect'
heatmap.io(fit, RS_label, ES_x = toy.ES, ES_y = ES2, 
           low = '#00fcef', high = 'blueviolet')


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
