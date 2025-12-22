\name{SOS}
\alias{SOS}
\alias{plotSOS}
\alias{GND}

\title{
Gives the SOS values for a given node.
}
\description{
\code{SOS} gives the SOS value for the node, used in the interpretation of node-based analysis of species distributions after running \code{Node_analysis}.  \code{plotSOS} plots the values on a map. \code{GND} gives the GND values.  
}
\usage{
SOS(nodiv_result, node)
plotSOS(nodiv_result, node, zlim, ...)
GND(nodiv_result, node = NULL)
}

\arguments{
  \item{nodiv_result}{
The result of running \code{Node_analysis}
}
  \item{node}{
The node label or node number specifying the node to plot. Optional for \code{GND}
}

  \item{zlim}{
The values to use for the extremes of the color palette
}
  \item{\dots}{
Further arguments to pass to the plotting function
}
}
\details{
The first descendant clade (i.e. the first returned by a call to \code{Descendants}) is associated with positive SOS values, the second descendant clade is associated with negative values. The plot will be done as a spatial grid or as colored points, depending on the \code{type} of the \code{nodiv_result} object. If the \code{nodiv_result} object has a \code{shape} object of type \code{raster}, this will be plotted normally in the background - but resizing the window will cause the plots/pixels and the underlying raster object to lose alignment, due to a bug in the \code{raster} package.
}
\value{
\code{SOS} returns a numerical vector with the SOS values. If \code{nodiv_result} has \code{type raster}, a raster object of the SOS values is returned by \code{plotSOS}. 
}
\references{
Borregaard, M.K., Rahbek, C., Fjeldsaa, J., Parra, J.L., Whittaker, R.J. & Graham, C.H. 2014. Node-based analysis of species distributions. Methods in Ecology and Evolution 5(11): 1225-1235. DOI: 10.1111/2041-210X.12283
}
\author{
Michael Krabbe Borregaard
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Node_analysis}}
}

