\name{distrib_data}
\alias{distrib_data}
\alias{nodiv_data}
\alias{plot.distrib_data}
\alias{plot.nodiv_data}
\alias{print.distrib_data}
\alias{print.nodiv_data}
\alias{summary.distrib_data}
\alias{identify.distrib_data}
\alias{summary.nodiv_data}

\title{
Data objects for node-based analysis
}
\description{
Functions to collect and display data with matching species distributions and phylogenies
}
\usage{
distrib_data(commatrix, coords = NULL, proj4string_in = CRS(as.character(NA)), 
  type = c("auto", "grid", "points"), shape = NULL)
nodiv_data(phylo, commatrix, coords, proj4string_in = CRS(as.character(NA)), 
  type = c("auto", "grid", "points"), shape = NULL)
\method{summary}{distrib_data}(object, ...)
\method{summary}{nodiv_data}(object, ...)
\method{plot}{distrib_data}(x, ...)
\method{plot}{nodiv_data}(x,  ...)
\method{identify}{distrib_data}(x,  ...)
}

\arguments{
  \item{phylo}{
A phylogeny in ape's phylo format, with tip.labels that correspond to the species names in commatrix
}
  \item{commatrix}{
A community matrix or data.frame of species distributions, with species as columns, sites as rows, and 0/1 denoting the absence or presence of a species in a given site. Alternatively, the functions accept a \code{data.frame} in the phylocom format, i.e. with three columns containing plot id, abundance (ignored) and species id. The \code{nodiv_data} function also accepts an object of type \code{distrib_data}, in which case \code{coords} is ignored. The \code{nodiv_data} function matches the species in the matrix to the tips of \code{phylo}.
}
  \item{coords}{
The coordinates of all sites in the data. The input can be of a variety of different formats. Can be a \code{data.frame} or \code{matrix} with two columns. If more columns are present, the function will search for typical coordinate names such as X/Y or Lon/Lat. Alternatively, an object inheriting from \code{SpatialPoints} from package \code{sp} is accepted. The coordinates are transformed to \code{SpatialPoints} using the proj4string argument. Site names can be given as a third column of the data.frame, as rownames for the matrix or omitted. If site names are given, and \code{commatrix} has row.names, these are matched. 
}
  \item{proj4string_in}{
Projection string as defined in package \code{sp} 
}
  \item{type}{
A descriptor of whether the sites are point (community) sites, or the centres of regular grid cells. If omitted or set to \code{auto}, the function will guess the type from \code{coords}.
}
  \item{shape}{
An optional shapefile to overlay plots upon.
}
 \item{object}{
An object of type distrib_data or nodiv_data
}
 \item{x}{
An object of type distrib_data or nodiv_data
}
 \item{\dots}{
Further arguments to be passed to the plotting function
}
}
\details{
When making an object of type 'grid', the coordinates will be stored as \code{SpatialPixels} from package \code{sp}. This may result in a warning if there are rows or columns in the grid without data. The function removes these. This may affect plotting, but can otherwise be ignored.
}
\value{
The \code{distrib_data} function produces an object of type \code{distrib_data}, which has the following components:
 \item{coords}{A \code{SpatialPointsDataFrame} or \code{SpatialPixelsDataFrame} (from \code{sp}) with spatial coordinates, and one column named \code{sites} containing the site names}
 \item{comm}{The community matrix}
 \item{type}{A character vector describing the type of data}
 \item{species}{A list of species names}
 \item{shape}{An optional slot detailing a shapefile object for plotting the data}
 The \code{nodiv_data} function produces an object of type \code{nodiv_data}, which contains the additional component:
 \item{phylo}{A phylogeny of type \code{phylo} from package \code{ape}} 

}

\author{
Michael Krabbe Borregaard
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{Node_analysis}}
}

