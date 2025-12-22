\name{Node_analysis}
\alias{Node_analysis}
\alias{nodiv_result}
\alias{print.nodiv_result}
\alias{summary.nodiv_result}
\alias{print.summary_nodiv_result}
\alias{plot.nodiv_result}


\title{
Calculate GND and SOS scores for a phylogeny and community data set
}
\description{
Goes through each node in the phylogeny, and compares the distribution overlap of sister clades to that expected from a null mode that considers node size, species richness and range size.
}
\usage{
Node_analysis(nodiv_data, repeats = 100, method = c("rdtable", "quasiswap"), 
                cores = 1, log_parallel_progress = FALSE)
\method{summary}{nodiv_result}(object, ...)
\method{print}{nodiv_result}(x, printlen = 4, ...)
\method{plot}{nodiv_result}(x, label = nodenumbers(x), zlim = 0:1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nodiv_data}{
An object created by the \code{nodiv_data} function
}
  \item{repeats}{
Number of null communities to create at each node
}
  \item{method}{
The null model algorithm used to simulate random communities
}
  \item{cores}{
The number of cores to use in the computation. Note that multi-core processing is yet unimplemented.
}
  \item{log_parallel_progress}{
If this is TRUE, the results from each node will be saved as an '.rda' file in the working directory. Useful for large parallel computations.
}
  \item{object}{
The \code{nodiv_result} object
}
  \item{x}{
The \code{nodiv_result} object
}
  \item{printlen}{
The number of species to print to screen
}
  \item{label}{
The text to use for labelling nodes in the plot
}
  \item{zlim}{
The values of GND to use for the extremes of the color scale on the plot
}
  \item{\dots}{
Further arguments to be passed to the function
}
}
\details{
The "rdtable" algorithm is efficient, but may underestimate the deviation between nodes. It should primarily be used for exploratory data analysis. Note that the more robust "quasiswap" algorithm may take days to run on larger data sets.

The \code{plot} function for \code{nodiv_result} plots the phylogenetic tree with colored symbols at the nodes. The color and size the nodes are proportional with the GND value of the node.
}
\value{
The result is an object of type \code{nodiv_result}, which has the following components:
 \item{GND}{A numeric vector between 0 and 1 describing the distributional divergence associated with a node}
 \item{SOS}{A numeric matrix given the SOS of values, which expresses the difference between empirical and expected species richness of each clade at each site}
 It also includes all the elements of the \code{nodiv_data} object:
 \item{coords}{A \code{SpatialPointsDataFrame} or \code{SpatialPixelsDataFrame} (from \code{sp}) with spatial coordinates, and one column named \code{sites} containing the site names}
 \item{comm}{The community matrix}
 \item{type}{A character vector describing the type of data}
 \item{species}{A list of species names}
 \item{shape}{An optional slot detailing a shapefile object for plotting the data}
 \item{phylo}{A phylogeny of type \code{phylo} from package \code{ape}} 

}
\references{
Borregaard, M.K., Rahbek, C., Fjeldsaa, J., Parra, J.L., Whittaker, R.J. & Graham, C.H. 2014. Node-based analysis of species distributions. Methods in Ecology and Evolution 5(11): 1225-1235. DOI: 10.1111/2041-210X.12283
}
\author{
Michael Krabbe Borregaard
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{plotSOS}}
}
\examples{
data(coquettes)
res <- Node_analysis(coquettes, 50, "rdtable")
opar <- par(mfrow = c(1,2))
plot(res)
plotSOS(res, 28, shapefill = terrain.colors(64))
summary(res)
par(opar)
}

