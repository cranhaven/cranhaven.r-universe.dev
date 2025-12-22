\name{nodiv-package}
\alias{nodiv-package}
\alias{nodiv}
\docType{package}
\title{
nodiv - Node-based analysis of species distributions
}
\description{
The package implements Borregaard et al. (2014) method for identifying nodes in a phylogeny associated with divergent distributions. The main algorithm goes through each node in the phylogeny and relates node overlap to a null model. The package also provides functions for preparing the data sets, for exploratory plots and further analysis.
}
\details{
The workflow starts by creating a \code{nodiv_data} data object, which makes sure that the phylogeny, the species distributions and the spatial coordinates of sites are matched correctly. The function takes a number of data types. There are several functions for exploratory data analysis, including \code{plot}, \code{summary}, \code{richness}, \code{Node_occupancy}, \code{Node_size} etc. The core of the package are the functions \code{Nodesig}, which compares the distributions of the two clades descending from a node, and \code{Node_analysis}, which applies this function to all nodes in the phylogeny and summarizes the results as a \code{nodiv_result} object. There is a set of functions for interpretation of the results, including \code{plot}, \code{plotSOS}, and \code{summary}. The package also provides basic functions for plotting and manipulating data sets that combine spatial distributions with phylogenies, e.g. \code{subsample}, \code{plot_grid} and \code{plot_points}.  
Note that the package may still be unstable - if you experience problems, try cloning the github repository instead: \code{library(devtools); install_github("mkborregaard/nodiv")}, or feel free to contact the maintainer.
}
\author{
Michael Krabbe Borregaard

Maintainer: Michael Krabbe Borregaard <mkborregaard@sund.ku.dk>
}
\references{
Borregaard, M.K., Rahbek, C., Fjeldsaa, J., Parra, J.L., Whittaker, R.J. & Graham, C.H. 2014. Node-based analysis of species distributions. Methods in Ecology and Evolution 5(11): 1225-1235. <DOI:10.1111/2041-210X.12283>
}

\keyword{ package }

\examples{
data(coquettes)
res <- Node_analysis(coquettes, 50, "rdtable")
opar <- par(mfrow = c(1,2))
plot(res)
plotSOS(res, 28, shapefill = terrain.colors(64))
summary(res)
par(opar)
}
