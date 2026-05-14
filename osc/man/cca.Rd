\name{cca}
\alias{cca}
\alias{cca.single}
\title{
City Clustering Algorithm (CCA)
}
\description{
CCA is initialized by selecting an arbitrary populated cell which is burnt. Then, the populated neighbors are also burnt. The algorithm keeps growing the cluster by iteratively burning neighbors of the burnt cells until there are no further populated neighboring cells. Next, another unburned populated cell is picked and the procedure is repeated until all populated cells are assigned to a cluster.

The City Clustering Algorithm (CCA) is based on the burning algorithm [1] and was first introduced in the context of cities [2]. Among other things, it was also used for a global urban percolation study [3].
}
\usage{
cca(data, s=1, mode=3, count.cells=FALSE, 
    count.max=ncol(data)*3,  
    res.x=NULL, res.y=NULL, cell.class=1,
    unit="", compare="")
cca.single(data, s, x,y, mode = 3)
}
\arguments{
  \item{data}{
 data to be clustered. This can be either a raster, a matrix or a data.frame. See details.
}
  \item{s}{
 The radius/shell size of the burning procedure (i.e. how tolerant to small gaps the algorithm is). The unit is 'number of cells' if data is a numeric matrix and 'meters' if data is a raster or data.frame.
}
  \item{x}{
 The starting position in x-direction. Only used if data is a numeric matrix.
}
  \item{y}{
 The starting position in y-direction. Only used if data is a numeric matrix.
}
  \item{mode}{
The algorithm for a non-georeferenced matrix comes in three versions which affect which close cells are 
included to the considered cluster:
(mode=1) nearest neighbors
(mode=2) cells within a shell (i.e. squares of certain size)
(mode=3) cells within a radius
Whereas (mode=1) is equivalent to (mode=3) with r=1 and 
(mode=2) with r=1 is equivalent to (mode=3) with r=2.
 Only used if data is a numeric matrix.
}
  \item{count.cells}{
  Set this option TRUE, if you want to know the number of cells which belongs to each cluster.
 Only used if data is a numeric matrix.
}
  \item{count.max}{
  This defines the maximum number of clusters, It is set per default to ncol*3.
 Only used if data is a numeric matrix.
}
  \item{res.y}{
  The resolution of the data-set expressed as the distance between two cell centers in degrees (geographical coordinate system). Only needed if data is a data.frame and unit="m".
}
  \item{res.x}{
 X-resolution. Only needed if data is a data.frame and unit="m".
}
  \item{cell.class}{
  Only required if data is a raster. Specify which cell class (eg. land use type) will be clustered. Can be an integer or a vector and can be combined with the compare option.
}
 \item{unit}{
  If unit = "m" (meter) the cluster algorithm will be done for a cluster distance in 'meter'. Otherwise, the clustering is done in the degrees. If you want to do a pixel-wise clustering, then choose the resolution as cluster distance.
}
 \item{compare}{
  If compare = "g" then cells greater than the given cell.class will be chosen. If compare = "s" cells smaller then the cell.class will be chosen.
}

}
\details{
cca is implemented in two versions, depending on the format of the data. For numerical matrices, a matrix based version is called. For raster and data.frame based data, a list based version is used, which is faster for sparse matrices and large cluster distances. (See vignette, section: Comparison - matrix vs list)

For matrix:

The matrix is a simple numerical matrix. A value equal 0 or smaller is treated as an unimportant cell, a value above 0 is treated as a cell of interest. Clusters of connected cells are identified.

For raster:

A sub-function will be called to extract the coordinates of a given cell type (cell.class). Also, a threshold determining which cells can be burnt is possible by using compare = "g" (eg. minimum population to consider a cell as populated) Following steps; see data.frame.


For data.frame:

A data frame with two columns specifying the longitude (first column) and latitude (second column) coordinates. The algorithm identifies all points with a distance to each other smaller than the cluster distance s. If unit="m" the orthodromic distance, otherwise the Euclidean distance will be used.


}
\value{
For matrix:

A matrix that defines for each cell to which cluster it belongs.

For raster / data.frame:

List with two entries - 1. data frame with longitude and latitude coordinates of the cells and the cluster_id. and 2. a vector giving the size of the cluster in units of the primitive cell. The first number is the size of the cluster with cluster_id 1, second the size of the cluster with cluster_id 2, and so on.

}
\references{
[1] Stauffer, D., & Aharony, A. (2014). Introduction to percolation theory. Taylor & Francis.

[2] Rozenfeld, H. D., Rybski, D., Andrade, J. S., Batty, M., Stanley, H. E., & Makse, H. A. (2008). Laws of population growth. Proceedings of the National Academy of Sciences, 105(48), 18702-18707.

[3] Fluschnik, T., Kriewald, S., Garcia Cantu Ros, A., Zhou, B., Reusser, D., Kropp, J., & Rybski, D. (2016). The size distribution, scaling properties and spatial organization of urban clusters: a global and regional percolation perspective. ISPRS International Journal of Geo-Information, 5(7), 110.
}
\author{
Steffen Kriewald, Till Fluschnik, Dominik Reusser,  Diego Rybski
}


\examples{

# for a matrix
data(population)
image(population)

clusters <- cca(population, s=5)
cols <- c("white",rep(rainbow(10), length.out=length(table(clusters))) )
image(clusters, col=cols, xlab="", ylab="")

one.cluster <- cca.single(population, s=1, x=125, y=125)
image(one.cluster, col=cols, xlab="", ylab="")

# for a raster-object
data(landcover)

# clustering urban areas
urban <- cca(landcover, cell.class=1,s=2000, unit="m")
str(urban)

# plot the result    
result <- landcover*NA
result[cellFromXY(result,urban$cluster[,c("long","lat")])]<-urban$cluster[,"cluster_id"]*(-1)
plot(result, col=rainbow(9))

}
\keyword{ utils }
