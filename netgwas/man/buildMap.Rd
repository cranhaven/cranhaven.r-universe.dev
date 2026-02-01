\name{buildMap}
\alias{buildMap}

\title{ linkage group detection and ordering markers for class "netgwasmap"}

\description{
Implements different algorithms for detecting linkage groups and ordering markers in each linkage group.
}

\usage{
buildMap( res, opt.index, min.m = NULL, use.comu = FALSE)
}

\arguments{
	\item{res}{ An object with S3 class "netgwasmap"}
	\item{opt.index}{ An index of a desired regularization parameter.}
	\item{min.m}{Expected minimum number of markers in a chromosome. Optional}
	\item{use.comu}{Using community detection algorithm to detect linkage groups. Default is FALSE.}
}

\details{
This function determines linkage groups and order markers within each linkage group for class "netgwasmap".
}

\value{
An object with S3 class \code{"netgwasmap"} is returned:  
 \item{map}{ Constructed linkage map associated with \code{opt.index}.}
 \item{opt.index}{ The index of a desired 3-D map to construct linkage map.}
 \item{cross}{The specified cross type by user. }
 \item{allres}{ A list containing results for different regularization parameter. Belongs to class "netgwas". To visualize a path of different 3D maps consider function \code{\link{plot.netgwas}}. Note that the input data is reordered based on the estimated linkage map and is saved as data in this argument.}
 \item{man}{stays TRUE.}
}

\references{
1. Behrouzi, P., and Wit, E. C. (2018). De novo construction of polyploid linkage maps using discrete graphical models. Bioinformatics. \cr
2. Behrouzi, P., and Wit, E. C. (2017c). netgwas: An R Package for Network-Based Genome-Wide Association Studies. arXiv preprint, arXiv:1710.01236.
}

\author{ 
  Pariya Behrouzi and Ernst C.Wit \cr
  Maintainer: Pariya Behrouzi \email{pariya.behrouzi@gmail.com} \cr
}

\seealso{\code{\link{netmap}}}

\examples{
\dontrun{
data(CviCol)
#Randomly change the order of markers across the genome
cvicol <- CviCol[ ,sample(ncol(CviCol))]

#Constructing linkage map for Cvi x Col genotype data
out <- netmap(cvicol, cross= "inbred", ncores=1); out
plot(out)
map <- out$map; map

#Visualization of other networks
plot(out$allres)  
#Constructing a linkage map for 5th network
bm <- buildMap(out, opt.index=5); bm
plot(bm, vis= "summary")
#or
plot(bm, vis= "interactive", label.vertex="all")
}
}