\name{netmap}
\alias{netmap}

\title{Constructing linkage map for diploids and polyploids}

\description{
This is one of the main functions of \pkg{netgwas} package. This function reconstructs linkage maps for biparental diploid and polyploid organisms using three methods.
}

\usage{
netmap(data, method = "npn", cross= NULL, rho = NULL, n.rho = NULL, 
      rho.ratio = NULL, min.m= NULL, use.comu= FALSE, ncores = "all",
		  em.iter = 5, verbose = TRUE)
}

\arguments{
	\item{data}{ An (\eqn{n \times p}) matrix or a \code{data.frame} corresponding to a genotype data matrix (\eqn{n} is the sample size and \eqn{p} is the number of variables). Input data can contain missing values.}
	\item{method}{Three available methods to construct linkage map: "gibbs", "approx", and "npn". Default is "npn"}
	\item{rho}{ A decreasing sequence of non-negative numbers that control the sparsity level. Leaving the input as \code{rho = NULL}, the program automatically computes a sequence of \code{rho} based on \code{n.rho} and \code{rho.ratio}. Users can also supply a decreasing sequence values to override this.}
	\item{n.rho}{ The number of regularization parameters. The default value is \code{6}.}
	\item{rho.ratio}{ Determines distance between the elements of \code{rho} sequence. A small value of \code{rho.ratio} results in a large distance between the elements of \code{rho} sequence. And a large value of \code{rho.ratio} results into a small distance between elements of \code{rho}. If keep it as NULL the program internally chooses a value.}
	\item{cross}{To be specified either "inbred" or "outbred".}
	\item{min.m}{ Expected minimum number of markers in a chromosome. Optional}
	\item{use.comu}{Use community detection algorithm to detect linkage groups. Default is FALSE.}
	\item{ncores}{ The number of cores to use for the calculations. Using \code{ncores = "all"} automatically detects number of available cores and runs the computations in parallel on (available cores - 1).}
	\item{em.iter}{The number of EM iterations. The default value is 5.}
	\item{verbose}{Providing a detail message for tracing output. The default value is \code{TRUE}.}
}

\details{
Constructing linkage maps for diploid and polyploid organisms. Diploid organisms contain two sets of chromosomes, one from each parent, whereas polyploids contain more than two sets of chromosomes. Inbreeding is mating between two parental lines where they have recent common biological ancestors. If they have no common ancestors up to roughly e.g. 4-6 generations, this is called outcrossing. In both cases the genomes of the derived progenies are random mosaics of the genome of the parents. However, in the case of inbreeding parental alleles are distinguishable in the genome of the progeny; in outcrossing this does not hold.
}

\value{
An object with S3 class \code{"netgwasmap"} is returned:  
 \item{map}{Constructed linkage map.}
 \item{opt.index}{ The index of selected graph using model selection.}
 \item{cross}{The pre-specified cross type. }
 \item{allres}{ A list containing results for different regularization parameter. Belongs to class "netgwas". To visualize a path of different 3D maps consider function \code{\link{plot.netgwas}}. Note that the input data is reordered based on the estimated linkage map and is saved as data in this argument.}
\item{man}{Stays FALSE.}
}

\author{ 
  Pariya Behrouzi and Ernst C. Wit \cr
  Maintainers: Pariya Behrouzi \email{pariya.behrouzi@gmail.com}\cr
}

\references{
1. Behrouzi, P., and Wit, E. C. (2018). De novo construction of polyploid linkage maps using discrete graphical models. Bioinformatics. \cr
2. Behrouzi, Pariya, and Ernst C. Wit. "netgwas: An R Package for Network-Based Genome-Wide Association Studies." arXiv preprint arXiv:1710.01236 (2017). \cr
3. Guo, Jian, Elizaveta Levina, George Michailidis, and Ji Zhu. "Graphical models for ordinal data." Journal of Computational and Graphical Statistics 24, no. 1 (2015): 183-204. \cr
4. Liu, Han, Fang Han, Ming Yuan, John Lafferty, and Larry Wasserman. "High-dimensional semiparametric Gaussian copula graphical models." The Annals of Statistics 40, no. 4 (2012): 2293-2326. \cr 
5. Witten, Daniela M., Jerome H. Friedman, and Noah Simon. "New insights and faster computations for the graphical lasso." Journal of Computational and Graphical Statistics 20, no. 4 (2011): 892-900.
}

\examples{
\dontrun{
data(CviCol)
#Randomly change the order of markers across the genome
cvicol <- CviCol[ ,sample(ncol(CviCol))]
 
#Constructing linkage map using gibbs method
out <- netmap(cvicol, cross= "inbred", ncores=1); out
#Estimated linkage map
map <- out$map; map
#Plot the associated network
plot(out)
#Visualizing the path networks
plot(out$allres)
#Build a linkage map for 5th networks
bm <- buildMap(out, opt.index=5); bm
####################

#Constructing linkage map using approx method
out2 <- netmap(cvicol, method="approx", cross= "inbred", ncores=1); out2
#Estimated linkage map
map2 <- out2$map; map2
#Plot the related network
plot(out2)
#Visualize other networks
plot(out2$allres)
#Build a linkage map for 5th network
bm2 <- buildMap(out2, opt.index=5); bm2

#Constructing linkage map using npn method
out3 <- netmap(cvicol, method="npn", cross= "inbred", ncores=1); out3
#Estimated linkage map
map3 <- out3$map; map3
#Plot the related network
plot(out3)
}
}