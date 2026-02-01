\name{netsnp}
\alias{netsnp}

\title{Reconstructs intra- and inter- chromosomal conditional interactions among genetic loci}

\description{
This is one of the main functions of the \pkg{netgwas} package. This function can be used to reconstruct the intra- and inter-chromosomal interactions among genetic loci in diploids and polyploids. The input data can be belong to any biparental genotype data which contains at least two genotype states. Two methods are available to reconstruct the network, namely (1) approximation method, and (2) gibbs sampling within the Gaussian copula graphical model. Both methods are able to deal with missing genotypes.
}

\usage{
netsnp(data, method = "gibbs", rho = NULL, n.rho = NULL, rho.ratio = NULL, 
		ncores = 1, em.iter = 5, em.tol = .001, verbose = TRUE)
}

\arguments{
	\item{data}{ An (\eqn{n \times p}) matrix or a \code{data.frame} corresponding to a genotype data matrix (\eqn{n} is the sample size and \eqn{p} is the number of variables). It also could be an object of class \code{"simgeno"}. Input data can contain missing values.}
	\item{method}{ Reconstructs intra- and inter- chromosomal conditional interactions (epistatic selection) network with three methods: "gibbs", "approx", and "npn". For a medium (~500) and a large number of variables we would recommend to choose "gibbs" and "approx", respectively. For a very large number of variables (> 2000) choose "npn". The default method is "gibbs".}
	\item{rho}{ A decreasing sequence of non-negative numbers that control the sparsity level. Leaving the input as \code{rho = NULL}, the program automatically computes a sequence of \code{rho} based on \code{n.rho} and \code{rho.ratio}. Users can also supply a decreasing sequence values to override this.}
	\item{n.rho}{ The number of regularization parameters. The default value is \code{10}.}
	\item{rho.ratio}{Determines the distance between the elements of \code{rho} sequence. A small value of \code{rho.ratio} results in a large distance between the elements of \code{rho} sequence. And a large value of \code{rho.ratio} results into a small distance between elements of \code{rho}. If keep it as NULL the program internally chooses a value.}
	\item{ncores}{ The number of cores to use for the calculations. Using \code{ncores = "all"} automatically detects number of available cores and runs the computations in parallel on (available cores - 1).}
	\item{em.iter}{ The number of EM iterations. The default value is 5.}
	\item{em.tol}{ A criteria to stop the EM iterations. The default value is .001. }
	\item{verbose}{Providing a detail message for tracing output. The default value is \code{TRUE}.}
}

\details{
Viability is a phenotype that can be considered. This function detects the conditional dependent short- and long-range linkage disequilibrium structure of genomes and thus reveals aberrant marker-marker associations that are due to epistatic selection.
This function can be used to estimate conditional independence relationships between partially observed data that not follow Gaussianity assumption (e.g. continuous non-Gaussian, discrete, or mixed dataset).
}

\value{
An object with S3 class \code{"netgwas"} is returned:  
 \item{Theta}{ A list of estimated p by p precision matrices that show the conditional independence relationships patterns among genetic loci.}
 \item{path}{ A list of estimated p by p adjacency matrices. This is the graph path corresponding to \code{Theta}.}
 \item{ES}{ A list of estimated p by p conditional expectation corresponding to \code{rho}.}
 \item{Z}{ A list of n by p transformed data based on Gaussian copula. }
 \item{rho}{ A \code{n.rho} dimensional vector containing the penalty terms.}
 \item{loglik }{ A \code{n.rho} dimensional vector containing the maximized log-likelihood values along the graph path. }
 \item{data}{ The \eqn{n} by \eqn{p} input data matrix. }
}

\note{ This function estimates a graph path . To select an optimal graph please refer to \code{\link{selectnet}}.}

\author{ 
  Pariya Behrouzi and Ernst C. Wit \cr
  Maintainers: Pariya Behrouzi \email{pariya.behrouzi@gmail.com}\cr
}

\references{
1. Behrouzi, P., and Wit, E. C. (2019). Detecting epistatic selection with partially observed genotype data by using copula graphical models. Journal of the Royal Statistical Society: Series C (Applied Statistics), 68(1), 141-160. \cr
2. Behrouzi, P., and Wit, E. C. (2017c). netgwas: An R Package for Network-Based Genome-Wide Association Studies. arXiv preprint, arXiv:1710.01236.
3. D. Witten and J. Friedman. New insights and faster computations for the graphical lasso. \emph{Journal of Computational and Graphical Statistics}, to appear, 2011. \cr
4. Guo, Jian, et al. "Graphical models for ordinal data." Journal of Computational and Graphical Statistics 24.1 (2015): 183-204. \cr
}

\seealso{\code{\link{selectnet}}}

\examples{
    \dontshow{
		D <- simgeno(p=20, n=10, k= 3, adjacent = 1, alpha = 0.06 , beta = 0.3)
		out <- netsnp(data = D$data, n.rho= 2, ncores= 1)
		plot(out)
		sel <- selectnet(out)
       }
	\donttest{
		data(CviCol)
		out <- netsnp(CviCol); out
		plot(out)
		
		#select optimal graph
		epi <- selectnet(out)
		plot(epi, vis="CI", xlab="markers", ylab="markers", 
		    n.mem = c(24,14,17,16,19), vertex.size=4)
		    
		#Visualize interactive plot of the selected network
		#Different colors for each chromosome
		cl <- c(rep("red", 24), rep("white",14), rep("tan1",17), 
		      rep("gray",16), rep("lightblue2",19))
		plot(epi, vis="interactive", vertex.color= cl)
		
		#Partial correlations between markers on genome
		image(as.matrix(epi$par.cor), xlab="markers", ylab="markers", sub="")
	}	
}