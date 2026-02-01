\name{netphenogeno}
\alias{netphenogeno}

\title{Reconstructs conditional dependence network among genetic loci and phenotypes}

\description{
This is one of the main functions of the \pkg{netgwas} package. This function reconstructs a conditional independence network between genotypes and phenotypes for diploids and polyploids. Three methods are available to reconstruct networks, namely (i) Gibbs sampling, (ii) approximation method, and (iii) nonparanormal approach within the Gaussian copula graphical model. The first two methods are able to deal with missing genotypes. The last one is computationally faster.
}

\usage{
netphenogeno(data, method = "gibbs", rho = NULL, n.rho = NULL, rho.ratio = NULL,
		ncores = 1, em.iter = 5, em.tol=.001, verbose = TRUE)
}

\arguments{
	\item{data}{ An (\eqn{n \times p}) matrix or a \code{data.frame} corresponding to the data matrix (\eqn{n} is the sample size and \eqn{p} is the number of variables). The \eqn{p} columns include either a marker or trait(s) information. Input data can contain missing values.}
	\item{method}{Reconstructing both genotype-phenotype interactions network and genotype-phenotype-environment interactions network with three methods: "gibbs", "approx", and "npn". For a medium (~500) and a large number of variables we recommend to choose "gibbs" and "approx", respectively. Choosing "npn" for a very large number of variables (> 2000) is computationally efficient. The default method is "gibbs".}
	\item{rho}{ A decreasing sequence of non-negative numbers that control the sparsity level. Leaving the input as \code{rho = NULL}, the program automatically computes a sequence of \code{rho} based on \code{n.rho} and \code{rho.ratio}. Users can also supply a decreasing sequence values to override this.}
	\item{n.rho}{ The number of regularization parameters. The default value is \code{10}.}
	\item{rho.ratio}{ Determines distance between the elements of \code{rho} sequence. A small value of \code{rho.ratio} results in a large distance between the elements of \code{rho} sequence. And a large value of \code{rho.ratio} results into a small distance between elements of \code{rho}. The default value is \code{0.3}.}
	\item{ncores}{ The number of cores to use for the calculations. Using \code{ncores = "all"} automatically detects number of available cores and runs the computations in parallel on (available cores - 1).}
	\item{em.iter}{ The number of EM iterations. The default value is 5.}
	\item{em.tol}{ A criteria to stop the EM iterations. The default value is .001. }
	\item{verbose}{Providing a detail message for tracing output. The default value is \code{TRUE}.}
}

\details{
This function reconstructs both genotype-phenotype network and genotype-phenotype-environment interactions network. In genotype-phenotype networks nodes are either markers or phenotypes; each phenotype is connected by an edge to a marker if there is a direct association between them given the rest of the variables. Different phenotypes may also interconnect.  In addition to markers and phenotypes information, the input data can include environmental variables. Then, the interactions network shows the conditional dependence relationships between markers, phenotypes and environmental factors.}

\value{
An object with S3 class \code{"netgwas"} is returned:  
 \item{Theta}{ A list of estimated p by p precision matrices that show the conditional independence relationships patterns among measured items.}
 \item{path}{ A list of estimated p by p adjacency matrices. This is the graph path corresponding to \code{Theta}.}
 \item{ES}{ A list of estimated p by p conditional expectation corresponding to \code{rho}.}
 \item{Z}{ A list of n by p transformed data based on Gaussian copula. }
 \item{rho}{ A \code{n.rho} dimensional vector containing the penalty terms.}
 \item{loglik }{ A \code{n.rho} dimensional vector containing the maximized log-likelihood values along the graph path. }
 \item{data}{ The \eqn{n} by \eqn{p} input data matrix. The \eqn{n} by \eqn{p} transformed data in case of using "npn". }
}

\note{This function estimates a graph path . To select an optimal graph please refer to \code{\link{selectnet}}.}

\author{ 
  Pariya Behrouzi and Ernst C. Wit \cr
  Maintainers: Pariya Behrouzi \email{pariya.behrouzi@gmail.com}\cr
}

\references{
1. Behrouzi, P., and Wit, E. C. (2019). Detecting epistatic selection with partially observed genotype data by using copula graphical models. Journal of the Royal Statistical Society: Series C (Applied Statistics), 68(1), 141-160. \cr
2. Behrouzi, P., and Wit, E. C. (2017c). netgwas: An R Package for Network-Based Genome-Wide Association Studies. arXiv preprint, arXiv:1710.01236. \cr
3. D. Witten and J. Friedman. New insights and faster computations for the graphical lasso. \emph{Journal of Computational and Graphical Statistics}, to appear, 2011. \cr
4. Guo, Jian, et al. "Graphical models for ordinal data." Journal of Computational and Graphical Statistics 24.1 (2015): 183-204. \cr
}

\seealso{\code{\link{selectnet}}}

\examples{
    \dontshow{
		data(thaliana)
		D <- thaliana[1:5, 1:10]
		res <-  netphenogeno(data= D, n.rho=2, em.iter = 1, ncores=1)
       }
    \donttest{
		data(thaliana)
		head(thaliana, n=3)
		#Construct a path for genotype-phenotype interactions network in thaliana data
		res <-  netphenogeno(data = thaliana); res
		plot(res)
		#Select an optimal network
		sel <- selectnet(res)
		#Plot selected network and the conditional correlation (CI) relationships 
		plot(sel, vis="CI")
		plot(sel, vis="CI", n.mem = c(8, 56, 31, 33, 31, 30), w.btw =50, w.within= 1)
		
		#Visualize interactive plot for the selected network
		#Color "red" for 8 phenotypes, and different colors for each chromosome.
		cl <- c(rep("red", 8), rep("white",56), rep("tan1",31), 
		      rep("gray",33), rep("lightblue2",31), rep("salmon2",30))
		      
		#The IDs of phenotypes and SNPs to be shown in the network       
    id <- c("DTF_LD","CLN_LD","RLN_LD","TLN_LD","DTF_SD","CLN_SD","RLN_SD", 
        "TLN_SD","snp15","snp16","snp17","snp49","snp50","snp60","snp75",
        "snp76","snp81","snp83","snp84","snp86","snp82", "snp113","snp150",
        "snp155","snp159","snp156","snp161","snp158","snp160","snp162","snp181")
		
		plot(sel, vis="interactive", n.mem = c(8, 56, 31, 33, 31, 30), vertex.color= cl,
		    label.vertex= "some", sel.nod.label= id, edge.color= "gray", w.btw= 50,
		    w.within= 1)
		
		#Partial correlations between genotypes and phenotypes in the thaliana dataset.
		library(Matrix)
		image(sel$par.cor, xlab="geno-pheno", ylab="geno-pheno", sub="")
	}
}