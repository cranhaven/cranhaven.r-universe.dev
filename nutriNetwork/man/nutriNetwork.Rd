\name{nutriNetwork}
\alias{nutriNetwork}

\title{Reconstructs conditional (in)dependence networks among variables}

\description{
This is the main functions of the \pkg{nutriNetwork} package. This function infers the direct associations between variables. In another words, it measures pairwise associations among variables while correcting the effect of remaining variables. Three methods are available to reconstruct networks, namely (i) Gibbs sampling, (ii) approximation method, and (iii) nonparanormal approach within the copula graphical model. The first two methods are able to deal with missing genotypes. The last one is computationally faster.
}

\usage{
nutriNetwork(data, method = "gibbs", rho = NULL, n.rho = NULL, rho.ratio = NULL,
		ncores = 1, em.iter = 5, em.tol=.001, verbose = TRUE)
}

\arguments{
	\item{data}{ An (\eqn{n \times p}) matrix or a \code{data.frame} corresponding to the data matrix, where \eqn{n} is sample size and \eqn{p} is the number of variables. Input data can contain missing values.}
	\item{method}{Reconstructing undirected graph using the three methods: "gibbs", "approx", and "npn". For a medium (~500) and a large number of variables we recommend to choose "gibbs" and "approx", respectively. Choosing "npn" for a very large number of variables (> 2000) is computationally efficient. The default method is "gibbs".}
	\item{rho}{Optional. A decreasing sequence of non-negative numbers that control the sparsity level. Leaving the input as \code{rho = NULL}, the program automatically computes a sequence of \code{rho} based on \code{n.rho} and \code{rho.ratio}. Users can also supply a decreasing sequence values to override this.}
	\item{n.rho}{Optional. The number of regularization parameters. The default value is \code{10}.}
	\item{rho.ratio}{Optional. Determines distance between the elements of \code{rho} sequence. A small value of \code{rho.ratio} results in a large distance between the elements of \code{rho} sequence. And a large value of \code{rho.ratio} results into a small distance between elements of \code{rho}.Optional. The default value is \code{0.3}.}
	\item{ncores}{Optional. The number of cores to use for the calculations. Using \code{ncores = "all"} automatically detects number of available cores and runs the computations in parallel on (available cores - 1).}
	\item{em.iter}{Optional. The number of EM iterations. The default value is 5.}
	\item{em.tol}{Optional. A criteria to stop the EM iterations. The default value is .001. }
	\item{verbose}{Optional. Providing a detail message for tracing output. The default value is \code{TRUE}.}
}

\details{ This function estimates a graph path . To select an optimal graph please refer to \code{\link{selectnet}}.
}

\value{
An object with S3 class \code{"nutriNetwork"} is returned:  
 \item{Theta}{ A list of estimated p by p precision matrices that show the conditional independence relationships patterns among measured items.}
 \item{path}{ A list of estimated p by p adjacency matrices. This is the graph path corresponding to \code{Theta}.}
 \item{ES}{ A list of estimated p by p conditional expectation corresponding to \code{rho}.}
 \item{Z}{ A list of n by p transformed data based on Gaussian copula. }
 \item{rho}{ A \code{n.rho} dimensional vector containing the penalty terms.}
 \item{loglik }{ A \code{n.rho} dimensional vector containing the maximized log-likelihood values along the graph path. }
 \item{data}{ The \eqn{n} by \eqn{p} input data matrix. The \eqn{n} by \eqn{p} transformed data in case of using "npn". }
}

\author{ 
  Pariya Behrouzi \cr
  Maintainers: Pariya Behrouzi \email{pariya.behrouzi@gmail.com}\cr
}

\seealso{\code{\link{selectnet}}}

\examples{
######## toy example
data(vfit)
test_dat <- vfit[1:10, c("sex", "ani.pro", "veg.pro", "B6", 
           "B12", "B9", "SPPB.total", "HandGrip"  )]
 out_test <- nutriNetwork(test_dat, method = "gibbs")  
 ########
  \dontshow{ 
 out_check <- nutriNetwork(vfit, method ="npn")
  }         
  \donttest{
 out <- nutriNetwork(vfit, method = "gibbs")
 sel <- selectnet(out)
  		
 cl <- c(rep("gray70", 7), rep("green3",17), rep("red3",5))
 plot(sel, vis= "parcor.network", sign.edg = TRUE, 
      vertex.color = cl, curve = TRUE, layout.tree= TRUE, 
      root.node= c(26, 29), pos.legend= "bottomleft", 
      cex.legend=1) 
 #diffeent visualization      
 plot(sel, vis= "parcor.network", sign.edg = TRUE, layout = NULL, 
     vertex.color = cl, curve = TRUE, pos.legend= "topleft", 
     cex.legend=1 )
  }         
}