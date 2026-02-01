\name{selectnet}
\alias{selectnet}

\title{ Model selection for optimal graph estimation}

\description{
Estimate the optimal graph based on different information criteria .
}

\usage{
selectnet(nutriNetwork.obj, opt.index= NULL, criteria= NULL, ebic.gamma=0.5,
		   ncores= NULL, verbose= TRUE)
}

\arguments{
	\item{nutriNetwork.obj}{An object with S3 class "nutriNetwork"}
	\item{opt.index}{The program internally determines an optimal graph using \code{opt.index= NULL}. Otherwise, to manually choose an optimal graph from the graph path.}
	\item{criteria}{Model selection criteria. "ebic" and "aic" are available. BIC model selection can be calculated by fixing \code{ebic.gamma = 0}. Applicable only if \code{opt.index= NULL}.}
	\item{ebic.gamma}{The tuning parameter for ebic. The\code{ebic.gamma = 0} results in bic model selection. The default value is 0.5. Applicable only \code{opt.index= NULL}.}
	\item{ncores}{ The number of cores to use for the calculations. Using \code{ncores = "all"} automatically detects number of available cores and runs the computations in parallel.}
	\item{verbose}{If \code{verbose = FALSE}, printing information is disabled. The default value is \code{TRUE}. Applicable only \code{opt.index= NULL}.}
}

\value{
An obj with S3 class "selectnet" is returned:
 \item{opt.adj}{ The optimal graph selected from the graph path}
 \item{opt.theta}{The optimal precision matrix from the graph path}
 \item{opt.sigma}{ The optimal covariance matrix from the graph path}
 \item{ebic.scores}{ Extended BIC scores for regularization parameter selection at the EM convergence. Applicable if \code{opt.index = NULL}.}
 \item{opt.index}{ The index of optimal regularization parameter. }
 \item{opt.rho}{ The selected regularization parameter. }
 \item{par.cor}{ A partial correlation matrix. }

and anything else that is included in the input \code{nutriNetwork.obj}.
 }

\references{
1. Behrouzi, P., and Wit, E. C. (2019). Detecting epistatic selection with partially observed genotype data by using copula graphical models. Journal of the Royal Statistical Society: Series C (Applied Statistics), 68(1), 141-160. \cr
2. Behrouzi, P., and Wit, E. C. (2017c). netgwas: An R Package for Network-Based Genome-Wide Association Studies. arXiv preprint, arXiv:1710.01236. \cr
3. Ibrahim, Joseph G., Hongtu Zhu, and Niansheng Tang. (2012). Model selection criteria for missing-data problems using the EM algorithm. Journal of the American Statistical Association.
4. D. Witten and J. Friedman. (2011). New insights and faster computations for the graphical lasso. \emph{Journal of Computational and Graphical Statistics}, to appear. \cr
5. J. Friedman, T. Hastie and R. Tibshirani. (2007). Sparse inverse covariance estimation with the lasso, \emph{Biostatistics}.\cr
6. Foygel, R. and M. Drton. (2010). Extended bayesian information criteria for Gaussian graphical models. In Advances in Neural Information Processing Systems, pp. 604-612.
}

\author{
  Pariya Behrouzi \cr
  Maintainer: Pariya Behrouzi \email{pariya.behrouzi@gmail.com} \cr
}

\examples{
######## toy example
data(vfit)
test_dat <- vfit[1:10, c("sex", "ani.pro", "veg.pro", "B6",
            "B12", "B9", "SPPB.total", "HandGrip"  )]
out_test <- nutriNetwork(test_dat, method = "gibbs")
sel_test <- selectnet(out_test)
########
\dontshow{
out_check <- nutriNetwork(vfit, method ="npn")
sel_check <- selectnet(out_check)
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
