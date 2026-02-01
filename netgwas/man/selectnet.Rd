\name{selectnet}
\alias{selectnet}

\title{ Model selection }

\description{
Estimate the optimal regularization parameter at EM convergence based on different information criteria .
}

\usage{
selectnet(netgwas.obj, opt.index= NULL, criteria= NULL, ebic.gamma=0.5,
		   ncores= NULL, verbose= TRUE)
}

\arguments{
	\item{netgwas.obj}{An object with S3 class "netgwas"}
	\item{opt.index}{The program internally determines an optimal graph using \code{opt.index= NULL}. Otherwise, to manually choose an optimal graph from the graph path.}
	\item{criteria}{Model selection criteria. "ebic" and "aic" are available. BIC model selection can be calculated by fixing \code{ebic.gamma = 0}. Applicable only if \code{opt.index= NULL}.}
	\item{ebic.gamma}{The tuning parameter for ebic. The\code{ebic.gamma = 0} results in bic model selection. The default value is 0.5. Applicable only \code{opt.index= NULL}.}
	\item{ncores}{ The number of cores to use for the calculations. Using \code{ncores = "all"} automatically detects number of available cores and runs the computations in parallel.}
	\item{verbose}{If \code{verbose = FALSE}, printing information is disabled. The default value is \code{TRUE}. Applicable only \code{opt.index= NULL}.}
}

\details{
This function computes extended Bayesian information criteria (ebic), Bayesian information criteria, Akaike information criterion (aic) at EM convergence based on observed or joint log-likelihood.
The observed log-likelihood can be obtained through

\deqn{\ell_Y(\widehat{\Theta}_\lambda) = Q(\widehat{\Theta}_\lambda | \widehat{\Theta}^{(m)}) - H (\widehat{\Theta}_\lambda | \widehat{\Theta}^{(m)}),}

Where \eqn{Q} can be calculated from \code{\link{netmap}}, \code{\link{netsnp}}, \code{\link{netphenogeno}} function and H function is
\deqn{H(\widehat{\Theta}_\lambda | \widehat{\Theta}^{(m)}_\lambda) = E_z[\ell_{Z | Y}(\widehat{\Theta}_\lambda) | Y; \widehat{\Theta}_\lambda] = E_z[\log f(z)| Y ;\widehat{\Theta}_\lambda ] - \log p(y).}

The "ebic" and "aic" model selection criteria can be obtained as follow
\deqn{ebic(\lambda) = -2 \ell(\widehat{\Theta}_\lambda) +  ( \log n + 4 \gamma \log p) df(\lambda)}

\deqn{aic(\lambda) = -2 \ell(\widehat{\Theta}_\lambda) +  2 df(\lambda)}
where \eqn{df} refers to the number of non-zeros offdiagonal elements of \eqn{\hat{\Theta}_\lambda}, and \eqn{\gamma \in [0, 1]}. Typical value for for \code{ebic.gamma} is 1/2, but it can also be tuned by experience. Fixing \code{ebic.gamma = 0 } results in bic model selection.
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
 \item{V.names}{ Variables name whose are not isolated. }

and anything else that is included in the input \code{netgwas.obj}.
 }

\references{
1. BBehrouzi, P., and Wit, E. C. (2019). Detecting epistatic selection with partially observed genotype data by using copula graphical models. Journal of the Royal Statistical Society: Series C (Applied Statistics), 68(1), 141-160. \cr
2. Behrouzi, P., Arends, D., and Wit, E. C. (2023). netgwas: An R Package for Network-Based Genome-Wide Association Studies. The R journal, 14(4), 18-37. \cr
3. Ibrahim, Joseph G., Hongtu Zhu, and Niansheng Tang. (2012). Model selection criteria for missing-data problems using the EM algorithm. Journal of the American Statistical Association.
4. D. Witten and J. Friedman. (2011). New insights and faster computations for the graphical lasso. \emph{Journal of Computational and Graphical Statistics}, to appear. \cr
5. J. Friedman, T. Hastie and R. Tibshirani. (2007). Sparse inverse covariance estimation with the lasso, \emph{Biostatistics}.\cr
6. Foygel, R. and M. Drton. (2010). Extended bayesian information criteria for Gaussian graphical models. In Advances in Neural Information Processing Systems, pp. 604-612.
}

\author{
  Pariya Behrouzi and Ernst C.Wit \cr
  Maintainer: Pariya Behrouzi \email{pariya.behrouzi@gmail.com} \cr
}

\seealso{\code{\link{netmap}}, \code{\link{netsnp}}, \code{\link{netphenogeno}}}

\examples{
	\dontshow{
		D <- simgeno(p=20, n=10, k= 3, adjacent = 1, alpha = 0.06 , beta = 0.3)
		out <- netsnp(data = D$data, n.rho= 2, ncores= 1)
		plot(out)
		sel <- selectnet(out, criteria = "ebic")
    }
	\donttest{
		#simulate data
		D <- simgeno(p=50, n=100, k= 3, adjacent = 3, alpha = 0.06 , beta = 0.06)
		plot(D)

		#explore intra- and inter-chromosomal interactions
		out  <-  netsnp(D$data, n.rho= 5, ncores= 1)
		plot(out)

		#different graph selection methods
		sel.ebic1 <- selectnet(out, criteria = "ebic")
		plot(sel.ebic1, vis = "CI")

		sel.aic <- selectnet(out, criteria = "aic")
		plot(sel.aic, vis = "CI")

		sel.bic <- selectnet(out, criteria = "ebic", ebic.gamma = 0)
		plot(sel.bic, vis = "CI")
	}
}
