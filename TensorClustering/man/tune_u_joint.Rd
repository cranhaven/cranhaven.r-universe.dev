\name{tune_u_joint}
\alias{tune_u_joint}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Tuning envelope dimension jointly by BIC in TEMM.}

\description{Tuning envelope dimension jointly by BIC in TEMM.}

\usage{
tune_u_joint(u_candi, K, X, iter.max = 500, stop = 0.001, trueY = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{u_candi}{A list of length \code{M} containing candidate envelope dimension for each mode.}
  \item{K}{Number of clusters, greater than or equal to \code{2}.}
  \item{X}{The tensor for clustering, should be array type, the last dimension is the sample size \code{n}.}
  \item{iter.max}{Maximum number of iterations. Default value is \code{500}.}
  \item{stop}{Convergence threshold of relative change in cluster means. Default value is \code{1e-3}.}
  \item{trueY}{A vector of true cluster labels of each observation. Default value is NULL.}
}


\details{
The \code{\link{tune_u_joint}} function searches over all the combinations of \eqn{u\equiv(u_1,\dots,u_M)} in the neighborhood of \eqn{\widetilde{u}}, \eqn{\mathcal{N}(\widetilde u)=\{u:\ \max(1,\widetilde u_m-2) \leq u_m \leq \min(\widetilde u_m+2,p_m),\ m=1,\dots,M\}}, that minimizes	
\deqn{\mathrm{BIC}(u) = -2\sum_{i=1}^{n}\log(\sum_{k=1}^{K}\widehat{\pi}_k^u f_k(\mathbf{X}_i;\widehat{\bm{\theta}}^u)) + \log(n)\cdot K_u.}
In the above BIC, \eqn{K_u=(K-1)\prod_{m=1}^M u_m + \sum_{m=1}^{M}p_m(p_m+1)/2} is the total number of parameters in TEMM, \eqn{\widehat{\pi}_k^u} and \eqn{\widehat{\bm{\theta}}^{u}} are the estimated parameters with envelope dimension fixed at \eqn{u}. The \code{\link{tune_u_joint}} function intrinsically selects the initial point and return the optimal estimated labels.
}


\value{
  \item{opt.u}{Optimal envelope dimension selected.}
  \item{opt.id}{Estimated labels fitted by TEMM with the optimal envelope dimension.}
  \item{opt.Mu}{Estimated cluster means fitted by TEMM with the optimal envelope dimension.}
  \item{bic}{BIC value.}
}


\references{
Deng, K. and Zhang, X. (2021). Tensor Envelope Mixture Model for Simultaneous Clustering and Multiway Dimension Reduction. \emph{Biometrics}.
}

\author{Kai Deng, Yuqing Pan, Xin Zhang and Qing Mai}

\seealso{\code{\link{TEMM}}, \code{\link{tune_u_sep}}}

\examples{
  A = array(c(rep(1,20),rep(2,20))+rnorm(40),dim=c(2,2,10))
  mytune = tune_u_joint(u_candi=list(1:2,1:2),K=2,A)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

