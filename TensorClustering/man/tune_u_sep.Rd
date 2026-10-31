\name{tune_u_sep}
\alias{tune_u_sep}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Tuning envelope dimension separately by BIC in TEMM.}

\description{Tuning envelope dimension separately by BIC in TEMM.}

\usage{
tune_u_sep(m, u_candi, K, X, C = 1, oneD = TRUE, 
iter.max = 500, stop = 0.001, trueY = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m}{The tensor mode to be tuned, can take value in \code{1,...,M}.}
  \item{u_candi}{A vector of candidate envelope dimension.}
  \item{K}{Number of clusters, greater than or equal to \code{2}.}
  \item{X}{The tensor for clustering, should be array type, the last dimension is the sample size \code{n}.}
  \item{C}{Constant in separate BIC criterion. Default value is \code{1}.}
  \item{oneD}{Whether to apply 1D-BIC tuning. Default value is TRUE.}
  \item{iter.max}{Maximum number of iterations. Default value is \code{500}.}
  \item{stop}{Convergence threshold of relative change in cluster means. Default value is \code{1e-3}.}
  \item{trueY}{A vector of true cluster labels of each observation. Default value is NULL.}
}


\details{
For tensor mode \eqn{m=1,\dots,M}, the \code{\link{tune_u_sep}} function selects the envelope dimension \eqn{\widetilde{u}_m} by minimizing the following BIC-type criterion over the set \eqn{\{0,1,\dots,p_m\}},
\deqn{\mathrm{BIC}_m(u_m) = \log|\bm{\Gamma}_m^T \widehat{\mathbf{M}}_m \bm{\Gamma}_m|+\log|\bm{\Gamma}_{m}^T \widehat{\mathbf{N}}_m^{-1} \bm{\Gamma}_{m}| + C \cdot u_m \log(n)/n.}
This separate selection over each mode \eqn{m} is less sensitive to the complex interrelationships of each mode of the tensor. The default constant \eqn{C} is set as \eqn{1} as suggested by Zhang and Mai (2018).
}



\value{
  \item{opt.u}{Optimal envelope dimension selected.}
  \item{bic}{BIC value.}
}


\references{
Deng, K. and Zhang, X. (2021). Tensor Envelope Mixture Model for Simultaneous Clustering and Multiway Dimension Reduction. \emph{Biometrics}.

Zhang, X. and Mai, Q. (2018). Model-free envelope dimension selection. \emph{Electronic Journal of Statistics} 12, 2193-2216.
}

\author{Kai Deng, Yuqing Pan, Xin Zhang and Qing Mai}

\seealso{\code{\link{TEMM}}, \code{\link{tune_u_joint}}}

\examples{
  A = array(c(rep(1,20),rep(2,20))+rnorm(40),dim=c(2,2,10))
  mytune = tune_u_sep(1,1:2,K=2,A)
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
