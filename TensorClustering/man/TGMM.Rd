\name{TGMM}
\alias{TGMM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Fit the Tensor Gaussian Mixture Model (TGMM)}

\description{Fit the Tensor Gaussian Mixture Model (TGMM)}

\usage{
TGMM(Xn, K, shape = "shared", initial = "kmeans", 
iter.max = 500, stop = 1e-3, trueY = NULL, print = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Xn}{The tensor for clustering, should be array type, the last dimension is the sample size \code{n}.}
  \item{K}{Number of clusters, greater than or equal to \code{2}.}
  \item{shape}{"shared" if assume common covariance across mixtures, "distinct" if allow different covariance structures. Default value is "shared".}
  \item{initial}{Initialization meth0d for the regularized EM algorithm. Default value is "kmeans".}
  \item{iter.max}{Maximum number of iterations. Default value is \code{500}.}
  \item{stop}{Convergence threshold of relative change in cluster means. Default value is \code{1e-3}.}
  \item{trueY}{A vector of true cluster labels of each observation. Default value is NULL.}
  \item{print}{Whether to print information including current iteration number, relative change in cluster means 
  and clustering error (\code{\%}) in each iteration.}
}

\details{The \code{\link{TGMM}} function fits the Tensor Gaussian Mixture Model (TGMM) through the classical EM algorithm. TGMM assumes the following tensor normal mixture distribution of M-way tensor data \eqn{\mathbf{X}}:
\deqn{
\mathbf{X}\sim\sum_{k=1}^K\pi_k \mathrm{TN}(\bm{\mu}_k,\mathcal{M}_k),\quad i=1,\dots,n,
}
where \eqn{0<\pi_k<1} is the prior probability for \eqn{\mathbf{X}} to be in the \eqn{k}-th cluster such that \eqn{\sum_{k=1}^{K}\pi_k=1}, \eqn{\bm{\mu}_k} is the mean of the \eqn{k}-th cluster, \eqn{\mathcal{M}_k \equiv \{\bm{\Sigma}_{km}, m=1,\dots,M\}} is the set of covariances of the \eqn{k}-th cluster. If \eqn{\mathcal{M}_k}'s are the same for \eqn{k=1,\dots,K}, call \code{\link{TGMM}} with argument \code{shape="shared"}.

}

\value{
  \item{id}{A vector of estimated labels.}
  \item{pi}{A vector of estimated prior probabilities for clusters.}
  \item{eta}{A \code{n} by \code{K} matrix of estimated membership weights.}
  \item{Mu.est}{A list of estimated cluster means.}
  \item{SIG.est}{A list of estimated covariance matrices.}
}

\references{
Deng, K. and Zhang, X. (2021). Tensor Envelope Mixture Model for Simultaneous Clustering and Multiway Dimension Reduction. \emph{Biometrics}.

Tait, P. A. and McNicholas, P. D. (2019). Clustering higher order data: Finite mixtures of multidimensional arrays. \emph{arXiv:1907.08566}.
}

\author{Kai Deng, Yuqing Pan, Xin Zhang and Qing Mai}

\seealso{\code{\link{TEMM}}}

\examples{
  A = array(c(rep(1,20),rep(2,20))+rnorm(40),dim=c(2,2,10))
  myfit = TGMM(A,K=2,shape="shared")
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

