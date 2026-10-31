\name{DEEM}
\alias{DEEM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Doubly-enhanced EM algorithm}

\description{Doubly-enhanced EM algorithm for tensor clustering}

\usage{
DEEM(X, nclass, niter = 100, lambda = NULL, dfmax = n, pmax = nvars, pf = rep(1, nvars),
eps = 1e-04, maxit = 1e+05, sml = 1e-06, verbose = FALSE, ceps = 0.1,
initial = TRUE, vec_x = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{Input tensor (or matrix) list of length \eqn{n}{n}, where \eqn{n}{n} is the number of observations. Each element of the list is a tensor or matrix. The order of tensor can be any positive integer not less than 2.}
  \item{nclass}{Number of clusters.}
  \item{niter}{Maximum iteration times for EM algorithm. Default value is 100.}
  \item{lambda}{A user-specified \code{lambda} value. \code{lambda} is the weight of L1 penalty and a smaller \code{lambda} allows more variables to be nonzero}

  \item{dfmax}{The maximum number of selected variables in the model. Default is the number of observations \code{n}.}

  \item{pmax}{The maximum number of potential selected variables during iteration. In middle step, the algorithm can select at most \code{pmax} variables and then shrink part of them such that the number of final selected variables is less than \code{dfmax}.}

  \item{pf}{Weight of lasso penalty. Default is a vector of value \code{1} and length \code{p}, representing L1 penalty of length \eqn{p}{p}. Can be modified to use adaptive lasso penalty.}

  \item{eps}{Convergence threshold for coordinate descent difference between iterations. Default value is \code{1e-04}.}
  \item{maxit}{Maximum iteration times for coordinate descent for all lambda. Default value is \code{1e+05}.}
  \item{sml}{Threshold for ratio of loss function change after each iteration to old loss function value. Default value is \code{1e-06}.}
  \item{verbose}{Indicates whether print out lambda during iteration or not. Default value is \code{FALSE}.}
  \item{ceps}{Convergence threshold for cluster mean difference between iterations. Default value is \code{1}.}
  \item{initial}{Whether to initialize algorithm with K-means clustering. Default value is \code{TRUE}.}
  \item{vec_x}{Vectorized tensor data. Default value is \code{NULL}}
}


\details{The \code{\link{DEEM}} function implements the Doubly-Enhanced EM algorithm (DEEM) for tensor clustering. The observations \eqn{\mathbf{X}_i} are assumed to be following the tensor normal mixture model (TNMM) with common covariances across different clusters:
\deqn{
\mathbf{X}_i\sim\sum_{k=1}^K\pi_k \mathrm{TN}(\bm{\mu}_k;\bm{\Sigma}_1,\ldots,\bm{\Sigma}_M),\quad i=1,\dots,n,
}
where \eqn{0<\pi_k<1} is the prior probability for \eqn{\mathbf{X}} to be in the \eqn{k}-th cluster such that \eqn{\sum_{k=1}^{K}\pi_k=1}, \eqn{\bm{\mu}_k} is the cluster mean of the \eqn{k}-th cluster and \eqn{\bm{\Sigma}_1,\ldots,\bm{\Sigma}_M)} are the common covariances across different clusters. Under the TNMM framework, the optimal clustering rule can be showed as
\deqn{
\widehat{Y}^{opt}=\arg\max_k\{\log\pi_k+\langle\mathbf{X}-(\bm{\mu}_1+\bm{\mu}_k)/2,\mathbf{B}_k\rangle\},
}
where \eqn{\mathbf{B}_k=[\![\bm{\mu}_k-\bm{\mu}_1;\bm{\Sigma}_1^{-1},\ldots,\bm{\Sigma}_M^{-1}]\!]}. In the enhanced E-step, \code{\link{DEEM}} imposes sparsity directly on the optimal clustering rule as a flexible alternative to popular low-rank assumptions on tensor coefficients \eqn{\mathbf{B}_k} as 
\deqn{
\min_{\mathbf{B}_2,\dots,\mathbf{B}_K}\bigg[\sum_{k=2}^K(\langle\mathbf{B}_k,[\![\mathbf{B}_k,\widehat{\bm{\Sigma}}_1^{(t)},\ldots,\widehat{\bm{\Sigma}}_M^{(t)}]\!]\rangle-2\langle\mathbf{B}_k,\widehat{\bm{\mu}}_k^{(t)}-\widehat{\bm{\mu}}_1^{(t)}\rangle) +\lambda^{(t+1)}\sum_{\mathcal{J}}\sqrt{\sum_{k=2}^Kb_{k,\mathcal{J}}^2}\bigg],
}
where \eqn{\lambda^{(t+1)}} is a tuning parameter. In the enhanced M-step, \code{\link{DEEM}} employs a new estimator for the tensor correlation structure, which facilitates both the computation and the theoretical studies.}

\value{
  \item{pi}{A vector of estimated prior probabilities for clusters.}
  \item{mu}{A list of estimated cluster means.}
  \item{sigma}{A list of estimated covariance matrices.}
  \item{gamma}{A \code{n} by \code{nclass} matrix of estimated membership weights.}
  \item{y}{A vector of estimated labels.}
  \item{iter}{Number of iterations until convergence.}
  \item{df}{Average zero elements in beta over iterations.}
  \item{beta}{A matrix of vectorized \code{B_k}.}
}

\references{Mai, Q., Zhang, X., Pan, Y. and Deng, K. (2021). A Doubly-Enhanced EM Algorithm for Model-Based Tensor Clustering. \emph{Journal of the American Statistical Association}.}

\author{Kai Deng, Yuqing Pan, Xin Zhang and Qing Mai}

\seealso{\code{\link{tune_lamb}}, \code{\link{tune_K}}}

\examples{
dimen = c(5,5,5)
nvars = prod(dimen)
K = 2
n = 100
sigma = array(list(),3)

sigma[[1]] = sigma[[2]] = sigma[[3]] = diag(5)

B2=array(0,dim=dimen)
B2[1:3,1,1]=2

y = c(rep(1,50),rep(2,50))
M = array(list(),K)
M[[1]] = array(0,dim=dimen)
M[[2]] = B2

vec_x=matrix(rnorm(n*prod(dimen)),ncol=n)
X=array(list(),n)
for (i in 1:n){
  X[[i]] = array(vec_x[,i],dim=dimen)
  X[[i]] = M[[y[i]]] + X[[i]]
}

myfit = DEEM(X, nclass=2, lambda=0.05)
}
