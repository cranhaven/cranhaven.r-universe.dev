\name{tune_K}
\alias{tune_K}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Select the number of clusters \code{K} in DEEM}

\description{Select the number of clusters \code{K} along with tuning parameter \code{lambda} through BIC in DEEM.}

\usage{tune_K(X, seqK, seqlamb, initial = TRUE, vec_x = NULL)}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{X}{Input tensor (or matrix) list of length \eqn{n}{n}, where \eqn{n}{n} is the number of observations. Each element of the list is a tensor or matrix. The order of tensor can be any positive integer not less than 2.}
  \item{seqK}{A sequence of user-specified number of clusters.}

  \item{seqlamb}{A sequence of user-specified \code{lambda} values. \code{lambda} is the weight of L1 penalty and a smaller \code{lambda} allows more variables to be nonzero}

  \item{initial}{Whether to initialize algorithm with K-means clustering. Default value is \code{TRUE}.}
  \item{vec_x}{Vectorized tensor data. Default value is \code{NULL}}
}

\details{
The \code{\link{tune_K}} function runs \code{\link{tune_lamb}} function \code{length(seqK)} times to choose the tuning parameter \eqn{\lambda} and number of clusters \eqn{K} simultaneously. Let \eqn{\widehat{\bm{\theta}}^{\{\lambda,K\}}} be the output of \code{\link{DEEM}} with the tuning parameter and number of clusters fixed at \eqn{\lambda} and \eqn{K} respectively, \code{\link{tune_K}} looks for the values of \eqn{\lambda} and \eqn{K} that minimizes
\deqn{\mathrm{BIC}(\lambda,K)=-2\sum_{i=1}^n\log(\sum_{k=1}^K\widehat{\pi}^{\{\lambda,K\}}_kf_k(\mathbf{X}_i;\widehat{\bm{\theta}}_k^{\{\lambda,K\}}))+\log(n)\cdot |\widehat{\mathcal{D}}^{\{\lambda,K\}}|,}
where \eqn{\widehat{\mathcal{D}}^{\{\lambda,K\}}=\{(k, {\mathcal{J}}): \widehat b_{k,{\mathcal{J}}}^{\lambda} \neq 0 \}} is the set of nonzero elements in \eqn{\widehat{\bm{B}}_2^{\{\lambda,K\}},\ldots,\widehat{\bm{B}}_K^{\{\lambda,K\}}}. The \code{\link{tune_K}} function intrinsically selects the initial point and return the optimal estimated labels.
}

\value{
  \item{opt_K}{Selected number of clusters that leads to optimal BIC.}
  \item{opt_lamb}{Tuned \code{lambda} that leads to optimal BIC.}
  \item{Krank}{A selection summary.}
}


\references{Mai, Q., Zhang, X., Pan, Y. and Deng, K. (2021). A Doubly-Enhanced EM Algorithm for Model-Based Tensor Clustering. \emph{Journal of the American Statistical Association}.}

\author{Kai Deng, Yuqing Pan, Xin Zhang and Qing Mai}

\seealso{\code{\link{DEEM}}, \code{\link{tune_lamb}}}

\examples{
\donttest{
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

mytune = tune_K(X, seqK=2:4, seqlamb=seq(0.01,0.1,by=0.01))
}
}
