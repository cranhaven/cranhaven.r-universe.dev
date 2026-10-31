\name{TEMM}
\alias{TEMM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Fit the Tensor Envelope Mixture Model (TEMM)}

\description{Fit the Tensor Envelope Mixture Model (TEMM)}

\usage{
TEMM(Xn, u, K, initial = "kmeans", iter.max = 500, 
stop = 1e-3, trueY = NULL, print = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Xn}{The tensor for clustering, should be array type, the last dimension is the sample size \code{n}.}
  \item{u}{A vector of envelope dimension}
  \item{K}{Number of clusters, greater than or equal to \code{2}.}
  \item{initial}{Initialization meth0d for the regularized EM algorithm. Default value is "kmeans".}
  \item{iter.max}{Maximum number of iterations. Default value is \code{500}.}
  \item{stop}{Convergence threshold of relative change in cluster means. Default value is \code{1e-3}.}
  \item{trueY}{A vector of true cluster labels of each observation. Default value is NULL.}
  \item{print}{Whether to print information including current iteration number, relative change in cluster means 
  and clustering error (\code{\%}) in each iteration.}
}

\details{The \code{\link{TEMM}} function fits the Tensor Envelope Mixture Model (TEMM) through a subspace-regularized EM algorithm. For mode \eqn{m}, let \eqn{(\bm{\Gamma}_m,\bm{\Gamma}_{0m})\in R^{p_m\times p_m}} be an orthogonal matrix where \eqn{\bm{\Gamma}_{m}\in R^{p_{m}\times u_{m}}}, \eqn{u_{m}\leq p_{m}}, represents the material part. Specifically, the material part \eqn{\mathbf{X}_{\star,m}=\mathbf{X}\times_{m}\bm{\Gamma}_{m}^{T}} follows a tensor normal mixture distribution, while the immaterial part \eqn{\mathbf{X}_{\circ,m}=\mathbf{X}\times_{m}\bm{\Gamma}_{0m}^{T}} is unimodal, independent of the material part and hence can be eliminated without loss of  clustering information. Dimension reduction is achieved by focusing on the material part \eqn{\mathbf{X}_{\star,m}=\mathbf{X}\times_{m}\bm{\Gamma}_{m}^{T}}. Collectively, the joint reduction from each mode is
\deqn{
\mathbf{X}_{\star}=[\![\mathbf{X};\bm{\Gamma}_{1}^{T},\dots,\bm{\Gamma}_{M}^{T}]\!]\sim\sum_{k=1}^{K}\pi_{k}\mathrm{TN}(\bm{\alpha}_{k};\bm{\Omega}_{1},\dots,\bm{\Omega}_{M}),\quad \mathbf{X}_{\star}\perp\!\!\!\perp\mathbf{X}_{\circ,m},
}
where \eqn{\bm{\alpha}_{k}\in R^{u_{1}\times\cdots\times u_{M}}} and \eqn{\bm{\Omega}_m\in R^{u_m\times u_m}} are the dimension-reduced clustering parameters and \eqn{\mathbf{X}_{\circ,m}} does not vary with cluster index \eqn{Y}. In the E-step, the membership weights are evaluated as
\deqn{
\widehat{\eta}_{ik}^{(s)}=\frac{\widehat{\pi}_{k}^{(s-1)}f_{k}(\mathbf{X}_i;\widehat{\bm{\theta}}^{(s-1)})}{\sum_{k=1}^{K}\widehat{\pi}_{k}^{(s-1)}f_{k}(\mathbf{X}_i;\widehat{\bm{\theta}}^{(s-1)})},
}
where \eqn{f_k} denotes the conditional probability density function of \eqn{\mathbf{X}_i} within the \eqn{k}-th cluster. In the subspace-regularized M-step, the envelope subspace is iteratively estimated through a Grassmann manifold optimization that minimize the following log-likelihood-based objective function:
\deqn{
G_m^{(s)}(\bm{\Gamma}_m) = \log|\bm{\Gamma}_m^T \mathbf{M}_m^{(s)} \bm{\Gamma}_m|+\log|\bm{\Gamma}_m^T (\mathbf{N}_m^{(s)})^{-1} \bm{\Gamma}_m|,
}
where \eqn{\mathbf{M}_{m}^{(s)}} and \eqn{\mathbf{N}_{m}^{(s)}} are given by
\deqn{
\mathbf{M}_m^{(s)} = \frac{1}{np_{-m}}\sum_{i=1}^{n} \sum_{k=1}^{K}\widehat{\eta}_{ik}^{(s)} (\bm{\epsilon}_{ik}^{(s)})_{(m)}(\widehat{\bm{\Sigma}}_{-m}^{(s-1)})^{-1}  (\bm{\epsilon}_{ik}^{(s)})_{(m)}^T,
}
\deqn{
\mathbf{N}_m^{(s)} = \frac{1}{np_{-m}}\sum_{i=1}^{n} (\mathbf{X}_i)_{(m)}(\widehat{\bm{\Sigma}}_{-m}^{(s-1)})^{-1}(\mathbf{X}_i)_{(m)}^T.
}
The intermediate estimators \eqn{\mathbf{M}_{m}^{(s)}} can be viewed the mode-\eqn{m} conditional variation estimate of \eqn{\mathbf{X}\mid Y} and \eqn{\mathbf{N}_{m}^{(s)}} is the mode-\eqn{m} marginal variation estimate of \eqn{\mathbf{X}}.
}

\value{
  \item{id}{A vector of estimated labels.}
  \item{pi}{A vector of estimated prior probabilities for clusters.}
  \item{eta}{A \code{n} by \code{K} matrix of estimated membership weights.}
  \item{Mu.est}{A list of estimated cluster means.}
  \item{SIG.est}{A list of estimated covariance matrices.}
  \item{Mm}{Estimation of \code{Mm} defined in paper.}
  \item{Nm}{Estimation of \code{Nm} defined in paper.}
  \item{Gamma.est}{A list of estimated envelope basis.}
  \item{PGamma.est}{A list of envelope projection matrices.}
}

\references{
Deng, K. and Zhang, X. (2021). Tensor Envelope Mixture Model for Simultaneous Clustering and Multiway Dimension Reduction. \emph{Biometrics}.
}

\author{Kai Deng, Yuqing Pan, Xin Zhang and Qing Mai}

\seealso{\code{\link{TGMM}}, \code{\link{tune_u_sep}}, \code{\link{tune_u_joint}}}

\examples{
  A = array(c(rep(1,20),rep(2,20))+rnorm(40),dim=c(2,2,10))
  myfit = TEMM(A,u=c(2,2),K=2)
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
