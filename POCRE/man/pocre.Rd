\name{pocre}
\alias{pocre}
\title{
Penalized Orthogonal-Components Regression (POCRE)
}
\description{
Apply POCRE with a pre-specified tuning parameter to build a linear regression model with orthogonal components \eqn{X\vartheta_1, X\vartheta_2, \dots},
\deqn{Y=\mu+\sum_j (X\varpi_j)\vartheta_j+\epsilon=\mu+X\beta+\epsilon,}
where \eqn{var[\epsilon]=\sigma^2} and \eqn{\beta=\sum_j \varpi_j\vartheta_j}. These orthogonal components are sequentially constructed according to supervised dimension reduction under penalty set by the pre-specified tuning parameter.

While the orthogonal components are constructed using the centralized covariates, the intercept \eqn{\mu} and regression coefficients in \eqn{\beta} are estimated for original covariates. The sequential construction stops when no new component can be constructed (returning bSparse=1), or the new component is constructed with more than maxvar covariates (returning bSparse=0).
}
\usage{
pocre(y, x, lambda=1, x.nop=NA, maxvar=dim(x)[1]/2,
      maxcmp=10, ptype=c('ebtz','ebt','l1','scad','mcp'),
      maxit=100, tol=1e-6, gamma=3.7, pval=FALSE)
}
\arguments{
  \item{y}{
n*q matrix, values of q response variables (allow for multiple response variables).
}
  \item{x}{
n*p matrix, values of p predicting variables (excluding the intercept).
}
  \item{lambda}{
the tuning parameter (=1 by default).
}
  \item{x.nop}{
a vector indicating indices of covariates which are excluded only when evaluating the significance of components.
}
  \item{maxvar}{
maximum number of selected variables.
}
  \item{maxcmp}{
maximum number of components to be constructed.
}
  \item{ptype}{
a character to indicate the type of penalty: \code{'ebtz'} (emprical Bayes thresholding after Fisher's z-transformation, by default), \code{'ebt'} (emprical Bayes thresholding by Johnstone & Silverman (2004)), \code{'l1'} (L_1 penalty), \code{'scad'} (SCAD by Fan & Li (2001)), \code{'mcp'} (MCP by Zhang (2010)).
}
  \item{maxit}{
maximum number of iterations to be allowed.
}
  \item{tol}{
tolerance of precision in iterations.
}
  \item{gamma}{
a parameter used by SCAD and MCP (=3.7 by default).
}
  \item{pval}{
a logical value indicating whether to calculate the p-values of components.
}
}
\value{
\item{mu}{estimated intercept of the linear regression.}
\item{beta}{estimated coefficients of the linear regression.}
\item{varpi}{loadings of the constructed components.}
\item{vartheta}{the regression coefficients of the constructed components.}
\item{bSparse}{a logical value indicating whether estimated beta has less than maxvar nonzero values.}
\item{lambda}{value of the tuning paramete.}
\item{nCmp}{number of constructed components.}
\item{n}{sample size.}
\item{p}{number of covariates.}
\item{xShift}{the column means of x.}
\item{yShift}{the column means of y.}
\item{sigmae2}{estimated error variance \eqn{\sigma^2}.}
\item{rsq}{\eqn{R^2} value of the fitted regression model.}
\item{nzBeta}{number of non-zero regression coefficients in \eqn{\beta}.}
\item{omega}{internal matrix.}
\item{theta}{internal matrix.}
\item{pvalue}{p-values of constructed components, available when \code{pval=TRUE}.}
\item{seqpv}{Type I p-values of components when sequentially including them into the model, available when \code{pval=TRUE}.}
\item{indpv}{p-values of components when marginally testing each component, available when pval=TRUE.}
\item{loglik}{the loglikelihood function, available when \code{pval=TRUE}.}
\item{effp}{the effective number of predictors, excluding redundant ones, available when pval=TRUE.}
}
\references{
Fan J and Li R (2001). Variable selection via nonconcave penalized likelihood and its oracle properties. \emph{Journal of the American Statistical Association}, 96:1348-1360

Johnstone IM and Silverman BW (2004). Needles and straw in haystacks: empirical Bayes estimates of possibly sparse sequences. \emph{Annals of Statistics}, 32: 1594-1649.

Zhang C-H (2010). Nearly unbiased variable selection under minimax concave penalty. \emph{The Annals of Statistics}, 38: 894-942.

Zhang D, Lin Y, and Zhang M (2009). Penalized orthogonal-components regression for large p small n data. \emph{Electronic Journal of Statistics}, 3: 781-796.
}
\author{Dabao Zhang, Zhongli Jiang, Zeyu Zhang, Department of Statistics, Purdue University}
\seealso{
\code{\link{plot.pocre}}, \code{\link{pocrescreen}}, \code{\link{pocrepath}}, \code{\link{cvpocre}}.
}
\examples{
data(simdata)
xx <- simdata[,-1]
yy <- simdata[,1]

#pres <- pocre(yy,xx,lambda=0.9)
pres <- pocre(yy,xx)   # lambda=1 by default
}
