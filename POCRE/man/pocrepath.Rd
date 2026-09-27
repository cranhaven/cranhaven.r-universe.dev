\name{pocrepath}
\alias{pocrepath}
\title{
Build a POCRE Path for Different Values of Tuning Parameters
}
\description{
Applying POCRE for a series of tuning parameters chosen by a pre-specified step size. The tuning parameter will increase until non-component can be constructed, and then decrease until a non-sparse regression is constructed (i.e., the number of non-zero coefficients in \eqn{\beta} is more than maxvar).
}
\usage{
pocrepath(y, x, delta=0.1, maxvar=dim(x)[1]/2, x.nop=NA, maxcmp=10,
          ptype=c('ebtz','ebt','l1','scad','mcp'), lambda.init=1,
          maxit=100, tol=1e-6, maxtps=500, gamma=3.7, pval=(dim(y)[2]==1))
}
\arguments{
  \item{y}{
n*q matrix, values of q response variables (allow for multiple response variables).
}
  \item{x}{
n*p matrix, values of p predicting variables (excluding the intercept).
}
  \item{delta}{
step size to increase or decrase from current tuning parameter.
}
  \item{maxvar}{
maximum number of selected variables.
}
  \item{x.nop}{
a vector indicating indices of covariates which are excluded only when evaluating the significance of components.
}
  \item{maxcmp}{
maximum number of components to be constructed.
}
  \item{ptype}{
a character to indicate the type of penalty: \code{'ebtz'} (emprical Bayes thresholding after Fisher's z-transformation, by default), \code{'ebt'} (emprical Bayes thresholding by Johnstone & Silverman (2004)), \code{'l1'} (L_1 penalty), \code{'scad'} (SCAD by Fan & Li (2001)), \code{'mcp'} (MCP by Zhang (2010)).
}
  \item{lambda.init}{
initial value of the tuning parameter (=1 by default).
}
  \item{maxit}{
maximum number of iterations to be allowed.
}
  \item{tol}{
tolerance of precision in iterations.
}
  \item{maxtps}{
maximum number of different values that the tuning parameter is allowed.
}
  \item{gamma}{
a parameter used by SCAD and MCP (=3.7 by default).
}
  \item{pval}{
a logical value indicating whether to calculate the p-values of components (not implemented for q>1, i.e., multiple response variables).
}
}
\value{
{A list of results from \link{pocre}, each for a specific value of the tuning parameter. }
}
\references{
Fan J and Li R (2001). Variable selection via nonconcave penalized likelihood and its oracle properties. \emph{Journal of the American Statistical Association}, 96:1348-1360

Johnstone IM and Silverman BW (2004). Needles and straw in haystacks: empirical Bayes estimates of possibly sparse sequences. \emph{Annals of Statistics}, 32: 1594-1649.

Zhang C-H (2010). Nearly unbiased variable selection under minimax concave penalty. \emph{The Annals of Statistics}, 38: 894-942.

Zhang D (2018). R package POCRE: Exploring high-dimensional data via supervised dimension reduction. Manuscript.

Zhang D, Lin Y, and Zhang M (2009). Penalized orthogonal-components regression for large p small n data. \emph{Electronic Journal of Statistics}, 3: 781-796.
}
\author{Dabao Zhang, Zhongli Jiang, Zeyu Zhang, Department of Statistics, Purdue University}
\seealso{
\code{\link{plot.pocrepath}}, \code{\link{selectmodel}}, \code{\link{pocre}}.
}
\examples{
data(simdata)
xx <- simdata[,-1]
yy <- simdata[,1]

ppres <- pocrepath(yy,xx)
}
