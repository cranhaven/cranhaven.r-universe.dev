\name{cvpocre}
\alias{cvpocre}
\title{
Use k-Fold Cross-Validation to Choose the Tuning Parameter for POCRE
}
\description{
Choose the optimal tuning parameter via k-fold cross-validation for POCRE.
}
\usage{
cvpocre(y, x, n.folds=10, delta=0.1, maxvar=dim(x)[1]/2,
        ptype=c('ebtz','ebt','l1','scad','mcp'), maxit=100,
        maxcmp=10, gamma=3.7, lambda.init=1, tol=1e-6,
        crit=c('press','Pearson','Spearman','Kendall'))
}
\arguments{
  \item{y}{
n*q matrix, values of q response variables (allow for multiple response variables).
}
  \item{x}{
n*p matrix, values of p predicting variables (excluding the intercept).
}
  \item{n.folds}{
number of folds to split the data (10-fold CV by default).
}
  \item{delta}{
step size of different values of the tuning parameter.
}
  \item{maxvar}{
maximum number of selected variables.
}
  \item{ptype}{
a character to indicate the type of penalty: \code{'ebtz'} (emprical Bayes thresholding after Fisher's z-transformation, by default), \code{'ebt'} (emprical Bayes thresholding by Johnstone & Silverman (2004)), \code{'l1'} (L_1 penalty), \code{'scad'} (SCAD by Fan & Li (2001)), \code{'mcp'} (MCP by Zhang (2010)).
}
  \item{maxit}{
maximum number of iterations to be allowed.
}
  \item{maxcmp}{
maximum number of components to be constructed.
}
  \item{gamma}{
a parameter used by SCAD and MCP (=3.7 by default).
}
  \item{lambda.init}{
initial value of the tuning parameter (=1 by default).
}
  \item{tol}{
tolerance of precision in iterations.
}
  \item{crit}{
a character to indicate the validation criterion: \code{'press'} (prediction residual error sum of squares, by default), \code{'Pearson'} (Pearson correlation coefficient), \code{'Spearman'} (Spearman's rank correlation coefficient), \code{'Kendall'} (Kendall's rank correlation coefficient).
}
}
\details{
Use k-folds cross-validation to find the optinal value for the tuning parameter. The validation criterion can be chosen from PRESS, or different types of correlation coefficients, such as Pearson's, Spearman's, or Kendall's.
}
\value{
{The optimal value of the tuning parameter.}
}
\references{
Fan J and Li R (2001). Variable selection via nonconcave penalized likelihood and its oracle properties. \emph{Journal of the American Statistical Association}, 96:1348-1360

Johnstone IM and Silverman BW (2004). Needles and straw in haystacks: empirical Bayes estimates of possibly sparse sequences. \emph{Annals of Statistics}, 32: 1594-1649.

Zhang C-H (2010). Nearly unbiased variable selection under minimax concave penalty. \emph{The Annals of Statistics}, 38: 894-942.

Zhang D, Lin Y, and Zhang M (2009). Penalized orthogonal-components regression for large p small n data. \emph{Electronic Journal of Statistics}, 3: 781-796.
}
\author{Dabao Zhang, Zhongli Jiang, Zeyu Zhang, Department of Statistics, Purdue University}
\seealso{
\code{\link{pocrescreen}}, \code{\link{pocrepath}}, \code{\link{pocre}}.
}
\examples{
\dontrun{
data(simdata)
n <- dim(simdata)[1]
xx <- simdata[,-1]
yy <- simdata[,1]

# tp <- cvpocre(yy,xx,delta=0.01)
tp <- cvpocre(yy,xx)
print(paste("  pocre: Optimal Tuning Parameter = ", tp))
cvpres <- pocre(yy,xx,lambda=tp,maxvar=n/log(n))
}
}
