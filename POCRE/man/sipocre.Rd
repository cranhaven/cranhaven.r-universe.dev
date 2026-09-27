\name{sipocre}
\alias{sipocre}
\title{
Penalized Orthogonal-Components Regression (POCRE) with Significance Inference
}
\description{
Applying POCRE to select variables and evaluate the significance of selected variables using the multiple splitting method by Meinshausen et al. (2009). The tuning parameter may be selected based on either an information criterion or k-fold cross-validation. The tuning parameter can also be fixed at a prespecified value.
}
\usage{
sipocre(y, x, n.splits=10, delta=0.1, crit=c('ic','cv','fixed'),
        ptype=c('ebtz','ebt','l1','scad','mcp'), maxvar=dim(x)[1]/2,
        msc=NA, maxit=100, maxcmp=50, gamma=3.7, tol=1e-6,
        n.folds=10, lambda=1, n.train=round(nrow(x)/2))
}
\arguments{
  \item{y}{
n*q matrix, values of q response variables (allow for multiple response variables).
}
  \item{x}{
n*p matrix, values of p predicting variables (excluding the intercept).
}
  \item{n.splits}{
number of random splits (=10 by default).
}
  \item{delta}{
step size to increase or decrase from current tuning parameter.
}
  \item{crit}{
character indicating the criterion to choose the tuning parameter: \code{'ic'} (information criteria such as AIC, AICc, BIC, EBIC), \code{'cv'} (k-folds cross-valdiation) or \code{'fixed'} (a pre-specified value).
}
  \item{ptype}{
a character to indicate the type of penalty: \code{'ebtz'} (emprical Bayes thresholding after Fisher's z-transformation, by default), \code{'ebt'} (emprical Bayes thresholding by Johnstone & Silverman (2004)), \code{'l1'} (L_1 penalty), \code{'scad'} (SCAD by Fan & Li (2001)), \code{'mcp'} (MCP by Zhang (2010)).
}
  \item{maxvar}{
maximum number of selected variables.
}
  \item{msc}{
value(s) to indicate the penalty related to the information criterion: 0~1 for (E)BIC, 2 for AIC, 3 for AICc, used when \code{crit='ic'}.
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
  \item{tol}{
tolerance of precision in iterations.
}
  \item{n.folds}{
number of folds in k-folds cross-validation, used when \code{crit='cv'}.
}
  \item{lambda}{
pre-sepcified value for the tuning parameter, used when \code{crit='fixed'}.
}
  \item{n.train}{
sample size of the training data set.
}
}
\value{
a list consisting of the following components,
  \item{cpv}{
component-based p-values which are calculated by testing the constructed components, either a matrix (when \code{crit='ic'}, in this case each column corresponds to one value in msc) or a vector (when \code{crit='cv'} or \code{crit='fixed'}).
}
  \item{xpv}{
traditional p-values, either a matrix (when \code{crit='ic'}, in this case each column corresponds to one value in msc) or a vector (when \code{crit='cv'} or \code{crit='fixed'}).
}
}
\references{
Fan J and Li R (2001). Variable selection via nonconcave penalized likelihood and its oracle properties. \emph{Journal of the American Statistical Association}, 96:1348-1360

Johnstone IM and Silverman BW (2004). Needles and straw in haystacks: empirical Bayes estimates of possibly sparse sequences. \emph{Annals of Statistics}, 32: 1594-1649.

Meinshausen N, Meier L, and Buhlmann P (2009) p-Values for High-Dimensional Regression. \emph{Journal of the American Statistical Association}, 104: 1671-1681.

Zhang C-H (2010). Nearly unbiased variable selection under minimax concave penalty. \emph{The Annals of Statistics}, 38: 894-942.

Zhang D, Lin Y, and Zhang M (2009). Penalized orthogonal-components regression for large p small n data. \emph{Electronic Journal of Statistics}, 3: 781-796.
}
\author{Dabao Zhang, Zhongli Jiang, Zeyu Zhang, Department of Statistics, Purdue University}
\seealso{
\code{\link{pocre}}.
}
\examples{
\dontrun{
data(simdata)
xx <- simdata[,-1]
yy <- simdata[,1]

sipres <- sipocre(yy,xx)
}
}
