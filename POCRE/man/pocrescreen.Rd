\name{pocrescreen}
\alias{pocrescreen}
\title{
Screen Variables Using Penalized Orthogonal-Components Regression (POCRE)
}
\description{
Screen for a pre-specified number (i.e., maxvar) of covariates by constructing maxcmp components with POCRE. Each component will be constructed by selecting maxvar/macmp covariates which are most relevant to the response variable(s). Here POCRE selects covariates for their top relevance to the response variable(s) without penalization.
}
\usage{
pocrescreen(y, x, maxvar=nrow(x), maxcmp=5, x.include=NULL,
            tol=1e-6, maxit=100)
}
\arguments{
  \item{y}{
n*q matrix, values of q response variables (allow for multiple response variables).
}
  \item{x}{
n*p matrix, values of p predicting variables (excluding the intercept).
}
  \item{maxvar}{
maximum number of selected variables.
}
  \item{maxcmp}{
maximum number of components to be constructed.
}
  \item{x.include}{
a vector of indices indicating covariates which should always be included in the model (so not counted into selected maxvar covariates).
}
  \item{tol}{
tolerance of precision in iterations.
}
  \item{maxit}{
maximum number of iterations to be allowed.
}
}
\value{
a vector of indices of selected covariates (excluding those in x.include).
}
\references{
Zhang D (2018). R package POCRE: Exploring high-dimensional data via supervised dimension reduction. Manuscript.

Zhang D, Lin Y, and Zhang M (2009). Penalized orthogonal-components regression for large p small n data. \emph{Electronic Journal of Statistics}, 3: 781-796.
}
\author{Dabao Zhang, Zhongli Jiang, Zeyu Zhang, Department of Statistics, Purdue University}
\seealso{
\code{\link{pocre}}, \code{\link{pocrepath}}, \code{\link{cvpocre}}.
}
\examples{
data(simdata)
xx <- simdata[,-1]
yy <- simdata[,1]

# Screen for 50 covariates
sidx <- pocrescreen(yy,xx,maxvar=50)

# Screen for 50 additional covariates besides the first 10
xinc <- 1:10
sidx <- pocrescreen(yy,xx,maxvar=50,x.include=xinc)
sidx <- c(xinc,sidx)
}
