\name{cv.spfac}
\alias{cv.spfac}
\title{Cross validation for selecting penalty parameters}
\description{
  Evalute the CV values on a set of grids of penalty parameters.
}
\usage{
  cv.spfac(X, lambda1_set, lambda2_set, nfolds=5)
}
\arguments{
  \item{X}{a \code{n}-by-\code{p} matrix, the observed data}
  \item{lambda1_set}{a positve vector, the grid for lambda_1.}
  \item{lambda2_set}{a positve vector, the grid for lambda_2.}
  \item{nfolds}{a positve integer, the folds of cross validation.}
}

\value{
  return a list including following components:

    \item{lamcv.min}{a 3-dimensional vector, the penalty value for lambda_1 and lambda_2 corresponding to the minimum CV on grids.}
  \item{lamcvMat}{a numeric matrix with three columns named lambda_1, lambda_2 and cv, where each row is corresponding to each grid.}
  \item{lambda1_set}{the used grid for lambda_1.}
  \item{lambda2_set}{the used grid for lambda_2.}
}
\references{Wei Liu, Huazhen Lin, (2019). Estimation and inference on high-dimensional sparse factor models.
}
\author{
  Liu Wei
}
\note{
  nothing
}
\seealso{
  \code{\link{gsspFactorm}}.
}
\examples{
  datlist1 <- gendata_Fac(n= 100, p = 300, rho=1)
  X <- datlist1$X
  spfac <- gsspFactorm(X, q=NULL)  # use default values for lambda's.
  assessBsFun(spfac$sphB, datlist1$B0)
  lambda1_set <- seq(0.2, 2, by=0.3)
  lambda2_set <- 1:8
  # select lambda's values using CV method.
  lamList <- cv.spfac(X, lambda1_set, lambda2_set, nfolds=5)
  spfac <- gsspFactorm(X, q=NULL,lamList$lamcv.min[1], lamList$lamcv.min[2])
  assessBsFun(spfac$sphB, datlist1$B0)

}
