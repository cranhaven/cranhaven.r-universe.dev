\name{gps}
\alias{gps}
\title{
Screen Variables for Generalized Linear Models via Generalized POCRE
}
\description{
A pre-specified number (i.e., maxvar) of covariates will be selected for generalized linear models by constructing maxcmp components with generalized POCRE. Each component will be constructed by selecting maxvar/macmp covariates which are most relevant to the response variable(s). Similar to \link{pocrescreen}, gps selects covariates for their top relevance to the response variable(s) without penalization.
}
\usage{
gps(y, x, family="binomial", bc.method="optimal", x.include=NULL,
    weights=NULL, maxcmp=10, maxvar=NULL, tol = 1e-6, maxit = 100)
}
\arguments{
  \item{y}{
    n*q matrix, values of q response variables (allow for multiple response variables).
  }
  \item{x}{
    n*p matrix, values of p predicting variables (excluding the intercept).
  }
  \item{family}{
    Family objects as \code{\link{family}}. Currently only support \code{"gaussian"}, \code{"binomial"} (by default), and \code{"poisson"}.
  }  
  \item{bc.method}{
    Bias correction method.
  } 
  \item{x.include}{
    a vector of indices indicating covariates which should always be included in the model (so not counted into selected maxvar covariates).
  }
  \item{weights}{
    A vector, including a prespecified weight for each observation (set as 1/n by default).
  }
  \item{maxcmp}{
    maximum number of components to be constructed.
  }
  \item{maxvar}{
    maximum number of selected variables.
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
Zhang D, Lin Y, and Zhang M (2009). Penalized orthogonal-components regression for large p small n data. \emph{Electronic Journal of Statistics}, 3: 781-796.
}
\author{Dabao Zhang, Zhongli Jiang, Yu-ting Chen, Department of Statistics, Purdue University
}
\seealso{
  \code{\link{pocrescreen}}.
}
\examples{
  # Binomial Data
  data(simbin)
  gps(simbin[,1], simbin[,-1], maxcmp=3, maxvar=9)
  gps(simbin[,1], simbin[,-1], x.include=103:104, maxcmp=3, maxvar=9)
 
 # Count Data
  data(simpoi)
  gps(simpoi[,1], simpoi[,-1], family='poisson',maxcmp=5,maxvar=10)
}
