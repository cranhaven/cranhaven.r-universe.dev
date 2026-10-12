\name{summary.MED}
\alias{summary.MED}
\alias{print.summary.MED}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summarizing output of study.
}
\description{
\code{summary} method for class \code{"MED"}
}
\usage{
\method{summary}{MED}(object, ...)

\method{print}{summary.MED}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
An object of class \code{"MED"}, usually a result of a call 
to \code{\link{MED}}.
}
\item{x}{
An object of class \code{"summary.MED"}, usually a result of 
a call to \code{\link{summary.MED}}.
}
  \item{\dots}{
Further arguments passed to or from methods.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
\code{print.summary.MED} prints a simplified output similar to \code{\link{print.summary.lm}}. The resulting table provides the point estimates, estimated standard errors, 95\% Wald confidence intervals, the Z-statistic and the P-values for a Z-test.
}
\value{
The function \code{summary.MED} returns a list with the following components
\item{Estimate}{
A matrix with point estimates along with standard errors, confidence intervals etc. This is the matrix users see with the \code{print.summary.RIPW} function.
}
\item{vcov}{
The variance-covariance matrix of the point estimates.
}
\item{Conv}{
The convergence result of the \code{object}.
}
\item{weights}{
The weights for each subject in each treatment arm. These are same as the \code{weight} component of the \code{"RIPW" object}.
}
\item{call}{
The \code{call} passed on as an argument of the function which is equivalent to \code{object$call}.
}
}
\author{
Gary Chan
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{MED}}
}
\examples{
library(MED)
#binary treatment and binary mediator

set.seed(25)
n <- 200
Z <- matrix(rnorm(4*n),ncol=4,nrow=n)
prop.e <- 1 / (1 + exp(Z[,1] - 0.5 * Z[,2] + 0.25*Z[,3] + 0.1 * Z[,4]))
treat <- rbinom(n, 1, prop.e)
prop.m <- 1 / (1 + exp(-(0.5 - Z[,1] + 0.5 * Z[,2] - 0.9 *Z [,3] + Z[,4] - 1.5 * treat)))
M <- rbinom(n, 1, prop.m)
Y <- 200 + treat + M + 27.4*Z[,1] + 13.7*Z[,2] +
          13.7*Z[,3] + 13.7*Z[,4] + rnorm(n)
X <- cbind(exp(Z[,1])/2,Z[,2]/(1+exp(Z[,1])),
          (Z[,1]*Z[,3]/25+0.6)^3,(Z[,2]+Z[,4]+20)^2)

#estimation of natural mediation effecs
fit1<-MED(Y,treat,M,X)
summary(fit1)

}
