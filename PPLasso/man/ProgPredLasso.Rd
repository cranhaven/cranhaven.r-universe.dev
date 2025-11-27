\name{ProgPredLasso}
\alias{ProgPredLasso}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Identification of prognostic and predictive biomarkers
}
\description{
The computes the regularization path of the Prognostic Predictive Lasso described in the paper Zhu et al. (2022) given in the references.
}
\usage{
ProgPredLasso(X1, X2, Y=Y, cor_matrix=NULL, gamma=0.99, maxsteps=500, lambda='single')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X1}{
Design matrix of patients characteristics with treatment 1
}
  \item{X2}{
Design matrix of patients characteristics with treatment 2
}
  \item{Y}{
Response variable
}
  \item{cor_matrix}{
Correlation matrix of biomarkers. If not specified, the function \code{\link{cvCovEst}} from package \code{\link{cvCovEst}} will be used to estimate this matrix.
}
  \item{gamma}{
Parameter \eqn{\gamma} defined in the paper Zhu et al. (2020) given in the references. Its default value is 0.99.}
  \item{maxsteps}{
Integer specifying the maximum number of steps for the generalized Lasso algorithm. Its default value is 500.}
  \item{lambda}{
  Using single tuning parameter or both.
  }

}

\value{
Returns a list with the following components
\item{lambda }{different values of the parameter \eqn{\lambda} considered.}
\item{beta }{matrix of the estimations of \eqn{\beta} for all the \eqn{\lambda} considered.}
\item{beta.min }{estimation of \eqn{\beta} which minimize the MSE.}
\item{bic }{BIC for all the \eqn{\lambda} considered.}
\item{mse }{MSE for all the \eqn{\lambda} considered.}
}

\author{
Wencan Zhu, Celine Levy-Leduc, Nils Ternes
}

