\name{mspeFHlin}
\alias{mspeFHlin}
\alias{mspeFHPR}
\alias{mspeFHDL}
\alias{mspeFHDRS}
\alias{mspeFHMPR}
\title{
Compute MSPE through linearization method for Fay Herriot model
}
\description{
This function returns MSPE estimator with linearization method for Fay Herriot model. These include the seminal Prasad-Rao method and its generalizations by Datta-Lahiri, Datta-Rao-Smith and Liu et.al. All these methods are developed for general linear mixed effects models
}
\usage{
mspeFHlin(formula, data, D, method = "PR", var.method = "default", na_rm, na_omit)

mspeFHPR(formula, data, D, var.method = "default", na_rm, na_omit)

mspeFHDL(formula, data, D, var.method = "default", na_rm, na_omit)

mspeFHDRS(formula, data, D, var.method = "default", na_rm, na_omit)

mspeFHMPR(formula, data, D, var.method = "default", na_rm, na_omit)

}

\arguments{
  \item{formula}{
(formula). Stands for the model formula that specifies the auxiliary variables to be used in the regression model. 
    This should follow the R model formula syntax.
}
  \item{data}{
(data frame). It represents the data containing the response values and auxiliary variables for the Nested Error Regression Model.
}
  \item{D}{
(vector). Stands for the known sampling variances of each small area levels.
}
  \item{method}{
The MSPE estimation method to be used. See "Details".
}
  \item{var.method}{
The variance component estimation method to be used. See "Details".
}
  \item{na_rm}{
  A logical value indicating whether to remove missing values (NaN) from the input matrices and vectors. 
  If \code{TRUE}, missing values in the input data (\code{X}, \code{Y}, and \code{D}) are automatically cleaned using internal functions.
  If \code{FALSE}, missing values are not removed. Defaults to \code{FALSE}.
}
  \item{na_omit}{
  A logical value indicating whether to stop the execution if missing values (NaN) are present in the input data. 
  If \code{TRUE}, the function will check for missing values in \code{X}, \code{Y}, and \code{D}. 
  If any missing values are found, an error message will be raised, prompting the user to handle the missing data before proceeding. 
  Defaults to \code{FALSE}.
}
}
\details{
Default \code{method} for \code{mspeFHlin} is "PR",proposed by N. G. N. Prasad and J. N. K. Rao, Prasad-Rao (PR) method uses Taylor series expansion to obtain a second-order approximation to the MSPE. Function \code{mspeFHlin} also provide the following methods:

Method "DL" proposed by Datta and Lahiri, It advanced PR method to cover the cases when the variance components are estimated by ML and REML estimator. Set \code{method = "DL"}.

Method "DRS" proposed by Datta and Smith, It focus on the second order unbiasedness appoximation when the variance component is replaced by Empirical Bayes estimator. Set \code{method = "DRS"}.

Method "MPR" is a modified version of "PR", It was proposed by Liu et al. It is a robust method that broaden the mean function from the linear form. Set \code{method = "MPR"}.

Default \code{var.method} and available variance component estimation method for each method is list as follows:

For \code{method = "PR"}, \code{var.method = "MOM"} is the only available variance component estimation method,

For \code{method = "DL"}, \code{var.method = "ML"} or \code{var.method = "REML"} is available,

For \code{method = "DRS"}, \code{var.method = "EB"} is the only available variance component estimation method,

For \code{method = "MPR"}, \code{var.method = "OBP"} is the only available variance component estimation method.

}
\value{
This function returns a list with components:
\item{MSPE}{(vector) MSPE estimates for Fay Herriot model.}
\item{bhat}{(vector) Estimates of the unknown regression coefficients.}
\item{Ahat}{(numeric) Estimates of the variance component.}
}

\references{
N. G. N. Prasad and J. N. K. Rao. The estimation of the mean squared error of small-area estimators. \emph{Journal of the American Statistical Association}, 85(409):163-171, 1990.

G. S. Datta and P. Lahiri. A unified measure of uncertainty of estimated best linear unbiased predictors in small area estimation problems. \emph{Statistica Sinica}, 10(2):613-627, 2000.

G. S. Datta and R. D. D. Smith. On measuring the variability of small area estimators under a basic area level model. \emph{Biometrika}, 92(1):183-196, 2005.

X. Liu, H. Ma, and J. Jiang. That prasad-rao is robust: Estimation of mean squared prediction error of observed best predictor under potential model misspecification. \emph{Statistica Sinica}, 2020.
}
\author{
Peiwen Xiao, Xiaohui Liu, Yu Zhang, Yuzi Liu, Jiming Jiang
}


\examples{
X = matrix(runif(10 * 3), 10, 3)
X[,1] = rep(1, 10) 
D = (1:10) / 10 + 0.5
Y = X \%*\% c(0.5,1,1.5) + rnorm(10, 0, sqrt(2)) + rnorm(10, 0, sqrt(D))
data <- data.frame(Y = Y, X1 = X[,2], X2 = X[,3])
formula <- Y ~ X1 + X2
result <- mspeFHlin(formula, data, D, method = "PR", var.method = "default")
}

