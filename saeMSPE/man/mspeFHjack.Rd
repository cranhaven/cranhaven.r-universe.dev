\name{mspeFHjack}
\alias{mspeFHjack}
\title{
Compute MSPE through Jackknife-based MSPE estimation method for Fay Herriot model
}
\description{
This function returns MSPE estimator with jackknife method for Fay Herriot model.
}
\usage{
mspeFHjack(formula, data, D, method = 1, na_rm, na_omit)
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
This bias-corrected jackknife MSPE estimator was proposed by J. Jiang and L. S. M. Wan, it covers a fairly general class of mixed models which includes gLMM, mixed logistic model and some of the widely used mixed linear models as special cases.

Default value for \code{method} is 1, \code{method = 1} represents the MOM method, \code{method = 2} and \code{method = 3} represents ML and REML method, respectively.

}

\value{
This function returns a list with components:
\item{MSPE}{(vector) MSPE estimates for Fay Herriot model.}
\item{bhat}{(vector) Estimates of the unknown regression coefficients.}
\item{Ahat}{(numeric) Estimates of the variance component.}
}

\references{
M. H. Quenouille. Approximate tests of correlation in time series. \emph{Journal of the Royal Statistical Society. Series B (Methodological)}, 11(1):68-84, 1949.

J. W. Tukey. Bias and confidence in not quite large samples. \emph{Annals of Mathematical Statistics}, 29(2):614, 1958.

J. Jiang and L. S. M. Wan. A unified jackknife theory for empirical best prediction with m estimation. \emph{Annals of Statistics}, 30(6):1782-1810, 2002.

}

\author{
Peiwen Xiao, Xiaohui Liu, Yu Zhang, Yuzi Liu, Jiming Jiang
}


\examples{
X <- matrix(runif(10 * 3), 10, 3)
X[,1] <- rep(1, 10) 
D <- (1:10) / 10 + 0.5
Y <- X \%*\% c(0.5, 1, 1.5) + rnorm(10, 0, sqrt(2)) + rnorm(10, 0, sqrt(D))
data <- data.frame(Y = Y, X1 = X[,2], X2 = X[,3])
formula <- Y ~ X1 + X2
result <- mspeFHjack(formula, data, D, method = 1)
}

