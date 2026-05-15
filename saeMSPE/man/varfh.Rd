\name{varfh}
\alias{varfh}
\alias{varOBP}
\title{
Estimates of the variance component using several methods for Fay Herriot model
}
\description{
This function returns the estimate of variance component with several existing method for Fay Herriot model. This function does not accept missing values
}
\usage{
varfh(formula, data, D, method, na_rm, na_omit)
varOBP(formula, data, D, na_rm, na_omit)
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
(vector). It represents the knowing sampling variance for Fay Herriot model.
}
  \item{method}{
Variance component estimation method. See "Details".
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
\value{
This function returns a list with components:
\item{bhat}{(vector) Estimates of the unknown regression coefficients.}
\item{Ahat}{(numeric) Estimates of the variance component.}
}
\details{
Default value for \code{method} is 1, It represents the moment estimator, Also called ANOVA estimator, The available variance component estimation method are list as follows:

\code{method = 1} represents the moment (MOM) estimator, ; 

\code{method = 2} represents the restricted maximum likelihood (REML) estimator; 

\code{method = 3} represents the maximum likelihood (ML) estimator;  

\code{method = 4} represents the empirical bayesian (EB) estimator;  

}

\references{
J. Jiang. Linear and Generalized Linear Mixed Models and Their Applications. 2007.
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
result <- varfh(formula, data, D, method = 1)
}

