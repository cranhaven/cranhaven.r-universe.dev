\name{varner}
\alias{varner}
\title{
Estimates of the variance component using several methods for Nested error regression model.
}
\description{
This function returns the estimate of variance component with several existing method for Nested error regression model. This function does not accept missing values.
}
\usage{
varner(ni, formula, data, method, na_rm, na_omit)
}

\arguments{
  \item{ni}{
(vector). It represents the sample number for every small area.
}
  \item{formula}{
(formula). Stands for the model formula that specifies the auxiliary variables to be used in the regression model. 
    This should follow the R model formula syntax.
}
  \item{data}{
(data frame). It represents the data containing the response values and auxiliary variables for the Nested Error Regression Model.
}
  \item{method}{
The variance component estimation method to be used. See "Details".
}
  \item{na_rm}{
  A logical value indicating whether to remove missing values (NaN) from the input matrices and vectors. 
  If \code{TRUE}, missing values in the input data (\code{X}, \code{Y}, \code{D}, and \code{ni}) are automatically cleaned using internal functions.
  If \code{FALSE}, missing values are not removed. Defaults to \code{FALSE}.
}
  \item{na_omit}{
  A logical value indicating whether to stop the execution if missing values (NaN) are present in the input data. 
  If \code{TRUE}, the function will check for missing values in \code{X}, \code{Y}, \code{D}, and \code{ni}. 
  If any missing values are found, an error message will be raised, prompting the user to handle the missing data before proceeding. 
  Defaults to \code{FALSE}.
}
}
\value{
This function returns a list with components:
\item{bhat}{(vector) Estimates of the unknown regression coefficients.}
\item{sigvhat2}{(numeric) Estimates of the area-specific variance component.}
\item{sigehat2}{(numeric) Estimates of the random error variance component.}
}
\details{
Default value for \code{method} is 1, It represents the moment estimator, Also called ANOVA estimator, The available variance component estimation method are list as follows:

\code{method = 1} represents the MOM estimator; 

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
### parameter setting 
Ni <- 1000
sigmaX <- 1.5
m <- 10
beta <- c(0.5, 1)
sigma_v2 <- 0.8
sigma_e2 <- 1
ni <- sample(seq(1,10), m, replace = TRUE)
n <- sum(ni)
p <- length(beta)
### population function
pop.model <- function(Ni, sigmaX, beta, sigma_v2, sigma_e2, m) {
  x <- rnorm(m * Ni, 1, sqrt(sigmaX))
  v <- rnorm(m, 0, sqrt(sigma_v2))
  y <- numeric(m * Ni)
  theta <- numeric(m)
  kk <- 1
  for (i in 1:m) {
    sumx <- 0
    for (j in 1:Ni) {
      sumx <- sumx + x[kk]
      y[kk] <- beta[1] + beta[2] * x[kk] + v[i] + rnorm(1, 0, sqrt(sigma_e2))
      kk <- kk + 1
    }
    meanx <- sumx / Ni
    theta[i] <- beta[1] + beta[2] * meanx + v[i]
  }
  group <- rep(seq(m), each = Ni)
  x <- cbind(rep(1, m*Ni), x)
  data <- data.frame(y = y, group = group, x1 = x[,2])
  return(list(data = data, theta = theta))
}
### sample function
sampleXY <- function(Ni, ni, m, Population) {
  Indx <- c()
  for (i in 1:m) {
    Indx <- c(Indx, sample(c(((i - 1) * Ni + 1) : (i * Ni)), ni[i]))
  }
  Sample <- Population[Indx, ]
  return(Sample)
}

Population <- pop.model(Ni, sigmaX, beta, sigma_v2, sigma_e2, m)$data
XY <- sampleXY(Ni, ni, m, Population)

formula <- y ~ x1
data <- XY

result <- varner(ni, formula, data, method = 1)
}

