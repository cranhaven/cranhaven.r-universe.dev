\name{mspeNERjack}
\alias{mspeNERjack}
\title{
Compute MSPE through Jackknife-based MSPE estimation method for Nested error regression model
}
\description{
This function returns MSPE estimator with Jackknife-based MSPE estimation method for Nested error regression model.
}
\usage{
mspeNERjack(ni, formula, data, Xmean, method = 1, na_rm, na_omit)
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
  \item{Xmean}{
(matrix). Stands for the population mean of auxiliary values.
}
  \item{method}{
The MSPE estimation method to be used. See "Details".
}
  \item{na_rm}{
  A logical value indicating whether to remove missing values (NaN) from the input matrices and vectors. 
  If \code{TRUE}, missing values in the input data (\code{X}, \code{Y}, \code{ni}, and \code{Xmean}) are automatically cleaned using internal functions.
  If \code{FALSE}, missing values are not removed. Defaults to \code{FALSE}.
}
  \item{na_omit}{
  A logical value indicating whether to stop the execution if missing values (NaN) are present in the input data. 
  If \code{TRUE}, the function will check for missing values in \code{X}, \code{Y}, \code{ni}, and \code{Xmean}. 
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
\item{MSPE}{(vector) MSPE estimates for NER model.}
\item{bhat}{(vector) Estimates of the unknown regression coefficients.}
\item{sigvhat2}{(numeric) Estimates of the area-specific variance component.}
\item{sigehat2}{(numeric) Estimates of the random error variance component.}
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
### parameter setting 
Ni <- 1000
sigmaX <- 1.5
m <- 5
beta <- c(0.5, 1)
sigma_v2 <- 0.8
sigma_e2 <- 1
ni <- sample(seq(1, 10), m, replace = TRUE)
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
  data <- data.frame(y = y, group = group, x1 = x)
  return(list(data = data, theta = theta))
} 

### sample function
sampleXY <- function(Ni, ni, m, Population) {
  Indx <- c()
  for (i in 1:m) {
    Indx <- c(Indx, sample(c(((i - 1) * Ni + 1):(i * Ni)), ni[i]))
  }
  Sample <- Population[Indx, ]
  return(Sample)
} 

### data generation process
Population <- pop.model(Ni, sigmaX, beta, sigma_v2, sigma_e2, m)$data
XY <- sampleXY(Ni, ni, m, Population)

### Creating formula and data frame
formula <- y ~ x1
data <- XY

### Compute group-wise means for X
Xmean <- matrix(NA, m, p)
for (tt in 1:m) {
  Xmean[tt, ] <- colMeans(Population[which(Population$group == tt), "x1", drop = FALSE])
}

result <- mspeNERjack(ni, formula, data, Xmean, method = 1)
}

