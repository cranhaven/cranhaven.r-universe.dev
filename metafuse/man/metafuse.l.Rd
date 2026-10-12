\name{metafuse.l}
\alias{metafuse.l}
\title{fit a GLM with fusion penalty for data integraion, with a fixed lambda}
\usage{
metafuse.l(X = X, y = y, sid = sid, fuse.which = c(0:ncol(X)),
  family = "gaussian", intercept = TRUE, alpha = 0, lambda = lambda)
}
\arguments{
\item{X}{a matrix (or vector) of predictor(s), with dimensions of \code{N*(p-1)}, where \code{N} is the total sample size of the integrated dataset}

\item{y}{a vector of response, with length \code{N}; when \code{family="cox"}, \code{y} is a data frame with cloumns \code{time} and \code{status}}

\item{sid}{data source ID of length \code{N}, must contain integers numbered from 1 to \code{K}}

\item{fuse.which}{a vector of integers from 0 to \code{p-1}, indicating which covariates are considered for fusion; 0 corresponds to the intercept; coefficients of covariates not in this vector are homogeneously estimated across all datasets}

\item{family}{response vector type, \code{"gaussian"} if \code{y} is a continuous vector, \code{"binomial"} if \code{y} is binary vector, \code{"poisson"} if \code{y} is a count vector, \code{"cox"} if \code{y} is a data frame with cloumns \code{time} and \code{status}}

\item{intercept}{if \code{TRUE}, intercept will be included, default is \code{TRUE}}

\item{alpha}{the ratio of sparsity penalty to fusion penalty, default is 0 (i.e., no variable selection, only fusion)}

\item{lambda}{tuning parameter for fusion penalty}
}
\value{
A list containing the following items will be returned:
\item{family}{the response/model type}

\item{alpha}{the ratio of sparsity penalty to fusion penalty}

\item{if.fuse}{whether covariate is assumed to be heterogeneous (1) or homogeneous (0)}

\item{betahat}{the estimated regression coefficients}

\item{betainfo}{additional information about the fit, including degree of freedom, optimal lambda value, maximum lambda value to fuse all coefficients, and estimated friction of fusion}
}
\description{
Fit a GLM with fusion penalty on coefficients within each covariate at given lambda.
}
\details{
Adaptive lasso penalty is used. See Zou (2006) for detail.
}

\references{
Lu Tang, and Peter X.K. Song. Fused Lasso Approach in Regression Coefficients Clustering - Learning Parameter Heterogeneity in Data Integration. \emph{Journal of Machine Learning Research}, 17(113):1-23, 2016.\cr

Fei Wang, Lu Wang, and Peter X.K. Song. Fused lasso with the adaptation of parameter ordering in combining multiple studies with repeated measurements.  \emph{Biometrics}, DOI:10.1111/biom.12496, 2016. \cr
}
\examples{
########### generate data ###########
n <- 200    # sample size in each dataset (can also be a K-element vector)
K <- 10     # number of datasets for data integration
p <- 3      # number of covariates in X (including the intercept)

# the coefficient matrix of dimension K * p, used to specify the heterogeneous pattern
beta0 <- matrix(c(0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,   # beta_0 of intercept
                  0.0,0.0,0.0,0.0,0.0,1.0,1.0,1.0,1.0,1.0,   # beta_1 of X_1
                  0.0,0.0,0.0,0.0,0.5,0.5,0.5,1.0,1.0,1.0),  # beta_2 of X_2
                K, p)

# generate a data set, family=c("gaussian", "binomial", "poisson", "cox")
data <- datagenerator(n=n, beta0=beta0, family="gaussian", seed=123)

# prepare the input for metafuse
y       <- data$y
sid     <- data$group
X       <- data[,-c(1,ncol(data))]

########### run metafuse ###########
# fit metafuse at a given lambda
metafuse.l(X=X, y=y, sid=sid, fuse.which=c(0,1,2), family="gaussian", intercept=TRUE,
          alpha=1, lambda=0.5)
}

