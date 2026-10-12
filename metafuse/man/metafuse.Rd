\name{metafuse}
\alias{metafuse}
\title{fit a GLM with fusion penalty for data integraion}
\usage{
metafuse(X = X, y = y, sid = sid, fuse.which = c(0:ncol(X)),
  family = "gaussian", intercept = TRUE, alpha = 0, criterion = "EBIC",
  verbose = TRUE, plots = FALSE, loglambda = TRUE)
}
\arguments{
\item{X}{a matrix (or vector) of predictor(s), with dimensions of \code{N*(p-1)}, where \code{N} is the total sample size of the integrated dataset}

\item{y}{a vector of response, with length \code{N}; when \code{family="cox"}, \code{y} is a data frame with cloumns \code{time} and \code{status}}

\item{sid}{data source ID of length \code{N}, must contain integers numbered from 1 to \code{K}}

\item{fuse.which}{a vector of integers from 0 to \code{p-1}, indicating which covariates are considered for fusion; 0 corresponds to the intercept; coefficients of covariates not in this vector are homogeneously estimated across all datasets}

\item{family}{response vector type, \code{"gaussian"} if \code{y} is a continuous vector, \code{"binomial"} if \code{y} is binary vector, \code{"poisson"} if \code{y} is a count vector, \code{"cox"} if \code{y} is a data frame with cloumns \code{time} and \code{status}}

\item{intercept}{if \code{TRUE}, intercept will be included, default is \code{TRUE}}

\item{alpha}{the ratio of sparsity penalty to fusion penalty, default is 0 (i.e., no variable selection, only fusion)}

\item{criterion}{\code{"AIC"} for AIC, \code{"BIC"} for BIC, \code{"EBIC"} for extended BIC,default is \code{"BIC"}}

\item{verbose}{if \code{TRUE}, outputs whenever a fusion event happens, and returns the current value of lambda, default is \code{TRUE}}

\item{plots}{if \code{TRUE}, create solution paths and fusogram plots to visualize the clustering of regression coefficients across datasets, default is \code{FALSE}}

\item{loglambda}{if \code{TRUE}, lambda will be plotted in log-10 scale, default is \code{TRUE}}
}
\value{
A list containing the following items will be returned:
\item{family}{the response/model type}

\item{criterion}{model selection criterion used}

\item{alpha}{the ratio of sparsity penalty to fusion penalty}

\item{if.fuse}{whether covariate is assumed to be heterogeneous (1) or homogeneous (0)}

\item{betahat}{the estimated regression coefficients}

\item{betainfo}{additional information about the fit, including degree of freedom, optimal lambda value, maximum lambda value to fuse all coefficients, and estimated friction of fusion}
}
\description{
Fit a GLM with fusion penalty on coefficients within each covariate across datasets, generate solution path and fusograms for visualization of the model selection.
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
# fuse slopes of X1 (which is heterogeneous with 2 clusters)
metafuse(X=X, y=y, sid=sid, fuse.which=c(1), family="gaussian", intercept=TRUE, alpha=0,
          criterion="EBIC", verbose=TRUE, plots=TRUE, loglambda=TRUE)

# fuse slopes of X2 (which is heterogeneous with 3 clusters)
metafuse(X=X, y=y, sid=sid, fuse.which=c(2), family="gaussian", intercept=TRUE, alpha=0,
          criterion="EBIC", verbose=TRUE, plots=TRUE, loglambda=TRUE)

# fuse all three covariates
metafuse(X=X, y=y, sid=sid, fuse.which=c(0,1,2), family="gaussian", intercept=TRUE, alpha=0,
          criterion="EBIC", verbose=TRUE, plots=TRUE, loglambda=TRUE)

# fuse all three covariates, with sparsity penalty
metafuse(X=X, y=y, sid=sid, fuse.which=c(0,1,2), family="gaussian", intercept=TRUE, alpha=1,
          criterion="EBIC", verbose=TRUE, plots=TRUE, loglambda=TRUE)
}
