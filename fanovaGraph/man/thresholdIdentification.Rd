\name{thresholdIdentification}
\alias{thresholdIdentification}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to identify a suitable threshold for an \code{estimateGraph} object
}
\description{
From an \code{estimateGraph} object and a corresponding data set, candidate treshold values are compared on the prediction performance of the corresponding additive Kriging model. The candidate thresholds are chosen by the biggest jumps in \code{plotDeltaJumps} together with 0 (the full model) and 1 (the complete additive model). For each of them the Kriging model with corresponding kernel is estimated and the leave-one-out
crossvalidiations on the original data sets are compared on scatterplots and RMSE-values. 

}
\usage{
thresholdIdentification(g, x, y, n.cand = 3, covtype = "matern5_2", KM = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{object of class \code{graphlist} as obtained from \code{estimateGraph}}
  \item{x}{design matrix of input variables corresponding to \code{g}, number of columns should be number of variables}
  \item{y}{vector of output variables of the same length as the columns of \code{x}}
  \item{n.cand}{integer, the \code{n.cand} biggest jumps are chosen as candidate threshold values. The default value is \code{3}}
  \item{covtype}{optional character string specifying the covariance structure to be used. The default is \code{"matern5_2"}}
  \item{KM}{optional object of class \code{km}, the full kriging model corresponding to \code{g}. With default value \code{NULL}, this kriging model is computed by the function itself}
}

\value{
 a list including
 \item{delta}{vector of threshold candidates}
 \item{models}{list of full model and models with applied thresholds}
 \item{y.cv}{list of vectors containing crossvalidation predictions for each model}
 \item{RMSE}{vector of residual mean squared errors for each model}
}

\author{
J. Fruth, M. Jastrow
}

\seealso{
\code{\link{plotDeltaJumps}}, \code{\link{plotGraphChange}}
}

\examples{
############ simple 3-dimensional example with one interaction

### data (usually existing)
x <- matrix(seq(0,1,,20), 20, 3)
x <- apply(x,2,sample)
y <- 2*(x[,1]-0.5) * (x[,2]-0.5) + 0.1*sin(10*x[,3])

### FANVOA graph (usually estimated from a meta model over the data)
g <- list(d=3, 
  tii = matrix(c(0.0140, 0.0008, 0.0002)),
  V = 0.0222, 
  tii.scaled = matrix(c(0.6976, 0.0432, 0.0113))
)
class(g) <- "graphlist"

### plot complete graph
plot(g, plot.i1=FALSE)

### Compare candidate thresholds on prediction performance
set.seed(1)
comparison <- thresholdIdentification(g, x, y, n.cand = 1)}
