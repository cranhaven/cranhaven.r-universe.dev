\name{image.hglasso}
\alias{image.hglasso}
\alias{image.hcov}
\alias{image.hbn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Image plot of an object of class \code{hglasso}, \code{hcov}, or \code{hbn}
}
\description{
This function plots a hglasso or hcov --- the estimated matrix V and Z from \code{\link{hglasso}}, \code{\link{hcov}}, or \code{\link{hbn}}
}
\usage{
\method{image}{hglasso}(x, \dots) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
an object of class hglasso, hcov, or hbn.
}
  \item{\dots}{
additional parameters to be passed to \code{\link{image}}.
}
}
\details{
The estimated inverse covariance matrix from \code{\link{hglasso}},  covariance matrix from \code{\link{hcov}}, and estimated binary network \code{\link{hbn}} can be decomposed as Z + V + t(V), where V is a matrix that contains hub nodes.  This function creates image plots of Z and V.   
}
\references{
Tan et al. (2014). Learning graphical models with hubs. To appear in Journal of Machine Learning Research. arXiv.org/pdf/1402.7349.pdf.
}
\author{
Kean Ming Tan
}

\seealso{
\code{\link{plot.hglasso}}
\code{\link{summary.hglasso}}
\code{\link{hglasso}}
\code{\link{hcov}}
\code{\link{hbn}}
}
\examples{
##############################################
# Example from Figure 1 in the manuscript
# A toy example to illustrate the results from 
# Hub Graphical Lasso
##############################################
library(mvtnorm)
set.seed(1)
n=100
p=100

# A network with 4 hubs
Theta<-HubNetwork(p,0.99,4,0.1)$Theta

# Generate data matrix x
x <- rmvnorm(n,rep(0,p),solve(Theta))
x <- scale(x)

# Run Hub Graphical Lasso to estimate the inverse covariance matrix
res1 <- hglasso(cov(x),0.3,0.2,2)

# image plots for the matrix V and Z
image(res1)
dev.off()

}

