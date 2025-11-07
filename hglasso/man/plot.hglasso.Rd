\name{plot.hglasso}
\alias{plot.hglasso}
\alias{plot.hcov}
\alias{plot.hbn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot an object of class \code{hglasso}, \code{hcov}, or \code{hbn}
}
\description{
This function plots an object hglasso or hcov --- graphical representation of the estimated inverse covariance matrix from \code{\link{hglasso}}, covariance matrix from \code{\link{hcov}}, or binary network from \code{\link{hbn}}
}
\usage{
\method{plot}{hglasso}(x, layout=NULL,\dots) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
an object of class \code{\link{hglasso}}, \code{\link{hcov}}, or \code{\link{hbn}}.
}
  \item{layout}{
the layout of the graph to use.  If not specified, \code{\link{layout.kamada.kawai}} is used.
}
  \item{\dots}{
additional parameters to be passed to \code{\link{plot.igraph}}.
}

}
\details{
This function plots a graphical representation of the estimated inverse covariance matrix or covariance matrix.  The hubs are colored in red and has a large vertex size. Features indices for hubs are shown.     
}

\references{
Tan et al. (2014). Learning graphical models with hubs. To appear in Journal of Machine Learning Research. arXiv.org/pdf/1402.7349.pdf.
}
\author{
Kean Ming Tan 
}


\seealso{
\code{\link{image.hglasso}}
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
res1 <- hglasso(cov(x),0.3,0.3,1.5)

# Graphical representation of the estimated Theta
plot(res1,main="conditional independence graph")

}

