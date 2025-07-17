\name{net.edges}
\alias{net.edges}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
List the edges in a network
}
\description{
For each class, list every pair of connected nodes.
}
\usage{
net.edges(theta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
A list of pXp matrices, each an estimated sparse inverse covariance matrix.  (For example, the result of FGL or GGL.)
}
}

\value{
edges, a K-length list, each element of the list an igraph.es object detailing all pairs of connected nodes in the class.   
}
\references{
Patrick Danaher, Pei Wang and Daniela Witten (2011).  The joint graphical lasso for inverse covariance estimation across multiple classes.  http://arxiv.org/abs/1111.0324
}
\author{
Patrick Danaher
}

\examples{
## load an example dataset with K=two classes, p=200 features, and n=100 samples per class:
data(example.data)
str(example.data)
## run fgl:
fgl.results = JGL(Y=example.data,penalty="fused",lambda1=.25,lambda2=.1)
## get edges list:
net.edges(fgl.results$theta)

}