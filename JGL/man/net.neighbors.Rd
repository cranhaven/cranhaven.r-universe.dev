\name{net.neighbors}
\alias{net.neighbors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get network neighbors of a node
}
\description{
For each class, returns the names of the nodes connected to a given node.
}
\usage{
net.neighbors(theta, index)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
A list of pXp matrices, each an estimated sparse inverse covariance matrix.  (For example, the result of FGL or GGL.)
}
  \item{index}{
The row number of the node to be investigated.
}
}

\value{
neighbors, a list of length K, each element of which is a vector of the row names of the nodes neighboring the node of interest.
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
fgl.results = JGL(Y=example.data,penalty="fused",lambda1=.25,lambda2=.1,return.whole.theta=TRUE)
## get neighbors of gene 195:
net.neighbors(fgl.results$theta,index=195)

}
