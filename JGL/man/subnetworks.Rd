\name{subnetworks}
\alias{subnetworks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Identify subnetwork membership
}
\description{
For each class, returns lists of all features belonging to subnetworks.  (A subnetwork is defined as a collection of features C for which theta[C,!C]==0, and within which no further subnetworks can be identified.  In other words, a block in the block diagonal structure of theta, or a set of features that can be connected through theta's edges.)
}
\usage{
subnetworks(theta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
A list of pXp matrices, each an estimated sparse inverse covariance matrix.  (For example, the result of FGL or GGL.)
}
}

\value{
A list length K, each element of which is a list of subnetworks in class K.  Each subnetwork is represented as a vector of feature names.}
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
## identify subnetworks
subnetworks(fgl.results$theta)
}
