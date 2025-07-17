\name{net.hubs}
\alias{net.hubs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get degrees of most connected nodes.
}
\description{
List the degrees of the most connected nodes in each class.
}
\usage{
net.hubs(theta, nhubs = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
A list of pXp matrices, each an estimated sparse inverse covariance matrix.  (For example, the result of FGL or GGL.)
}
  \item{nhubs}{
The number of hubs to be identified.  net.hubs() will list the degree of the nhubs most connected nodes in each class.
}
}

\value{
hubs, a list of length K, each element of which is a vector giving the degree of the most connected nodes in the corresponding class.}
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
## get hubs list:
net.hubs(fgl.results$theta)
}
