\name{net.degree}
\alias{net.degree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
List the degree of every node in all classes.  
}
\description{
For each class, lists the degree of every node.  
}
\usage{
net.degree(theta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
A list of pXp matrices, each an estimated sparse inverse covariance matrix.  (For example, the result of FGL or GGL.)
}
}

\value{
degree, a list of p-length vectors, each giving the degree of all p nodes in the network for the corresponding class.}
\references{
Patrick Danaher, Pei Wang and Daniela Witten (2011).  The joint graphical lasso for inverse covariance estimation across multiple classes.  http://arxiv.org/abs/1111.0324
}
\author{
Patrick Danaher}


\examples{
## load an example dataset with K=two classes, p=200 features, and n=100 samples per class:
data(example.data)
str(example.data)
## run fgl:
fgl.results = JGL(Y=example.data,penalty="fused",lambda1=.25,lambda2=.1)
## get degree list:
net.degree(fgl.results$theta)

}

