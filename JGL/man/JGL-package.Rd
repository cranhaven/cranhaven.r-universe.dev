\name{JGL-package}
\alias{JGL-package}
\docType{package}
\title{
Joint Graphical Lasso
}
\description{
Runs the Fused Graphical Lasso and the Group Graphical Lasso for network estimation and sparse inverse covariance estimation across multiple classes of data.  
}
\details{

The Fused Graphical Lasso (FGL) and the Group Graphical Lasso (GGL) are two methods for estimating sparse inverse covariance matrices that are similar across classes.  
A motivating example is the analysis of gene expression data from tumor and healthy cells: FGL and GGL allow joint estimation of gene expression conditional dependency networks in both cancer and healthy cells
FGL is recommended over GGL for most purposes.  The function JGL can implement either of these methods.

The JGL package includes a number of functions to help analyze estimated networks: subnetworks(), net.degree(), net.edges(), net.hubs(), net.neighbors(), print.jgl() and plot.jgl().  These functions rely on the igraph package.  

A large number of other functions are called by the above functions, and are not generally useful when called by the user.  
}
\author{
Patrick Danaher

Maintainer: Patrick Danaher - pdanaher at uw dot edu
}
\references{
Patrick Danaher, Pei Wang and Daniela Witten (2011).  The joint graphical lasso for inverse covariance estimation across multiple classes.  http://arxiv.org/abs/1111.0324
}
\keyword{ package }
\examples{
## load an example dataset with K=two classes, p=200 features, and n=100 samples per class:
data(example.data)
str(example.data)
## run FGL:
fgl.results = JGL(Y=example.data,penalty="fused",lambda1=.25,lambda2=.1)
str(fgl.results)
print.jgl(fgl.results)
## get subnetwork membership of FGL results:
subnetworks(fgl.results$theta)
}
