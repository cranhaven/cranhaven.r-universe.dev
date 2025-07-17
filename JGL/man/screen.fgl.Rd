\name{screen.fgl}
\alias{screen.fgl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Quickly identify connected features in the solution to FGL
}
\description{
Applies the FGL screening rule to identify (before running FGL) which features are connected (have degree > 0 in any class) or unconnected in the solution.  
screen.fgl returns exactly the right list of connected nodes when K=2.  When K is larger than 2, screen.fgl applies a weaker condition that screens out many, but not all unconnected nodes.  
This algorithm is set up to be memory-efficient, but not fast: it can be applied to very large dimension datasets, but it will take time to run.
}
\usage{
screen.fgl(Y, lambda1, lambda2, weights = "equal")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
A list of nXp data matrices.
}
  \item{lambda1}{
The tuning parameter for the graphical lasso penalty.  Must be greater than or equal to 0.  
}
  \item{lambda2}{
The tuning parameter for the fused lasso penalty.  Must be greater than or equal to 0.  
}
  \item{weights}{
The weights to assign to each class.  The higher a class's weights, the weaker the effect of the penalties on its estimated inverse covariance matrix.  If "equal", the classes are weighted equally, regardless of sample size.  If "sample.size", the classes are weighted by sample size.  Custom weightings are achievable by entering a vector of K weights.  
}
}
\value{
connected, a logical vector identifying the connected nodes.
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
## which nodes will be connected?
screen.fgl(example.data,lambda1=.2,lambda2=.1,weights="equal")

}