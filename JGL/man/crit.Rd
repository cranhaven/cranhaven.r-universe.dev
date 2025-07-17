\name{crit}
\alias{crit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the critical value of the FGL objective funciton.  
}
\description{
crit() calculates the critical value of the FGL objective funciton.  It is used to confirm that the FGL algorithm is converging.  
}
\usage{
crit(theta, S, n, lam1, lam2, penalize.diagonal)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
A list of pXp inverse covariance matrices.  
}
  \item{S}{
A list of pXp empirical covariance matrices.  
}
  \item{n}{
A vector of sample sizes to attribute to each of the K data matrices.  n controls the relative weights of the classes: for example, with n==c(1,1), each class's theta will be penalized equally.  
}
  \item{lam1}{
The tuning parameter for the graphical lasso penalty.
}
  \item{lam2}{
The tuning parameter for the fused lasso penalty.  
}
  \item{penalize.diagonal}{
Logical value determing whether the graphical lasso penalty should also be applied to the diagonal of the inverse covariance matrices.  
}
}
\details{
A function called by FGL to calculate the critical value of the objective function.  
}
\value{
crit, the critical value of the list of inverse covariance matrices.  
}
\references{
Patrick Danaher, Pei Wang and Daniela Witten (2011).  The joint graphical lasso for inverse covariance estimation across multiple classes.  http://arxiv.org/abs/1111.0324
}
\author{
Patrick Danaher
}
