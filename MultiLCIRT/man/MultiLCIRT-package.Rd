\name{MultiLCIRT-package}

\alias{MultiLCIRT-package}

\alias{MultiLCIRT}

\docType{package}

\title{Multidimensional Latent Class (LC) Item Response Theory (IRT) Models}

\description{This package provides a flexible framework for the Item Response Theory (IRT) analysis 
of dichotomous and ordinal polytomous outcomes 
under the assumption of multidimensionality and discreteness of latent traits (abilities). 
Every level of the abilities identify a latent class of subjects. The fitting 
algorithms are based on the Expectation-Maximization (EM) paradigm and 
allow for missing responses and for different item parameterizations. The package also allows for the inclusion
individual covariates affecting the class weights.}

\details{
\tabular{ll}{
Package: \tab MultiLCIRT\cr
Type: \tab Package\cr
Version: \tab 2.11\cr
Date: \tab 2017-05-19\cr
License: \tab GPL (>= 2)\cr
}
Function \code{est_multi_poly} performs the parameter estimation of the following IRT models,
allowing for one or more latent traits:

- Binary responses: Rasch model, 2-Parameter Logistic (2PL) model;

- Ordinal polythomous responses: Samejima's Graded Response Model (GRM) and 
constrained versions with fixed 
discrimination parameters and/or additive decomposition of difficulty parameters (rating scale
parameterization); 
Muraki's Generalized Partial Credit Model and constrained versions with fixed discrimination parameters and/or 
additive decomposition of difficulty parameters, such as Partial Credit Model and Rating Scale Model.

The basic input arguments for est_multi_poly are the person-item matrix of available response configurations 
and the corresponding frequencies, the number of latent classes, the type of link function, the specification of 
constraints on the discriminating and difficulty item parameters, and the allocation of items to the latent traits. 
Missing responses are coded with NA, and units and items without responses are automatically removed.

Function \code{test_dim} performs a likelihood ratio test to choose the optimal number of latent traits (or 
dimensions) by comparing nested models that differ in the number of latent traits, being all the other
elements let equal (i.e., number of latent classes, type of link function, constraints on item parameters). 
The basic input arguments for \code{test_dim} are similar as those for \code{est_multi_poly}.

Function \code{class_item} performs a hierarchical clustering of items based on a specified LC IRT model.
The basic input arguments are given by the number of latent classes, the type of model, and the constraints 
on the item parameters (only for polythomous responses). 
An allocation of items to the different latent traits is obtained
depending on the cut-point of the resulting dendrogram.
}
\author{Francesco Bartolucci, Silvia Bacci, Michela Gnaldi - University of Perugia (IT)

Maintainer: Francesco Bartolucci <bart@stat.unipg.it>
}
\references{
Bartolucci, F. (2007), A class of multidimensional IRT models for testing unidimensionality and clustering
items, \emph{Psychometrika}, \bold{72}, 141-157.

Bacci, S., Bartolucci, F. and Gnaldi, M. (2014), A class of Multidimensional Latent Class IRT models for 
ordinal polytomous item responses, \emph{Communication in Statistics - Theory and Methods}, \bold{43}, 787-800.

Bartolucci, F., Bacci, S. and Gnaldi, M. (2014), MultiLCIRT: An R package for multidimensional latent class item response models, \emph{Computational Statistics and Data Analysis}, \bold{71}, 971-985.
}
%~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory ~~
\keyword{ package }
%\seealso{
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
%}
\examples{
## Estimation of different Multidimensional LC IRT models with binary
## responses
# Aggregate data
data(naep)
X = as.matrix(naep)
out = aggr_data(X)
S = out$data_dis
yv = out$freq
# Define matrix to allocate each item on one dimension 
multi1 = rbind(c(1,2,9,10),c(3,5,8,11),c(4,6,7,12))
# Three-dimensional LC Rasch model with 4 latent classes
# less severe tolerance level to check convergence (to be modified)
out1 = est_multi_poly(S,yv,k=4,start=0,link=1,multi=multi1,tol=10^-6)
}
