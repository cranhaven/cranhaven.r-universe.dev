\name{shock-package}
\alias{shock-package}
\alias{shock}
\docType{package}
\title{
Slope Heuristic for Block-Diagonal Covariance Selection in High Dimensional Gaussian Graphical Models
}
\description{
Block-diagonal covariance selection for high dimensional Gaussian graphical models. The selection procedure is based on the slope heuristics.
}
\details{
\tabular{ll}{
Package: \tab shock\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-11-07\cr
License: \tab GPL (>= 3)\cr
}
%~~ An overview of how to use the package, including the most important functions ~~
The function main function of the package (performShock) performs block-diagonal covariance selection for high-dimensional Gaussian graphical models.
}
\author{
Emilie Devijver, Melina Gallopin

Maintainer: Melina Gallopin <melina.gallopin@gmail.com>
}

\references{
Devijver, E., Gallopin, M. (2015). Block-diagonal covariance selection for high dimensional Gaussian graphical models. Inria Research Report . Available at \url{http://arxiv.org/abs/1511.04033}.
}


%\keyword{slope heuristic}
%\keyword{network inference}
%\keyword{model selection}

\examples{

## load data to test 
data(dataTest)

## dimension of the dataset expdata
n <- dim(dataTest)[1]
p <- dim(dataTest)[2]

## perform partition of variables selection
## based on the slope heuristic
resShock <- shockSelect(dataTest)


## verify that the two slope heuristic 
## calibrations give the same result
table(resShock$SHDJlabels == resShock$SHRRlabels)

## collect the labels of variables 
SHlabels  <- resShock$SHDJlabels

## SHadjaMat: adjacency matrix of the inferred network
## Shock network inference 
SHadjaMat<- diag(p)
for(itt in 1:length(unique(SHlabels))){  
  stepdata <- as.matrix(dataTest[,SHlabels==itt],nrow=dim(dataTest)[1])
  if(dim(stepdata)[2]>1){
    resNet <- networkInferenceGlassoBIC(stepdata)
    SHadjaMat[SHlabels==itt,SHlabels==itt] <- resNet$A
  }
}


}
