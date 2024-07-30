\name{wald.coef}
\alias{wald.coef}

\title{Wald statistic for variable selection in generalized linear models} 
\description{
Computes the value of Wald's statistic, testing the significance of the excluded 
variables, in the context of variable subset selection in generalized linear models
}

\details{

Variable selection in the context of generalized linear models is typically based
on the minimization of statistics that test the significance of excluded variables. 
In particular, the likelihood ratio, Wald's, Rao's and some adaptations
of such statistics, are often proposed as comparison criteria for variable subsets
of the same dimensionality. All these statistics are assympotically equivalent and
can be converted into information criteria, like the AIC, that are also able to compare 
subsets of different dimensionalities (see references [1] and [2] for further 
details).

Among these criteria, Wald's statistic has some computational advantages because it can
always be derived from the same (concerning the full model) maximum likelihood and Fisher 
information estimates. In particular, if \eqn{W_{allv}} is the value of
the Wald statistic testing
the significance of the full covariate vector, b and FI are coefficient and Fisher 
information estimates and H is an auxiliary rank-one matrix given by
\code{H =  FI \%*\% b \%*\% t(b) \%*\% FI}, it follows that the value of Wald's statistic for the 
excluded variables (\eqn{W_{excv}}) in a given subset  is given by 
\eqn{W_{excv} = W_{allv} - tr (FI_{indices}^{-1} H_{indices}) ,} where
\eqn{FI_{indices}} and \eqn{H_{indices}} are the  
portions of the FI and H matrices associated with the selected variables.

The FI and H matrices can be retrieved (from a glm object) by the \code{\link{glmHmat}}
function and may be used as input to the search functions \code{\link{anneal}}, 
\code{\link{genetic}}, \code{\link{improve}} and \code{\link{eleaps}}. The Wald function
computes the value of Wald statistc from these matrices for a subset specified by \code{indices}  

The fact that \code{indices} can be a matrix or 3-d array allows for
the computation of the Wald statistic values of subsets produced by the search
functions \code{\link{anneal}}, \code{\link{genetic}}, 
\code{\link{improve}} and
\code{\link{eleaps}} (whose output option \code{$subsets} are
matrices or 3-d arrays), using a different criterion (see the example
below).

}

\usage{wald.coef(mat, H, indices,
tolval=10*.Machine$double.eps, tolsym=1000*.Machine$double.eps)} 

\arguments{
  \item{mat}{An estimate (FI) of Fisher's information matrix for the 
  full model variable-coefficient estimates}

  \item{H}{A matrix product of the form \code{FI \%*\% b \%*\% t(b) \%*\% FI} 
  where b is a vector of variable-coefficient estimates}
 
  \item{indices}{a numerical vector, matrix or 3-d array of integers
  giving the indices of the variables in the subset. If a matrix is
  specified, each row is taken to represent a different
  \emph{k}-variable subset. If a 3-d array is given, it is assumed
  that the third dimension corresponds to different cardinalities.}

  \item{tolval}{the tolerance level to be used in checks for 
   ill-conditioning and positive-definiteness of the Fisher Information and
   the auxiliar (H) matrices. Values smaller than \code{tolval} are
   considered equivalent to zero.}

 \item{tolsym}{the tolerance level for symmetry of the
   Fisher Information and the auxiliar (H) matrices. If corresponding 
   matrix entries differ by more than this
   value, the input matrices will be considered asymmetric and execution
   will be aborted. If corresponding entries are different, but by less
   than this value, the input matrix will be replaced by its symmetric
   part, i.e., input matrix A becomes (A+t(A))/2.}
}

\value{
The value of the Wald statistic.
}

\references{
[1] Lawless, J. and Singhal, K. (1978). Efficient Screening of Nonnormal 
Regression Models, \emph{Biometrics}, Vol. 34, 318-327.

[2] Lawless, J. and Singhal, K. (1987). ISMOD: An All-Subsets Regression
Program for Generalized Models I. Statistical and Computational Background, 
\emph{Computer Methods and Programs in Biomedicine}, Vol. 24, 117-124.

}

\examples{

## ---------------------------------------------------------------


##  An example of variable selection in the context of binary response
##  regression models. The logarithms and original physical measurements
##  of the "Leptograpsus variegatus crabs" considered in the MASS crabs 
##  data set are used to fit a logistic model that takes the sex of each crab
##  as the response variable.

library(MASS)
data(crabs)
lFL <- log(crabs$FL)
lRW <- log(crabs$RW)
lCL <- log(crabs$CL)
lCW <- log(crabs$CW)
logrfit <- glm(sex ~ FL + RW + CL + CW  + lFL + lRW + lCL + lCW,
crabs,family=binomial) 
## Warning message:
## fitted probabilities numerically 0 or 1 occurred in: glm.fit(x = X, y = Y, 
## weights = weights, start = start, etastart = etastart, 

lHmat <- glmHmat(logrfit) 
wald.coef(lHmat$mat,lHmat$H,c(1,6,7),tolsym=1E-06)
## [1] 2.286739
## Warning message:

## The covariance/total matrix supplied was slightly asymmetric: 
## symmetric entries differed by up to 6.57252030578093e-14.
## (less than the 'tolsym' parameter).
## It has been replaced by its symmetric part.
## in: validmat(mat, p, tolval, tolsym)


## ---------------------------------------------------------------

## 2) An example computing the value of the Wald statistic in a logistic 
##  model for five subsets produced when a probit model was originally 
##  considered

library(MASS)
data(crabs)
lFL <- log(crabs$FL)
lRW <- log(crabs$RW)
lCL <- log(crabs$CL)
lCW <- log(crabs$CW)
probfit <- glm(sex ~ FL + RW + CL + CW  + lFL + lRW + lCL + lCW,
crabs,family=binomial(link=probit)) 
## Warning message:
## fitted probabilities numerically 0 or 1 occurred in: glm.fit(x = X, y = Y, 
## weights = weights, start = start, etastart = etastart) 

pHmat <- glmHmat(probfit) 
probresults <-eleaps(pHmat$mat,kmin=3,kmax=3,nsol=5,criterion="Wald",H=pHmat$H,
r=1,tolsym=1E-10)
## Warning message:

## The covariance/total matrix supplied was slightly asymmetric: 
## symmetric entries differed by up to 3.14059889205964e-12.
## (less than the 'tolsym' parameter).
## It has been replaced by its symmetric part.
## in: validmat(mat, p, tolval, tolsym) 

logrfit <- glm(sex ~ FL + RW + CL + CW  + lFL + lRW + lCL + lCW,
crabs,family=binomial) 
## Warning message:
## fitted probabilities numerically 0 or 1 occurred in: glm.fit(x = X, y = Y, 
## weights = weights, start = start, etastart = etastart)

lHmat <- glmHmat(logrfit) 
wald.coef(lHmat$mat,H=lHmat$H,probresults$subsets,tolsym=1e-06)
##             Card.3
## Solution 1 2.286739
## Solution 2 2.595165
## Solution 3 2.585149
## Solution 4 2.669059
## Solution 5 2.690954
## Warning message:

## The covariance/total matrix supplied was slightly asymmetric: 
## symmetric entries differed by up to 6.57252030578093e-14.
## (less than the 'tolsym' parameter).
## It has been replaced by its symmetric part.
## in: validmat(mat, p, tolval, tolsym)

}

\keyword{manip}

