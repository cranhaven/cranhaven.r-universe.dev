\name{glmHmat}
\alias{glmHmat}
\alias{glmHmat.glm}
\title{Input matrices for subselect search routines in generalized linear models}

\description{glmHmat uses a glm object (fitdglmmodel) to build an estimate of 
Fisher's Information (FI) matrix together with an auxiliarly rank-one positive-defenite 
matrix (H), such that the positive eigenvalue of \eqn{FI^{-1} H} equals the value of Wald's statistic 
for testing the global significance of fitdglmmodel. These matrices may be used as input to the
variable selection search routines \code{\link{anneal}}, \code{\link{genetic}} 
\code{\link{improve}} or \code{\link{eleaps}}, usign the minimization of Wald's statistic
as criterion for discarding variables. }

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
the Wald statistic testing the significance of the full covariate
vector, b and FI are coefficient and Fisher  
information estimates and H is an auxiliary rank-one matrix given by
\code{H =  FI \%*\% b \%*\% t(b) \%*\% FI}, it
follows that the value of Wald's statistic for the  
excluded variables (\eqn{W_excv}) in a given subset  is given by 
\eqn{W_{excv} = W_{allv} - tr (FI_{indices}^{-1} H_{indices}) ,} where
\eqn{FI_indices} and \eqn{H_indices} are the  
portions of the FI and H matrices associated with the selected variables.

glmHmat retrieves the values of the FI and H matrices from a glm object. These matrices may 
then be used as input to the search functions \code{\link{anneal}}, 
\code{\link{genetic}}, \code{\link{improve}} and \code{\link{eleaps}}.

}

\references{
[1] Lawless, J. and Singhal, K. (1978). Efficient Screening of Nonnormal 
Regression Models, \emph{Biometrics}, Vol. 34, 318-327.

[2] Lawless, J. and Singhal, K. (1987). ISMOD: An All-Subsets Regression
Program for Generalized Models I. Statistical and Computational Background, 
\emph{Computer Methods and Programs in Biomedicine}, Vol. 24, 117-124.
}

\usage{

\method{glmHmat}{glm}(fitdglmmodel,...)

}

\arguments{
  \item{fitdglmmodel}{A glm object containaing the estimates, and respective
    covariance matrix, of a generalized linear model.}
  \item{...}{further arguments for the method.} 
  }

\value{
A list with four items:
  \item{mat}{An estimate (FI) of Fisher's information matrix for the full model variable-coefficient estimates}
  \item{H}{A product of the form \code{(FI \%*\% b \%*\% t(b) \%*\% FI)} where b is a vector of variable-coefficient estimates}
  \item{r}{The rank of the H matrix. Always set to one in glmHmat.} 
  \item{call}{The function call which generated the output.}
}

\seealso{\code{\link{anneal}}, \code{\link{genetic}},
\code{\link{improve}}, \code{\link{eleaps}}, \code{\link{glm}}.} 
\examples{
##----------------------------------------------------------------

##----------------------------------------------------------------

##  An example of variable selection in the context of binary response 
##  regression models. We consider the last 100 observations of
##  the iris data set (versicolor an verginica species) and try
##  to find the best variable subsets for models that take species
##  as the response variable.

data(iris)
iris2sp <- iris[iris$Species != "setosa",]

# Create the input matrices for the  search routines in a logistic regression model 

modelfit <- glm(Species ~ Sepal.Length + Sepal.Width + Petal.Length +
Petal.Width,iris2sp,family=binomial) 
Hmat <- glmHmat(modelfit)
Hmat

## $mat
##              Sepal.Length Sepal.Width Petal.Length Petal.Width
## Sepal.Length  0.28340358 0.03263437  0.09552821  -0.01779067
## Sepal.Width   0.03263437 0.13941541  0.01086596 0.04759284
## Petal.Length  0.09552821 0.01086596  0.08847655  -0.01853044
## Petal.Width   -0.01779067 0.04759284  -0.01853044 0.03258730

## $H
##              Sepal.Length  Sepal.Width Petal.Length  Petal.Width
## Sepal.Length   0.11643732  0.013349227 -0.063924853 -0.050181400
## Sepal.Width    0.01334923  0.001530453 -0.007328813 -0.005753163
## Petal.Length  -0.06392485 -0.007328813  0.035095164  0.027549918
## Petal.Width   -0.05018140 -0.005753163  0.027549918  0.021626854

## $r
## [1] 1

## $call
## glmHmat(fitdglmmodel = modelfit)

# Search for the 3 best variable subsets of each dimensionality by an exausitve search 

eleaps(Hmat$mat,H=Hmat$H,r=1,criterion="Wald",nsol=3)

## $subsets
## , , Card.1

##            Var.1 Var.2 Var.3
## Solution 1     4     0     0
## Solution 2     1     0     0
## Solution 3     3     0     0

## , , Card.2

##           Var.1 Var.2 Var.3
## Solution 1     1     3     0
## Solution 2     3     4     0
## Solution 3     2     4     0

## , , Card.3

##            Var.1 Var.2 Var.3
## Solution 1     2     3     4
## Solution 2     1     3     4
## Solution 3     1     2     3


## $values
##              card.1   card.2   card.3
## Solution 1 4.894554 3.522885 1.060121
## Solution 2 5.147360 3.952538 2.224335
## Solution 3 5.161553 3.972410 3.522879

## $bestvalues
##   Card.1   Card.2   Card.3 
## 4.894554 3.522885 1.060121 

## $bestsets
##        Var.1 Var.2 Var.3
## Card.1     4     0     0
## Card.2     1     3     0
## Card.3     2     3     4

## $call
## eleaps(mat = Hmat$mat, nsol = 3, criterion = "Wald", H = Hmat$H, 
##    r = 1)


## It should be stressed that, unlike other criteria in the
## subselect package, the Wald criterion is not bounded above by
## 1 and is a decreasing function of subset quality, so that the
## 3-variable subsets do, in fact, perform better than their smaller-sized
## counterparts.


## > 
## > proc.time()
## [1] 0.680 0.064 0.736 0.000 0.000
}
\keyword{manip}


