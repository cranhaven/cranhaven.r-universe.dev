\name{eleaps}

\alias{eleaps}
\alias{leaps}

\concept{Leaps and Bounds algorithm}

\title{A Leaps and Bounds Algorithm for finding the best variable subsets} 

\description{An exact Algorithm for optimizing criteria that measure the 
quality of k-dimensional variable subsets as approximations to a given
set of variables, or to a set of its Principal Components.}

\details{
For each cardinality k (with k ranging from \code{kmin} to \code{kmax}),
\code{eleaps} performs a branch and bound search for the best \code{nsol} variable 
subsets according to a specified criterion. Leaps implements Duarte Silva's 
adaptation (references [2] and [3]) of Furnival and Wilson's Leaps and
Bounds Algorithm (reference [4]) for variable selection in Regression
Analysis. If the search is  not completed within a user defined time
limit, \code{eleaps} exits with a warning message. 

The user may force variables to be included and/or excluded from the
\emph{k}-subsets.

In order to improve computation times, the bulk of computations are
carried out by C++ routines. Further details about the Algorithm can 
be found in references [2] and [3] and in the comments to the C++ code.
A discussion of the criteria considered can be found in References [1]
and [3].

The function checks for ill-conditioning of the input matrix
(specifically, it checks whether the ratio of the input matrix's
smallest and largest eigenvalues is less than \code{tolval}). For an
ill-conditioned input matrix, the search is restricted to its 
well-conditioned subsets. The function \code{\link{trim.matrix}} may 
be used to obtain a well-conditioned input matrix.

In a general descriptive (Principal Components Analysis) setting, the
three criteria Rm, Rv and Gcd can be used to select good k-variable
subsets. Arguments \code{H} and \code{r} are not used in this context.
See reference [1]  and the \code{Examples} for a more detailed
discussion. 

In the setting of a multivariate linear model, \eqn{X = A \Psi  +
U}{X = A B + U},
criteria Ccr12, Tau2, Xi2 and Zeta2 can be used to select subsets
according to their  contribution to an effect characterized by the
violation of  a reference hypothesis, \eqn{C \Psi = 0}{CB = 0} (see
reference [3] for 
further details). In this setting, arguments \code{mat} and \code{H}
should be set respectively to the usual Total (Hypothesis + Error) and
Hypothesis, Sum of Squares and Cross-Products (SSCP) matrices. 
Argument \code{r} should be set to the expected rank of \code{H}.
Currently, for reasons of computational efficiency,
criterion Ccr12 is available only when \eqn{\code{r} \leq  3}{\code{r}<=3}. 
Particular cases in this setting include Linear Discriminant Analyis
(LDA), Linear Regression Analysis (LRA), Canonical Correlation
Analysis (CCA) with one set of variables fixed, and several extensions of
these and other classical multivariate methodologies.

In the setting of a generalized linear model, criterion Wald can be used 
to select subsets according to the (lack of) significance of the discarded
variables, as measured by the respective Wald's statistic (see
reference [5] for further details). In this setting arguments \code{mat} and \code{H} 
should be set respectively to FI and \code{FI \%*\% b \%*\% t(b) \%*\% FI}, where b is a 
column vector of variable coefficient estimates and FI is an estimate of the 
corresponding Fisher information matrix.

The auxiliary functions \code{\link{lmHmat}}, \code{\link{ldaHmat}} 
\code{\link{glhHmat}} and \code{\link{glmHmat}} are provided to automatically 
create the matrices \code{mat} and \code{H} in all the cases considered.
}

\usage{eleaps(mat,kmin=length(include)+1,kmax=ncol(mat)-length(exclude)-1,nsol=1,
exclude=NULL,include=NULL,criterion="default",pcindices="first_k",timelimit=15,
H=NULL,r=0, tolval=1000*.Machine$double.eps,
tolsym=1000*.Machine$double.eps,maxaperr=1E-4)} 


\arguments{

  \item{mat}{a covariance/correlation, information or sums of squares and products
    matrix of the variables from which the k-subset is to be selected. 
    See the \code{Details} section below.}

  \item{kmin}{the cardinality of the smallest subset that is wanted.}

  \item{kmax}{the cardinality of the largest subset that is wanted.}

  \item{nsol}{the number of different subsets of each cardinality that
   are requested .}

  \item{exclude}{a vector of variables (referenced by their row/column
  numbers in matrix \code{mat}) that are to be forcibly excluded from
  the subsets.} 

  \item{include}{a vector of variables (referenced by their row/column
  numbers in matrix \code{mat}) that are to be forcibly included in
  the subsets.} 

  \item{criterion}{Character variable, which indicates which criterion
  is to be used in judging the quality of the subsets. Currently, 
  the "Rm", "Rv", "Gcd", "Tau2", "Xi2", "Zeta2", "Ccr12" and "Wald" criteria are
  supported  (see the \code{Details} section, the \code{References} and
  the links   \code{\link{rm.coef}},  
  \code{\link{rv.coef}}, \code{\link{gcd.coef}}, \code{\link{tau2.coef}},
  \code{\link{xi2.coef}}, \code{\link{zeta2.coef}}, \code{\link{ccr12.coef}} 
  and \code{\link{wald.coef}} for further details). The default
  criterion is "Rm" if parameter \code{r} is zero (exploratory and PCA problems),
  "Wald" if \code{r} is equal to one and \code{mat} has a "FisherI" attribute set to TRUE 
  (generalized linear models), and "Tau2" otherwise (multivariate linear model framework).}

  \item{pcindices}{either a vector of ranks of Principal Components that are to be
  used for comparison with the k-variable subsets (for the Gcd
  criterion only, see \code{\link{gcd.coef}}) or the default text
  \code{first_k}. The latter will associate PCs 1 to \emph{k} with each
  cardinality \emph{k} that has been requested by the user.}

  \item{timelimit}{a user specified limit (in seconds) for the maximum time 
   allowed to conduct the search. After this limit is exceeded, \code{eleaps} exits
   with a waring message stating that it was not possible to find the otpimal
   subsets within the allocated time.}
 
  \item{H}{Effect description matrix. Not used with the Rm, Rv or Gcd
    criteria, hence the NULL default value. See the \code{Details} section below.}
  
  \item{r}{Expected rank of the effects (\code{H}) matrix. Not used with the
    Rm, Rv or Gcd criteria. See the \code{Details} section below.}

 \item{tolval}{the tolerance level for the reciprocal of the 2-norm
condition number of the correlation/covariance or sums of squares
matrix, i.e., for the ratio of the smallest to the largest eigenvalue of the input matrix. 
Matrices with a reciprocal of the condition number smaller than \code{tolval} will activate 
a restricted-search (for well conditioned sets as defined by the value of the \code{maxaperr} 
argument) algorithm.} 

 \item{tolsym}{the tolerance level for symmetry of the
   covariance/correlation/total matrix and for the effects (\code{H})
   matrix. If corresponding matrix entries differ by more than this
   value, the input matrices will be considered asymmetric and execution
   will be aborted. If corresponding entries are different, but by less
   than this value, the input matrix will be replaced by its symmetric
   part, i.e., input matrix A becomes (A+t(A))/2.}

  \item{maxaperr}{the tolerance level for the relative rounding error of the 
   criterion. When a restricted search in employed subsets where a first order 
   estimate of this error is higher than \code{maxaperr} will be excluded from 
   the analysis.}

 }

\value{A list with five items:

   \item{subsets}{An \code{nsol} x \code{kmax} x
   length(\code{kmin}:\code{kmax}) 3-dimensional 
   array, giving for each cardinality (dimension 3) and each solution
   (dimension 1) the list of variables (referenced by their row/column
   numbers in matrix \code{mat}) in the subset (dimension 2). (For
   cardinalities  smaller than \code{kmax}, the extra final positions are set
   to zero).} 

   \item{values}{An \code{nsol} x length(\code{kmin}:\code{kmax})
   matrix, giving for each cardinality (columns), the criterion values
   of the best \code{nsol} (rows) subsets according to the chosen criterion.} 

   \item{bestvalues}{A length(\code{kmin}:\code{kmax}) vector giving
   the overall best values of the criterion for each cardinality.} 

   \item{bestsets}{A length(\code{kmin}:\code{kmax}) x \code{kmax}
   matrix, giving, for each cardinality (rows), the variables
   (referenced by their row/column numbers in matrix \code{mat}) in
   the best k-subset.}

   \item{call}{The function call which generated the output.}
}

\seealso{\code{\link{rm.coef}}, \code{\link{rv.coef}},
\code{\link{gcd.coef}},  \code{\link{tau2.coef}},
\code{\link{wald.coef}}, \code{\link{xi2.coef}},
\code{\link{zeta2.coef}}, \code{\link{ccr12.coef}},
\code{\link{anneal}}, \code{\link{genetic}},  
\code{\link{anneal}}, \code{\link{trim.matrix}},
\code{\link{lmHmat}}, \code{\link{ldaHmat}}, \code{\link{glhHmat}},
\code{\link{glmHmat}}.}

\references{
[1] Cadima, J. and Jolliffe, I.T. (2001). Variable Selection and the
Interpretation of Principal Subspaces, \emph{Journal of Agricultural,
Biological and Environmental Statistics}, Vol. 6, 62-79.

[2]  Duarte Silva, A.P. (2001) Efficient Variable Screening for
Multivariate Analysis, Journal of Multivariate Analysis Vol. 76,
35-62. 

[3] Duarte Silva, A.P. (2002) Discarding Variables in a Principal 
Component Analysis: Algorithms for All-Subsets Comparisons,
\emph{Computational Statistics}, Vol. 17, 251-271.

[4] Furnival, G.M. and Wilson, R.W. (1974). Regressions by Leaps and
Bounds, \emph{Technometrics}, Vol. 16, 499-511.

[5] Lawless, J. and Singhal, K. (1978). Efficient Screening of Nonnormal 
Regression Models, \emph{Biometrics}, Vol. 34, 318-327.

}

\examples{

## --------------------------------------------------------------------

## 
## 1) For illustration of use, a small data set.
## Subsets of variables of all cardinalities are sought using the
## RM criterion.
##

data(swiss)
eleaps(cor(swiss),nsol=3, criterion="RM")

##$subsets
##, , Card.1
##
##           Var.1 Var.2 Var.3 Var.4 Var.5
##Solution 1     3     0     0     0     0
##Solution 2     1     0     0     0     0
##Solution 3     4     0     0     0     0
##
##, , Card.2
##
##           Var.1 Var.2 Var.3 Var.4 Var.5
##Solution 1     3     6     0     0     0
##Solution 2     4     5     0     0     0
##Solution 3     1     2     0     0     0
##
##, , Card.3
##
##           Var.1 Var.2 Var.3 Var.4 Var.5
##Solution 1     4     5     6     0     0
##Solution 2     1     2     5     0     0
##Solution 3     3     4     6     0     0
##
##, , Card.4
##
##           Var.1 Var.2 Var.3 Var.4 Var.5
##Solution 1     2     4     5     6     0
##Solution 2     1     2     5     6     0
##Solution 3     1     4     5     6     0
##
##, , Card.5
##
##           Var.1 Var.2 Var.3 Var.4 Var.5
##Solution 1     1     2     3     5     6
##Solution 2     1     2     4     5     6
##Solution 3     2     3     4     5     6
##
##
##$values
##              card.1    card.2    card.3    card.4    card.5
##Solution 1 0.6729689 0.8016409 0.9043760 0.9510757 0.9804629
##Solution 2 0.6286185 0.7982296 0.8791856 0.9506434 0.9776338
##Solution 3 0.6286130 0.7945390 0.8777509 0.9395708 0.9752551
##
##$bestvalues
##   Card.1    Card.2    Card.3    Card.4    Card.5 
##0.6729689 0.8016409 0.9043760 0.9510757 0.9804629 
##
##$bestsets
##       Var.1 Var.2 Var.3 Var.4 Var.5
##Card.1     3     0     0     0     0
##Card.2     3     6     0     0     0
##Card.3     4     5     6     0     0
##Card.4     2     4     5     6     0
##Card.5     1     2     3     5     6
##
##$call
##eleaps(cor(swiss), nsol = 3, criterion="RM")


## --------------------------------------------------------------------

##
## 2) Asking only for 2- and 3- dimensional subsets and excluding 
## variable number 6.
## 

data(swiss)
eleaps(cor(swiss),2,3,exclude=6,nsol=3,criterion="rm")

##$subsets
##, , Card.2
##
##           Var.1 Var.2 Var.3
##Solution 1     4     5     0
##Solution 2     1     2     0
##Solution 3     1     3     0
##
##, , Card.3
##
##           Var.1 Var.2 Var.3
##Solution 1     1     2     5
##Solution 2     1     4     5
##Solution 3     2     4     5
##
##
##$values
##              card.2    card.3
##Solution 1 0.7982296 0.8791856
##Solution 2 0.7945390 0.8686515
##Solution 3 0.7755232 0.8628693
##
##$bestvalues
##   Card.2    Card.3 
##0.7982296 0.8791856 
##
##$bestsets
##       Var.1 Var.2 Var.3
##Card.2     4     5     0
##Card.3     1     2     5
##
##$call
##eleaps(cor(swiss), 2, 3, exclude = 6, nsol = 3, criterion = "gcd")

## --------------------------------------------------------------------

##
## 3) Searching for 2- and 3- dimensional subsets that best approximate 
## the spaces generated by the first three Principal Components
##


data(swiss)
eleaps(cor(swiss),2,3,criterion="gcd",pcindices=1:3,nsol=3)

##$subsets
##, , Card.2
##
##           Var.1 Var.2 Var.3
##Solution 1     4     5     0
##Solution 2     5     6     0
##Solution 3     4     6     0
##
##, , Card.3
##
##           Var.1 Var.2 Var.3
##Solution 1     4     5     6
##Solution 2     3     5     6
##Solution 3     2     5     6
##
##
##$values
##              card.2    card.3
##Solution 1 0.7831827 0.9253684
##Solution 2 0.7475630 0.8459302
##Solution 3 0.7383665 0.8243032
##
##$bestvalues
##   Card.2    Card.3 
##0.7831827 0.9253684 
##
##$bestsets
##       Var.1 Var.2 Var.3
##Card.2     4     5     0
##Card.3     4     5     6
##
##$call
##eleaps(cor(swiss), 2, 3, criterion = "gcd", pcindices = 1:3, nsol = 3)


## --------------------------------------------------------------------

##
## 4) An example of subset selection in the context of Multiple Linear
## Regression. Variable 5 (average car price) in the Cars93 MASS library 
## data set is regressed on 13 other variables. A best subset of linear 
## predictors is sought, using the default criterion ("TAU_2") which, 
## in the case of a Linear Regression, is merely  the standard Coefficient
## of Determination, R^2 (as are the other three criteria for the
## multivariate linear hypothesis, "XI_2", "CCR1_2" and "ZETA_2").
##

library(MASS)
data(Cars93)
CarsHmat <- lmHmat(Cars93[,c(7:8,12:15,17:22,25)],Cars93[,5])

names(Cars93[,5,drop=FALSE])
## [1] "Price"

colnames(CarsHmat$mat)

##  [1] "MPG.city"           "MPG.highway"        "EngineSize"        
##  [4] "Horsepower"         "RPM"                "Rev.per.mile"      
##  [7] "Fuel.tank.capacity" "Passengers"         "Length"            
## [10] "Wheelbase"          "Width"              "Turn.circle"       
## [13] "Weight"            


eleaps(CarsHmat$mat, kmin=4, kmax=6, H=CarsHmat$H, r=1)

## $subsets
## , , Card.4
## 
##            Var.1 Var.2 Var.3 Var.4 Var.5 Var.6
## Solution 1     4     5    10    11     0     0
## 
## , , Card.5
## 
##            Var.1 Var.2 Var.3 Var.4 Var.5 Var.6
## Solution 1     4     5    10    11    12     0
## 
## , , Card.6
## 
##            Var.1 Var.2 Var.3 Var.4 Var.5 Var.6
## Solution 1     4     5     9    10    11    12
## 
## 
## $values
##               card.4    card.5   card.6
## Solution 1 0.7143794 0.7241457 0.731015
## 
## $bestvalues
##    Card.4    Card.5    Card.6 
## 0.7143794 0.7241457 0.7310150 
## 
## $bestsets
##        Var.1 Var.2 Var.3 Var.4 Var.5 Var.6
## Card.4     4     5    10    11     0     0
## Card.5     4     5    10    11    12     0
## Card.6     4     5     9    10    11    12
##

## --------------------------------------------------------------------

## 5) A Linear Discriminant Analysis example with a very small data set. 
## We consider the Iris data and three groups, defined by species (setosa, 
## versicolor and virginica). The goal is to select the 2- and 3-variable
## subsets that are optimal for the linear discrimination (as measured 
## by the "CCR1_2" criterion).
  
data(iris)
irisHmat <- ldaHmat(iris[1:4],iris$Species)
eleaps(irisHmat$mat,kmin=2,kmax=3,H=irisHmat$H,r=2,crit="ccr12")

## $subsets
## , , Card.2
## 
##            Var.1 Var.2 Var.3
## Solution 1     1     3     0
## 
## , , Card.3
## 
##            Var.1 Var.2 Var.3
## Solution 1     2     3     4
## 
## 
## $values
##               card.2   card.3
## Solution 1 0.9589055 0.967897
## 
## $bestvalues
##    Card.2    Card.3 
## 0.9589055 0.9678971 
## 
## $bestsets
##        Var.1 Var.2 Var.3
## Card.2     1     3     0
## Card.3     2     3     4


## --------------------------------------------------------------------

## 6) An example of subset selection in the context of a Canonical
## Correlation Analysis. Two groups of variables within the Cars93
## MASS library data set are compared. The goal is to select 4- to
## 6-variable subsets of the 13-variable 'X' group that are optimal in
## terms of preserving the canonical correlations, according to the
## "ZETA_2" criterion (Warning: the 3-variable 'Y' group is kept
## intact; subset selection is carried out in the 'X' 
## group only).  The 'tolsym' parameter is used to relax the symmetry
## requirements on the effect matrix H which, for numerical reasons,
## is slightly asymmetric. Since corresponding off-diagonal entries of
## matrix H are different, but by less than tolsym, H is replaced  
## by its symmetric part: (H+t(H))/2.

library(MASS)
data(Cars93)
CarsHmat <- lmHmat(Cars93[,c(7:8,12:15,17:22,25)],Cars93[,4:6])

names(Cars93[,4:6])
## [1] "Min.Price" "Price"     "Max.Price"

##  colnames(CarsHmat$mat)


##  [1] "MPG.city"           "MPG.highway"        "EngineSize"        
##  [4] "Horsepower"         "RPM"                "Rev.per.mile"      
##  [7] "Fuel.tank.capacity" "Passengers"         "Length"            
## [10] "Wheelbase"          "Width"              "Turn.circle"       
## [13] "Weight"            


eleaps(CarsHmat$mat, kmin=4, kmax=6, H=CarsHmat$H, r=3,
crit="zeta2", tolsym=1e-9)

## $subsets
## , , Card.4
## 
##            Var.1 Var.2 Var.3 Var.4 Var.5 Var.6
## Solution 1     3     4    10    11     0     0
## 
## , , Card.5
## 
##            Var.1 Var.2 Var.3 Var.4 Var.5 Var.6
## Solution 1     4     5     9    10    11     0
## 
## , , Card.6
## 
##            Var.1 Var.2 Var.3 Var.4 Var.5 Var.6
## Solution 1     4     5     9    10    11    12
## 
## 
## $values
##               card.4    card.5    card.6
## Solution 1 0.4827353 0.5018922 0.5168627
## 
## $bestvalues
##    Card.4    Card.5    Card.6 
## 0.4827353 0.5018922 0.5168627 
## 
## $bestsets
##        Var.1 Var.2 Var.3 Var.4 Var.5 Var.6
## Card.4     3     4    10    11     0     0
## Card.5     4     5     9    10    11     0
## Card.6     4     5     9    10    11    12
## 
## Warning message:
## 
##  The effect description matrix (H) supplied was slightly asymmetric: 
##  symmetric entries differed by up to 3.63797880709171e-12.
##  (less than the 'tolsym' parameter).
##  The H matrix has been replaced by its symmetric part.
##  in: validnovcrit(mat, criterion, H, r, p, tolval, tolsym) 

## --------------------------------------------------------------------


##  7) An example of variable selection in the context of a logistic 
##  regression model. We consider the last 100 observations of
##  the iris data set (versicolor an verginica species) and try
##  to find the best variable subsets for the model that takes species
##  as response variable.

data(iris)
iris2sp <- iris[iris$Species != "setosa",]
logrfit <- glm(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
iris2sp,family=binomial)
Hmat <- glmHmat(logrfit)
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
## --------------------------------------------------------------------

## It should be stressed that, unlike other criteria in the
## subselect package, the Wald criterion is not bounded above by
## 1 and is a decreasing function of subset quality, so that the
## 3-variable subsets do, in fact, perform better than their smaller-sized
## counterparts.


}

\keyword{manip}


