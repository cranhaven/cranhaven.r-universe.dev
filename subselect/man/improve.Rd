\name{improve}

\alias{improve}
\concept{Local search algorithms}

\title{Restricted Local Improvement search for an optimal k-variable subset} 

\description{Given a set of variables,  a Restricted Local Improvement
algorithm seeks a k-variable subset which is
optimal, as a surrogate for the whole set, with respect to a given criterion. 
}

\details{An initial k-variable subset (for k ranging from \code{kmin}
to \code{kmax})  of a full set of p  variables is
randomly selected and the variables not belonging to this subset are
placed in a queue. The possibility of replacing a variable in the
current k-subset with a variable from the queue is then explored.  
More precisely, a variable is selected, removed from the queue, and the 
k values of the criterion which would result from swapping this selected
variable with each variable in the current subset are computed. If the
best of these values improves the current criterion value, the current
subset is updated accordingly. In this case, the variable which leaves
the subset is added to the queue, but only if it has not previously
been in the queue (i.e., no variable can enter the queue twice). The
algorithm proceeds until the queue is emptied.  

The user may force variables to be included and/or excluded from the
k-subsets, and may specify initial solutions.

For each cardinality k, the total number of calls to the procedure
which computes the criterion 
values is O(\code{nsol} x k x p). These calls are the dominant computational
effort in each iteration of the algorithm.  

In order to improve computation times, the bulk of computations are
carried out in a Fortran routine. Further details about the algorithm can
be found in Reference 1 and in the comments to the Fortran code (in
the \code{src} subdirectory for this package). For datasets with a very
large number of variables (currently p > 400), it is 
necessary to set the \code{force} argument to TRUE for the function to
run, but this may cause a session crash if there is not enough memory
available. 

The function checks for ill-conditioning of the input matrix
(specifically, it checks whether the ratio of the input matrix's
smallest and largest eigenvalues is less than \code{tolval}). For an
ill-conditioned input matrix, the search is restricted to its 
well-conditioned subsets. The function \code{\link{trim.matrix}} may 
be used to obtain a well-conditioned input matrix.

In a general descriptive (Principal Components Analysis) setting, the
three criteria Rm, Rv and Gcd can be used to select good k-variable
subsets. Arguments \code{H} and \code{r} are not used in this context.
See references [1] and [2] and the \code{Examples} for a more detailed
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
Analysis (CCA) with one set of variables fixed  and several extensions of
these and other classical multivariate methodologies.

In the setting of a generalized linear model, criterion Wald can be used 
to select subsets according to the (lack of) significance of the discarded
variables, as measured by the respective Wald's statistic (see
reference [4] for further details). In this setting arguments \code{mat} and \code{H} 
should be set respectively to FI and \code{FI \%*\% b \%*\% t(b) \%*\% FI}, where b is a 
column vector of variable coefficient estimates and FI is an estimate of the 
corresponding Fisher information matrix.

The auxiliary functions \code{\link{lmHmat}}, \code{\link{ldaHmat}} 
\code{\link{glhHmat}} and \code{\link{glmHmat}} are provided to automatically 
create the matrices \code{mat} and \code{H} in all the cases considered.
}

\usage{improve( mat, kmin, kmax = kmin, nsol = 1, exclude = NULL,
include = NULL, setseed = FALSE, criterion = "default", pcindices="first_k",
initialsol = NULL, force = FALSE, H=NULL, r=0,
tolval=1000*.Machine$double.eps,tolsym=1000*.Machine$double.eps)
}

\arguments{

 \item{mat}{a covariance/correlation, information or sums of squares and products
    matrix of the variables from which the k-subset is to be selected. 
    See the \code{Details} section below.}

  \item{kmin}{the cardinality of the smallest subset that is wanted.}

  \item{kmax}{the cardinality of the largest subset that is wanted.}

  \item{nsol}{the number of different subsets (runs of the algorithm) wanted.}

  \item{exclude}{a vector of variables (referenced by their row/column
  numbers in matrix \code{mat}) that are to be forcibly excluded from
  the subsets.} 

  \item{include}{a vector of variables (referenced by their row/column
  numbers in matrix \code{mat}) that are to be forcibly included from
  the subsets.} 

  \item{setseed}{logical variable indicating whether to fix an initial 
  seed for the random number generator, which will be re-used in future
  calls to this function whenever setseed is again set to TRUE.}

  \item{criterion}{Character variable, which indicates which criterion
  is to be used in judging the quality of the subsets. Currently, 
  the "Rm", "Rv", "Gcd", "Tau2", "Xi2", "Zeta2", "ccr12" and "Wald" criteria 
  are supported  (see the \code{Details} section, the \code{References} and
  the links   \code{\link{rm.coef}},  
  \code{\link{rv.coef}}, \code{\link{gcd.coef}}, \code{\link{tau2.coef}},
  \code{\link{xi2.coef}}, \code{\link{zeta2.coef}} and
 \code{\link{ccr12.coef}} for further details).  The default
  criterion is "Rm" if parameter \code{r} is zero (exploratory and PCA problems),
  "Wald" if \code{r} is equal to one and \code{mat} has a "FisherI" attribute set to TRUE 
  (generalized linear models), and "Tau2" otherwise (multivariate linear model framework).}

  \item{pcindices}{either a vector of ranks of Principal Components that are to be
  used for comparison with the k-variable subsets (for the Gcd
  criterion only, see \code{\link{gcd.coef}}) or the default text
  \code{first_k}. The latter will associate PCs 1 to \emph{k} with each
  cardinality \emph{k} that has been requested by the user.}

  \item{initialsol}{vector, matrix or 3-d array of initial solutions
  for the restricted local improvement search. If a \emph{single
  cardinality} is 
  required, \code{initialsol} may be a vector of length
  \emph{k}(accepted even if \code{nsol} > 1, in
  which case it is used as the initial solution for all \code{nsol}
  final solutions that are requested with a warning that the same
  initial solution necessarily produces the same final solution); 
  a 1 x \emph{k} matrix (as
  produced by the \code{$bestsets} output value of the algorithm functions
  \code{anneal}, \code{\link{genetic}}, or \code{\link{improve}}), or
  a 1 x \emph{k} x 1 array (as produced by the
  \code{$subsets} output value), in
  which case it will be treated as the above k-vector; or an
  \code{nsol} x \code{k} matrix, or  \code{nsol} x \code{k} x 1 3-d
  array, in which case each row (dimension 1) will be used 
  as the initial solution for each of the \code{nsol} final solutions
  requested. If \emph{more than one cardinality} is requested,
  \code{initialsol} can be a 
  \code{length(kmin:kmax)} x \code{kmax} matrix (as produced by the
  \code{$bestsets} option of the algorithm functions) (even if
  \code{nsol} > 1, in which case
  each row will be replicated to produced the initial solution for all
  \code{nsol} final solutions requested in each cardinality, with a
  warning that a single initial solution necessarily produces
  identical final solutions), or a
  \code{nsol} x \code{kmax} x \code{length(kmin:kmax)} 3-d array (as
  produced by the 
  \code{$subsets} output option), in which case each row (dimension 1)
  is interpreted as a different initial solution.

  If the \code{exclude} and/or \code{include} options are used,
  \code{initialsol} must also respect those requirements. }

  \item{force}{a logical variable indicating whether, for large data
    sets (currently \code{p} > 400) the algorithm should proceed
    anyways, regardless of possible memory problems which may crash the
    R session.}
 
  \item{H}{Effect description matrix. Not used with the Rm, Rv or Gcd
    criteria, hence the NULL default value. See the \code{Details} section below.}
  
  \item{r}{Expected rank of the effects (\code{H}) matrix. Not used with the
    Rm, Rv or Gcd criteria. See the \code{Details} section below.}

  \item{tolval}{the tolerance level for the reciprocal of the 2-norm
    condition number of the correlation/covariance matrix, i.e., for the
    ratio of the smallest to the largest eigenvalue of the input matrix. 
    Matrices with a reciprocal of the condition number smaller than 
    \code{tolval} will activate a restricted-search for well conditioned 
    subsets.} 
  
 \item{tolsym}{the tolerance level for symmetry of the
   covariance/correlation/total matrix and for the effects (\code{H})
   matrix. If corresponding matrix entries differ by more than this
   value, the input matrices will be considered asymmetric and execution
   will be aborted. If corresponding entries are different, but by less
   than this value, the input matrix will be replaced by its symmetric
   part, i.e., input matrix A becomes (A+t(A))/2.}

}

\value{A list with five items:

   \item{subsets}{An \code{nsol} x \code{kmax} x
   length(\code{kmin}:\code{kmax}) 3-dimensional array, giving for
   each cardinality (dimension 3) and each solution (dimension 1) the
   list of variables (referenced by their row/column numbers in matrix
   \code{mat}) in the subset (dimension 2). (For cardinalities
   smaller than \code{kmax}, the extra final positions are set to zero).}

   \item{values}{An \code{nsol} x length(\code{kmin}:\code{kmax})
   matrix, giving for each cardinality (columns), the criterion values
   of the \code{nsol} (rows) solutions obtained.}

   \item{bestvalues}{A length(\code{kmin}:\code{kmax}) vector giving
   the best values of the criterion obtained for each cardinality.}

   \item{bestsets}{A length(\code{kmin}:\code{kmax}) x \code{kmax}
   matrix, giving, for each cardinality (rows), the variables
   (referenced by their row/column numbers in matrix \code{mat}) in the
   best k-subset that was found.}

   \item{call}{The function call which generated the output.}
}

\seealso{\code{\link{rm.coef}}, \code{\link{rv.coef}},
\code{\link{gcd.coef}}, \code{\link{tau2.coef}}, \code{\link{xi2.coef}},
\code{\link{zeta2.coef}}, \code{\link{ccr12.coef}}, \code{\link{genetic}},
\code{\link{anneal}}, \code{\link{eleaps}}, \code{\link{trim.matrix}},
\code{\link{lmHmat}}, \code{\link{ldaHmat}}, \code{\link{glhHmat}},
\code{\link{glmHmat}}.}

\references{
[1] Cadima, J., Cerdeira, J. Orestes and Minhoto, M. (2004)
Computational aspects of algorithms for variable selection in the
context of principal components. \emph{Computational Statistics and Data Analysis}, 47, 225-236.

[2]Cadima, J. and Jolliffe, I.T. (2001). Variable Selection and the
Interpretation of Principal Subspaces, \emph{Journal of Agricultural,
Biological and Environmental Statistics}, Vol. 6, 62-79.

[3]Duarte Silva, A.P. (2001) Efficient Variable Screening for Multivariate Analysis,
\emph{Journal of Multivariate Analysis}, Vol. 76, 35-62.

[4] Lawless, J. and Singhal, K. (1978). Efficient Screening of Nonnormal 
Regression Models, \emph{Biometrics}, Vol. 34, 318-327.

}

\examples{

## --------------------------------------------------------------------

##
## 1) For illustration of use, a small data set with very few iterations
## of the algorithm. 
## Subsets of 2 and of 3 variables are sought using the RM criterion.
##

data(swiss)
improve(cor(swiss),2,3,nsol=4,criterion="GCD")
## $subsets
## , , Card.2
##
##            Var.1 Var.2 Var.3
## Solution 1     3     6     0
## Solution 2     3     6     0
## Solution 3     3     6     0
## Solution 4     3     6     0
##
## , , Card.3
##
##            Var.1 Var.2 Var.3
## Solution 1     4     5     6
## Solution 2     4     5     6
## Solution 3     4     5     6
## Solution 4     4     5     6
##
##
## $values
##               card.2   card.3
## Solution 1 0.8487026 0.925372
## Solution 2 0.8487026 0.925372
## Solution 3 0.8487026 0.925372
## Solution 4 0.8487026 0.925372
##
## $bestvalues
##    Card.2    Card.3 
## 0.8487026 0.9253720 
##
## $bestsets
##        Var.1 Var.2 Var.3
## Card.2     3     6     0
## Card.3     4     5     6
##
##$call
##improve(cor(swiss), 2, 3, nsol = 4, criterion = "GCD")


## --------------------------------------------------------------------

##
## 2) Forcing the inclusion of variable 1 in the subset
##

 improve(cor(swiss),2,3,nsol=4,criterion="GCD",include=c(1))

## $subsets
## , , Card.2
##
##            Var.1 Var.2 Var.3
## Solution 1     1     6     0
## Solution 2     1     6     0
## Solution 3     1     6     0
## Solution 4     1     6     0
##
## , , Card.3
##
##            Var.1 Var.2 Var.3
## Solution 1     1     5     6
## Solution 2     1     5     6
## Solution 3     1     5     6
## Solution 4     1     5     6
##
##
## $values
##               card.2    card.3
## Solution 1 0.7284477 0.8048528
## Solution 2 0.7284477 0.8048528
## Solution 3 0.7284477 0.8048528
## Solution 4 0.7284477 0.8048528
##
## $bestvalues
##    Card.2    Card.3 
## 0.7284477 0.8048528 
##
## $bestsets
##        Var.1 Var.2 Var.3
## Card.2     1     6     0
## Card.3     1     5     6
##
##$call
##improve(cor(swiss), 2, 3, nsol = 4, criterion = "GCD", include = c(1))

## --------------------------------------------------------------------

## 3) An example of subset selection in the context of Multiple Linear
## Regression. Variable 5 (average car price) in the Cars93 MASS library 
## data set is regressed on 13 other variables. Three variable subsets of 
## cardinalities 4, 5 and 6 are requested, using the "XI_2" criterion which, 
## in the case of a Linear Regression, is merely  the standard Coefficient of
## Determination, R^2 (as are the other three criteria for the
## multivariate linear hypothesis, "TAU_2", "CCR1_2" and "ZETA_2").

library(MASS)
data(Cars93)
CarsHmat <- lmHmat(Cars93[,c(7:8,12:15,17:22,25)],Cars93[,5])

names(Cars93[,5,drop=FALSE])
##  [1] "Price"

colnames(CarsHmat$mat)

##  [1] "MPG.city"           "MPG.highway"        "EngineSize"        
##  [4] "Horsepower"         "RPM"                "Rev.per.mile"      
##  [7] "Fuel.tank.capacity" "Passengers"         "Length"            
## [10] "Wheelbase"          "Width"              "Turn.circle"       
## [13] "Weight"            


improve(CarsHmat$mat, kmin=4, kmax=6, H=CarsHmat$H, r=1, crit="xi2", nsol=3)

## $subsets
## , , Card.4
## 
##            Var.1 Var.2 Var.3 Var.4 Var.5 Var.6
## Solution 1     3     4    11    13     0     0
## Solution 2     3     4    11    13     0     0
## Solution 3     4     5    10    11     0     0
## 
## , , Card.5
## 
##            Var.1 Var.2 Var.3 Var.4 Var.5 Var.6
## Solution 1     3     4     8    11    13     0
## Solution 2     4     5    10    11    12     0
## Solution 3     4     5    10    11    12     0
## 
## , , Card.6
## 
##            Var.1 Var.2 Var.3 Var.4 Var.5 Var.6
## Solution 1     4     5     6    10    11    12
## Solution 2     4     5     8    10    11    12
## Solution 3     4     5     9    10    11    12
## 
## 
## $values
##               card.4    card.5    card.6
## Solution 1 0.6880773 0.6899182 0.7270257
## Solution 2 0.6880773 0.7241457 0.7271056
## Solution 3 0.7143794 0.7241457 0.7310150
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
## $call
## improve(mat = CarsHmat$mat, kmin = 4, kmax = 6, nsol = 3, criterion = "xi2", 
##     H = CarsHmat$H, r = 1)



## --------------------------------------------------------------------

## 4) A Linear Discriminant Analysis example with a very small data set. 
## We consider the Iris data and three groups, defined by species (setosa, 
## versicolor and virginica). The goal is to select the 2- and 3-variable
## subsets that are optimal for the linear discrimination (as measured 
## by the "TAU_2" criterion).

data(iris)
irisHmat <- ldaHmat(iris[1:4],iris$Species)
improve(irisHmat$mat,kmin=2,kmax=3,H=irisHmat$H,r=2,crit="ccr12")

## $subsets
## , , Card.2
## 
##            Var.1 Var.2 Var.3
## Solution 1     2     3     0
## 
## , , Card.3
## 
##            Var.1 Var.2 Var.3
## Solution 1     2     3     4
## 
## 
## $values
##               card.2    card.3
## Solution 1 0.8079476 0.8419635
## 
## $bestvalues
##    Card.2    Card.3 
## 0.8079476 0.8419635 
## 
## $bestsets
##        Var.1 Var.2 Var.3
## Card.2     2     3     0
## Card.3     2     3     4
## 
## $call
## improve(mat = irisHmat$mat, kmin = 2, kmax = 3, 
##     criterion = "tau2", H = irisHmat$H, r = 2)
## 

## --------------------------------------------------------------------

## 5) An example of subset selection in the context of a Canonical
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

colnames(CarsHmat$mat)

##  [1] "MPG.city"           "MPG.highway"        "EngineSize"        
##  [4] "Horsepower"         "RPM"                "Rev.per.mile"      
##  [7] "Fuel.tank.capacity" "Passengers"         "Length"            
## [10] "Wheelbase"          "Width"              "Turn.circle"       
## [13] "Weight"            


improve(CarsHmat$mat, kmin=4, kmax=6, H=CarsHmat$H, r=3, crit="zeta2", tolsym=1e-9)

## $subsets
## , , Card.4
## 
##            Var.1 Var.2 Var.3 Var.4 Var.5 Var.6
## Solution 1     3     4    11    13     0     0
## 
## , , Card.5
## 
##            Var.1 Var.2 Var.3 Var.4 Var.5 Var.6
## Solution 1     3     4     9    11    13     0
## 
## , , Card.6
## 
##            Var.1 Var.2 Var.3 Var.4 Var.5 Var.6
## Solution 1     3     4     5     9    10    11
## 
## 
## $values
##               card.4    card.5    card.6
## Solution 1 0.4626035 0.4875495 0.5071096
## 
## $bestvalues
##    Card.4    Card.5    Card.6 
## 0.4626035 0.4875495 0.5071096 
## 
## $bestsets
##        Var.1 Var.2 Var.3 Var.4 Var.5 Var.6
## Card.4     3     4    11    13     0     0
## Card.5     3     4     9    11    13     0
## Card.6     3     4     5     9    10    11
## 
## $call
## improve(mat = CarsHmat$mat, kmin = 4, kmax = 6, criterion = "zeta2", 
##     H = CarsHmat$H, r = 3, tolsym = 1e-09)
## 
## Warning message:
## 
##  The effect description matrix (H) supplied was slightly asymmetric: 
##  symmetric entries differed by up to 3.63797880709171e-12.
##  (less than the 'tolsym' parameter).
##  The H matrix has been replaced by its symmetric part.
##  in: validnovcrit(mat, criterion, H, r, p, tolval, tolsym) 

## --------------------------------------------------------------------

##  6) An example of variable selection in the context of a logistic 
##  regression model. We consider the last 100 observations of
##  the iris data set (versicolor and verginica species) and try
##  to find the best variable subsets for the model that takes species
##  as response variable.

data(iris)
iris2sp <- iris[iris$Species != "setosa",]
logrfit <- glm(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
iris2sp,family=binomial)
Hmat <- glmHmat(logrfit)
improve(Hmat$mat,1,3,H=Hmat$H,r=1,criterion="Wald")

## $subsets
## , , Card.1
##
##           Var.1 Var.2 Var.3
## Solution 1     4     0     0

## , , Card.2

##            Var.1 Var.2 Var.3
## Solution 1     1     3     0

## , , Card.3

##            Var.1 Var.2 Var.3
## Solution 1     2     3     4


## $values
##              card.1   card.2   card.3
## Solution 1 4.894554 3.522885 1.060121

## $bestvalues
##   Card.1   Card.2   Card.3 
## 4.894554 3.522885 1.060121 

## $bestsets
##        Var.1 Var.2 Var.3
## Card.1     4     0     0
## Card.2     1     3     0
## Card.3     2     3     4

## $call
## improve(mat = Hmat$mat, kmin = 1, kmax = 3, criterion = "Wald", 
##     H = Hmat$H, r = 1)
## --------------------------------------------------------------------

## It should be stressed that, unlike other criteria in the
## subselect package, the Wald criterion is not bounded above by
## 1 and is a decreasing function of subset quality, so that the
## 3-variable subsets do, in fact, perform better than their smaller-sized
## counterparts.

}
\keyword{manip}

