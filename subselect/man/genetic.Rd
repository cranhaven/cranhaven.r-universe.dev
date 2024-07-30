\name{genetic}

\alias{genetic}
\concept{Genetic algorithms}

\title{Genetic Algorithm searching for an optimal k-variable subset} 

\description{Given a set of variables,  a Genetic Algorithm
algorithm seeks a k-variable subset which is
optimal, as a surrogate for the whole set, with respect to a given criterion. 
}

\details{
For each cardinality k (with k ranging from \code{kmin} to \code{kmax}),
an initial population of \code{popsize} k-variable subsets is randomly
selected from a full set of p  variables. 
In each iteration, \code{popsize}/2 couples
are formed from among the population and each couple generates a child
(a new k-variable subset)
which inherits properties of its parents (specifically, it inherits
all variables common to both parents and a random selection of
variables in the symmetric difference of its parents' genetic makeup).
Each offspring may optionally undergo a mutation (in the form of a
local improvement algorithm -- see function \code{\link{improve}}),
with a user-specified probability. The parents
and offspring are ranked according to their criterion value, and the
best \code{popsize} of these k-subsets will make up the next
generation, which is used as the current population in the subsequent
iteration. 

The stopping rule for the algorithm is the number of generations (\code{nger}).

Optionally, the best \emph{k}-variable subset produced by the Genetic
Algorithm may be passed as input to a restricted local improvement
algorithm, for possible further improvement (see function
\code{\link{improve}}). 

The user may force variables to be included and/or excluded from the
\emph{k}-subsets, and may specify an initial population.

For each cardinality \emph{k}, the total number of calls to the
procedure which computes the criterion 
values is \eqn{popsize + nger} x \eqn{popsize/2}. These calls are the
dominant computational effort in each iteration of the algorithm.  

In order to improve computation times, the bulk of computations are
carried out by a Fortran routine. Further details about the Genetic
Algorithm can 
be found in Reference 1 and in the comments to the Fortran code (in
the \code{src} subdirectory for this package).  For datasets with a very
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
create the matrices \code{mat} and \code{H} in all the cases considered.}

\usage{genetic( mat, kmin, kmax = kmin, popsize = max(100,2*ncol(mat)), nger = 100,
mutate = FALSE, mutprob = 0.01, maxclone = 5, exclude = NULL,
include = NULL, improvement = TRUE, setseed= FALSE, criterion = "default",
pcindices = "first_k", initialpop = NULL, force = FALSE, H=NULL, r=0,
tolval=1000*.Machine$double.eps,tolsym=1000*.Machine$double.eps)
}

\arguments{

  \item{mat}{a covariance/correlation, information or sums of squares and products
    matrix of the variables from which the k-subset is to be selected. 
    See the \code{Details} section below.}

  \item{kmin}{the cardinality of the smallest subset that is wanted.}

  \item{kmax}{the cardinality of the largest subset that is wanted.}

  \item{popsize}{integer variable indicating the size of the
  population.}

  \item{nger}{integer variable giving the number of generations for
  which the genetic algorithm will run.}  

  \item{mutate}{logical variable indicating whether each  child
  undergoes a mutation, with probability \code{mutprob}. By default, FALSE.}

  \item{mutprob}{variable giving the probability of each  child
  undergoing a mutation, if \code{mutate} is TRUE. By default, 0.01.
  High values slow down the algorithm considerably and tend to
  replicate the same solution.}

  \item{maxclone}{integer variable specifying the maximum number of
  identical replicates (clones) of individuals that is acceptable in
  the population. Serves to ensure that the population has sufficient
  genetic diversity, which is necessary to enable the algorithm to
  complete the specified number of generations. However, even maxclone=0
  does not guarantee that there are no repetitions: only the offspring 
  of couples are tested for clones. If any such clones are rejected, they  
  are replaced by a k-variable subset chosen at random, without any
  further clone tests.}

  \item{exclude}{a vector of variables (referenced by their row/column
  numbers in matrix \code{mat}) that are to be forcibly excluded from
  the subsets.} 

  \item{include}{a vector of variables (referenced by their row/column
  numbers in matrix \code{mat}) that are to be forcibly included in
  the subsets.} 

  \item{improvement}{a logical variable indicating whether or not the
  best final subset (for each cardinality) is to be passed as input to a
  local improvement algorithm (see function \code{\link{improve}}).}

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
 \code{\link{ccr12.coef}} for further details). The default
  criterion is "Rm" if parameter \code{r} is zero (exploratory and PCA problems),
  "Wald" if \code{r} is equal to one and \code{mat} has a "FisherI" attribute set to TRUE 
  (generalized linear models), and "Tau2" otherwise (multivariate linear model framework).}

  \item{pcindices}{either a vector of ranks of Principal Components that are to be
  used for comparison with the k-variable subsets (for the Gcd
  criterion only, see \code{\link{gcd.coef}}) or the default text
  \code{first_k}. The latter will associate PCs 1 to \emph{k} with each
  cardinality \emph{k} that has been requested by the user.}

  \item{initialpop}{vector, matrix or 3-d array of initial population
  for the genetic algorithm. If a \emph{single cardinality} is
  required, \code{initialpop} may be a \code{popsize} x \emph{k}
  matrix or a \code{popsize} x \emph{k} x 1 array (as produced by the
  \code{$subsets} output value of any of the 
  algorithm functions \code{anneal}, \code{genetic}, or
  \code{improve}). If \emph{more 
  than one cardinality} is requested, \code{initialpop} must be a
  \code{popsize x kmax x length(kmin:kmax)} 3-d array (as produced by the
  \code{$subsets} output value).

  If the \code{exclude} and/or \code{include} options are used,
  \code{initialpop} must also respect those requirements. }

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

   \item{subsets}{A \code{popsize} x \code{kmax} x
   length(\code{kmin}:\code{kmax}) 3-dimensional array, giving for
   each cardinality (dimension 3) and each subset in the final
   population  (dimension 1) the list of variables (referenced by
   their row/column numbers in matrix \code{mat}) in the subset
   (dimension 2). (For cardinalities  smaller than \code{kmax}, the
   extra final positions are set to zero).} 

   \item{values}{A \code{popsize} x length(\code{kmin}:\code{kmax})
   matrix, giving for each cardinality (columns), the (ordered)
   criterion values of the \code{popsize} (rows) subsets in the final
   generation.} 

   \item{bestvalues}{A length(\code{kmin}:\code{kmax}) vector giving
   the best values of the criterion obtained for each cardinality. If
   \code{improvement} is TRUE, these values result from the final
   restricted local search algorithm (and may therefore exceed the
   largest value for that cardinality in \code{values}).}

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

[2] Cadima, J. and Jolliffe, I.T. (2001). Variable Selection and the
Interpretation of Principal Subspaces, \emph{Journal of Agricultural,
Biological and Environmental Statistics}, Vol. 6, 62-79.

[3] Duarte Silva, A.P. (2001) Efficient Variable Screening for Multivariate Analysis,
\emph{Journal of Multivariate Analysis}, Vol. 76, 35-62.

[4] Lawless, J. and Singhal, K. (1978). Efficient Screening of Nonnormal 
Regression Models, \emph{Biometrics}, Vol. 34, 318-327.

}


\examples{

## --------------------------------------------------------------------

##
## 1) For illustration of use, a small data set with very few iterations
## of the algorithm.  Escoufier's 'RV' criterion is used to select variable
## subsets of size 3 and 4.
##

data(swiss)
genetic(cor(swiss),3,4,popsize=10,nger=5,criterion="Rv")

## For cardinality k=
##[1] 4
## there is not enough genetic diversity in generation number 
##[1] 3
## for acceptable levels of consanguinity (couples differing by at least 2 genes).
## Try reducing the maximum acceptable number  of clones (maxclone) or 
## increasing the population size (popsize)
## Best criterion value found so far:
##[1] 0.9557145
##$subsets
##, , Card.3
##
##            Var.1 Var.2 Var.3 Var.4
##Solution 1      1     2     3     0
##Solution 2      1     2     3     0
##Solution 3      1     2     3     0
##Solution 4      3     4     6     0
##Solution 5      3     4     6     0
##Solution 6      3     4     5     0
##Solution 7      3     4     5     0
##Solution 8      1     3     6     0
##Solution 9      1     3     6     0
##Solution 10     1     3     6     0
##
##, , Card.4
##
##            Var.1 Var.2 Var.3 Var.4
##Solution 1      2     4     5     6
##Solution 2      1     2     5     6
##Solution 3      1     2     3     5
##Solution 4      1     2     4     5
##Solution 5      1     2     4     5
##Solution 6      1     4     5     6
##Solution 7      1     4     5     6
##Solution 8      1     4     5     6
##Solution 9      1     3     4     5
##Solution 10     1     3     4     5
##
##
##$values
##               card.3    card.4
##Solution 1  0.9141995 0.9557145
##Solution 2  0.9141995 0.9485699
##Solution 3  0.9141995 0.9455508
##Solution 4  0.9034868 0.9433203
##Solution 5  0.9034868 0.9433203
##Solution 6  0.9020271 0.9428967
##Solution 7  0.9020271 0.9428967
##Solution 8  0.8988192 0.9428967
##Solution 9  0.8988192 0.9357982
##Solution 10 0.8988192 0.9357982
##
##$bestvalues
##   Card.3    Card.4 
##0.9141995 0.9557145 
##
##$bestsets
##       Var.1 Var.2 Var.3 Var.4
##Card.3     1     2     3     0
##Card.4     2     4     5     6
##
##$call
##genetic(mat = cor(swiss), kmin = 3, kmax = 4, popsize = 10, nger = 5, 
##    criterion = "Rv")



## --------------------------------------------------------------------

##
## 2) An example of subset selection in the context of Multiple Linear
## Regression. Variable 5 (average car price) in the Cars93 MASS library 
## data set is regressed on 13 other variables. The six-variable subsets 
## of linear predictors are chosen using the "CCR1_2" criterion which, 
## in the case of a Linear Regression, is merely  the standard Coefficient 
## of Determination, R^2 (as are the other three criteria for the
## multivariate linear hypothesis, "XI_2", "TAU_2" and "ZETA_2").
##

library(MASS)
data(Cars93)
CarsHmat <- lmHmat(Cars93[,c(7:8,12:15,17:22,25)],Cars93[,5])

names(Cars93[,5,drop=FALSE])
##  [1] "Price"

colnames(CarsHmat)

##  [1] "MPG.city"           "MPG.highway"        "EngineSize"        
##  [4] "Horsepower"         "RPM"                "Rev.per.mile"      
##  [7] "Fuel.tank.capacity" "Passengers"         "Length"            
## [10] "Wheelbase"          "Width"              "Turn.circle"       
## [13] "Weight"            


genetic(CarsHmat$mat, kmin=6,  H=CarsHmat$H, r=1, crit="CCR12")

## 
## (Partial results only)
## 
## $subsets
##             Var.1 Var.2 Var.3 Var.4 Var.5 Var.6
## Solution 1      4     5     9    10    11    12
## Solution 2      4     5     9    10    11    12
## Solution 3      4     5     9    10    11    12
## Solution 4      4     5     9    10    11    12
## Solution 5      4     5     9    10    11    12
## Solution 6      4     5     9    10    11    12
## Solution 7      4     5     8    10    11    12
## 
## (...)
## 
## Solution 94      1     4     5     6    10    11
## Solution 95      1     4     5     6    10    11
## Solution 96      1     4     5     6    10    11
## Solution 97      1     4     5     6    10    11
## Solution 98      1     4     5     6    10    11
## Solution 99      1     4     5     6    10    11
## Solution 100     1     4     5     6    10    11
## 
## $values
##   Solution 1   Solution 2   Solution 3   Solution 4   Solution 5   Solution 6 
##    0.7310150    0.7310150    0.7310150    0.7310150    0.7310150    0.7310150 
##   Solution 7   Solution 8   Solution 9  Solution 10  Solution 11  Solution 12 
##    0.7310150    0.7271056    0.7271056    0.7271056    0.7271056    0.7271056 
##  Solution 13  Solution 14  Solution 15  Solution 16  Solution 17  Solution 18 
##    0.7271056    0.7270257    0.7270257    0.7270257    0.7270257    0.7270257 
## 
## (...)
## 
##  Solution 85  Solution 86  Solution 87  Solution 88  Solution 89  Solution 90 
##    0.7228800    0.7228800    0.7228800    0.7228800    0.7228800    0.7228800 
##  Solution 91  Solution 92  Solution 93  Solution 94  Solution 95  Solution 96 
##    0.7228463    0.7228463    0.7228463    0.7228463    0.7228463    0.7228463 
##  Solution 97  Solution 98  Solution 99 Solution 100 
##    0.7228463    0.7228463    0.7228463    0.7228463 
## 
## $bestvalues
##   Card.6 
## 0.731015 
## 
## $bestsets
## Var.1 Var.2 Var.3 Var.4 Var.5 Var.6 
##     4     5     9    10    11    12 
## 
## $call
## genetic(mat = CarsHmat$mat, kmin = 6, criterion = "CCR12", H = CarsHmat$H, 
##     r = 1)


## --------------------------------------------------------------------

## 3) An example of subset selection in the context of a Canonical
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


genetic(CarsHmat$mat, kmin=5, kmax=6, H=CarsHmat$H, r=3, crit="zeta2", tolsym=1e-9)

##  (PARTIAL RESULTS ONLY)
## 
## $subsets
##
##             Var.1 Var.2 Var.3 Var.4 Var.5 Var.6
## Solution 1      4     5     9    10    11     0
## Solution 2      4     5     9    10    11     0
## Solution 3      4     5     9    10    11     0
## Solution 4      4     5     9    10    11     0
## Solution 5      4     5     9    10    11     0
## Solution 6      4     5     9    10    11     0
## Solution 7      4     5     9    10    11     0
## Solution 8      3     4     9    10    11     0
## Solution 9      3     4     9    10    11     0
## Solution 10     3     4     9    10    11     0
##  
## (...)
##
## Solution 87      3     4     6     9    10    11
## Solution 88      3     4     6     9    10    11
## Solution 89      3     4     6     9    10    11
## Solution 90      2     3     4    10    11    12
## Solution 91      2     3     4    10    11    12
## Solution 92      2     3     4    10    11    12
## Solution 93      2     3     4    10    11    12
## Solution 94      2     3     4    10    11    12
## Solution 95      2     3     4    10    11    12
## Solution 96      2     3     4    10    11    12
## Solution 97      1     3     4     6    10    11
## Solution 98      1     3     4     6    10    11
## Solution 99      1     3     4     6    10    11
## Solution 100     1     3     4     6    10    11
## 
## 
## $values
##
##                 card.5    card.6
## Solution 1  0.5018922 0.5168627
## Solution 2  0.5018922 0.5168627
## Solution 3  0.5018922 0.5168627
## Solution 4  0.5018922 0.5168627
## Solution 5  0.5018922 0.5168627
## Solution 6  0.5018922 0.5168627
## Solution 7  0.5018922 0.5096500
## Solution 8  0.4966191 0.5096500
## Solution 9  0.4966191 0.5096500
## Solution 10 0.4966191 0.5096500
## 
## (...)
##
## Solution 87  0.4893824 0.5038649
## Solution 88  0.4893824 0.5038649
## Solution 89  0.4893824 0.5038649
## Solution 90  0.4893824 0.5035489
## Solution 91  0.4893824 0.5035489
## Solution 92  0.4893824 0.5035489
## Solution 93  0.4893824 0.5035489
## Solution 94  0.4893824 0.5035489
## Solution 95  0.4893824 0.5035489
## Solution 96  0.4893824 0.5035489
## Solution 97  0.4890986 0.5035386
## Solution 98  0.4890986 0.5035386
## Solution 99  0.4890986 0.5035386
## Solution 100 0.4890986 0.5035386
## 
## $bestvalues
##    Card.5    Card.6 
## 0.5018922 0.5168627 
## 
## $bestsets
##        Var.1 Var.2 Var.3 Var.4 Var.5 Var.6
## Card.5     4     5     9    10    11     0
## Card.6     4     5     9    10    11    12
## 
## $call
## genetic(mat = CarsHmat$mat, kmin = 5, kmax = 6, criterion = "zeta2", 
##     H = CarsHmat$H, r = 3, tolsym = 1e-09)
## 
## Warning message:
## 
##  The effect description matrix (H) supplied was slightly asymmetric: 
##  symmetric entries differed by up to 3.63797880709171e-12.
##  (less than the 'tolsym' parameter).
##  The H matrix has been replaced by its symmetric part.
##  in: validnovcrit(mat, criterion, H, r, p, tolval, tolsym) 
##

## The selected best variable subsets

colnames(CarsHmat$mat)[c(4,5,9,10,11)]

## [1] "Horsepower" "RPM"        "Length"     "Wheelbase"  "Width"     

colnames(CarsHmat$mat)[c(4,5,9,10,11,12)]

## [1] "Horsepower"  "RPM"         "Length"      "Wheelbase"   "Width"      
## [6] "Turn.circle"

## --------------------------------------------------------------------

}
\keyword{manip}

