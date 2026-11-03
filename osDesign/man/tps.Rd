\name{tps}
\Rdversion{2.0}
\alias{tps}
\title{
	Estimation for two-phase designs.
}
\description{
Fits a logistic regression model to data arising from two phase designs
}
\usage{
tps(formula=formula(data), data=parent.frame(), nn0, nn1, group,
    contrasts=NULL, method="PL", cohort=TRUE, alpha=1)
}
\arguments{
  \item{formula}{
    A formula expression as for other binomial response regression models, of the form  response ~ predictors, where both the response and predictors corresponds to observations at phase II sample. The response can be either a vector of 0's and 1's or else a matrix with two columns representing number of cases (response=1) and controls (response=0) corresponding to the rows of the design matrix.
  }
  \item{data}{
    An optional data frame for phase two sample in which to interpret the variables occurring in the formula.
  }
  \item{nn0}{
    A numeric vector of length K, indicating the numbers of controls for each Phase I strata.
  }
  \item{nn1}{
    A numeric vector of length K, indicating the numbers of cases for each Phase I strata.
  }
  \item{group}{
    A numeric vector providing stratum identification for phase II data. Values should be in \code{\{1,...,K\}}, where K is the number of strata
    (vector of same length as the response and predictors). A vector indicating a stratum for each row of the design matrix.
  }
  \item{contrasts}{
    A list of contrasts to be used for some or all of the factors appearing as variables in the model formula. See the documentation of \code{\link{glm}} for more details.
  }
  \item{method}{
  Three different procedures are available. The default method is "PL" which implements pseudo-likelihood as developed by Breslow and Cain (1988). Other possible choices
  are "WL" and "ML"  which implements, respectively, weighted likelihood (Flanders and Greenland, 1991; Zhao and Lipsitz, 1992) and maximum likelihood
  (Breslow and Holubkov, 1997; Scott and Wild, 1997).
}
\item{cohort}{
    Logical flag. TRUE indicates phase I is drawn as a cohort; FALSE indicates phase I is drawn as a case-control sample.
}
  \item{alpha}{
    Marginal odds of observing a case in the population. This is only used when cohort=F is specified and must be correctly specified in order to obtain a correct estimate of the intercept.
  }
}
\details{
Returns estimates and standard errors  from logistic regression fit to data arising from two phase designs. Three semiparametric methods are implemented to obtain estimates of the
regression coefficients and their standard errors. Use of this function requires existence of a finite number of strata (K) so that the phase one data consist of a joint
classification into 2K cells according to binary outcome and stratum. This function can also handle certain missing value and measurement error problems with validation data.

The phase I sample can involve either cohort or case-control sampling. This software yields correct estimates (and standard errors) of all the regression coefficients
(including the intercept) under cohort sampling at phase I. When phase I  involves case-control sampling one  cannot estimate the intercept, except, when the marginal odds
of observing a case in the population is specified. Then the software yields a correct estimate and standard error for the intercept also.  

The WL method fits a logistic regression model to the phase II data with a set of weights. Each unit is weighted by the ratio of frequencies (phase I/phase II) for the
corresponding outcome X stratum cell. This estimator has its origins in sampling theory and is well known as Horvitz-Thompson method. The PL method maximizes the product
of conditional probabilities of "being a case" given the covariates and the fact of inclusion in the phase II sample. This is  called the "complete data likelihood" by some
researchers. The estimate is obtained by fitting a logistic regression model to the phase II data with a set of offsets. The ML procedure maximizes the full likelihood of
the data (phase I and II) jointly with respect to the regression parameters and the marginal distribution of the covariates. The resulting concentrated score equations
(Breslow and Holubkov (1997) , eq. 18) were solved using a modified Newton-Raphson algorithm. Schill's (1993) partial likelihood estimates are used as the starting values.

NOTE: In some settings, the current implementation of the ML estimator returns point estimates that do not satisfy the phase I and/or phase II constraints. If this is the case a warning is printed and the "fail" elements of the returned list is set to TRUE. An example of this is phenomenon is given below. When this occurs, users are encouraged to either report the PL estimator or consider using Chris Wild's 'missreg' package.

}
\value{
	\code{tps()} returns a list that includes estimated regression coefficients and one or two estimates of their asymptotic variance-covariance matrix:
	\item{coef}{Regression coefficient estimates}
	\item{covm}{Model based variance-covariance matrix. This is available for \code{method} = "PL" and "ML".}
	\item{cove}{Empirical variance-covariance matrix. This is available for all the three methods.}
	\item{fail}{Indicator of whether or not the phase I and/or the phase II constraints are satisfied; only relevant for the ML estimator.}
}
\references{
Flanders W. and Greenland S. (1991) "Analytic methods for two-stage case-control studies and other stratified designs." Statistics in Medicine 10:739-747.

Zhao L. and Lipsitz S. (1992) "Design and analysis of two-stage studies."Statistics in Medicine 11:769-782.

Schill, W., Jockel K-H., Drescher, K. and Timm, J.(1993). "Logistic analysis in case-control studies under validation sampling." Biometrika 80:339-352.

Scott, A. and Wild, C. (1997) "Fitting regression models to case control data by maximum likelihood." Biometrika 78:705-717.

Breslow, N. and Holubkov, R. (1997) "Maximum likelihood estimation for logistic regression parameters under two-phase, outcome dependent sampling." J. Roy. Statist. Soc. B. 59:447-461.

Breslow, N. and Cain, K. (1988) "Logistic regression for two-stage case control data." Biometrika 75:11-20.

Breslow, N. and Chatterjee, N. (1999) "Design and analysis of two phase studies with binary outcome applied to Wilms tumour prognosis." Applied Statistics 48:457-468.

Haneuse, S. and Saegusa, T. and Lumley, T. (2011) "osDesign: An R Package for the Analysis, Evaluation, and Design of Two-Phase and Case-Control Studies." Journal of Statistical Software, 43(11), 1-29.
}
\author{
  Nilanjan Chaterjee, Norman Breslow, Sebastien Haneuse
}
\examples{
##
data(Ohio)

## Phase I stratification based on age
##
Ohio$S <- Ohio$Age + 1
K <- length(unique(Ohio$S))

## Phase I data
##
Ohio$nonDeath <- Ohio$N-Ohio$Death
nn0 <- aggregate(Ohio$nonDeath, list(S=Ohio$S), FUN=sum)$x
nn1 <- aggregate(Ohio$Death, list(S=Ohio$S), FUN=sum)$x

## Phase II sample sizes
##
nPhIIconts <- rep(100, 3)
nPhIIcases <- rep(100, 3)

## 'Generate' phase II data
##
Ohio$conts <- NA
Ohio$cases <- NA
for(k in 1:K)
{
  Ohio$conts[Ohio$S == k] <- rmvhyper(Ohio$nonDeath[Ohio$S == k],
                                      nPhIIconts[k])
  Ohio$cases[Ohio$S == k] <- rmvhyper(Ohio$Death[Ohio$S == k],
                                      nPhIIcases[k])
}

## Three estimators
##
tps(cbind(cases, conts) ~ factor(Age) + Sex + Race, data=Ohio,
    nn0=nn0, nn1=nn1,
    group=Ohio$S, method="WL")
tps(cbind(cases, conts) ~ factor(Age) + Sex + Race, data=Ohio,
    nn0=nn0, nn1=nn1,
    group=Ohio$S, method="PL")
tps(cbind(cases, conts) ~ factor(Age) + Sex + Race, data=Ohio,
    nn0=nn0, nn1=nn1,
    group=Ohio$S, method="ML")

## An example where (most of the time) the constraints are not satisfied and a warning is returned
##
tps(cbind(cases, conts) ~ Sex + Race, data=Ohio,
    nn0=nn0, nn1=nn1,
    group=Ohio$S, method="ML")
}

