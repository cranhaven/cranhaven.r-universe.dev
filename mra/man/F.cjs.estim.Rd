\name{F.cjs.estim}
\alias{F.cjs.estim}
\alias{cr.estim}
\alias{CJS}
\alias{cjs}
\alias{Cormack}
\alias{Jolly}
\alias{Seber}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ F.cjs.estim - Cormack-Jolly-Seber estimation }
\description{
   Estimates Cormack-Jolly-Seber (CJS) capture-recapture models with individual, time, and
	individual-time varying covariates using the "regression" parametrization of 
	Amstrup et al (2005, Ch 9). For live recaptures only. Losses on capture allowed.
	Uses a logistic link function to relate probability of capture and survival 
	to external covariates.  
}
\usage{
F.cjs.estim(capture, survival, histories, cap.init, sur.init, group,  nhat.v.meth = 1, 
c.hat = -1, df = NA, intervals=rep(1,ncol(histories)-1), conf=0.95, 
link="logit", control=mra.control())
}

%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{capture}{ Formula specifying the capture probability model.  Must be a formula object with 
	no response. I.e.,  "~" followed by the names of 2-D arrays of covariates to fit in the capture model. 
	For example: 'capture = ~ age + sex', where age and sex are matrices of size NAN X NS 
	containing the age and sex covariate values.  NAN = number of animals = number of rows in 
	\code{histories} matrix (see below).  NS = number of samples = number of columns in histories
	matrix (see below). Number of matrices specified in the capture model is assumed to be NX. Time 
    varying and individual varying vectors are fitted using \code{ivar()} and \code{tvar()} (see Details).
    Factors are allowed within \code{ivar()} and \code{tvar()}. }  
  \item{survival}{ Formula specifying the survival probability model.  Must be a formula object with 
	no response. I.e., "~" followed by the names of 2-D arrays of covariates to fit in the survival model. 
	For example: 'survival = ~ year + ageclass' where year and ageclass are matrices of size NAN X NS 
	containing year and ageclass covariate values.  Number of matrices specified in the survival 
    model is assumed to be NY. 
    Time varying and individual varying vectors are fitted using \code{ivar()} and \code{tvar()} 
    (see Details). Factors are allowed within \code{ivar()} and \code{tvar()}.  }
  \item{histories}{ A NAN X NS = (number of animals) X (number of capture occasions) matrix 
	containing capture histories.  Capture histories are comprised of 0's, 1',s and 2's. 
	0 in cell (i,j) means animal i was not captured on occasion j, 1 in cell (i,j) means animal 
	i was captured on occasion j and released live back into the population, 2 in cell (i,j) means 
	animal i was captured on occasion j and was not released back into the population (e.g., it died).
	Animals with '2' as the last non-zero entry of their history are considered 'censored'. Their 
    lack of capture information  
	is removed from the likelihood after the occasion with the 2. Rows of all zeros (i.e., no captures) are allowed
    in the history matrix, but do not affect coefficient or population size estimates.  A warning is thrown 
    if rows of all zeros exist.  Capture and survival probabilities are 
    computed for animals with all zero histories.  In this way, it is possible to have the routine 
    compute capture or survival estimates for combinations of covariates that do not exist in the data
    by associating the covariate combinations with histories that have all zero entries.}
  \item{cap.init}{ (optional) Vector of initial values for coefficients in the capture model.  One element 
	per covariate in \code{capture}. The default value usually works. }
  \item{sur.init}{ (optional) Vector or initial values for coefficients in the survival model.  One element 
	per covariate in \code{survival}. The default value usually works. }
  \item{group}{ (optional) A vector of length NAN giving the (non-changing) group membership of every 
	captured animal (e.g., sex). Group is used only for computing TEST 2 and TEST 3. 
	TEST 2 and TEST 3 are computed separately for each group. E.g., if group=sex, 
	TEST 2 and TEST 3 are computed for each sex. TEST 2 and TEST3 are used only
	to estimate C-hat. See \code{c.hat} for pooling rules for these 
	test components to estimate C-hat.}
  \item{nhat.v.meth}{ Integer specifying method for computing variance estimates 
	of population size estimates. \code{nhat.v.meth} = 1 uses the variance estimator of 
	Taylor et al. 2002, Ursus, p. 188 which is the so-called Huggins variance estimator, and incorporates 
	covariances.  \code{nhat.v.meth} = 2 uses the variance estimator of Amstrup et al. 
	2005 (p. 244, Eqn. 9.10), which is the same variance estimator as \code{nhat.v.meth} = 1 
	with more 2nd order approximation terms included.  Method 2 should provide better variances 
	than method 1, especially if the coefficient of variation of capture probabilities 
	are >1.0, but method 2 has not been studied as much as method 1. 
	\code{nhat.v.meth} = 3 uses the variance estimator of McDonald and Amstrup, 1999, JABES, which is a 1st order approximation
	that does not incorporate covariances. Method 3 is much faster than methods 1 and 2 
	and could be easily calculated by hand, but 
	should only be used when there is little capture heterogeneity. }
  \item{c.hat}{ External (override) estimate of variance inflation factor (\code{c.hat}) to use
	during estimation.  If input value of \code{c.hat} is <= 0, MRA computes 
	an estimate of variance inflation based on TEST 2 and TEST 3
	applied to groups (if called for, see \code{group} above) 
	using Manly, McDonald, and McDonald, 1993, rules for pooling. I.e., 
	all cells in each TEST 2 or TEST 3 Chi-square component table must be 
	>= 5 before that component contributes to the estimate of C-hat.   This rules is  
	slightly different than program MARK's pooling rules, so MRA's and MARK's
	estimates of \code{c.hat} will generally be different.  If the input \code{c.hat} > 0, 
	MRA does not estimate C.hat, and uses the supplied value. }
  \item{df}{ External (override) model degrees of freedom to use during estimation.  
	If \code{df} == NA, the number of parameters is estimated from the rank of the 
	matrix of 2nd derivatives or Hessian, depending on \code{cov.meth} parameter.  
	If \code{df} <= 0, the number of parameters will be 
	set to NX+NY = the number of estimated coefficients. Otherwise, if \code{df} > 0, 
	the supplied value is used.  Only AIC, QAIC, AICc, and QAICc are dependent on 
	this value (in their penalty terms). }
  \item{intervals}{Time intervals.  This is a vector of length \code{ncol(histories)-1} (i.e., 
	number of capture occasions minus 1) specifying relative time intervals between occasions. 
	For example, if capture occasions occurred in 1999, 2000, 2005, and 2007 \code{intervals} would 
	be set to \code{c(1,5,2)}.  Estimates of survival are adjusted for time intervals between 
	occasions assuming an exponential lifetime model, i.e., probability of surviving 
	from occasion \code{j} to occasion \code{j+1} is \code{Phi(j)^(jth interval length)}, and 
	it is the \code{Phi(j)}'s that are related to covariates through the survival model.  In 
	other words, all survival estimates are for an interval of length 1.  If an interval 
	of 1 is one year, then all survival estimates will be annual survival, with probability 
	of surviving 2 years equal to annual survival squared, probability of surviving 3 years 
	equal to annual survival cubed, etc.}    
  \item{conf}{Confidence level for the confidence intervals placed around estimates 
    of population size. Default 95\% confidence.}
  \item{link}{The link function to be used.  The link function converts linear predictors in the 
    range (-infinity, infinity) to probabilities in the range (0,1).  Valid values for the link 
    function are "logit" (default), "sine", and "hazard".  (see Examples for a plot of the link functions)
     \itemize{
        \item The "logit" link is \eqn{\eta = log( \frac{p}{1 - p} )}{eta = log( p / (1 - p) )} with 
            inverse \eqn{p = \frac{1}{1 + exp(-\eta)}}{p = 1 / (1 + exp(-eta))}.  
        \item The "sine" link is \eqn{\eta = \frac{8asin( 2p - 1 )}{\pi}}{eta = 8*asin( 2*p - 1 ) / pi }, which ranges from -4 to 4. 
            The inverse "sine" link is \eqn{p = \frac{1 + sin( \eta\pi/8 )}{2}}{p = 0.5*(1 + sin( eta*pi/8 ))} for 
            values of \eqn{\eta}{eta} between 
            -4 and 4.  For values of \eqn{\eta}{eta} < -4, \eqn{p} = 0.  For values of  \eqn{\eta}{eta} > 4, \eqn{p} = 1.
            Scaling of the sine link was chosen to yield coefficients roughly the 
            same magnitude as the logit link.
        \item The "hazard" link is \eqn{\eta = log( -log( 1 - p ))}{eta = log( -log( 1 - p ))}, with inverse 
        \eqn{1 - exp( -exp( \eta ))}{1 - exp( -exp( eta ))}.  The value of \eqn{p} from the inverse hazard 
            link approaches 0 as \eqn{\eta}{eta} decreases.  For values of \eqn{\eta}{eta} > 3, \eqn{p} = 1 
            for all intents and purposes. 
     }
    } 
  \item{control}{A list containing named control parameters for the minimization and estimation process. 
    Control parameters include number of iterations, covariance estimation method, etc.  
    Although the default values work in the vast majority of cases, changes to these 
    variables can effect speed and performance for ill-behaved models. See 
    \code{mra.control()} for a description of the individual control parameters.} 
 
}

\details{
	This is the work-horse routine for estimating CJS models. It compiles all the 
	covariate matrices, then calls a Fortran routine to maximize the CJS likelihood 
	and perform goodness-of-fit tests.  Horvitz-Thompson-type population size estimates 
	are also computed by default. 
  
	If \code{control=mra.control(trace=1)}, a log file, named \code{mra.log}, is written to the current directory.  This file contains
     	additional details, such as individual Test 2 and Test 3 components, in a semi-friendly 
     	format.  This file is overwritten each run.  See \code{help(mra.control)} for more details. 

    \bold{Model Specification:} Both the \code{capture} and \code{survival} model can be 
    specified as any combination of 2-d matrices (time and individual varying covariates), 
    1-d time varying vectors, 1-d individual 
    varying vectors, 1-d time varying factors, and 1-d individual varying factors. 

    Specification of time or individual varying effects uses the 
    \code{tvar} (for 'time varying') and \code{ivar} (for 'individual varying') functions. 
    These functions expand covariate vectors along the appropriate dimension to 
    be 2-d matrices suitable for fitting in the model.  \code{ivar} expands 
    an individual varying vector to all occasions.  \code{tvar} expands a time 
    varying covariate to all individuals.  To do the expansion, both \code{tvar} 
    and \code{ivar} need to know the size of the 'other' dimension. Thus, \code{tvar(x,100)}
    specifies a 2-d matrix with size \code{100} by \code{length(x)}. 
    \code{ivar(x,100)}
    specifies a 2-d matrix with size \code{length(x)} by \code{100}. 

    For convenience, the 'other' dimension of time or individual varying covariates 
    can be specified as an attribute of the vector.  Assuming \code{x} 
    is a NS vector and the 'nan' attribute of \code{x} has been set as
    \code{attr(x,"nan") <- NAN}, \code{tvar(x,NAN)} and 
    \code{ tvar(x) } are equivalent. Same, but vise-versa, for individual varying 
    covariates (i.e., 
    assign the number of occasions using \code{attr(x,"ns")<-NS}).  This saves 
    some typing in model specification.

    Factors are allowed in \code{ivar} and \code{tvar}.  When a factor is specified, 
    the \code{contr.treatment} coding is used.  By default, an intercept 
    is assumed and the first level of all 
    factors are dropped from the model (i.e., first levels are the reference levels, 
    the default R action).  However, there are applications where more than one level 
    will need to be dropped, and the user has control over this via the \code{drop.levels}
    argument to \code{ivar} and \code{tvar}.  For example, 
    \code{tvar(x,drop.levels=c(1,2))} drops the first 2 levels of factor x.  
    \code{tvar(x,drop.levels=length(levels(x)))} does the SAS thing and drops 
    the last level of factor \code{x}.  If \code{drop.levels} is outside the range 
    [1,\code{length(levels(x))}] (e.g., negative or 0), no levels of the factor 
    are dropped.  If no intercept is fitted in the model, this results in the 
    so-called cell means coding for factors. 

    
    Example model specifications:  Assume 'age' is a NAN x NS 2-d matrix of ages, 
    'effort' is a size NS 1-d vector of efforts, and 'sex' is a size NAN 1-d factor 
    of sex designations ('M' and 'F'). 
    \enumerate{
        \item capture= ~ 1 : constant effect over all individuals and time (intercept only model)
        \item capture= ~ age  : Intercept plus age 
        \item capture= ~ age + tvar(effort,NAN) : Intercept plus age plus effort
        \item capture= ~ age + tvar(effort,NAN) + ivar(sex,NS) : Intercept plus age plus effort plus sex. 
            Females (1st level) are the reference.
        \item capture= ~ -1 + ivar(sex,NS,0) : sex as a factor, cell means coding
        \item capture= ~ tvar(as.factor(1:ncol(histories)),nrow(histories),c(1,2)) : time varying effects
    }

   
    \bold{Values in 2-d Matrix Covariates:} Even though covariate matrices are required to 
    be NAN x NS (same size as capture histories), there are not that many parameters.
    The first capture probability cannot be estimated in CJS models, and the NS-th survival 
    parameter does not exist.  When a covariate matrix appears in the \emph{capture} model,
    only values in columns 2:ncol(histories) are used.  When a covariate matrix appears in 
    the \emph{survival} model, only values in columns 1:(ncol(histories)-1) are used. 
    See examples for demonstration.
    

}

\note{MARK Users:  Due to differences in the way MRA and MARK parameterize the sine link, \emph{coefficient} 
    estimates will differ between the two packages when this link is used to fit the same model in both packages.  
    The fit (measured by deviance, AIC, etc.) will agree between the two packages.  Capture and survival probability
    estimates will also agree between the two packages.  
    
    MARK does not contain a hazard rate link function. 
}


\value{An object (list) of class c("cjs","cr") with many components.  Use \code{print.cr} to print 
	it nicely. Use \code{names(fit)}, where the call was \code{fit <- F.cr.estim(...)}, 
	to see names of all returned components. To see values of individual components, 
	issue commands like fit$s.hat, fit$se.s.hat, fit$n.hat, etc. 

 	Components of the returned object are as follows:
  \item{histories}{The input capture history matrix.}
  \item{aux}{Auxiliary information about the fit, mostly stored input values. This is a list containing 
    the following components:
    \itemize{
        \item call = original call
        \item nan = number of animals
        \item ns = number of samples = number of capture occasions
        \item nx = number of coefficients in capture model
        \item ny = number of coefficients in survival model
        \item cov.name = names of all coefficients
        \item ic.name = name of capture history matrix. 
        \item mra.version =	version number of MRA package used to estimate the model
        \item R.version = R version used for during estimation
        \item run.date = date the model was estimated.
    }
  }  
  \item{loglik}{Maximized CJS likelihood value for the model}
  \item{deviance}{Model deviance = -2*\code{loglik}.  This is relative deviance, see help for 
	\code{F.sat.lik}.}
  \item{aic}{AIC for the model = \code{deviance} + 2*(df).  df is either the estimated number of independent 
	parameters (by default), or NX+NY, or a specified value, depending on the input value of DF parameter.}
  \item{qaic}{QAIC (quasi-AIC) = (\code{deviance} / \code{vif}) + 2(df)}
  \item{aicc}{AIC with small sample correction = AIC + (2*\code{df}*(\code{df}+1)) / (\code{nan} - \code{df} - 1)}
  \item{qaicc}{QAIC with small sample correction = QAIC + (2*\code{df}*(\code{df}+1))/(\code{nan} - \code{df} - 1)}
  \item{vif}{Variance inflation factor used = estimate of c.hat = \code{chisq.vif} / \code{chisq.df} }
  \item{chisq.vif}{Composite Chi-square statistic from Test 2 and Test 3 used to compute \code{vif}, based 
	on pooling rules.}
  \item{vif.df}{Degrees of freedom for composite chi-square statistic from Test 2 and Test 3, based on pooling rules.}
  \item{parameters}{Vector of all coefficient estimates, NX capture probability coefficients first, 
	then NY survival coefficients.  This vector is length NX+NY regardless of estimated DF.}   
  \item{se.param}{Standard error estimates for all coefficients.  Length NX+NY.}
  \item{capcoef}{Vector of coefficients in the capture model.  Length NX.}
  \item{se.capcoef}{Vector of standard errors for coefficients in capture model. Length NX.}
  \item{surcoef}{Vector of coefficients in the survival model. Length NY.}
  \item{se.surcoef}{Vector of standard errors for coefficients in survival model. Length NY.}
  \item{covariance}{Variance-covariance matrix for the estimated model coefficients.  
	Size (NX+NY) X (NX+NY).}
  \item{p.hat}{Matrix of estimated capture probabilities computed from the model.  
	One for each animal each occasion.  Cell (i,j) is estimated capture probability 
	for animal i during capture occasion j.
	Size NAN X NS.  First column corresponding to first capture probability is NA because
	cannot estimate P1 in a CJS model.}
  \item{se.p.hat}{Matrix of standard errors for estimated capture probabilities. One for each animal 
	each occasion.	Size NAN X NS.  First column is NA.}
  \item{s.hat}{Matrix of estimated survival probabilities computed from the model.  One for each animal each occasion.
	Size NAN X NS.  Cell (i,j) is estimated probability animal i survives from 
	occasion j to j+1.  There are only NS-1 intervals between occasions. Last 
	column corresponding to survival between occasion NS and NS+1 is NA.}
  \item{se.s.hat}{Matrix of standard errors for estimated survival probabilities. 
	Size NAN X NS.  Last column is NA.}
  \item{df}{The number of parameters assumed in the model.  This value was used in the penalty term of AIC, AICc, QAIC, and QAICc. 
    This value is either the number of independent 
	parameters estimated from the rank of the variance-covariance matrix (by default), 
	or NX+NY, or a specified value, depending on the input value of DF parameter.  See \code{F.update.df} to update this value after 
    the model is fitted.} 
  \item{df.estimated}{The number of parameters estimated from the rank of the variance-covariance matrix.  This 
    is stored so that \code{df} can be updated using \code{F.update.df}. }
  \item{control}{A list containing the input maximization and estimation control parameters.}
  \item{message}{A vector of strings interpreting various codes about the estimation. 
    The messages interpret, in this order, the codes for (1) maximization algorithm used, 
    (2) exit code from the maximization algorithm (interprets \code{exit.code}), and 
    (3) covariance matrix code (interprets \code{cov.code}). }
  \item{exit.code}{Exit code from the maximization routine.  Interpretation for \code{exit.code} is in \code{message}.
    Exit codes are as follows: 
    \itemize{
        \item exit.code = 0: FAILURE: Initial Hessian not positive definite.
        \item exit.code = 1: SUCCESS: Convergence criterion met.
        \item exit.code = 2: FAILURE: G'dX > 0, rounding error.
        \item exit.code = 3: FAILURE: Likelihood evaluated too many times.
        \item exit.code =-1: FAILURE: Unknown optimization algorithm."
    }      
  }
  \item{cov.code}{A code indicating the method used to compute the covariance matrix.}
  \item{fn.evals}{The number of times the likelihood was evaluated prior to exit from the minimization routine. 
    If \code{exit.code} = 3, \code{fn.evals} equals the maximum set in \code{mra.control}.  This, in combination 
    with the exit codes and execution time, can help detect non-convergence or bad behavior.   }
  \item{ex.time}{Execution time for the maximization routine, in \emph{minutes}.  This is returned for 
    2 reasons.  First, this is useful for benchmarking. Second, in conjunction with 
    \code{exit.code}, \code{cov.code}, and \code{fn.evals}, this could be used to detect ill-behaved 
    or marginally unstable problems, if you know what you are doing.  Assuming \code{maxfn} is set high 
    in \code{mra.control()} (e.g., 1000), 
    if \code{exit.code = 1} but the model takes a long time to execute relative to similarly sized problems, 
    it could indicate unstable or marginally ill-behaved models. }  
  \item{n.hat}{Vector of Horvitz-Thompson estimates of population size.  The Horvitz-Thompson 
    estimator of size is, 
    \deqn{\hat{N}_{ij} = \sum_{i=1}^{NAN} \frac{h_{ij}}{\hat{p}_{ij}}}{N(ij) = sum( h(ij) / p(ij))}
    Length of \code{n.hat} = NS.  No estimate for 
	first occasion.}
  \item{se.n.hat}{Estimated standard errors for \code{n.hat} estimates.  Computed using method 
	specified in \code{nhat.v.meth}.}
  \item{n.hat.lower}{Lower limit of \code{n.hat.conf} percent on \code{n.hat}.  Length NS.} 
  \item{n.hat.upper}{Upper limit of \code{n.hat.conf} percent on \code{n.hat}.  Length NS.} 
  \item{n.hat.conf}{Confidence level of intervals on \code{n.hat}}
  \item{nhat.v.meth}{Code for method used to compute variance of \code{n.hat}}
  \item{num.caught}{Vector of observed number of animals captured each occasion.  Length NS.}
  \item{fitted}{Matrix of fitted values for the capture histories.  Size NAN X NS.  
    Cell (i,j)
	is expected value of capture indicator in cell (i,j) of \code{histories} matrix.}
  \item{residuals}{Matrix of Pearson residuals defined as, 
    \deqn{r_{ij} = \frac{(h_{ij} - \Psi_{ij})^2}{\Psi_{ij}}}{(h(ij) - Psi(ij))^2 / Psi(ij)}, 
    where \eqn{\Psi_{ij}}{Psi(ij)} is the expected (or fitted) value for cell 
    (i,j) and \eqn{h_{ij}}{h(ij)} is the capture indicator for 
	animal i at occasion j.  This matrix has size NAN X NS. See parts pertaining to the "overall test" in 
	documentation for \code{F.cjs.gof} for a description of \eqn{\Psi_{ij}}{Psi(ij)}. }
  \item{resid.type}{String describing the type of residuals computed.  Currently, only Pearson
	residuals are returned.}
}



\references{ 
Taylor, M. K., J. Laake, H. D. Cluff, M. Ramsay, and F. Messier. 2002. Managing the risk from hunting for 
	the Viscount Melville Sound polar bear population. Ursus 13:185-202.

Manly, B. F. J., L. L. McDonald, and T. L. McDonald. 1999. The robustness of mark-recapture methods: a 
	case study for the northern spotted owl. Journal of Agricultural, Biological, and Environmental 
	Statistics 4:78-101.

Huggins, R. M. 1989. On the statistical analysis of capture experiments. Biometrika 76:133-140.

Amstrup, S. C., T. L. McDonald, and B. F. J. Manly (editors). 2005. Handbook of 
	Capture-Recapture Analysis. Princeton University Press.

Peterson. 1986. Statistics and Probability Letters. p.227.

McDonald, T. L., and S. C. Amstrup. 2001. Estimation of population size using 
	open capture-recapture models. Journal of Agricultural, Biological, and Environmental 
	Statistics 6:206-220.
 }
\author{ Trent McDonald, WEST-INC, tmcdonald@west-inc.com }

\seealso{ \code{\link{tvar}}, \code{\link{ivar}}, \code{\link{print.cjs}}, \code{\link{residuals.cjs}}, \code{\link{plot.cjs}}, 
  \code{\link{F.cjs.covars}}, \code{\link{F.cjs.gof}}, \code{\link{mra.control}}, \code{\link{F.update.df}} }

\examples{



## Fit CJS model to dipper data, time-varying capture and survivals.
## Method 1 : using factors
data(dipper.histories)
ct <- as.factor( paste("T",1:ncol(dipper.histories), sep=""))
attr(ct,"nan")<-nrow(dipper.histories)
dipper.cjs <- F.cjs.estim( ~tvar(ct,drop=c(1,2)), ~tvar(ct,drop=c(1,6,7)), dipper.histories )

## Method 2 : same thing using 2-d matrices
xy <- F.cjs.covars( nrow(dipper.histories), ncol(dipper.histories) )
# The following extracts 2-D matrices of 0s and 1s
for(j in 1:ncol(dipper.histories)){ assign(paste("x",j,sep=""), xy$x[,,j]) } 
dipper.cjs <- F.cjs.estim( ~x3+x4+x5+x6+x7, ~x2+x3+x4+x5, dipper.histories )

## Values in the 1st column of capture covariates do not matter
x3.a <- x3
x3.a[,1] <- 999
dipper.cjs2 <- F.cjs.estim( ~x3.a+x4+x5+x6+x7, ~x2+x3+x4+x5, dipper.histories )
# compare dipper.cjs2 to dipper.cjs

## Values in the last column of survival covariates do not matter
x3.a <- x3
x3.a[,ncol(dipper.histories)] <- 999
dipper.cjs2 <- F.cjs.estim( ~x3+x4+x5+x6+x7, ~x2+x3.a+x4+x5, dipper.histories )
# compare dipper.cjs2 to dipper.cjs


## A plot to compare the link functions
sine.link <- function(eta){ ifelse( eta < -4, 0, ifelse( eta > 4, 1, .5*(1+sin(eta*pi/8)))) }
eta <- seq(-5,5, length=40)
p1 <- 1 / (1 + exp(-eta))
p2 <- sine.link(eta)
p3 <- 1.0 - exp( -exp( eta ))
plot(eta, p1, type="l" )
lines(eta, p2, col="red" )
lines(eta, p3, col="blue" )
legend( "topleft", legend=c("logit", "sine", "hazard"), col=c("black", "red", "blue"), lty=1)




}

\keyword{ survival }% at least one, from doc/KEYWORDS

\keyword{ models }% __ONLY ONE__ keyword per line
