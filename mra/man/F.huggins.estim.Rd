\name{F.huggins.estim}
\alias{F.huggins.estim}
\alias{huggins.estim}
\alias{huggins}
\alias{Huggins}

\title{ F.huggins.estim - Estimation of Huggins closed 
    population capture-recapture model. }

\description{
   Estimates Huggin's closed population capture-recapture models with individual, time, and
    individual-time varying covariates using the "regression" parameterization of 
    Amstrup et al (2006, Ch 9). For live recaptures only. 
    A logistic link function is used to relate probability of capture 
    to external covariates.  
}

\usage{
F.huggins.estim(capture, recapture=NULL, histories, remove=FALSE, cap.init, recap.init,
    nhat.v.meth=1, df=NA, link="logit", control=mra.control())
}


\arguments{
  \item{capture}{ Formula specifying covariates to included in the initial 
    capture probability model.  Must be a formula object without 
    a response. Specify ~, followed by the names of 2-D arrays of covariates to relate to 
    initial capture probability. 
    For example: 'capture = ~ age + sex', where age and sex are matrices of size NAN X NS 
    containing the age and sex covariate values.  NAN = number of individuals = number of rows in 
    \code{histories} matrix (see below).  NS = number of samples = number of columns in histories
    matrix (see below). Number of matrices specified in the initial capture model is 
    assumed to be NX. Specify 
    intercept only model as 'capture = ~ 1'.  Specify model without an intercept 
    using 'capture = ~ -1 + x'.  }
  \item{recapture}{ Formula specifying covariates to included in the 
    recapture probability model, or NULL.  Should be specified the same way as the 
    \code{capture} model.  
    For example: 'recapture = ~ behave + sex'. The number of covariates specified in
    the recapture model is NY.  Total number of parameters this routine 
    attempts to estimate is NX+NY. See \code{df} argument. If \code{recapture=NULL}, 
    no recapture model (or the empty model) is estimated.  In this case, 
    recapture probabilities equal initial capture probabilities and both depend on the 
    model in \code{capture}. Note that NULL models are specified without the ~.}
  \item{histories}{ A NAN X NS = (number of individuals) X (number of capture occasions) matrix 
    containing capture histories.  Capture histories are comprised of 0's and 1's only. 
    0 in cell (i,j) of this matrix means individual i was not captured on occasion j, 
    1 in cell (i,j) means individual 
    i was captured on occasion j and released live back into the population.  Because 
    the population 
    being sampled is assumed closed, deaths on capture (known removals) are not allowed. 
    If deaths on capture occurred and an estimate of N at the beginning 
    of the 
    study is sought, remove the entire history, estimate N using this routine from
    the remaining histories, and add back the number of deleted histories.}
  \item{remove}{A logical scalar, or vector of logical values, specifying which 
    \code{capture} covariates to remove from the \code{recapture} model. By default
    (\code{remove=FALSE}), no capture covariates are removed, meaning all terms
    in the model for initial capture also appear in the model for recaptures 
    \emph{with the same coefficient values}.  See Details section.  If \code{remove}
    is a vector, each entry specifies whether the corresponding effect in \code{capture} 
    should be removed from the \code{recapture} model.  
    If \code{remove} is shorter than NX (the number of matrices in 
    \code{capture}), it is replicated to have length NX.}
  \item{cap.init}{ (optional) Vector of initial values for coefficients in the initial 
    capture model.  One element 
    per covariate in \code{capture}. This parameter does not usually need to be specified. }
  \item{recap.init}{ (optional) Vector of initial values for coefficients in the 
    recapture model.  One element 
    per covariate in \code{recapture}. This parameter does not usually need to be specified. }
  \item{nhat.v.meth}{ Integer specifying method for computing variance estimate 
    for the population size estimate. Currently, only \code{nhat.v.meth} = 1 is implemented. 
    Plans are for \code{nhat.v.meth} = 2 to be a boot strap estimate of variance.
    \code{nhat.v.meth} = 1 is a delta method estimator utilizing the derivative of 
    P(ever captured) w.r.t. the capture parameters.  This is the same estimator as 
    used in program MARK. }
  \item{df}{ External (override) model degrees of freedom to use during estimation.  
    If \code{df} == NA, the number of parameters is estimated from the rank of the 
    matrix of 2nd derivatives or Hessian, depending on \code{cov.meth} parameter.  
    If \code{df} <= 0, the number of parameters will be 
    set to NX+NY = the number of estimated coefficients. Otherwise, if \code{df} > 0, 
    the supplied value is used.  Only AIC, QAIC, AICc, and QAICc are dependent on 
    this value (in their penalty terms). }
  \item{link}{The link function to be used.  The link function converts linear predictors in the 
    range (-infinity, infinity) to probabilities in the range (0,1).  Valid values for the link 
    function are "logit" (default), "sine", and "hazard".  (see Examples in help for \code{\link{F.cjs.estim}} 
    for a plot of the link functions)
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
    This routine compiles all the 
    covariate matrices, then calls a Fortran routine to maximize the 
    Huggins closed population likelihood. So-called heterogeneous models 
    that utilize mixture distributions for probability of capture cannot be
    fitted via this routine.  
  
    If \code{remove=FALSE} (default) the models for initial capture and 
    subsequent recapture are,
    \deqn{p_{ij} = \beta_0 + \beta_1 x_{ij1} + \ldots + \beta_a x_{ija}}{%
         p[i,j] = B0 + B1*x1[i,j] + ... + Ba*xa[i,j]}
    and     
    \deqn{c_{ij} = \beta_0 + \beta_1 x_{ij1} + \ldots + \beta_a x_{ija} + 
        \gamma_0 + \gamma_1 z_{ij1} + \ldots + \gamma_b z_{ijb}}{%
         c[i,j] = B0 + B1*x1[i,j] + ... + Ba*xa[i,j] + 
         G0 + G1*z1[i,j] + ... + Gb*zb[i,j]}
    where the x's and z's are covariate values specified in \code{capture} 
    and \code{recapture}, respectively, and the \eqn{\beta}{B}'s and 
    \eqn{\gamma}{G}'s are estimated coefficients. (For brevity, 
    '\emph{a}' has been substituted for NX, '\emph{b}' for NY.) In other words, by default 
    all effects in the capture model also appear in the recapture model 
    \emph{with the same estimated coefficients}.  This is done so that 
    capture and recapture probabilities can be constrained to equal one another. 
    If \code{capture=~1} and \code{recapture=NULL}, capture and recapture 
    probabilities are constant and equal to one another. 
    If \code{capture=~x1} and \code{recapture=NULL}, capture and recapture 
    probabilities are equal, and both are the exact same function of 
    covariate \code{x1}. A simple additive 
    behavioral (trap happy or trap shy) effect is fitted by specifying an 
    intercept-only model for recaptures, i.e.,
    \code{capture=~x1+x2+...+xp} and \code{recapture=~1}.  
    
    When a Huggin's model object is printed using the default 
    print method (\code{print.hug}), a "C" (for "capture") appears next to coefficients
    in the recapture model that are also in the initial capture model.  These 
    coefficients are fixed in the recapture model.  A "B" (for "behavioral")
    appears next to free coefficients in the recapture model that \emph{do not}
    appear in the initial capture model.  
    
    If \code{remove} is something other than FALSE, it is extended to have length
    NX, and if element \emph{i} equals TRUE, the \emph{i}-th 
    effect in the capture model is removed from the recapture model.  If 
    \code{remove=c(FALSE, TRUE, FALSE)}, \code{capture=~x1+x2}, and 
    \code{recapture=~x1+x3}, the models for initial capture and subsequent 
    recapture are, 
    \deqn{p_{ij} = \beta_0 + \beta_1 x_{ij1} + \beta_2 x_{ij2}}{%
         p[i,j] = B0 + B1*x1[i,j] + B2*x2[i,j]}
    and     
    \deqn{c_{ij} = \beta_0 + \beta_2 x_{ij2} + \gamma_0 + \gamma_1 x_{ij1} + \gamma_2 x_{ij3}.}{%
         c[i,j] = B0 + B2*x2[i,j] +  
         G0 + G1*x1[i,j] + G2*x3[i,j].}
    Note that \code{x1} appears in the recapture equation, but with a 
    different estimated coefficient. If \code{remove=TRUE}, all capture effects
    are removed from the recapture model and the models are completely separate.
    
    The ability to remove terms from the recapture model adds modeling flexibility. 
    For example, if initial captures were hypothesized to depend on the variable 
    \code{sex}, but recaptures were hypothesized to be constant (no \code{sex} effect), 
    the arguments to fit this model would be \code{capture=~sex}, \code{recapture=~1}, 
    and \code{remove=TRUE}.  A pure time-varying model with different time 
    effects in the initial and subsequent capture models can be fitted using 
    \code{capture=~tvar(1:ns,nan)}, \code{recapture=~tvar(1:ns,nan)}, 
    and \code{remove=TRUE}.  In this case, the same model, but parameterized differently, 
    can be fitted with \code{remove=FALSE}.  
    
    See Details of \code{help(F.cjs.estim)} for ways that 2-d matrices, 1-d 
    vectors, and 1-d factors can be specified in the capture and recapture
    models.  
    
    If argument \code{trace} in a call to \code{mra.control} is set to something 
    other than 0, a log file named \code{mra.log} is written to the current directory.  
    See \code{\link{mra.control}} for actions associated with values of \code{trace}.
    CAREFUL: \code{mra.log} is overwritten each run. 
    
    Values in 2-d Matrix Covariates: Even though covariate 
    matrices are required to be NAN x NS (same size as capture histories), 
    there are not that many recapture parameters. 
    Recapture parameters for the first occasion are not defined. 
    For all covariates in the recapture model, only values in 
    columns 2:ncol(histories) are used.  See examples for demonstration. 
}


\value{
An object (list) of class c("hug","cr") with many components.
    Use \code{print.hug} to print
    it nicely. Use \code{names(fit)}, where the call was \code{fit <- F.huggins.estim(...)},
    to see names of all returned components. To see values of individual components,
    issue commands like fit\$n.hat, fit\$se.n.hat, etc.

    Components of the returned object are as follows:
  \item{histories}{The input capture history matrix. Size NAN x NS}
  \item{aux}{Auxiliary information, mostly stored input values.
    This is a list containing:
    \$call,
    \$nan=number of individuals,
    \$ns=number of samples,
    \$nx=number of coefficients in the initial capture model,
    \$ny=number of coefficients in recapture model,
    \$cov.name=names of the covariates in both models (initial capture covariates first, then recapture
    covariates),
    \$ic.name=name of capture history matrix,
    \$mra.version=version number of this package,
    \$R.version=R version used,
    \$run.date=date the model was estimated.}
  \item{loglik}{Value of the Huggins log likelihood at it's maximum.}
  \item{deviance}{Model deviance = -2*\code{loglik}.  This is relative deviance
    because all covariates are individual and time varying.  When individual
    covariates are present, a saturated likelihood cannot be computed.  Use this
    to compute deviance differences only.}
  \item{aic}{AIC for the model = \code{deviance} + 2*(df).  df is either the estimated
    number of independent parameters (by default), or NX+NY, or a specified value,
    depending on the input value of \code{df} parameter.}
  \item{aicc}{AIC with small sample correction = AIC + (2*\code{df}*(\code{df}+1))
    / (\code{NAN} - \code{df} - 1)}
  \item{capcoef}{Vector of estimated coefficients in the initial capture model.
    Length NX.}
  \item{se.capcoef}{Vector of estimated standard errors for coefficients in
    initial capture model. Length NX.}
  \item{recapcoef}{Vector of estimated coefficients in the recapture model.
    Length NY.}
  \item{se.surcoef}{Vector of standard errors for coefficients in recapture model.
    Length NY.}
  \item{covariance}{Variance-covariance matrix for the estimated model coefficients.
    Size (NX+NY) X (NX+NY).}
  \item{p.hat}{Matrix of estimated initial capture probabilities
    computed from the model.  Size of this matrix is NAN x NS.
    Cell (i,j) is estimated probability of first capture for individual i
    during capture occasion j.}
  \item{se.p.hat}{Matrix of standard errors for estimated initial capture probabilities.
    Size NAN x NS.}
  \item{c.hat}{Matrix of estimated recapture probabilities computed from the model.
    Size NAN x NS.  Cell (i,j) is estimated probability of capturing individual i
    during occasion j given that it was initially captured prior to j. }
  \item{se.c.hat}{Matrix of standard errors for estimated recapture probabilities.
    Size NAN X NS.  }
  \item{df}{Number of estimable parameters in the model. \code{df} is either
    the estimated number of independent parameters (by default) based on rank
    of the variance matrix,
    or NX+NY, or a specified value, depending on the input value of \code{df} parameter.}
  \item{message}{A string indicating whether the maximization routine converged.}
  \item{exit.code}{Exit code from the maximization routine.
    Interpretation for \code{exit.code} is in \code{message}.}
  \item{cov.code}{A code indicating the method used to compute the covariance matrix.}
  \item{cov.meth}{String indicating method used to compute covariance matrix.
    Interprets \code{cov.code}.}
  \item{n.hat}{The Huggins estimate of population size.  This estimate is
    sum( 1/ pstar(i) ), where pstar(i) is probability of observing individual i,
    which equals 1 - p.hat[i,1]*p.hat[i,2]* ... *p.hat[i,NS], where p.hat is the
    returned value of \code{p.hat}.}
  \item{se.n.hat}{Estimated standard error of \code{n.hat}.  Computed using method
    specified in \code{nhat.v.meth}.}
  \item{n.hat.lower}{Lower limit of log based confidence interval for
    \code{n.hat}.  }
  \item{n.hat.upper}{Upper limit of log based confidence interval for
    \code{n.hat}.}
  \item{n.hat.conf}{Confidence level for the interval on \code{n.hat}.
    Currently, confidence level cannot be changed
    from 95\%. }
  \item{nhat.v.meth}{Code for method used to compute variance of \code{n.hat}.  Currently,
    this is 1 only.}
  \item{num.caught}{Number of individuals ever captured = number of
    rows in the \code{histories} matrix.}
  \item{n.effective}{Effective sample size = number of observed individuals times
    number of occasions = NAN * NS}

}    


\references{ 
Huggins, R. M. 1989. On the statistical analysis of capture experiments. Biometrika 76:133-140.

Amstrup, S. C., T. L. McDonald, and B. F. J. Manly (editors). 2005. Handbook of 
    Capture-Recapture Analysis. Princeton University Press.

 }
\author{ Trent McDonald, WEST-INC, tmcdonald@west-inc.com }

\seealso{ \code{\link{print.hug}}, 
  \code{\link{F.cjs.estim}} }

\examples{

# Fake the data for these examples
set.seed(3425)
ch.mat <- matrix( round(runif(30*5)), nrow=30, ncol=5)
ch.mat <- ch.mat[ apply(ch.mat,1,sum) > 0, ]  # no zero rows allowed
ct <- as.factor(1:ncol(ch.mat))
attr(ct,"nan") <- nrow(ch.mat)   # used to fit time varying factor
sex <- round(runif(nrow(ch.mat)))   # fake sex 
attr(sex,"ns") <- ncol(ch.mat)

# Models parallel to the 8 Otis et al. models.
# see Amstrup et al. (2005, p. 77)

# Constant model (model M(0)).
hug.0 <- F.huggins.estim( ~1, NULL, ch.mat )

# Time varying model (model M(t))
hug.t <- F.huggins.estim( ~tvar(ct), NULL, ch.mat)

# Additive Behavioral model (model M(b))
hug.b <- F.huggins.estim( ~1, ~1, ch.mat )

# Time and Behavioral model (model M(tb))
hug.tb <- F.huggins.estim( ~tvar(ct), ~1, ch.mat )

# Individual effects (model M(h))
hug.h <- F.huggins.estim( ~ivar(sex), NULL, ch.mat )

# Individual and Behavioral effects (model M(bh))
hug.bh <- F.huggins.estim( ~ivar(sex), ~1, ch.mat )

# Individual and time effects (model M(th))
hug.th <- F.huggins.estim( ~ivar(sex)+tvar(ct), NULL, ch.mat )

# Individual, time, and behavoral effects (model M(tbh))
hug.tbh <- F.huggins.estim( ~ivar(sex)+tvar(ct), ~1, ch.mat )

# Time varying initial captures, recaptures are constant and depend on sex.
hug.custom1 <- F.huggins.estim( ~tvar(ct), ~ivar(sex), ch.mat, remove=TRUE )

# Compare hug.custom1 to the following: Time varying initial captures with 
# time varying recaptures that depend on sex.
hug.custom2 <- F.huggins.estim( ~tvar(ct), ~ivar(sex), ch.mat, remove=FALSE )

# Values in first column of recapture covariates do not matter. 
# Below, mod.1 and mod.2 are identical.
mod.1 <- F.huggins.estim( ~tvar(ct), ~tvar( c( 0,1,2,3,4), nrow(ch.mat)), ch.mat, remove=TRUE)
mod.2 <- F.huggins.estim( ~tvar(ct), ~tvar( c(-9,1,2,3,4), nrow(ch.mat)), ch.mat, remove=TRUE)




}
\keyword{ survival }% at least one, from doc/KEYWORDS
\keyword{ models }% __ONLY ONE__ keyword per line
