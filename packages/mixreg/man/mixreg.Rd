\name{mixreg}
\alias{mixreg}
\alias{mixreg.default}
\alias{mixreg.formula}
\title{
Fit a mixture of linear regressions.
}
\description{
Estimates the parameters for a mixture of linear regressions, assuming
Gaussian errors, using the EM algorithm.
}
\usage{
mixreg(x, y, ncomp = NULL, intercept = TRUE, eqVar = FALSE,
       thetaStart = NULL, itmax = 1000,eps = 1e-06, verb = TRUE,
       digits = 7, maxTry = 5, seed = NULL, data = NULL,
       covMat=FALSE,MC=FALSE,warn=TRUE,...)
}
\arguments{
\item{x}{
  Either a formula specifying the regression model in question
  or a matrix of predictors for that regression model. 
  In the latter case \code{x} should \emph{NOT} include
  an initial column of 1s.  (If an intercept is required, leave
  \code{intercept} as \code{TRUE}!)  If there is only one predictor,
  x may be a vector rather than a one-column matrix.
}
\item{y}{
  The vector of responses for the regression models.  Not used and
  \bold{should not be specified} if \code{x} is a formula.
}
\item{ncomp}{
  The number of components in the mixture.  If \code{thetaStart}
  (see below) is supplied, then the number of components is
  determined from this argument and \code{ncomp} is ignored.
  If neither \code{ncomp} nor \code{thetaStart} is supplied then
  \code{ncomp} defaults to 2.
}
\item{intercept}{
  Logical scalar; should the linear regressions should have
  intercepts fitted?  Ignored if \code{x} is a formula.
}
\item{eqVar}{
  Logical scalar; should the error variance should be the same for
  all component?  (The alternative is that each component should be
  allowed a different error variance.)
}
\item{thetaStart}{
  Either a list or a matrix providing starting values for the
  estimation procedure.  If it is a list each of its entries is in
  turn a list with entries \code{beta} (vector of linear coefficients),
  \code{sigsq} (variance) and \code{lambda} (mixing probability).  If it is a
  matrix it must have \code{ncomp} rows and \code{ncoef + 2} columns,
  where \code{ncoef} is the number of regression coefficients in the
  model.  The latter quantity is \code{ncol(x)} if \code{intercept}
  is \code{FALSE} and is \code{1 + ncol(x)} if \code{intercept}
  is \code{TRUE}.

  If \code{eqVar} is \code{TRUE} then it is sensible to have
  all the starting values of \code{sigsq} equal, but this is not
  strictly necessary.  If \code{thetaStart} is not specified,
  starting values are produced \dQuote{randomly}.  This sometimes works,
  but sometimes doesn't.  The function \code{\link{visualFit}()}
  provides a convenient means of determining starting values by
  a \dQuote{visual} procedure.  Note that if \code{object} is the
  value returned by \code{visualFit()} then the appropriate value
  for \code{thetaStart} is \code{object$theta}.
}
\item{itmax}{
  The maximum number of EM steps to be undertaken.  If this maximum
  number of steps is exceeded then a warning is issued, but a
  fit (probably unreliable) is returned anyway.  The returned
  value will, in this case, have the \code{converged} component
  equal to \code{FALSE}.
}
\item{eps}{
  A value specifying the convergence criterion for the EM algorithm.
  If the maximum absolute value of the change in the parameters is
  less than \code{eps} the algorithm is considered to have converged.
}
\item{verb}{
  Logical argument; if \code{verb} is \code{TRUE} then details of the progress
  of the algorithm are printed out at each EM step.
}
\item{digits}{
  The number of digits to which the details are printed out, when
  \code{verb} is \code{TRUE}.`
}
\item{maxTry}{
  If the algorithm encounters a singularity in the likelihood (as may
  possibly occur when eqVar is FALSE) the algorithm is restarted
  using new (randomly generated) starting values.  The restart is
  attempted a maximum of maxTry times.
}
\item{seed}{
  A numeric scalar used to seed the random number generators if
  \code{thetaStart} is not specified (whence starting values are
  produced randomly).   If \code{seed} is not an integer it gets
  rounded to the nearest integer (so \code{seed=pi} has the same
  impact as \code{seed=3}.  If not supplied, \code{seed} is selected
  randomly from \code{1, 2, \dots, 1e5}.  The seed for random number
  generation is set to \code{seed} in \code{initRand()} before
  any random number generation is done.  The argument \code{seed}
  is ignored if \code{thetaStart} is supplied.
}
\item{data}{
  A list or data frame in which the values of the data \code{x} and
  \code{y} may be stored.  If \code{data} is not \code{NULL} then
  \code{mixreg()} looks for \code{x} and \code{y} first in \code{data}
  and then in the global environment.
}
\item{covMat}{
  Logical scalar; should the covariance matrix of the parameter estimates
  be calculated?  (This can take some time.)
}
\item{MC}{
  Logical scalar; should the covariance matrix of the parameter estimates
  be calculated by a Monte Carlo procedure?  Ignored if \code{covMat} is
  \code{FALSE}.
}
\item{warn}{
  Logical scalar.  Should a warning be issued if the EM algorithm fails
  to converge in \code{itmax} iterations?  Note that this failure
  is discernible anyway, from the \code{converged} component of the returned
  value, but it's probably best to be alerted to the failure.  This
  argument is present mainly so that it can be set to \code{FALSE}
  internally in \code{\link{ncMcTest}()}, where such warnings are
  redundant.
}
\item{...}{
  Optional arguments which are passed to \code{\link{covMixMC}()}
  (if \code{covMat} and \code{MC} are both \code{TRUE}.  These
  optional arguments may be \code{semiPar}, \code{conditional} or
  \code{cMseed}.  (Any other argument being given to \code{mixreg()}
  will trigger an error.)
}
}
\value{
  A list, of class mixreg, with components

\item{parmat}{
  The parameters of the fitted model arranged as a matrix, each
  row corresponding to one component of the mixture.
}
\item{theta}{
  The parameters of the fitted model as a list, each entry of
  the list being itself a list (like those in thetaStart) corresponding
  to one component of the mixture.
}
\item{log.like}{
  The log likelihood of the fitted model, based on Gaussian errors.
}
\item{aic}{
  The Akaike Information Criterion value for the fitted model;
  aic is equal to -2 * log.like + 2*M where M is the number of
  parameters in the model.
}
\item{intercept}{
  The value of the \code{intercept} argument.
}
\item{eqVar}{
  The value of the \code{eqVar} argument.
}
\item{nsteps}{
  The number of steps the EM algorithm took to converge.
}
\item{converged}{
  Logical scalar indicating whether the algorithm did indeed `converge
  or stopped because it reach the itmax EM step.
}
\item{data}{
  A list or data frame providing the data \code{x} and \code{y} to
  which the model was fitted.  It may be equal to the value of the
  \code{data} argument, or it may have been constructed, in whole or
  in part, from the \code{x} and \code{y} arguments.
}
\item{formula}{
 The formula used by \code{lm()} in fitting the regression models.
 Depending on circumstances it may be equal to the argument
 \code{fmla}, or it may have been constructed internally by
 \code{mixreg()}, in which case it is of the form \code{y ~ x}
 or, if \code{intercept} is \code{FALSE}, \code{y ~ x - 1} (with,
 of course, \code{x} and \code{y} replaced by the actual names of
 the objects provided as the corresponding arguments).
}

The returned value has an attribute \code{"seed"} specifying the
seed for the random number generators that was used in producing
random starting values.  If \code{thetaStart} was specified then
the attribute \code{"seed"} is \code{NA}.
}
\section{Warning}{
  If the \code{x} argument is a formula, then specifying \code{y}
  is not only meaningless, but will cause a (possibly mystifying)
  error to be thrown.
}
\section{Notes}{
  This function (and indeed the entire \code{mixreg} package) is
  structured so as to be able to deal with mixtures of regressions
  involving any number of predictors.  However I know of no practical
  examples in which more than one predictor is involved, and it seems
  likely that models involving more than one predictor would present
  substantial difficulties.  Some of the functions in this package
  (\code{cband()}, \code{visualFit()}, \code{plot.mixreg()}, \dots)
  can cope only with single-predictor models.

  The entries \code{parmat} and \code{theta} of the returned
  value contain the same information, presented in a different format.

  Even if \code{eqVar} is \code{TRUE}, each entry
  of \code{theta} still has its own \code{sigsq} entry.
  The values of these will all be equal, however, if \code{eqVar}
  is \code{TRUE}.

  For \emph{scalar} mixtures, allowing the components to
  have different variances can have the effect of introducing
  singularities in the likelihood function.  It is not clear
  to me what the impact of allowing different variances is in
  the mixtures-of-regressions setting.  In respect of the scalar
  setting, Aitkin and Tunnicliffe Wilson (see \bold{References})
  assert that the singularities that may arise \dQuote{do not
  cause any computational difficulty in practice}.  However in the
  mixtures of regressions setting, I have observed strange anomalies
  which appear to be alleviated by setting \code{eqVar=TRUE}.
  See \bold{Examples}.  If results seem to be unsatisfactory, you
  may be well-advised to try setting \code{eqVar=TRUE}, to see if
  that makes an improvement.
}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}
\references{
T. Rolf Turner (2000). Estimating the rate of spread of a
viral infection of potato plants via mixtures of regressions.
\emph{Applied Statistics} \bold{49} Part 3, pp. 371 -- 384.

A. P. Dempster, N. M. Laird and D. B. Rubin (1977).  Maximum likelihood
from incomplete data via the EM algorithm. \emph{Journal of the Royal Statistical
Society} series B, \bold{39}, pp. 1 -- 22.

M. Aitkin and G. Tunnicliffe Wilson (1980).  Mixture models, outliers,
and the EM algorithm.  \emph{Technometrics} \bold{22}, pp. 325 -- 331.
}
\seealso{
\code{\link{ncMcTest}()}, \code{\link{cband}()},
\code{\link{qqMix}()}, \code{\link{residuals.mixreg}()}
}
\examples{
# Aphids.
    fit1   <- mixreg(aphRel,plntsInf,ncomp=2,seed=42,data=aphids)
    plot(fit1)
    thStrt <- list(list(beta=c(3.0,0.1),sigsq=16,lambda=0.5),
                   list(beta=c(0.0,0.0),sigsq=16,lambda=0.5))
    fit2   <- mixreg(aphRel,plntsInf,ncomp=2,thetaStart=thStrt,data=aphids)
    fit3   <- mixreg(plntsInf ~ aphRel,ncomp=2,thetaStart=thStrt,data=aphids)
# Kilns.
\dontrun{
    vfit <- visualFit(y ~ x,ncomp=3, data=kilnAoneOut)
    fit  <- mixreg(y ~ x,ncomp=3,data=kilnAoneOut,thetaStart=vfit$theta)
}
    thStrt <- list(
                   list(beta=c(26.07,48808),sigsq=1.1573,lambda=0.33333333),
                   list(beta=c(23.48,32387),sigsq=1.8730,lambda=0.33333333),
                   list(beta=c(-0.0597,20760),sigsq=0.2478,lambda=0.33333333)
                 )
    # Roughly the value of vfit$theta.
    fit  <- mixreg(y ~ x,ncomp=3,data=kilnAoneOut,thetaStart=thStrt)
    plot(fit)
# Kilns, zero intercept model.
\dontrun{
    vfit <- visualFit(y ~ x - 1,ncomp=3, data=kilnAoneOut)
    fit  <- mixreg(y ~ x - 1,ncomp=3,data=kilnAoneOut,thetaStart=vfit$theta)
}
    thStrt <- list(list(beta=50900,sigsq=3.297,lambda=0.33333333),
                  list(beta=33800,sigsq=25.52,lambda=0.33333333),
                  list(beta=20755,sigsq=0.2477,lambda=0.33333333))
    # Roughly the value of vfit$theta.
    fit  <- mixreg(y ~ x - 1,ncomp=3,data=kilnAoneOut,thetaStart=thStrt)
    plot(fit) # Yikes!!!  (But a plot of vfit looks practically perfect.)
    fit  <- mixreg(y ~ x - 1,ncomp=3,data=kilnAoneOut,thetaStart=thStrt,eqVar=TRUE)
    plot(fit) # Looks fine.
}
\keyword{models}
\keyword{regression}
