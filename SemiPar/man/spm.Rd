\name{spm}
\alias{spm}
\title{Fit a SemiParametric regression Model}
\description{
\code{spm} is used to fit semiparametric
regression models using the mixed model
representation of penalized splines
(per Ruppert, Wand and Carroll, 2003).
}
\usage{
spm(form,random=NULL,group=NULL,family="gaussian",
                spar.method="REML",omit.missing=NULL)
}
\arguments{
  \item{form}{a formula describing the model to be fit. 
          Note, that an intercept is always included, whether 
          given in the formula or not.}
  \item{random}{"random=~1" specifies inclusion of a random intercept
         according to the groups specified by the "group" argument.}
  \item{group}{a vector of labels for specifying groups.}
  \item{family}{for specification of the type of likelihood
                model assumed in the fitting.
                May be "gaussian","binomial" or "poisson"}
  \item{spar.method}{method for automatic smoothing parameter
                     selection. May be "REML" (restricted maximum
                     likelihood) or "ML" (maximum likelihood).}
  \item{omit.missing}{a logical value indicating whether
                      fields with missing values are to be
                      omitted.}
}
\value{
  An list object of class \code{"spm"} containing the fitted model.
  The components are:
  \item{fit}{mimics fit object of lme() for
  family="gaussian" and glmmPQL() for family="binomial" or
  family="poisson".}
  \item{info}{information about the inputs.}
  \item{aux}{auxiliary information such as variability
   estimates.}
}
\details{See the SemiPar Users' Manual for details
and examples.
}
\references{
     Ruppert, D., Wand, M.P. and Carroll, R.J. (2003)\cr
     \emph{Semiparametric Regression} Cambridge University Press.\cr
     \url{http://stat.tamu.edu/~carroll/semiregbook/}

     Ganguli, B. and Wand, M.P. (2005)\cr
     \emph{SemiPar 1.0 Users' Manual}.\cr
     \url{http://matt-wand.utsacademics.info/SPmanu.pdf}
}
\author{M.P. Wand 
\email{mwand@uow.edu.au}
(other contributors listed in SemiPar Users' Manual).
}
\seealso{
\code{\link{gam}} (in package `mgcv')
\code{\link{lme}} (in package `nlme')
\code{\link{glmmPQL}} (in package `MASS')
\code{\link{plot.spm}}
\code{\link{summary.spm}}
}
\examples{
library(SemiPar)
data(fossil)
attach(fossil)
fit <- spm(strontium.ratio~f(age))
plot(fit)
summary(fit)

data(calif.air.poll)
attach(calif.air.poll)
fit <- spm(ozone.level ~ f(daggett.pressure.gradient)+
                         f(inversion.base.height) +
                         f(inversion.base.temp))
summary(fit)
par(mfrow=c(2,2))
plot(fit)

# The SemiPar User Manual contains several other examples
# and details of plotting parameters.
#
# The current version of the manual is posted on the web-site:
#
#     http://matt-wand.utsacademics.info/SPmanu.pdf
}
\keyword{nonlinear} \keyword{models} \keyword{smooth} \keyword{regression}

