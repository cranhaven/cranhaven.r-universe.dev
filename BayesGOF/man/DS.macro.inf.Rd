\name{DS.macro.inf}
\alias{DS.macro.inf}
\alias{DS.macro.inf.pge}
\alias{DS.macro.inf.pgu}
\alias{DS.macro.inf.bbu}
\alias{DS.macro.inf.nnu}
\alias{Local.Mode}
\alias{plot.DS_GF_macro_mean}
\alias{plot.DS_GF_macro_mode}
\alias{print.DS_GF_macro_mean}
\alias{print.DS_GF_macro_mode}
\alias{DS.GF.macro.obj}
\title{
Execute MacroInference (mean or mode) on a DS object
}
\description{
A function that generates macro-estimates with their uncertainty (standard error).}
\usage{
DS.macro.inf(DS.GF.obj, num.modes = 1, 
             method = c("mean", "mode"), 
             iters = 25, exposure = NULL)
}

\arguments{
  \item{DS.GF.obj}{Object from \code{DS.prior}.}
  \item{num.modes}{The number of modes indicated by \code{DS.prior} object.}
  \item{method}{Returns mean or mode(s) (based on user choice) along with the associated standard error(s).}
  \item{iters}{Integer value of total number of iterations.}
  \item{exposure}{In the case where \code{DS.GF.obj} is from a Poisson family with exposure, \code{exposure} is the vector of exposures.  Otherwise, the default is \code{NULL}.}
 
 }

\value{
  \item{DS.GF.macro.obj}{Object of class \code{DS.GF.macro} associated with either mean or mode.}
  \item{model.modes}{For \code{method = "mode"}, returns mode(s) of estimated DS prior.}
  \item{mode.sd}{For \code{method = "mode"}, provides the bootstrapped standard error for each mode.}
  \item{boot.modes}{For \code{method = "mode"}, returns all generated mode(s).}
  \item{model.mean}{For \code{method = "mean"}, returns mean of estimated DS prior.}
  \item{mean.sd}{For \code{method = "mean"}, provides the bootstrapped standard error for the mean.}
  \item{boot.mean}{For \code{method = "mean"}, returns all generated means.}
  \item{prior.fit}{Fitted values of estimated prior imported from the \code{DS.prior} object.}

  }
\references{
Mukhopadhyay, S. and Fletcher, D., 2018. "Generalized Empirical Bayes via Frequentist Goodness of Fit," \emph{Nature Scientific Reports}, 8(1), p.9983, \url{https://www.nature.com/articles/s41598-018-28130-5 }.}
\author{Doug Fletcher, Subhadeep Mukhopadhyay}
\examples{
\dontrun{
### MacroInference: Mode
data(rat)
rat.start <- gMLE.bb(rat$y, rat$n)$estimate
rat.ds <- DS.prior(rat, max.m = 4, rat.start. family = "Binomial")
rat.ds.macro <- DS.macro.inf(rat.ds, num.modes = 2, method = "mode", iters = 5)
rat.ds.macro
plot(rat.ds.macro)
### MacroInference: Mean
data(ulcer)
ulcer.start <- gMLE.nn(ulcer$y, ulcer$se)$estimate
ulcer.ds <- DS.prior(ulcer, max.m = 4, ulcer.start)
ulcer.ds.macro <- DS.macro.inf(ulcer.ds, num.modes = 1, method = "mean", iters = 5)
ulcer.ds.macro
plot(ulcer.ds.macro)}
}
\keyword{DS Main Functions}


