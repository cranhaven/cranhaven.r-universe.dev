\name{DS.prior}
\alias{DS.prior}
\alias{DS.prior.nnu}
\alias{DS.prior.pgu}
\alias{DS.prior.bbu}
\alias{plot.DS_GF}
\alias{print.DS_GF}
\alias{Reduce.LP.coef.univ}
\alias{DS.GF.obj}
\alias{LP.smooth}
\alias{maxent.LP.par}
\alias{maxent.obj.convert}
\alias{EXP.score}
\alias{weight.fun.univ}
\title{Prior Diagnostics and Estimation
}
\description{A function that generates the uncertainty diagnostic function (\code{U-function}) and estimates DS\eqn{(G,m)} prior model.
}
\usage{
DS.prior(input, max.m = 8, g.par, 
         family = c("Normal","Binomial", "Poisson"), 
         LP.type = c("L2", "MaxEnt"), 
         smooth.crit = "BIC", iters = 200, B = 1000,
		 max.theta = NULL)
}
\arguments{
  \item{input}{For \code{"Binomial"}, a dataframe that contains the \eqn{k} pairs of successes \eqn{y} and the corresponding total number of trials \eqn{n}.  For \code{"Normal"}, a dataframe that has the \eqn{k} means \eqn{y_i} in the first column and their respective standard errors \eqn{s_i} in the second.  For the \code{"Poisson"}, a vector of that includes the untabled count data.}
  \item{max.m}{The truncation point \eqn{m} reflects the concentration of true unknown \eqn{\pi} around known \eqn{g}. }
  \item{g.par}{Vector with estimated parameters for specified conjugate prior distribution \eqn{g} (i.e beta prior: \eqn{\alpha} and \eqn{\beta}; normal prior: \eqn{\mu} and \eqn{\tau^2}; gamma prior: \eqn{\alpha} and \eqn{\beta}). }
  \item{family}{The distribution of \eqn{y_i}. Currently accommodates three families: \code{Normal}, \code{Binomial}, and \code{Poisson}.}
  \item{LP.type}{User selects either \code{"L2"} for LP-orthogonal series representation of \code{U-function} or \code{"MaxEnt"} for the maximum entropy representation.  Default is \code{L2}.}
  \item{smooth.crit}{User selects either \code{"BIC"} or \code{"AIC"} as criteria to both determine optimal \eqn{m} and smooth final LP parameters; default is \code{"BIC"}.}
  \item{iters}{Integer value that gives the maximum number of iterations allowed for convergence; default is 200.}
  \item{B}{Integer value for number of grid points used for distribution output; default is 1000.}
  \item{max.theta}{For \code{"Poisson"}, user can provide a maximum theta value for prior; default is the maximum count value in \code{input}.}
  }
\details{ Function can take \eqn{m=0} and will return the Bayes estimate with given starting parameters.  Returns an object of class \code{DS.GF.obj}; this object can be used with plot command to plot the U-function (\code{Ufunc}), Deviance Plots (\code{mDev}), and DS-G comparison (\code{DS_G}).
}
\value{
  \item{LP.par}{\eqn{m} smoothed LP-Fourier coefficients, where \eqn{m} is determined by maximum deviance.}
  \item{g.par}{Parameters for \eqn{g}.}
  \item{LP.max.uns}{Vector of all LP-Fourier coefficients prior to smoothing, where the length is the same as \code{max.m}.}
  \item{LP.max.smt}{Vector of all smoothed LP-Fourier coefficients, where the length is the same as \code{max.m}.}
  \item{prior.fit}{Fitted values for the estimated prior.}
  \item{UF.data}{Dataframe that contains values required for plotting the U-function.}
  \item{dev.df}{Dataframe that contains deviance values for values of \eqn{m} up to \code{max.m}.}
  \item{m.val}{The value of \eqn{m} (less than or equal to the maximum \eqn{m} from user) that has the maximum deviance and represents the appropriate number of LP-Fourier coefficients.}
  \item{sm.crit}{Smoothing criteria; either \code{"BIC"} or \code{"AIC"}.}
  \item{fam}{The user-selected family.}
  \item{LP.type}{User-selected representation of \code{U-function}.}
  \item{obs.data}{Observed data provided by user for \code{input}.}
  
  }
\references{
Mukhopadhyay, S. and Fletcher, D., 2018. "Generalized Empirical Bayes via Frequentist Goodness of Fit," \emph{Nature Scientific Reports}, 8(1), p.9983, \url{https://www.nature.com/articles/s41598-018-28130-5 }.

Mukhopadhyay, S., 2017. "Large-Scale Mode Identification and Data-Driven Sciences," \emph{Electronic Journal of Statistics, 11}(1), pp.215-240.

}
\author{Doug Fletcher, Subhadeep Mukhopadhyay}
\examples{
data(rat)
rat.start <- gMLE.bb(rat$y, rat$n)$estimate
rat.ds <- DS.prior(rat, max.m = 4, rat.start, family = "Binomial")
rat.ds
plot(rat.ds, plot.type = "Ufunc")
plot(rat.ds, plot.type = "DSg")
plot(rat.ds, plot.type = "mDev")
}
\keyword{DS Main Functions}