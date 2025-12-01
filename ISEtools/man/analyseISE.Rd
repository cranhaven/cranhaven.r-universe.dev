\name{analyseISE}
\alias{analyseISE}
\title{
   Ion selective electrode characterisation and estimation of sample concentrations
}
\description{
   Use Bayesian calibration to estimate parameters for y = a + b log(x + c) + error, where error follows a normal distribution with mean 0 and standard deviation sigma.  The limit of detection (false positive/negative method or S/N=3 method) is also estimated.  These values are then used to the estimate sample concentrations.
}
\usage{
analyseISE(data, model.path=NA, model.name=NA, Z=NA, temperature = 21,
	burnin=25000, iters = 50000, chains=4, thin = 1,
	a.init= NA, b.init=NA, cstar.init=NA, logc.limits = c(-8.9, -1.9), 
	sigma.upper = 5, diagnostic.print=FALSE, offset = 1,
	alpha = 0.05, beta = 0.05, SN = NA, program="OpenBUGS")
}
\arguments{
  \item{data}{
	Calibration and experimental data (of class 'ISEdata'; see loadISEdata)
}
  \item{model.path}{
	The directory where the BUGS model is located (defaults to 'models' sub-directory under the location of ISEtools package, e.g. '.../ISEtools/models')
}
  \item{model.name}{
	The name of the BUGS model (e.g. 'Single_ISE_model.txt') (defaults are located in ISEtools package)
}
  \item{Z}{
	Ionic valence (e.g. for lead, Z = 2)
}
  \item{temperature}{
	temperature in degrees C
}
  \item{burnin}{
	Initial number of Monte Carlo simulations to discard.
}
  \item{iters}{
	Total number of iterations.
}
  \item{chains}{
	Number of parallel MCMC chains
}
  \item{thin}{
	Thinning rate, equal to 1/Proportion of simulations saved (e.g. thin = 10 records every tenth iteration).
}
  \item{a.init}{
	Initial value for parameter a
}
  \item{b.init}{
	Initial value for parameter b
}
  \item{cstar.init}{
	Initial value for parameter cstar (c = cstar^10)
}
  \item{logc.limits}{
	Upper and lower limits for log c initial values
}
  \item{sigma.upper}{
	Upper limit for initial value of sigma
}
  \item{diagnostic.print}{
	logical flag indicating whether a diagnostic printout is desired (default is F) 
}
  \item{offset}{
	The initial value for the slope is based on the last data point as sorted by concentration (i.e. the Nth point) and the (N - offset) data point.  The default is offset = 1, corresponding to the last and second to last data points.
}
  \item{alpha}{
	False positive rate used for detection threshold (not output) to calculate LOD(alpha, beta) {only returned if SN = NA}
}
  \item{beta}{
	False negative rate used to calculate LOD(alpha, beta) {only returned if SN = NA}
}
  \item{SN}{
	Desired signal-to-noise ratio for LOD(S/N) calculations (default is to calculate the S/N equivalent based on alpha, beta)
}
  \item{program}{
	Choice of "OpenBUGS" (default and recommended for Windows or Linux) or "jags" (for macOS, see manual for warnings).
}
}
\value{
analyseISE returns a list of class 'analyseISE'.  Individual components include:\cr
  \item{SampleID }{Sample identification number }
  \item{log10x.exp }{Estimated concentration (log scale, mol/l) }
  \item{ahat }{Estimated value for a (from the median of the posterior distribution) }
  \item{bhat }{Estimated value for b (from the median of the posterior distribution) }
  \item{chat }{Estimated value for c (from the median of the posterior distribution) }
  \item{cstarhat }{Estimated value for cstar (from the median of the posterior distribution) }
  \item{sigmahat }{Estimated value for cstar (from the median of the posterior distribution) }
  \item{LOD.info }{List describing LOD method (alpha, beta or S/N) and corresponding values (alpha, beta, SN) }
  \item{LOD.hat }{Estimated value for the limit of detection (from the median of the posterior distribution) }
  \item{<parametername>.lcl }{Lower limit for the above parameters (e.g. ahat.lcl, bhat.lcl, ...) (from the 2.5th percentile of the posterior distribution) }
  \item{<parametername>.ucl }{Upper limit for the above parameters (from the 97.5th percentile of the posterior distribution) }
  \item{LOD.Q1 }{25th percentile estimated value of the limit of detection}
  \item{LOD.Q3 }{75th percentile estimated value of the limit of detection}
}
\references{
Dillingham, P.W., Radu, T., Diamond, D., Radu, A. and McGraw, C.M. (2012). Bayesian Methods for Ion Selective Electrodes. \emph{Electroanalysis}, \strong{24}, 316-324. <doi:10.1002/elan.201100510>

Dillingham, P.W., Alsaedi, B.S.O. and McGraw, C.M. (2017). Characterising uncertainty in instrumental limits of detection when sensor response is non-linear. \emph{2017 IEEE SENSORS}, Glasgow, United Kingdom, pp. 1-3. <doi:10.1109/ICSENS.2017.8233898>

Dillingham, P.W., Alsaedi, B.S.O., Radu, A., and McGraw, C.M. (2019). Semi-automated data analysis for ion-selective electrodes and arrays using the R package ISEtools. \emph{Sensors} \strong{19}(20), 4544. <doi: 10.3390/s19204544>

Dillingham, P.W., Alsaedi, B.S.O., Granados-Focil, S., Radu, A., and McGraw, C.M. (2020). Establishing meaningful Limits of Detection for ion-selective electrodes and other nonlinear sensors \emph{ACS Sensors}, \strong{5}, 250-257. <doi:10.1021/acssensors.9b02133>
}
\author{
Peter Dillingham, \email{peter.dillingham@otago.ac.nz}
}

\examples{
# Fast-running example with only 100 MCMC iterations for testing:
data(LeadStdAdd)
example2test = analyseISE(LeadStdAdd, Z = 2, temperature = 21, 
  burnin=100, iters=200, chains=1, a.init=c(176, 146, -112), 
  b.init=c(29, 30, 31), cstar.init=c(0.26, 0.27, 0.22), program="jags")
print(example2test)
summary(example2test)
plot(example2test, ylim = c(-7, -3), xlab = "ID (Sample)", 
     ylab = expression(paste(log[10], " ", Pb^{paste("2","+",sep="")} )))
\donttest{
# Full example with 100,000 iterations (25,000 by 4 chains):
data(LeadStdAdd)
example2 = analyseISE(LeadStdAdd, Z = 2, temperature = 21)
print(example2)
summary(example2)
plot(example2, ylim = c(-7, -3), xlab = "ID (Sample)", 
	ylab = expression(paste(log[10], " ", Pb^{paste("2","+",sep="")} )))
}
}
