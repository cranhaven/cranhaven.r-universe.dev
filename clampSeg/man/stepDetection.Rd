\name{stepDetection}
\alias{stepDetection}
\alias{detection step}

\title{Detection of steps / jumps by a multiresolution criterion}
\description{Implements the detection step of \acronym{JULES} (\cite{Pein et al.}, 2018, Section III-A) which consists of a fit by a multiresolution criterion computed by a dynamic program and a postfilter step that removes incremental steps. This initial fit (reconstruction) can then be refined by local deconvolution implemented in \code{\link{deconvolveLocally}} to obtain \acronym{JULES}, also implemented in \code{\link{jules}}.\cr
If \code{q == NULL} a Monte-Carlo simulation is required for computing the critical value. Since a Monte-Carlo simulation lasts potentially much longer (up to several hours or days if the number of observations is in the millions) than the main calculations, the package saves them by default in the workspace and on the file system such that a second call that require the same Monte-Carlo simulation will be much faster. For more details, in particular to which arguments the Monte-Carlo simulations are specific, see Section \cite{Storing of Monte-Carlo simulations} below. Progress of a Monte-Carlo simulation can be reported by the argument \code{messages} and the saving can be controlled by the argument \code{option}, both can be specified in \code{\dots} and are explained in \code{\link{getCritVal}}.

}

\usage{
stepDetection(data, filter, q = NULL, alpha = 0.05, sd = NULL, startTime = 0,
              output = c("onlyFit", "everything"), ...)
}

\arguments{
\item{data}{a numeric vector containing the recorded data points}
\item{filter}{an object of class \code{\link{lowpassFilter}} giving the used analogue lowpass filter}
\item{q}{a single numeric giving the critical value q in (\cite{Pein et al.}, 2018, (7)), by default chosen automatically by \code{\link{getCritVal}}}
\item{alpha}{a probability, i.e. a single numeric between 0 and 1, giving the significance level to compute the critical value \code{q} (if \code{q == NULL}), see \code{\link{getCritVal}}. Its choice is a trade-off between data fit and parsimony of the estimator. In other words, this argument balances the risks of missing changes and detecting additional artefacts}
\item{sd}{a single positive numeric giving the standard deviation (noise level) \eqn{\sigma_0}{sigma0} of the data points before filtering, by default (NULL) estimated by \code{\link[stepR]{sdrobnorm}} with \code{lag = filter$len + 1L}}
\item{startTime}{a single numeric giving the time at which recording (sampling) of \code{data} started, sampling time points will be assumed to be \code{startTime + seq(along = data) / filter$sr}}
\item{output}{a string specifying the return type, see \cite{Value}}
\item{...}{additional parameters to be passed to \code{\link{getCritVal}}. \code{\link{getCritVal}} will be called automatically (if \code{q == NULL}), the number of data points \code{n = length(data)} will be set, the \code{family = "jules"} will be set and \code{alpha} and \code{filter} will be passed. For these parameter no user interaction is required and possible, all other parameters of \code{\link{getCritVal}} can be passed additionally}
}

\value{
The reconstruction (fit) obtained by the detection step of \acronym{JULES}. If \code{output == "onlyFit"} an object object of class \code{\link[stepR]{stepblock}} containing the fit. If \code{output == "everything"} a \code{\link{list}} containing the entries \code{fit} with the fit, \code{stepfit} with the fit before postfiltering, \code{q} with the given / computed critical value, \code{filter} with the given filter and \code{sd} with the given / estimated standard deviation.
}

\section{Storing of Monte-Carlo simulations}{
If \code{q == NULL} a Monte-Carlo simulation is required to compute the critical value. Since a Monte-Carlo simulation lasts potentially much longer (up to several hours or days if the number of observations is in the millions) than the main calculations, multiple possibilities for saving and loading the simulations are offered. Progress of a simulation can be reported by the argument \code{messages} which can be specified in \code{\dots} and is explained in the documentation of \code{\link{getCritVal}}. Each Monte-Carlo simulation is specific to the number of observations and the used filter. But note that also Monte-Carlo simulations for a (slightly) larger number of observations \eqn{n_q}{nq}, given in the argument \code{nq} in \code{\dots} and explained in the documentation of \code{\link{getCritVal}}, can be used, which avoids extensive resimulations for only a little bit varying number of observations, but results in a (small) loss of power. However, simulations of type \code{"vectorIncreased"}, i.e. objects of class \code{"MCSimulationMaximum"} with \code{nq} observations, have to be resimulated if \code{as.integer(log2(n1)) != as.integer(log2(n2))} when the saved simulation was computed with \code{n == n1} and the simulation now is required for \code{n == n2} and \code{nq >= n1} and \code{nq >= n2}. Simulations can either be saved in the workspace in the variable \code{critValStepRTab} or persistently on the file system for which the package \code{\link[R.cache]{R.cache}} is used. Moreover, storing in and loading from variables and \link[=readRDS]{RDS} files is supported. The simulation, saving and loading can be controlled by the argument \code{option} which can be specified in \code{\dots} and is explained in the documentation of \code{\link{getCritVal}}. By default simulations will be saved in the workspace and on the file system. For more details and for how simulation can be removed see Section \cite{Simulating, saving and loading of Monte-Carlo simulations} in \code{\link{getCritVal}}.
}

\seealso{\code{\link{jules}}, \code{\link{getCritVal}}, \code{\link{lowpassFilter}}, \code{\link{deconvolveLocally}}}

\encoding{utf8}
\references{
Pein, F., Tecuapetla-Gómez, I., \enc{Schütte}{Schuette}, O., Steinem, C., Munk, A. (2018) Fully-automatic multiresolution idealization for filtered ion channel recordings: flickering event detection. \emph{IEEE Transactions on NanoBioscience} \bold{17}(3), 300--320.
}

\examples{
## fit of the gramicidin A recordings given by gramA
# the used filter
filter <- lowpassFilter(type = "bessel", param = list(pole = 4L, cutoff = 1e3 / 1e4),
                        sr = 1e4)
\donttest{
# this call requires a Monte-Carlo simulation (if not performed before)
# and therefore might last a few minutes,
# progress of the Monte-Carlo simulation is reported
fit <- stepDetection(gramA, filter = filter, startTime = 9, messages = 100)

# this second call should be much faster
# as the previous Monte-Carlo simulation will be loaded
stepDetection(gramA, filter = filter, startTime = 9)

# much larger significance level alpha for a larger detection power,
# but also with the risk of detecting additional artefacts
# in this example much more changes are detected,
# most of them are probably artefacts, but for instance the event at 11.3699
# might be an additional small event that was missed before
stepDetection(gramA, filter = filter, alpha = 0.9, startTime = 9)

# getCritVal was called in stepDetection, can be called explicitly
# for instance outside of a for loop to save computation time
q <- getCritVal(length(gramA), filter = filter)
identical(stepDetection(gramA, q = q, filter = filter, startTime = 9), fit)

# more detailed output
every <- stepDetection(gramA, filter = filter, startTime = 9, output = "every")
identical(every$fit, fit)
identical(every$q, q)
identical(every$sd, stepR::sdrobnorm(gramA, lag = filter$len + 1L))
identical(every$filter, every$filter)

# for this data set no incremental changes occur
identical(every$stepfit, every$stepfit)

## zoom into a single event
time <- 9 + seq(along = gramA) / filter$sr # time points
plot(time, gramA, pch = 16, col = "grey30", ylim = c(20, 50),
     xlim = c(10.40835, 10.4103), ylab = "Conductance in pS", xlab = "Time in s")

# fit is a piecewise constant approximation of the observations
# hence its convolution does not fit the recorded data points appropriately
# fitting the observations requires a deconvolution
# either by calling deconveLocally,
# or as suggested by calling jules instead of stepDetection
# fit
lines(fit, col = "red", lwd = 3)

# fit convolved with the filter
ind <- seq(10.408, 10.411, 1e-6)
convolvedSignal <- lowpassFilter::getConvolution(ind, fit, filter)
lines(ind, convolvedSignal, col = "blue", lwd = 3)


# Monte-Carlo simulation depend on the number of observations and on the filter
# hence a simulation is required again (if called for the first time)
# to save some time the number of iterations is reduced to r = 1e3
# hence the critical value is computed with less precision
# In general, r = 1e3 is enough for a first impression
# for a detailed analysis r = 1e4 is suggested
stepDetection(gramA, filter = filter, startTime = 9, messages = 100L, r = 1e3L)

# simulation for a larger number of observations can be used (nq = 3e4)
# does not require a new simulation as the simulation from above will be used
# (if the previous call was executed first)
stepDetection(gramA, filter = filter, startTime = 9,
              messages = 100L, r = 1e3L, nq = 3e4L)

# simulation of type "vectorIncreased" for n1 observations can only be reused 
# for n2 observations if as.integer(log2(n1)) == as.integer(log2(n2))
# no simulation is required, since a simulation of type "matrixIncreased"
# will be loaded from the fileSystem
# this call also saves a simulation of type "vectorIncreased" in the workspace 
stepDetection(gramA[1:1e4], filter = filter, startTime = 9,
              nq = 3e4, messages = 100, r = 1e3)
# here a new simulation is required
# (if no appropriate simulation is saved from a call outside of this file)
stepDetection(gramA[1:1e3], filter = filter, startTime = 9,
              nq = 3e4, messages = 100, r = 1e3,
              options = list(load = list(workspace = c("vector", "vectorIncreased"))))

# the above calls saved and (attempted to) load Monte-Carlo simulations              
# in the following call the simulations will neither be saved nor loaded
stepDetection(gramA, filter = filter, startTime = 9, messages = 100L, r = 1e3L,
              options = list(load = list(), save = list()))

# only simulations of type "vector" and "vectorInceased" will save and
# loaded from the workspace, but no simulations of type "matrix" and
# "matrixIncreased" on the file system
stepDetection(gramA, filter = filter, startTime = 9, messages = 100L, r = 1e3L,
              options = list(load = list(workspace = c("vector", "vectorIncreased")),
                             save = list(workspace = c("vector", "vectorIncreased"))))

# explicit Monte-Carlo simulations, not recommended
stat <- stepR::monteCarloSimulation(n = length(gramA), , family = "mDependentPS",
                                    filter = filter, output = "maximum",
                                    r = 1e3, messages = 100)
stepDetection(gramA, filter = filter, startTime = 9, stat = stat)

# with given standard deviation
sd <- stepR::sdrobnorm(gramA, lag = filter$len + 1)
identical(stepDetection(gramA, filter = filter, startTime = 9, sd = sd), fit)
}}

\keyword{nonparametric}
