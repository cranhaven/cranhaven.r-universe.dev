\name{jules}
\alias{jules}

\title{JULES}
\description{Implements the \bold{JU}mp \bold{L}ocal d\bold{E}convolution \bold{S}egmentation (\acronym{JULES}) filter (\cite{Pein et al.}, 2018). This non-parametric (model-free) segmentation method combines statistical multiresolution techniques with local deconvolution for idealising patch clamp (ion channel) recordings. In particular, also flickering (events on small time scales) can be detected and idealised which is not possible with common thresholding methods. \acronym{JULES} requires that the underlying noise is homogeneous. If the noise is heterogeneous, \code{\link{hilde}} should be used instead. \code{\link{hilde}} might be also more powerful (but slower) when events are very short. If all events are very long, multiple times the filter length, the simpler approach \code{\link{jsmurf}} is suitable. Further details about how to decide whether the noise is homogeneous or heterogeneous and whether events are short are given in the accompanying vignette.\cr
If \code{q == NULL} a Monte-Carlo simulation is required for computing the critical value. Since a Monte-Carlo simulation lasts potentially much longer (up to several hours or days if the number of observations is in the millions) than the main calculations, the package saves them by default in the workspace and on the file system such that a second call requiring the same Monte-Carlo simulation will be much faster. For more details, in particular to which arguments the Monte-Carlo simulations are specific, see Section \cite{Storing of Monte-Carlo simulations} below. Progress of a Monte-Carlo simulation can be reported by the argument \code{messages} and the saving can be controlled by the argument \code{option}, both can be specified in \code{\dots} and are explained in \code{\link{getCritVal}}.
}

\usage{
jules(data, filter, q = NULL, alpha = 0.05, sd = NULL, startTime = 0,
      output = c("onlyIdealization", "eachStep", "everything"), ...)
}

\arguments{
\item{data}{a numeric vector containing the recorded data points}
\item{filter}{an object of class \code{\link{lowpassFilter}} giving the used analogue lowpass filter}
\item{q}{a single numeric giving the critical value q in (\cite{Pein et al.}, 2018, (7)), by default chosen automatically by \code{\link{getCritVal}}}
\item{alpha}{a probability, i.e. a single numeric between 0 and 1, giving the significance level to compute the critical value \code{q} (if \code{q == NULL}), see \code{\link{getCritVal}}. Its choice is a trade-off between data fit and parsimony of the estimator. In other words, this argument balances the risks of missing changes and detecting additional artefacts}
\item{sd}{a single positive numeric giving the standard deviation (noise level) \eqn{\sigma_0}{sigma0} of the data points before filtering, by default (NULL) estimated by \code{\link[stepR]{sdrobnorm}} with \code{lag = filter$len + 1L}}
\item{startTime}{a single numeric giving the time at which recording (sampling) of \code{data} started, sampling time points will be assumed to be \code{startTime + seq(along = data) / filter$sr}}
\item{output}{a string specifying the return type, see \cite{Value}}
\item{...}{additional parameters to be passed to \code{\link{getCritVal}} or \code{\link{deconvolveLocally}}:
\enumerate{
\item \code{\link{getCritVal}} will be called automatically (if \code{q == NULL}), the number of data points \code{n = length(data)} will be set, the \code{family = "jules"} will be set and \code{alpha} and \code{filter} will be passed. For these parameter no user interaction is required and possible, all other parameters of \code{\link{getCritVal}} can be passed additionally
\item \code{\link{deconvolveLocally}} will be called automatically, the by \code{\link{stepDetection}} computed reconstruction / fit will be passed to \code{fit} and \code{data}, \code{filter}, \code{startTime} will be passed and \code{output} will be set accordingly to the \code{output} argument. For these parameter no user interaction is required and possible, all other parameters of \code{\link{deconvolveLocally}} can be passed additionally
}
}
}

\value{
The idealisation (estimation, regression) obtained by \acronym{JULES}. If \code{output == "onlyIdealization"} an object object of class \code{\link[stepR]{stepblock}} containing the idealisation. If \code{output == "eachStep"} a \code{\link{list}} containing the entries \code{idealization} with the idealisation, \code{fit} with the fit obtained by the \code{\link[=stepDetection]{detection step}} only, \code{q} with the given / computed critical value, \code{filter} with the given filter and \code{sd} with the given / estimated standard deviation. If \code{output == "everything"} a \code{\link{list}} containing the entries \code{idealization} with a \code{\link{list}} containing the idealisation after each refining step in the \code{\link[=deconvolveLocally]{local deconvolution}}, \code{fit} with the fit obtained by the \code{\link[=stepDetection]{detection step}} only, \code{stepfit} with the fit obtained by the \code{\link[=stepDetection]{detection step}} before postfiltering, \code{q} with the given / computed critical value, \code{filter} with the given filter and \code{sd} with the given / estimated standard deviation. Additionally, in all cases, the idealisation has an \code{\link[=attributes]{attribute}} \code{"noDeconvolution"}, an integer vector, that gives the segments for which no deconvolution could be performed, since two short segments followed each other, see also \cite{details} in \code{\link{deconvolveLocally}}.
}

\section{Storing of Monte-Carlo simulations}{
If \code{q == NULL} a Monte-Carlo simulation is required to compute the critical value. Since a Monte-Carlo simulation lasts potentially much longer (up to several hours or days if the number of observations is in the millions) than the main calculations, multiple possibilities for saving and loading the simulations are offered. Progress of a simulation can be reported by the argument \code{messages} which can be specified in \code{\dots} and is explained in the documentation of \code{\link{getCritVal}}. Each Monte-Carlo simulation is specific to the number of observations and the used filter. But note that also Monte-Carlo simulations for a (slightly) larger number of observations \eqn{n_q}{nq}, given in the argument \code{nq} in \code{\dots} and explained in the documentation of \code{\link{getCritVal}}, can be used, which avoids extensive resimulations for only a little bit varying number of observations, but results in a (small) loss of power. However, simulations of type \code{"vectorIncreased"}, i.e. objects of class \code{"MCSimulationMaximum"} with \code{nq} observations, have to be resimulated if \code{as.integer(log2(n1)) != as.integer(log2(n2))} when the saved simulation was computed with \code{n == n1} and the simulation now is required for \code{n == n2} and \code{nq >= n1} and \code{nq >= n2}. Simulations can either be saved in the workspace in the variable \code{critValStepRTab} or persistently on the file system for which the package \code{\link[R.cache]{R.cache}} is used. Moreover, storing in and loading from variables and \link[=readRDS]{RDS} files is supported. The simulation, saving and loading can be controlled by the argument \code{option} which can be specified in \code{\dots} and is explained in the documentation of \code{\link{getCritVal}}. By default simulations will be saved in the workspace and on the file system. For more details and for how simulation can be removed see Section \cite{Simulating, saving and loading of Monte-Carlo simulations} in \code{\link{getCritVal}}.
}

\seealso{\code{\link{getCritVal}}, \code{\link{lowpassFilter}}, \code{\link{deconvolveLocally}}, \code{\link{stepDetection}}}

\encoding{utf8}
\references{
Pein, F., Tecuapetla-Gómez, I., \enc{Schütte}{Schuette}, O., Steinem, C., Munk, A. (2018) Fully-automatic multiresolution idealization for filtered ion channel recordings: flickering event detection. \emph{IEEE Transactions on NanoBioscience} \bold{17}(3), 300--320.
}

\examples{
## idealisation of the gramicidin A recordings given by gramA with jules
# the used filter
filter <- lowpassFilter(type = "bessel", param = list(pole = 4L, cutoff = 1e3 / 1e4),
                        sr = 1e4)
\donttest{
# idealisation by JULES
# this call requires a Monte-Carlo simulation
# and therefore might last a few minutes,
# progress of the Monte-Carlo simulation is reported
idealisation <- jules(gramA, filter = filter, startTime = 9, messages = 100)

# any second call should be much faster
# as the previous Monte-Carlo simulation will be loaded
jules(gramA, filter = filter, startTime = 9)

# much larger significance level alpha for a larger detection power,
# but also with the risk of detecting additional artefacts
# in this example much more changes are detected,
# most of them are probably artefacts, but for instance the event at 11.36972
# might be an additional small event that was missed before
jules(gramA, filter = filter, alpha = 0.9, startTime = 9)

# getCritVal was called in jules, can be called explicitly
# for instance outside of a for loop to save run time
q <- getCritVal(length(gramA), filter = filter)
identical(jules(gramA, q = q, filter = filter, startTime = 9), idealisation)

# both steps of JULES can be called separately
fit <- stepDetection(gramA, filter = filter, startTime = 9)
identical(deconvolveLocally(fit, data = gramA, filter = filter, startTime = 9),
          idealisation)

# more detailed output
each <- jules(gramA, filter = filter, startTime = 9, output = "each")
every <- jules(gramA, filter = filter, startTime = 9, output = "every")

identical(idealisation, each$idealization)
idealisationEvery <- every$idealization[[3]]
attr(idealisationEvery, "noDeconvolution") <- attr(every$idealization,
                                                   "noDeconvolution")
identical(idealisation, idealisationEvery)

identical(each$fit, fit)
identical(every$fit, fit)


## zoom into a single event, (Pein et al., 2018, Figure 2 lower left panel)
plot(time, gramA, pch = 16, col = "grey30", ylim = c(20, 50),
     xlim = c(10.40835, 10.4103), ylab = "Conductance in pS", xlab = "Time in s")

# idealisation
lines(idealisation, col = "red", lwd = 3)

# idealisation convolved with the filter
ind <- seq(10.408, 10.411, 1e-6)
convolvedSignal <- lowpassFilter::getConvolution(ind, idealisation, filter)
lines(ind, convolvedSignal, col = "blue", lwd = 3)

# fit prior to the deconvolution step
# does not fit the recorded data points appropriately
# fit
lines(fit, col = "orange", lwd = 3)

# fit convolved with the filter
ind <- seq(10.408, 10.411, 1e-6)
convolvedSignal <- lowpassFilter::getConvolution(ind, fit, filter)
lines(ind, convolvedSignal, col = "darkgreen", lwd = 3)


## zoom into a single jump
plot(9 + seq(along = gramA) / filter$sr, gramA, pch = 16, col = "grey30",
     ylim = c(20, 50), xlim = c(9.6476, 9.6496), ylab = "Conductance in pS",
     xlab = "Time in s")

# idealisation
lines(idealisation, col = "red", lwd = 3)

# idealisation convolved with the filter
ind <- seq(9.647, 9.65, 1e-6)
convolvedSignal <- lowpassFilter::getConvolution(ind, idealisation, filter)
lines(ind, convolvedSignal, col = "blue", lwd = 3)

# idealisation with a wrong filter
# does not fit the recorded data points appropriately
wrongFilter <- lowpassFilter(type = "bessel",
                             param = list(pole = 6L, cutoff = 0.2),
                             sr = 1e4)
# the needed Monte-Carlo simulation depends on the number of observations and the filter
# hence a new simulation is required (if called for the first time)
idealisationWrong <- jules(gramA, filter = wrongFilter, startTime = 9, messages = 100)

# idealisation
lines(idealisationWrong, col = "orange", lwd = 3)

# idealisation convolved with the filter
ind <- seq(9.647, 9.65, 1e-6)
convolvedSignal <- lowpassFilter::getConvolution(ind, idealisationWrong, filter)
lines(ind, convolvedSignal, col = "darkgreen", lwd = 3)


# simulation for a larger number of observations can be used (nq = 3e4)
# does not require a new simulation as the simulation from above will be used
# (if the previous call was executed first)
jules(gramA[1:2.99e4], filter = filter, startTime = 9, 
      nq = 3e4, r = 1e3, messages = 100)

# simulation of type "vectorIncreased" for n1 observations can only be reused 
# for n2 observations if as.integer(log2(n1)) == as.integer(log2(n2))
# no simulation is required, since a simulation of type "matrixIncreased"
# will be loaded from the fileSystem
# this call also saves a simulation of type "vectorIncreased" in the workspace  
jules(gramA[1:1e4], filter = filter, startTime = 9,
      nq = 3e4, messages = 100, r = 1e3)
# here a new simulation is required
# (if no appropriate simulation is saved from a call outside of this file)
jules(gramA[1:1e3], filter = filter, startTime = 9,
      nq = 3e4, messages = 100, r = 1e3,
      options = list(load = list(workspace = c("vector", "vectorIncreased"))))

# the above calls saved and (attempted to) load Monte-Carlo simulations
# in the following call the simulations will neither be saved nor loaded
jules(gramA, filter = filter, startTime = 9, messages = 100, r = 1e3,
      options = list(load = list(), save = list()))

# only simulations of type "vector" and "vectorInceased" will be saved and
# loaded from the workspace, but no simulations of type "matrix" and
# "matrixIncreased" on the file system
jules(gramA, filter = filter, startTime = 9, messages = 100,
      options = list(load = list(workspace = c("vector", "vectorIncreased")),
                     save = list(workspace = c("vector", "vectorIncreased"))))

# explicit Monte-Carlo simulations, not recommended
stat <- stepR::monteCarloSimulation(n = length(gramA), , family = "mDependentPS",
                                    filter = filter, output = "maximum",
                                    r = 1e3, messages = 100)
jules(gramA, filter = filter, startTime = 9, stat = stat)

# with given standard deviation
sd <- stepR::sdrobnorm(gramA, lag = filter$len + 1)
identical(jules(gramA, filter = filter, startTime = 9, sd = sd), idealisation)

# with less regularisation of the correlation matrix
jules(gramA, filter = filter, startTime = 9, regularization = 0.5)

# with estimation of the level of long segments by the mean
# but requiring 30 observations for it
jules(gramA, filter = filter, startTime = 9,
      localEstimate = mean, thresholdLongSegment = 30)

# with one refinement step less, but with a larger grid
# progress of the deconvolution is reported
# potential warning for no deconvolution is suppressed
jules(gramA, filter = filter, startTime = 9,
      gridSize = c(1 / filter$sr, 1 / 10 / filter$sr),
      windowFactorRefinement = 2, report = TRUE,
      suppressWarningNoDeconvolution = TRUE)
}}

\keyword{nonparametric}
