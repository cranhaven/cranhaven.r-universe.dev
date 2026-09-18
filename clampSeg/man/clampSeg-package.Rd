\name{clampSeg-package}
\alias{clampSeg-package}
\alias{clampSeg}
\docType{package}

\title{Idealisation of Patch Clamp Recordings}

\description{Implements the model-free multiscale idealisation approaches: \bold{J}ump-\bold{S}egmentation by \bold{MU}ltiResolution \bold{F}ilter (\acronym{JSMURF}) (\cite{Hotz et al.}, 2013), \bold{JU}mp \bold{L}ocal d\bold{E}convolution \bold{S}egmentation (\acronym{JULES}) filter (\cite{Pein et al.}, 2018) and \bold{H}eterogeneous \bold{I}dealization by \bold{L}ocal testing and \bold{DE}convolution (\acronym{HILDE}) (\cite{Pein et al.}, 2021). These methods combine multiscale testing with deconvolution to idealise patch clamp recordings. They allow to deal with subconductance states and flickering. Further details are given in the accompanying vignette.}

\details{
The main functions are \code{\link{jsmurf}}, \code{\link{jules}} and \code{\link{hilde}} which implement \acronym{JSMURF}, \acronym{JULES} and \acronym{HILDE}, respectively. \acronym{JSMURF} is the most simplest and fastest approach. If short events (flickering) occurs,  \acronym{JULES} or \acronym{HILDE} should be used instead. All three methods can assume homogeneous noise, but \acronym{JSMURF} and \acronym{HILDE} have options to allow for heterogeneous noise. Further details on when which method is suitable and on how to use them are given in Section II in the vignette.\cr
The signal underlying the data in a patch clamp recording is assumed to be a step (piecewise constant) function, e.g. constant conductance levels are assumed. The signal is perturbed by (Gaussian) white noise and convolved with a lowpass filter, resulting in a smooth signal perturbed by correlated noise with known correlation structure. The white noise is scaled by a constant if homogeneous noise is assumed and by an unknown piecewise constant function if heterogeneous noise is assumed. Heterogeneous noise can for instance by caused by open channel noise. The recorded data points are modelled as sampled (digitised) recordings of this process. For more details on this model see Section III in the vignette. A small example of such a recording, 3 seconds of a gramicidin A recording, is given by \code{\link{gramA}}.\cr
The filter can be created by the function \code{\link{lowpassFilter}}, currently only Bessel filters are supported. \code{\link{createLocalList}} allows pre-calculations for \code{\link{improveSmallScales}} and \code{\link{hilde}}. Doing so reduces the running time if \code{\link{improveSmallScales}} and \code{\link{hilde}} are called more than once. The multiscale step in all three approaches requires critical values. Be default, those values are automatically computed. Alternatively, they can be computed separately by \code{\link{getCritVal}}. Their computation relies on Monte-Carlo simulations.\cr
A Monte-Carlo simulation lasts potentially much longer (up to several hours or days if the number of observations is in the millions) than the main calculations. Hence, multiple possibilities for saving and loading the simulations are offered and used by default. Simulations can either be saved in the workspace in the variable \code{critValStepRTab} or persistently on the file system for which the package \code{\link[R.cache]{R.cache}} is used. Moreover, storing in and loading from variables and \link[=readRDS]{RDS} files is supported. The simulation, saving and loading can be controlled by the argument \code{option}. By default simulations will be saved in the workspace and on the file system. For more details and for how simulations can be removed see the documentation of \code{\link{getCritVal}}.\cr
The detection and estimation step of \acronym{JULES} can be obtained separately by the functions \code{\link{stepDetection}} and \code{\link{deconvolveLocally}}, respectively. The refinement steps (second and third step) of \acronym{HILDE}, which improve a fit on small temporal scales by testing for additional events and applying local deconvolution, can be accessed by the function \code{\link{improveSmallScales}}.
}


\encoding{utf8}
\references{
Pein, F., Bartsch, A., Steinem, C., Munk, A. (2021) Heterogeneous Idealization of Ion Channel Recordings - Open Channel Noise. \emph{IEEE Transactions on NanoBioscience} \bold{20}(1), 57--78.

Pein, F., Tecuapetla-Gómez, I., \enc{Schütte}{Schuette}, O., Steinem, C., Munk, A. (2018) Fully-automatic multiresolution idealization for filtered ion channel recordings: flickering event detection. \emph{IEEE Transactions on NanoBioscience} \bold{17}(3), 300--320.

Hotz, T., \enc{Schütte}{Schuette}, O., Sieling, H., Polupanow, T., Diederichsen, U., Steinem, C., and Munk, A. (2013) Idealizing ion channel recordings by a jump segmentation multiresolution filter. \emph{IEEE Transactions on NanoBioscience} \bold{12}(4), 376--386.

Pein, F., Sieling, H., Munk, A. (2017) Heterogeneous change point inference. \emph{Journal of the Royal Statistical Society, Series B}, \bold{79}(4), 1207--1227.

Frick, K., Munk, A. and Sieling, H. (2014) Multiscale change-point inference. With discussion and rejoinder by the authors. \emph{Journal of the Royal Statistical Society, Series B} \bold{76}(3), 495--580.
}

\keyword{package}
\keyword{nonparametric}

\seealso{\code{\link{jsmurf}}, \code{\link{jules}}, \code{\link{hilde}}, \code{\link{getCritVal}}, \code{\link{lowpassFilter}}, \code{\link{createLocalList}}, \code{\link{gramA}}, \code{\link{deconvolveLocally}}, \code{\link{stepDetection}}, \code{\link{improveSmallScales}}}

\examples{
## idealisation of the gramicidin A recording given by gramA
# the used filter
filter <- lowpassFilter(type = "bessel", param = list(pole = 4L, cutoff = 1e3 / 1e4),
                        sr = 1e4)

# the corresponding time points
time <- 9 + seq(along = gramA) / filter$sr

# plot of the data as in (Pein et al., 2018, Figure 1 lower panel)
plot(time, gramA, pch = ".", col = "grey30", ylim = c(20, 50),
     ylab = "Conductance in pS", xlab = "Time in s")

\donttest{
# idealisations require Monte-Carlo simulations
# and therefore might last a few minutes,
# progress of the Monte-Carlo simulations is reported
# JSMURF assuming homogeneous noise
fit1 <- jsmurf(gramA, filter = filter, family = "jsmurfPS",
               startTime = 9, messages = 100)
# JSMURF allowing heterogeneous noise
fit2 <- jsmurf(gramA, filter = filter, family = "hjsmurf",
               startTime = 9, messages = 100)
# JULES assuming homogeneous noise
fit3 <- jules(gramA, filter = filter, startTime = 9, messages = 100)
# HILDE assuming homogeneous noise
fit4 <- hilde(gramA, filter = filter, family = "jsmurfPS", method = "LR",
              startTime = 9, messages = 10)
# HILDE allowing heterogeneous noise
fit5 <- hilde(gramA, filter = filter, family = "hjsmurf", method = "2Param",
              startTime = 9, messages = 10, r = 100)
# r = 100 is used to reduce its run time,
# this is okay for illustration purposes, but for precise results
# a larger number of Monte-Carlo simulations is recommend

# gramA contains short peaks and the noise is homogeneous
# hence jules seems to be most appropriate
idealisation <- fit3

# add idealisation to the plot
lines(idealisation, col = "#FF0000", lwd = 3)

## in the following we use jules to illustrate various points,
## similar points are valid for jsmurf and hilde, too,
## see also their individual documentation for more details
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
}}
