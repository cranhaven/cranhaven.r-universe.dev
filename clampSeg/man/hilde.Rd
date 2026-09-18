\name{hilde}
\alias{hilde}

\title{HILDE}
\description{Implements the \bold{H}eterogeneous \bold{I}dealization by \bold{L}ocal testing and \bold{DE}convolution (\acronym{HILDE}) filter (\cite{Pein et al.}, 2021). This non-parametric (model-free) segmentation method combines statistical multiresolution techniques with local deconvolution for idealising patch clamp (ion channel) recordings. It is able to idealize short events (flickering) and allows for heterogeneous noise, but is rather slow. Hence, we recommend to use \code{\link{jsmurf}} or \code{\link{jules}} instead if they are suitable as well. Please see the arguments \code{family} and \code{method} as well as the \cite{examples} for how to access the function correctly depending on whether homogeneous is assumed or heterogeneous noise is allowed. \code{hilde} is a combination of \code{\link{jsmurf}} (with \code{locationCorrection == "none"}) and \code{\link{improveSmallScales}}. Further details about how to decide whether the noise is homogeneous or heterogeneous and whether events are short, and hence which method is suitable, are given in the accompanying vignette.\cr
If \code{q1 == NULL} or \code{q2 == NULL} a Monte-Carlo simulation is required for computing the critical values. Since a Monte-Carlo simulation lasts potentially much longer (up to several hours or days if the number of observations is in the millions) than the main calculations, this package saves them by default in the workspace and on the file system such that a second call requiring the same Monte-Carlo simulation will be much faster. For more details, in particular to which arguments the Monte-Carlo simulations are specific, see Section \cite{Storing of Monte-Carlo simulations} below. Progress of a Monte-Carlo simulation can be reported by the argument \code{messages} and the saving can be controlled by the argument \code{option}, both can be specified in \code{\dots} and are explained in \code{\link{getCritVal}}.
}

\usage{
hilde(data, filter, family = c("hjsmurf", "hjsmurfSPS", "hjsmurfLR",
                               "jsmurf", "jsmurfPS", "jsmurfLR"),
      method = c("2Param", "LR"), q1 = NULL, alpha1 = 0.01, q2 = NULL, alpha2 = 0.04, 
      sd = NULL, startTime = 0,
      output = c("onlyIdealization", "eachStep", "everything"), ...)
}

\arguments{
\item{data}{a numeric vector containing the recorded data points}
\item{filter}{an object of class \code{\link{lowpassFilter}} giving the used analogue lowpass filter}
\item{family}{the parametric family used in the \code{\link{jsmurf}} step; \code{"jsmurf"}, \code{"jsmurfPS"} and \code{"jsmurfLR"} assume homogeneous noise and \code{"hjsmurf"}, \code{"hjsmurfSPS"} and \code{"hjsmurfLR"} allow for heterogeneous noise. By default, we recommend to use \code{"jsmurfPS"} when homogeneous noise is assumed and \code{"hjsmurf"} when heterogeneous noise is allowed, see \cite{examples}. \code{"jsmurf"} is the standard statistic from (\cite{Hotz et al.}, 2013), \code{"jsmurfPS"} is a slightly more powerful partial sum statistic, \code{"jsmurfLR"} is a likelihood-ratio statistic, which is even more powerful but slow. \code{"hjsmurf"} is the standard statistic for heterogeneous noise which estimates the variance locally, \code{"hjsmurfSPS"} is a studentized partial sum statistic and \code{"hjsmurfLR"} is a likelihood ratio statistic, which is more powerful, but very slow}
\item{method}{the testing \code{method} for short events in the \code{\link{improveSmallScales}} step; \code{"2Param"} allows for heterogeneous noise, \code{"LR"} assumes homogeneous noise}
\item{q1}{will be passed to the argument \code{q} in \code{\link{jsmurf}}; by default chosen automatically by \code{\link{getCritVal}}, for families \code{"jsmurf"}, \code{"jsmurfPS"} and \code{"jsmurfLR"} a single numeric, for families \code{"hjsmurf"}, \code{"hjsmurfSPS"} and \code{"hjsmurfLR"} a numeric vector giving scale dependent critical values}
\item{alpha1}{will be passed to the argument \code{alpha} in \code{\link{jsmurf}}; a probability, i.e. a single numeric between 0 and 1, giving the significance level to compute \code{q1} (if \code{q1 == NULL}), see \code{\link{getCritVal}}. Its choice is a trade-off between data fit and parsimony of the estimator. In other words, this argument balances the risks of missing conductance changes and detecting additional artefacts}
\item{q2}{will be passed to the argument \code{q} in \code{\link{improveSmallScales}}; a numeric vector of the same length as \code{lengths} giving critical value for the tests for short events, by default chosen automatically by \code{\link{getCritVal}}}
\item{alpha2}{will be passed to the argument \code{alpha} in \code{\link{improveSmallScales}}; a probability, i.e. a single numeric between 0 and 1, giving the significance level to compute the critical value (if \code{q2 == NULL}), see \code{\link{getCritVal}}. Its choice balances the risks of missing short events and detecting additional artefacts}
\item{sd}{a single positive numeric giving the standard deviation (noise level) \eqn{\sigma_0}{sigma0} of the data points before filtering, by default (NULL) estimated by \code{\link[stepR]{sdrobnorm}} with \code{lag = filter$len + 1L}. For families \code{"hjsmurf"}, \code{"hjsmurfSPS"} and \code{"hjsmurfLR"} this argument is ignored with a \code{\link{warning}}}
\item{startTime}{a single numeric giving the time at which recording (sampling) of \code{data} started, sampling time points will be assumed to be \code{startTime + seq(along = data) / filter$sr}}
\item{output}{a string specifying the return type, see \cite{Value}}
\item{...}{additional parameters to be passed to \code{\link{getCritVal}} or \code{\link{improveSmallScales}}:
\enumerate{
\item \code{\link{getCritVal}} will be called automatically (if \code{q1 == NULL} or \code{q2 == NULL}), the number of data points \code{n = length(data)} will be set, the argument \code{family} will be assigned and \code{alpha} and \code{filter} will be passed. For these parameter no user interaction is required and possible, all other parameters of \code{\link{getCritVal}} can be passed additionally. Note that the same arguments will be passed twice if \code{q1} and \code{q2} have to be computed. If this is not suitable, \code{\link{getCritVal}} can be called instead
\item \code{\link{improveSmallScales}} will be called automatically, the by \code{\link{jsmurf}} computed fit will be passed to \code{fit} and \code{data}, \code{filter}, \code{method}, \code{q = q2}, \code{alpha = alpha2}, \code{startTime} will be passed and \code{output} will be set accordingly to the \code{output} argument. For these parameter no user interaction is required and possible, all other parameters of \code{\link{deconvolveLocally}} can be passed additionally
}
}
}

\value{
The idealisation (estimation, regression) obtained by \acronym{HILDE}. If \code{output == "onlyIdealization"} an object object of class \code{\link[stepR]{stepblock}} containing the idealisation. If \code{output == "eachStep"} a \code{\link{list}} containing the entries \code{idealization} with the idealisation, \code{fit} with the fit by \code{\link{jsmurf}}, \code{q1} and \code{q2} with the given / computed critical values, \code{filter} with the given filter and for families \code{"jsmurf"}, \code{"jsmurfPS"} and \code{"jsmurfLR"} \code{sd} with the given / estimated standard deviation. If \code{output == "everything"} a \code{\link{list}} containing the entries \code{idealization} with a \code{\link{list}} containing the idealisation after each refining step in the \code{\link[=improveSmallScales]{local deconvolution}}, \code{fit} with the fit by \code{\link{jsmurf}}, \code{q1} and \code{q2} with the given / computed critical values, \code{filter} with the given filter and for families \code{"jsmurf"}, \code{"jsmurfPS"} and \code{"jsmurfLR"} \code{sd} with the given / estimated standard deviation. Additionally, in all cases, the idealisation has an \code{\link[=attributes]{attribute}} \code{"noDeconvolution"}, an integer vector, that gives the segments for which no deconvolution could be performed, since two short segments followed each other, see also \cite{details} in \code{\link{improveSmallScales}}.
}

\section{Storing of Monte-Carlo simulations}{
If \code{q1 == NULL} or \code{q2 == NULL} a Monte-Carlo simulation is required to compute the critical values. Since a Monte-Carlo simulation lasts potentially much longer (up to several hours or days if the number of observations is in the millions) than the main calculations, multiple possibilities for saving and loading the simulations are offered. Progress of a simulation can be reported by the argument \code{messages} which can be specified in \code{\dots} and is explained in the documentation of \code{\link{getCritVal}}. Each Monte-Carlo simulation is specific to the parametric \code{family} / specified testing \code{method}, the number of observations and the used filter. Simulations related to computing \code{q2} are also specific to the arguments \code{thresholdLongSegment}, \code{localValue} and \code{localVar}. Currently, storing such a Monte-Carlo simulation is only possible for their default values. Note, that also Monte-Carlo simulations for a (slightly) larger number of observations \eqn{n_q}{nq}, given in the argument \code{nq} in \code{\dots} and explained in the documentation of \code{\link{getCritVal}}, can be used, which avoids extensive resimulations for only a little bit varying number of observations, but results in a (small) loss of power. However, simulations of type \code{"vectorIncreased"} (only possible for \code{q1} and families \code{"jsmurf"}, \code{"jsmurfPS"} and \code{"jsmurfLR"}) or \code{"matrixIncreased"}, i.e. objects of classes \code{"MCSimulationMaximum"} and \code{"MCSimulationVector"} with \code{nq} observations, have to be resimulated if \code{as.integer(log2(n1)) != as.integer(log2(n2))} when the saved simulation was computed with \code{n == n1} and the simulation now is required for \code{n == n2} and \code{nq >= n1} and \code{nq >= n2}. Simulations can either be saved in the workspace in the variable \code{critValStepRTab} or persistently on the file system for which the package \code{\link[R.cache]{R.cache}} is used. Moreover, storing in and loading from variables and \link[=readRDS]{RDS} files is supported. The simulation, saving and loading can be controlled by the argument \code{option} which can be specified in \code{\dots} and is explained in the documentation of \code{\link{getCritVal}}. By default simulations will be saved in the workspace and on the file system. For more details and for how simulation can be removed see Section \cite{Simulating, saving and loading of Monte-Carlo simulations} in \code{\link{getCritVal}}.
}

\seealso{\code{\link{getCritVal}}, \code{\link{jsmurf}}, \code{\link{jules}}, \code{\link{lowpassFilter}}, \code{\link{improveSmallScales}}, \code{\link{createLocalList}}}

\encoding{utf8}
\references{
Pein, F., Bartsch, A., Steinem, C., Munk, A. (2021) Heterogeneous Idealization of Ion Channel Recordings - Open Channel Noise. \emph{IEEE Transactions on NanoBioscience} \bold{20}(1), 57--78.

Hotz, T., \enc{Schütte}{Schuette}, O., Sieling, H., Polupanow, T., Diederichsen, U., Steinem, C., and Munk, A. (2013) Idealizing ion channel recordings by a jump segmentation multiresolution filter. \emph{IEEE Transactions on NanoBioscience} \bold{12}(4), 376--386.
}

\examples{
## idealisation of the gramicidin A recordings given by gramA with hilde
# the used filter
filter <- lowpassFilter(type = "bessel", param = list(pole = 4L, cutoff = 1e3 / 1e4),
                        sr = 1e4)
\donttest{
# idealisation by HILDE assuming homogeneous noise
# this call requires a Monte-Carlo simulation
# and therefore might last a few minutes,
# progress of the Monte-Carlo simulation is reported
idealisation <- hilde(gramA, filter = filter, family = "jsmurfPS", method = "LR",
                      startTime = 9, messages = 10)

# any second call should be much faster
# as the previous Monte-Carlo simulation will be loaded
hilde(gramA, filter = filter, family = "jsmurfPS", method = "LR", startTime = 9)

# HILDE allowing heterogeneous noise
hilde(gramA, filter = filter, family = "hjsmurf", method = "2Param",
      startTime = 9, messages = 10, r = 100)
# r = 100 is used to reduce its run time,
# this is okay for illustration purposes, but for precise results
# a larger number of Monte-Carlo simulations is recommend

# much larger significance level alpha1 for a larger detection power
# in the refinement step on small temporal scales,
# but also with the risk of detecting additional artefacts
hilde(gramA, filter = filter, family = "jsmurfPS", method = "LR",
      alpha1 = 0.9, alpha2 = 0.9, startTime = 9)

# getCritVal was called in hilde, can be called explicitly
# for instance outside of a for loop to save run time
q2 <- getCritVal(length(gramA), filter = filter, family = "LR")
identical(hilde(gramA, filter = filter, family = "jsmurfPS",
                method = "LR", startTime = 9, q2 = q2), idealisation)

# both steps of HILDE can be called separately
fit <- jsmurf(gramA, filter = filter, family = "jsmurfPS", alpha = 0.01,
              startTime = 9, locationCorrection = "none")
deconvolution <- improveSmallScales(fit, data = gramA, method = "LR", filter = filter,
                                    startTime = 9, messages = 100)
attr(deconvolution, "q") <- NULL
identical(deconvolution, idealisation)

# more detailed output
each <- hilde(gramA, filter = filter, family = "jsmurfPS", method = "LR",
              startTime = 9, output = "each")

every <- hilde(gramA, filter = filter, family = "jsmurfPS", method = "LR",
               startTime = 9, output = "every")

identical(idealisation, each$idealization)
idealisationEvery <- every$idealization[[3]]
attr(idealisationEvery, "noDeconvolution") <- attr(every$idealization,
                                                   "noDeconvolution")
identical(idealisation, idealisationEvery)

identical(each$fit, fit)
identical(every$fit, fit)


## zoom into a single event
## similar to (Pein et al., 2018, Figure 2 lower left panel)
plot(time, gramA, pch = 16, col = "grey30", ylim = c(20, 50),
     xlim = c(10.40835, 10.4103), ylab = "Conductance in pS", xlab = "Time in s")

# idealisation
lines(idealisation, col = "red", lwd = 3)

# idealisation convolved with the filter
ind <- seq(10.408, 10.411, 1e-6)
convolvedSignal <- lowpassFilter::getConvolution(ind, idealisation, filter)
lines(ind, convolvedSignal, col = "blue", lwd = 3)

# for comparison, fit prior to the improvement step
# does not contain the event and hence fits the recorded data points badly
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
idealisationWrong <- hilde(gramA, filter = wrongFilter, family = "jsmurfPS",
                           method = "LR", startTime = 9, messages = 10)

# idealisation
lines(idealisationWrong, col = "orange", lwd = 3)

# idealisation convolved with the filter
ind <- seq(9.647, 9.65, 1e-6)
convolvedSignal <- lowpassFilter::getConvolution(ind, idealisationWrong, filter)
lines(ind, convolvedSignal, col = "darkgreen", lwd = 3)


# simulation for a larger number of observations can be used (nq = 3e4)
# does not require a new simulation as the simulation from above will be used
# (if the previous call was executed first)
hilde(gramA[1:2.99e4], filter = filter, family = "jsmurfPS", method = "LR",
      startTime = 9, nq = 3e4)
# note that arguments to compute critical values are used to compute q1 and q2
# if this is not wanted, getCritVal can be called separately
q1 <- getCritVal(length(gramA[1:2.99e4]), filter = filter, family = "jsmurfPS",
                 messages = 100, r = 1e3)
hilde(gramA[1:2.99e4], filter = filter, family = "jsmurfPS", method = "LR",
      q1 = q1, startTime = 9, nq = 3e4) # nq = 3e4 is only used to compute q2

# simulation of type "vectorIncreased" for n1 observations can only be reused 
# for n2 observations if as.integer(log2(n1)) == as.integer(log2(n2))
# no simulation is required, since a simulation of type "matrixIncreased"
# will be loaded from the fileSystem
# this call also saves a simulation of type "vectorIncreased" in the workspace  
hilde(gramA[1:1e4], filter = filter, family = "jsmurfPS", method = "LR",
      startTime = 9, nq = 3e4)

# the above calls saved and (attempted to) load Monte-Carlo simulations
# in the following call the simulations will neither be saved nor loaded
# Monte-Carlo simulations are required for q1 and for q2
hilde(gramA, filter = filter, family = "jsmurfPS", method = "LR",
      startTime = 9, messages = 10, r = 100,
      options = list(load = list(), save = list()))

# with given standard deviation
sd <- stepR::sdrobnorm(gramA, lag = filter$len + 1)
identical(hilde(gramA, filter = filter, family = "jsmurfPS", method = "LR",
                startTime = 9, sd = sd), idealisation)

# with less regularisation of the correlation matrix
hilde(gramA, filter = filter, family = "jsmurfPS", method = "LR",
      startTime = 9, regularization = 0.5)

# with estimation of the level of long segments by the mean
# but requiring 30 observations for it
hilde(gramA, filter = filter, family = "jsmurfPS", method = "LR",
      startTime = 9, localValue = mean, thresholdLongSegment = 30)

# with one refinement step less, but with a larger grid
# progress of the deconvolution is reported
# potential warning for no deconvolution is suppressed
hilde(gramA, filter = filter, family = "jsmurfPS", method = "LR",
      startTime = 9, messages = 100,
      lengths = c(3:5, 8, 11, 16, 20),
      gridSize = c(1 / filter$sr, 1 / 10 / filter$sr),
      windowFactorRefinement = 2, report = TRUE,
      suppressWarningNoDeconvolution = TRUE)

# pre-computation of certain quantities using createLocalList
# this saves run time if hilde or (improveSmallScales) is called more than once
# localList is passed via ... to improveSmallScales
localList <- createLocalList(filter = filter, method = "LR")
identical(hilde(gramA, filter = filter, family = "jsmurfPS", method = "LR",
                startTime = 9, localList = localList), idealisation)
}}

\keyword{nonparametric}
