\name{jsmurf}
\alias{jsmurf}

\title{JSMURF}
\description{Implements the \bold{J}ump-\bold{S}egmentation by \bold{MU}ltiResolution \bold{F}ilter (\acronym{JSMURF}) (\cite{Hotz et al.}, 2013). This model-free multiscale idealization approach works well on medium and large temporal scales. If short events (flickering) occurs,  \code{\link{jules}} or \code{\link{hilde}} should be used instead. The original work (\cite{Hotz et al.}, 2013) assumed homogeneous noise, but an extension to heterogeneous noise was provided in (\cite{Pein et al.}, 2021). Please see the argument \code{family} and the \cite{examples} for how to access the function correctly depending on whether homogeneous noise is assumed or heterogeneous noise is allowed. Further details about how to decide whether the noise is homogeneous or heterogeneous and whether events are short are given in the accompanying vignette.\cr
If \code{q == NULL} a Monte-Carlo simulation is required for computing critical values. Since a Monte-Carlo simulation lasts potentially much longer (up to several hours or days if the number of observations is in the millions) than the main calculations, the package saves them by default in the workspace and on the file system such that a second call requiring the same Monte-Carlo simulation will be much faster. For more details, in particular to which arguments the Monte-Carlo simulations are specific, see Section \cite{Storing of Monte-Carlo simulations} below. Progress of a Monte-Carlo simulation can be reported by the argument \code{messages} and the saving can be controlled by the argument \code{option}, both can be specified in \code{\dots} and are explained in \code{\link{getCritVal}}.
}

\usage{
jsmurf(data, filter, family = c("jsmurf", "jsmurfPS", "jsmurfLR",
                               "hjsmurf", "hjsmurfSPS", "hjsmurfLR"),
       q = NULL, alpha = 0.05, sd = NULL, startTime = 0,
       locationCorrection = c("deconvolution", "constant", "none"),
       output = c("onlyIdealization", "eachStep", "everything"), ...) 
}

\arguments{
\item{data}{a numeric vector containing the recorded data points}
\item{filter}{an object of class \code{\link{lowpassFilter}} giving the used analogue lowpass filter}
\item{family}{the parametric family for the multiscale test in \acronym{JSMURF}. \code{"jsmurf"}, \code{"jsmurfPS"} and \code{"jsmurfLR"} assume homogeneous noise and \code{"hjsmurf"}, \code{"hjsmurfSPS"} and \code{"hjsmurfLR"} allow for heterogeneous noise. By default, we recommend to use \code{"jsmurfPS"} when homogeneous noise is assumed and \code{"hjsmurf"} when heterogeneous noise is allowed, see \cite{examples}. \code{"jsmurf"} is the standard statistic from (\cite{Hotz et al.}, 2013), \code{"jsmurfPS"} is a slightly more powerful partial sum statistic, \code{"jsmurfLR"} is a likelihood-ratio statistic, which is even more powerful but slow. \code{"hjsmurf"} is the standard statistic for heterogeneous noise which estimates the variance locally, \code{"hjsmurfSPS"} is a studentized partial sum statistic and \code{"hjsmurfLR"} is a likelihood ratio statistic, which is more powerful, but very slow}
\item{q}{by default chosen automatically by \code{\link{getCritVal}}, for families \code{"jsmurf"}, \code{"jsmurfPS"} and \code{"jsmurfLR"} a single numeric giving the critical value, for families \code{"hjsmurf"}, \code{"hjsmurfSPS"} and \code{"hjsmurfLR"} a numeric vector giving scale dependent critical values}
\item{alpha}{a probability, i.e. a single numeric between 0 and 1, giving the significance level to compute \code{q} (if \code{q == NULL}), see \code{\link{getCritVal}}. Its choice is a trade-off between data fit and parsimony of the estimator. In other words, this argument balances the risks of missing conductance changes and detecting additional artefacts}
\item{sd}{a single positive numeric giving the standard deviation (noise level) \eqn{\sigma_0}{sigma0} of the data points before filtering, by default (NULL) estimated by \code{\link[stepR]{sdrobnorm}} with \code{lag = filter$len + 1L}. For families \code{"hjsmurf"}, \code{"hjsmurfSPS"} and \code{"hjsmurfLR"} this argument is ignored with a \code{\link{warning}}}
\item{startTime}{a single numeric giving the time at which recording (sampling) of \code{data} started, sampling time points will be assumed to be \code{startTime + seq(along = data) / filter$sr}}
\item{locationCorrection}{indicating how the locations of conductance changes are corrected for smoothing effects due to lowpass filtering, if \code{"deconvolution"} the local deconvolution procedure in \code{\link{deconvolveLocally}} from (\cite{Pein et al.}, 2018) is called, if \code{"constant"} all changes are moved to the left by the constant \code{filter$jump / filter$sr}, this was the approach in (\cite{Hotz et al.}, 2013), if \code{"none"} no correction is applied}
\item{output}{a string specifying the return type, see \cite{Value}}
\item{...}{additional parameters to be passed to \code{\link{getCritVal}} or \code{\link{deconvolveLocally}}:
\enumerate{
\item \code{\link{getCritVal}} will be called automatically if \code{q == NULL}, the number of data points \code{n = length(data)} will be set and \code{family}, \code{alpha} and \code{filter} will be passed. For these parameter no user interaction is required and possible, all other parameters of \code{\link{getCritVal}} can be passed additionally
\item \code{\link{deconvolveLocally}} will be called automatically if \code{locationCorrection == "deconvolution"}, the obtained fit will be passed to \code{fit} and \code{data}, \code{filter}, \code{startTime} will be passed and \code{output} will be set accordingly to the \code{output} argument. For these parameter no user interaction is required and possible, all other parameters of \code{\link{deconvolveLocally}} can be passed additionally
}
}
}

\value{
The idealisation (estimation, regression) obtained by \acronym{JSMURF}. If \code{output == "onlyIdealization"} an object object of class \code{\link[stepR]{stepblock}} containing the idealisation. If \code{output == "eachStep"} a \code{\link{list}} containing the entries \code{idealization} with the idealisation, \code{fit} with the fit before locations of conductance changes were corrected for filtering, \code{q} with the given / computed critical values, \code{filter} with the given filter and for families \code{"jsmurf"}, \code{"jsmurfPS"} and \code{"jsmurfLR"} \code{sd} with the given / estimated standard deviation. If \code{output == "everything"} a \code{\link{list}} containing the entries \code{idealization} with a \code{\link{list}} containing the idealisation after each refining step in the \code{\link[=deconvolveLocally]{local deconvolution}}, \code{fit} with the fit before locations of conductance changes were corrected for filtering, \code{q} with the given / computed critical values, \code{filter} with the given filter and for families \code{"jsmurf"}, \code{"jsmurfPS"} and \code{"jsmurfLR"} \code{sd} with the given / estimated standard deviation. Additionally, if \code{locationCorrection == "deconvolution"}, the idealisation has an \code{\link[=attributes]{attribute}} \code{"noDeconvolution"}, an integer vector, that gives the segments for which no deconvolution could be performed, since two short segments followed each other, see also \cite{details} in \code{\link{deconvolveLocally}}.
}

\section{Storing of Monte-Carlo simulations}{
If \code{q == NULL} a Monte-Carlo simulation is required to compute the critical values. Since a Monte-Carlo simulation lasts potentially much longer (up to several hours or days if the number of observations is in the millions) than the main calculations, multiple possibilities for saving and loading the simulations are offered. Progress of a simulation can be reported by the argument \code{messages} which can be specified in \code{\dots} and is explained in the documentation of \code{\link{getCritVal}}. Each Monte-Carlo simulation is specific to parametric \code{family}, the number of observations and the used filter. But note that also Monte-Carlo simulations for a (slightly) larger number of observations \eqn{n_q}{nq}, given in the argument \code{nq} in \code{\dots} and explained in the documentation of \code{\link{getCritVal}}, can be used, which avoids extensive resimulations for only a little bit varying number of observations, but results in a (small) loss of power. However, simulations of type \code{"vectorIncreased"} or \code{"matrixIncreased"}, i.e. objects of classes \code{"MCSimulationMaximum"} and \code{"MCSimulationVector"} with \code{nq} observations, have to be resimulated if \code{as.integer(log2(n1)) != as.integer(log2(n2))} when the saved simulation was computed with \code{n == n1} and the simulation now is required for \code{n == n2} and \code{nq >= n1} and \code{nq >= n2}. Simulations can either be saved in the workspace in the variable \code{critValStepRTab} or persistently on the file system for which the package \code{\link[R.cache]{R.cache}} is used. Moreover, storing in and loading from variables and \link[=readRDS]{RDS} files is supported. The simulation, saving and loading can be controlled by the argument \code{option} which can be specified in \code{\dots} and is explained in the documentation of \code{\link{getCritVal}}. By default simulations will be saved in the workspace and on the file system. For more details and for how simulation can be removed see Section \cite{Simulating, saving and loading of Monte-Carlo simulations} in \code{\link{getCritVal}}.
}

\seealso{\code{\link{getCritVal}}, \code{\link{lowpassFilter}}, \code{\link{deconvolveLocally}}, \code{\link{jules}}, \code{\link{hilde}}}

\encoding{utf8}
\references{
Hotz, T., \enc{Schütte}{Schuette}, O., Sieling, H., Polupanow, T., Diederichsen, U., Steinem, C., and Munk, A. (2013) Idealizing ion channel recordings by a jump segmentation multiresolution filter. \emph{IEEE Transactions on NanoBioscience} \bold{12}(4), 376--386.

Pein, F., Bartsch, A., Steinem, C., Munk, A. (2021) Heterogeneous Idealization of Ion Channel Recordings - Open Channel Noise. \emph{IEEE Transactions on NanoBioscience} \bold{20}(1), 57--78.

Pein, F., Tecuapetla-Gómez, I., \enc{Schütte}{Schuette}, O., Steinem, C., Munk, A. (2018) Fully-automatic multiresolution idealization for filtered ion channel recordings: flickering event detection. \emph{IEEE Transactions on NanoBioscience} \bold{17}(3), 300--320.
}

\examples{
## idealisation of the gramicidin A recordings given by gramA with jsmurf
# the used filter
filter <- lowpassFilter(type = "bessel", param = list(pole = 4L, cutoff = 1e3 / 1e4),
                        sr = 1e4)
\donttest{
# idealisation by JSMURF assuming homogeneous noise
# this call requires a Monte-Carlo simulation
# and therefore might last a few minutes,
# progress of the Monte-Carlo simulation is reported
idealisation <- jsmurf(gramA, filter = filter, family = "jsmurfPS", 
                       startTime = 9, messages = 100)
# detects conductance changes, but misses short events (flickering)
# if they are not of interest the above idealisation is suitable
# otherwise JULES should be used instead

# JSMURF allowing heterogeneous noise
# for illustration, but less appropriate for this dataset
jsmurf(gramA, filter = filter, family = "hjsmurf", 
       startTime = 9, messages = 100)

# any second call should be much faster
# as the previous Monte-Carlo simulation will be loaded
jsmurf(gramA, filter = filter, family = "jsmurfPS", 
       startTime = 9, messages = 100)

# much larger significance level alpha for a larger detection power,
# but also with the risk of detecting additional artefacts
# in this example much more changes are detected,
# most of them are probably artefacts
jsmurf(gramA, filter = filter, family = "jsmurfPS", 
       alpha = 0.9, startTime = 9)


# getCritVal was called in jsmurf, can be called explicitly
# for instance outside of a for loop to save run time
q <- getCritVal(length(gramA), filter = filter, family = "jsmurfPS")
identical(jsmurf(gramA, q = q, filter = filter, family = "jsmurfPS",
                 startTime = 9), idealisation)

# more detailed output
each <-  jsmurf(gramA, filter = filter, family = "jsmurfPS",
                startTime = 9, output = "each")
every <-  jsmurf(gramA, filter = filter, family = "jsmurfPS",
                 startTime = 9, output = "every")

identical(idealisation, each$idealization)
idealisationEvery <- every$idealization[[3]]
attr(idealisationEvery, "noDeconvolution") <- attr(every$idealization,
                                                   "noDeconvolution")
identical(idealisation, idealisationEvery)

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
# Monte-Carlo simulations are specific the number of observations and the filter
# hence a new simulation is required (if called for the first time)
idealisationWrong <- jsmurf(gramA, filter = wrongFilter, family = "jsmurfPS",
                            startTime = 9, messages = 100)

# idealisation
lines(idealisationWrong, col = "orange", lwd = 3)

# idealisation convolved with the filter
ind <- seq(9.647, 9.65, 1e-6)
convolvedSignal <- lowpassFilter::getConvolution(ind, idealisationWrong, filter)
lines(ind, convolvedSignal, col = "darkgreen", lwd = 3)


# location correction by a constant, almost the same as the local deconvolution
idealisationConst <- jsmurf(gramA, filter = filter, family = "jsmurfPS",
                            locationCorrection = "constant", startTime = 9, messages = 100)

# idealisation
lines(idealisationConst, col = "brown", lwd = 3)

# idealisation convolved with the filter
ind <- seq(9.647, 9.65, 1e-6)
convolvedSignal <- lowpassFilter::getConvolution(ind, idealisationConst, filter)
lines(ind, convolvedSignal, col = "purple", lwd = 3)

# no correction of locations for filter effects, jump location is shifted to the left
idealisationNone <- jsmurf(gramA, filter = filter, family = "jsmurfPS",
                           locationCorrection = "none", startTime = 9, messages = 100)

# idealisation
lines(idealisationNone, col = "black", lwd = 3)

# idealisation convolved with the filter
ind <- seq(9.647, 9.65, 1e-6)
convolvedSignal <- lowpassFilter::getConvolution(ind, idealisationNone, filter)
lines(ind, convolvedSignal, col = "green", lwd = 3)

# local deconvolution can be called separately
identical(deconvolveLocally(idealisationNone, data = gramA, filter = filter, startTime = 9),
          idealisation)


# simulation for a larger number of observations can be used (nq = 3e4)
# does not require a new simulation as the simulation from above will be used
# (if the previous call was executed first)
jsmurf(gramA[1:2.99e4], filter = filter, family = "jsmurfPS", startTime = 9, 
       nq = 3e4, r = 1e3, messages = 100)

# simulation of type "vectorIncreased" for n1 observations can only be reused 
# for n2 observations if as.integer(log2(n1)) == as.integer(log2(n2))
# no simulation is required, since a simulation of type "matrixIncreased"
# will be loaded from the fileSystem
# this call also saves a simulation of type "vectorIncreased" in the workspace
jsmurf(gramA[1:1e4], filter = filter, family = "jsmurfPS", startTime = 9, 
       nq = 3e4, messages = 100, r = 1e3)
# here a new simulation is required
# (if no appropriate simulation is saved from a previous call)
jsmurf(gramA[1:1e3], filter = filter, family = "jsmurfPS", startTime = 9, 
       nq = 3e4, messages = 100, r = 1e3,
       options = list(load = list(workspace = c("vector", "vectorIncreased"))))

# the above calls saved and (attempted to) load Monte-Carlo simulations
# in the following call the simulations will neither be saved nor loaded
jsmurf(gramA, filter = filter, family = "jsmurfPS", startTime = 9, 
       messages = 100, r = 1e3, options = list(load = list(), save = list()))

# only simulations of type "vector" and "vectorInceased" will be saved and
# loaded from the workspace, but no simulations of type "matrix" and
# "matrixIncreased" on the file system
jsmurf(gramA[1:1e4], filter = filter, family = "jsmurfPS",
       startTime = 9, messages = 100,
       options = list(load = list(workspace = c("vector", "vectorIncreased")),
                      save = list(workspace = c("vector", "vectorIncreased"))))

# explicit Monte-Carlo simulations, not recommended
stat <- stepR::monteCarloSimulation(n = length(gramA), family = "jsmurfPS",
                                    filter = filter, output = "maximum",
                                    r = 1e3, messages = 100)
jsmurf(gramA, filter = filter, family = "jsmurfPS", startTime = 9, stat = stat)

# with given standard deviation
sd <- stepR::sdrobnorm(gramA, lag = filter$len + 1)
identical(jsmurf(gramA, filter = filter, family = "jsmurfPS", startTime = 9, 
                 sd = sd), idealisation)

# with one refinement step less, but with a larger grid
# progress of the deconvolution is reported
# potential warning for no deconvolution is suppressed
jsmurf(gramA, filter = filter, family = "jsmurfPS", startTime = 9, 
       gridSize = c(1 / filter$sr, 1 / 10 / filter$sr),
       windowFactorRefinement = 2, report = TRUE,
       suppressWarningNoDeconvolution = TRUE)
}}

\keyword{nonparametric}
