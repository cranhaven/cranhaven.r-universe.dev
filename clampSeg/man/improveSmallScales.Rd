\name{improveSmallScales}
\alias{improveSmallScales}

\title{Improves small scales}
\description{Implements the second and third step of \acronym{HILDE} (\cite{Pein et al.}, 2021). It refines an initial fit, e.g. obtained by \code{\link{jsmurf}} on small temporal scales by testing for events and local deconvolution. Refinment can be done assuming homogeneous noise, but also allow heterogeneous noise. Please see the argument \code{method} and the \cite{examples} for how to access the function correctly depending on whether homogeneous noise is assumed or heterogeneous noise is allowed. Further details about how to decide whether the noise is homogeneous or heterogeneous and whether events are short are given in the accompanying vignette.\cr
If \code{q == NULL} a Monte-Carlo simulation is required for computing critical values. Since a Monte-Carlo simulation lasts potentially much longer (up to several hours or days if the number of observations is in the millions) than the main calculations, the package saves them by default in the workspace and on the file system such that a second call requiring the same Monte-Carlo simulation will be much faster. For more details, in particular to which arguments the Monte-Carlo simulations are specific, see Section \cite{Storing of Monte-Carlo simulations} below. Progress of a Monte-Carlo simulation can be reported by the argument \code{messages} and the saving can be controlled by the argument \code{option}, both can be specified in \code{\dots} and are explained in \code{\link{getCritVal}}.
}

\usage{
improveSmallScales(fit, data, filter, method = c("2Param", "LR"),
                   lengths = NULL, q = NULL, alpha = 0.04, r = 1e3, startTime = 0,
                   thresholdLongSegment = if (method == "LR") 10L else 25L,
                   localValue = stats::median,
                   localVar = function(data) stepR::sdrobnorm(data,
                                                              lag = filter$len + 1L)^2,
                   regularization = 1,
                   gridSize = c(1, 1 / 10, 1 / 100) / filter$sr,
                   windowFactorRefinement = 1,
                   output = c("onlyIdealization", "everyGrid"), report = FALSE,
                   suppressWarningNoDeconvolution = FALSE,
                   localList = NULL, ...)
}

\arguments{
\item{fit}{an \code{\link[stepR]{stepblock}} object or a list containing an entry \code{fit} with a \code{\link[stepR]{stepblock}} object giving the initial fit (reconstruction), e.g. computed by \code{\link{jsmurf}} with \code{locationCorrection == "none"}}
\item{data}{a numeric vector containing the recorded data points}
\item{filter}{an object of class \code{\link{lowpassFilter}} giving the used analogue lowpass filter}
\item{method}{the testing method for short events, \code{"2Param"} allows for heterogeneous noise, \code{"LR"} assumes homogeneous noise}
\item{lengths}{a vector of integers giving the lengths on which tests will be performed to detect short events, should be chosen such that events on larger scales are already contained in \code{fit}}
\item{q}{a numeric vector of the same length as \code{lengths} giving scale dependent critical values for the tests for short events, by default chosen automatically by \code{\link{getCritVal}}}
\item{alpha}{a probability, i.e. a single numeric between 0 and 1, giving the significance level to compute the critical values (if \code{q == NULL}), see \code{\link{getCritVal}}. Its choice balances the risks of missing short events and detecting additional artefacts}
\item{r}{an integer giving the number of Monte-Carlo simulations (if \code{q == NULL}), a larger value increases accuracy, but also the run time}
\item{startTime}{a single numeric giving the time at which recording (sampling) of \code{data} started, sampling time points will be assumed to be \code{startTime + seq(along = data) / filter$sr}}
\item{thresholdLongSegment}{a single integer giving the threshold determining how many observations are necessary to estimate parameters, conductance value and its variance, without deconvolution; has to be chosen such that \code{localValue} and \code{localVar} can be applied to a vector of this and larger length}
\item{localValue}{a function for estimating the conductance levels on long segments, see \cite{details}, will be called with \code{localValue(data[i:j])} with \code{i} and \code{j} two integers in \code{1:length(data)} and \code{j - i >= thresholdLongSegment}, has to return a single numeric}
\item{localVar}{a function for estimating the variance on long segments, see \cite{details}, will be called with \code{localVar(data[i:j])} with \code{i} and \code{j} two integers in \code{1:length(data)} and \code{j - i >= thresholdLongSegment}, has to return a single, positive numeric}
\item{regularization}{a single positive numeric or a numeric vector with positive entries or a \code{\link{list}} of length \code{length(gridSize)}, with each entry a single positive numeric or a numeric vector with positive entries,  giving the regularisation added to the correlation matrix, see \cite{details}. For a \code{\link{list}} the i-th entry will be used in the i-th refinement}
\item{gridSize}{a numeric vector giving the size of the grids in the iterative grid search, see \cite{details}}
\item{windowFactorRefinement}{a single numeric or a numeric vector of length \code{length(gridSize) - 1} giving factors for the refinement of the grid, see \cite{details}. If a single numeric is given its value is used in all refinement steps}
\item{output}{a string specifying the return type, see \cite{Value}}
\item{report}{a single \code{\link{logical}}, if \code{TRUE} the progress will be reported by \code{\link[=message]{messages}}}
\item{suppressWarningNoDeconvolution}{a single \code{\link{logical}}, if \code{FALSE} a \code{\link{warning}} will be given if at least one segment exists for which no deconvolution can be performed, since two short segments follow each other immediately}
\item{localList}{an object of class \code{"localList"}, usually computed by \code{\link{createLocalList}}, doing such a pre-computation saves run time if \code{improveSmallScales} or \code{\link{hilde}} is called multiple times with the same arguments}
\item{...}{additional parameters to be passed to \code{\link{getCritVal}}, \code{\link{getCritVal}} will be called automatically (if \code{q == NULL}), the number of data points \code{n = length(data)} will be set, argument \code{method} will be assigned to \code{family} and \code{alpha} and \code{filter} will be passed. For these parameter no user interaction is required and possible, all other parameters of \code{\link{getCritVal}} can be passed additionally}
}

\value{
The idealisation (fit, regression) obtained by testing for short events and local deconvolution. If \code{output == "onlyIdealization"} an object of class \code{\link[stepR]{stepblock}} containing the final idealisation obtained by local deconvolution. If \code{output == "everyGrid"} a \code{\link{list}} of length \code{length(gridSize)} containing the idealisation after each refining step. Additionally, in both cases, an \code{\link[=attributes]{attribute}} \code{"noDeconvolution"}, an integer vector, gives the segments for which no deconvolution could be performed, since two short segments followed each other, see \cite{details}. Moreover, the (computed) \code{q} is returned as an \code{\link[=attributes]{attribute}}.
}

\details{
First of al, tests for additional short events are performed. Those tests take into account the lowpass filter explicitly. Afterwards all conductance levels (of the newly found and of the already existing event) and locations of the conductance changes are determined by local deconvolution. The local deconvolution consists of two parts.\cr
In the first part, all segments of the initial fit, potentially interrupted by newly detected events, will be divided into long and short ones. The first and lasts \code{filter$len} data points of each segment will be ignored and if the remaining data points \code{data[i:j]} are at least \code{thresholdLongSegment}, i.e. \code{j - i + 1 >= thresholdLongSegment}, the parameters, conductance level and variance, of this segment will be determined by \code{localValue(data[i:j])} and \code{localVar(data[i:j])}.\cr
The long segments allow in the second part to perform the deconvolution locally by maximizing the likelihood function by an iterative grid search. Three scenarios might occur: Two long segments can follow each other, in this case the change, but no level, has to be estimated by maximizing the likelihood function of only few observations in this single parameter. A single short segment can be in between of two long segments, in this case two changes and one level have to be estimated by maximizing the likelihood function of only few observations in these three parameters. Finally, two short segments can follow each other, in this case no deconvolution is performed and the initial parameters are returned for these segments together with entries in the \code{"noDeconvolution"} \code{\link[=attributes]{attribute}}. More precisely, let \code{i:j} be the short segments, then \code{i:j} will be added to the \code{"noDeconvolution"} \code{\link[=attributes]{attribute}} and for the idealisation (if \code{output == "everyGrid"} this applies for each entry) the entries \code{value[i:j]}, \code{leftEnd[i:(j + 1)]} and \code{rightEnd[(i - 1):j]} are kept from the initial fit without refinement by deconvolution. If \code{suppressWarningNoDeconvolution == FALSE}, additionally, a \code{\link{warning}} will be given at first occurrence.\cr
Maximisation of the likelihood is performed by minimizing (\cite{Pein et al.}, 2018, (9)), a term of the form \eqn{x^T\Sigma x}{x^T\Sigma x}, where \eqn{\Sigma} is the regularised correlation matrix and  \eqn{x} a numeric vector of the same dimension. More precisely, the (unregularised) correlations are \code{filter$acf}, to this the regularisation \code{regularization} is added. In detail, if \code{regularization} is a numeric, the regularised correlation is \preformatted{
cor <- filter$acf
cor[seq(along = regularization)] <- cor[seq(along = regularization)] + regularization
} and if \code{regularization} is a list the same, but \code{regularization} is in the \code{i-th} refinement replaced by \code{regularization[[i]]}. Then, \eqn{\Sigma} is a symmetric Toeplitz matrix with entries \code{cor}, i.e. a matrix with \code{cor[1]} on the main diagonal, \code{cor[2]} on the second diagonal, etc. and \code{0} for all entries outside of the first \code{length(cor)} diagonals.\cr
The minimisations are performed by an iterative grid search: In a first step potential changes will be allowed to be at the grid / time points \code{seq(cp - filter$len / filter$sr, cp, gridSize[1])}, with \code{cp} the considered change of the initial fit. For each grid point in case of a single change and for each combination of grid points in case of two changes the term in (9) is computed and the change(s) for which the minimum is attained is / are chosen. Afterwards, refinements are done with the grids \preformatted{
seq(cp - windowFactorRefinement[j - 1] * gridSize[j - 1],
    cp + windowFactorRefinement[j - 1] * gridSize[j - 1],
    gridSize[j]),
} with \code{cp} the change of the iteration before, as long as entries in \code{gridSize} are given.
}

\section{Storing of Monte-Carlo simulations}{
If \code{q == NULL} a Monte-Carlo simulation is required to compute the critical values. Since a Monte-Carlo simulation lasts potentially much longer (up to several hours or days if the number of observations is in the millions) than the main calculations, multiple possibilities for saving and loading the simulations are offered. Progress of a simulation can be reported by the argument \code{messages} which can be specified in \code{\dots} and is explained in the documentation of \code{\link{getCritVal}}. Each Monte-Carlo simulation is specific to test \code{method}, the number of observations and the used filter. Monte-Carlo simulations are also specific to the arguments \code{thresholdLongSegment}, \code{localValue} and \code{localVar}. Currently, storing a Monte-Carlo simulation is only possible for their default values. Note, that also Monte-Carlo simulations for a (slightly) larger number of observations \eqn{n_q}{nq}, given in the argument \code{nq} in \code{\dots} and explained in the documentation of \code{\link{getCritVal}}, can be used, which avoids extensive resimulations for only a little bit varying number of observations, but results in a (small) loss of power. However, simulations of type \code{"matrixIncreased"}, i.e. objects of class \code{"MCSimulationVector"} with \code{nq} observations, have to be resimulated if \code{as.integer(log2(n1)) != as.integer(log2(n2))} when the saved simulation was computed with \code{n == n1} and the simulation now is required for \code{n == n2} and \code{nq >= n1} and \code{nq >= n2}. Simulations can either be saved in the workspace in the variable \code{critValStepRTab} or persistently on the file system for which the package \code{\link[R.cache]{R.cache}} is used. Moreover, storing in and loading from variables and \link[=readRDS]{RDS} files is supported. The simulation, saving and loading can be controlled by the argument \code{option} which can be specified in \code{\dots} and is explained in the documentation of \code{\link{getCritVal}}. By default simulations will be saved in the workspace and on the file system. For more details and for how simulation can be removed see Section \cite{Simulating, saving and loading of Monte-Carlo simulations} in \code{\link{getCritVal}}.
}

\seealso{\code{\link{hilde}}, \code{\link{lowpassFilter}}, \code{\link{createLocalList}}}

\encoding{utf8}
\references{
Pein, F., Bartsch, A., Steinem, C., Munk, A. (2021) Heterogeneous Idealization of Ion Channel Recordings - Open Channel Noise. \emph{IEEE Transactions on NanoBioscience} \bold{20}(1), 57--78.

Pein, F., Tecuapetla-Gómez, I., \enc{Schütte}{Schuette}, O., Steinem, C., Munk, A. (2018) Fully-automatic multiresolution idealization for filtered ion channel recordings: flickering event detection. \emph{IEEE Transactions on NanoBioscience} \bold{17}(3), 300--320.
}

\examples{
## refinement of an initial fit of the gramicidin A recordings given by gramA
# the used filter
filter <- lowpassFilter(type = "bessel", param = list(pole = 4L, cutoff = 1e3 / 1e4),
                        sr = 1e4)
# initial fit, good on larger temporal scales, but misses short events
# with given q to save computation time
# this q is specific to length of the data and the filter
fit <- jsmurf(gramA, filter = filter, family = "jsmurfPS", q = 1.775696, startTime = 9,
              locationCorrection = "none")
\donttest{
# improvement on small temporal scales by testing for short events and local deconvolution
# this call requires a Monte-Carlo simulation
# and therefore might last a few minutes,
# progress of the Monte-Carlo simulation is reported
deconvolution <- improveSmallScales(fit, data = gramA, method = "LR", filter = filter,
                                    startTime = 9, messages = 100)

# any second call should be much faster
# as the previous Monte-Carlo simulation will be loaded
# return fit after each refinement
every <- improveSmallScales(fit, data = gramA, method = "LR", filter = filter,
                            startTime = 9, output = "every")
deconvolutionEvery <- every[[3]]
attr(deconvolutionEvery, "noDeconvolution") <- attr(every, "noDeconvolution")
attr(deconvolutionEvery, "q") <- attr(every, "q")
identical(deconvolution, deconvolutionEvery)

# identical to a direct idealisation by hilde
compare <- deconvolution
attr(compare, "q") <- NULL
identical(hilde(gramA, filter = filter, family = "jsmurfPS", method = "LR",
                startTime = 9), compare)

# allowing heterogeneous noise
fitH <- jsmurf(gramA, filter = filter, family = "hjsmurf", r = 100, startTime = 9,
               locationCorrection = "none")
improveSmallScales(fitH, data = gramA, method = "2Param", filter = filter,
                   startTime = 9, messages = 10, r = 100)
# r = 100 is used to reduce its run time,
# this is okay for illustration purposes, but for precise results
# a larger number of Monte-Carlo simulations is recommend


## zoom into a single event,
## similar to (Pein et al., 2018, Figure 2 lower left panel)
time <- 9 + seq(along = gramA) / filter$sr # time points
plot(time, gramA, pch = 16, col = "grey30", ylim = c(20, 50),
     xlim = c(10.40835, 10.4103), ylab = "Conductance in pS", xlab = "Time in s")

# deconvolution
lines(deconvolution, col = "red", lwd = 3)

# deconvolution convolved with the filter
ind <- seq(10.408, 10.411, 1e-6)
convolvedSignal <- lowpassFilter::getConvolution(ind, deconvolution, filter)
lines(ind, convolvedSignal, col = "blue", lwd = 3)

# for comparison, fit prior to the improvement step
# does not contain the event and hence fits the recorded data points badly
# fit
lines(fit, col = "orange", lwd = 3)

# fit convolved with the filter
ind <- seq(10.408, 10.411, 1e-6)
convolvedSignal <- lowpassFilter::getConvolution(ind, fit, filter)
lines(ind, convolvedSignal, col = "darkgreen", lwd = 3)


# with less regularisation of the correlation matrix
improveSmallScales(fit, data = gramA, method = "LR", filter = filter,
                   startTime = 9, messages = 100, regularization = 0.5)

# with estimation of the level of long segments by the mean
# but requiring 30 observations for it
improveSmallScales(fit, data = gramA, method = "LR", filter = filter,
                   startTime = 9, messages = 100,
                   localValue = mean, thresholdLongSegment = 30)

# with one refinement step less, but with a larger grid
# test are performed on less lengths
# progress of the deconvolution is reported
# potential warning for no deconvolution is suppressed
improveSmallScales(fit, data = gramA, method = "LR", filter = filter,
                   startTime = 9, messages = 100,
                   lengths = c(3:5, 8, 11, 16, 20),
                   gridSize = c(1 / filter$sr, 1 / 10 / filter$sr),
                   windowFactorRefinement = 2, report = TRUE,
                   suppressWarningNoDeconvolution = TRUE)

# pre-computation of quantities using createLocalList
# this saves run time if improveSmallScales (or hilde) is called more than once
localList <- createLocalList(filter = filter, method = "LR")
identical(improveSmallScales(fit, data = gramA, method = "LR", filter = filter,
                             startTime = 9, localList = localList), deconvolution)
}}

\keyword{nonparametric}
