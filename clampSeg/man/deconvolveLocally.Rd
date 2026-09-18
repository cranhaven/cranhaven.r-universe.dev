\name{deconvolveLocally}
\alias{deconvolveLocally}
\alias{local deconvolution}

\title{Local deconvolution}
\description{Implements the estimation step of \acronym{JULES} (\cite{Pein et al.}, 2018, Section III-B) in which an initial fit (reconstruction), e.g. computed by \code{\link{stepDetection}}, is refined by local deconvolution.}

\usage{
deconvolveLocally(fit, data, filter, startTime = 0, regularization = 1, 
                  thresholdLongSegment = 10L, localEstimate = stats::median,
                  gridSize = c(1, 1 / 10, 1 / 100) / filter$sr,
                  windowFactorRefinement = 1,
                  output = c("onlyIdealization", "everyGrid"), report = FALSE,
                  suppressWarningNoDeconvolution = FALSE)
}

\arguments{
\item{fit}{an \code{\link[stepR]{stepblock}} object or a list containing an entry \code{fit} with a \code{\link[stepR]{stepblock}} object giving the initial fit (reconstruction), e.g. computed by \code{\link{stepDetection}}}
\item{data}{a numeric vector containing the recorded data points}
\item{filter}{an object of class \code{\link{lowpassFilter}} giving the used analogue lowpass filter}
\item{startTime}{a single numeric giving the time at which recording (sampling) of \code{data} started, sampling time points will be assumed to be \code{startTime + seq(along = data) / filter$sr}}
\item{regularization}{a single positive numeric or a numeric vector with positive entries or a \code{\link{list}} of length \code{length(gridSize)}, with each entry a single positive numeric or a numeric vector with positive entries,  giving the regularisation added to the correlation matrix, see \cite{details}. For a \code{\link{list}} the i-th entry will be used in the i-th refinement}
\item{thresholdLongSegment}{a single integer giving the threshold determining how many observations are necessary to estimate a level (without deconvolution)}
\item{localEstimate}{a function for estimating the levels of all long segments, see \cite{details}, will be called with \code{localEstimate(data[i:j])} with \code{i} and \code{j} two integers in \code{1:length(data)} and \code{j - i >= thresholdLongSegment}}
\item{gridSize}{a numeric vector giving the size of the grids in the iterative grid search, see \cite{details}}
\item{windowFactorRefinement}{a single numeric or a numeric vector of length \code{length(gridSize) - 1} giving factors for the refinement of the grid, see \cite{details}. If a single numeric is given its value is used in all refinement steps}
\item{output}{a string specifying the return type, see \cite{Value}}
\item{report}{a single \code{\link{logical}}, if \code{TRUE} the progress will be reported by \code{\link[=message]{messages}}}
\item{suppressWarningNoDeconvolution}{a single \code{\link{logical}}, if \code{FALSE} a \code{\link{warning}} will be given if at least one segment exists for which no deconvolution can be performed, since two short segments follow each other immediately}
}

\value{
The idealisation (fit, regression) obtained by local deconvolution procedure of the estimation step of \acronym{JULES}. If \code{output == "onlyIdealization"} an object of class \code{\link[stepR]{stepblock}} containing the final idealisation obtained by local deconvolution. If \code{output == "everyGrid"} a \code{\link{list}} of length \code{length(gridSize)} containing the idealisation after each refining step. Additionally, in both cases, an \code{\link[=attributes]{attribute}} \code{"noDeconvolution"}, an integer vector, gives the segments for which no deconvolution could be performed, since two short segments followed each other, see \cite{details}.
}

\details{
The local deconvolution consists of two parts.\cr
In the first part, all segments of the initial fit will be divided into long and short ones. The first and lasts \code{filter$len} data points of each segment will be ignored and if the remaining data points \code{data[i:j]} are at least \code{thresholdLongSegment}, i.e. \code{j - i + 1 >= thresholdLongSegment}, the level (value) of this segment will be determined by \code{localEstimate(data[i:j])}.\cr
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

\seealso{\code{\link{jules}}, \code{\link{stepDetection}}, \code{\link{lowpassFilter}}}

\encoding{utf8}
\references{
Pein, F., Tecuapetla-Gómez, I., \enc{Schütte}{Schuette}, O., Steinem, C., Munk, A. (2018) Fully-automatic multiresolution idealization for filtered ion channel recordings: flickering event detection. \emph{IEEE Transactions on NanoBioscience} \bold{17}(3), 300--320.
}

\examples{
## refinement of an initial fit of the gramicidin A recordings given by gramA
# the used filter
filter <- lowpassFilter(type = "bessel", param = list(pole = 4L, cutoff = 1e3 / 1e4),
                        sr = 1e4)
# initial fit
# with given q to save computation time
# this q is specific to length of the data and the filter
fit <- stepDetection(gramA, q = 1.370737, filter = filter, startTime = 9)

deconvolution <- deconvolveLocally(fit, data = gramA, filter = filter, startTime = 9)
\donttest{
# return fit after each refinement
every <- deconvolveLocally(fit, data = gramA, filter = filter, startTime = 9,
                           output = "every")

deconvolutionEvery <- every[[3]]
attr(deconvolutionEvery, "noDeconvolution") <- attr(every, "noDeconvolution")
identical(deconvolution, deconvolutionEvery)

# identical to a direct idealisation by jules
identical(jules(gramA, q = 1.370737, filter = filter, startTime = 9),
          deconvolution)

## zoom into a single event, (Pein et al., 2018, Figure 2 lower left panel)
time <- 9 + seq(along = gramA) / filter$sr # time points
plot(time, gramA, pch = 16, col = "grey30", ylim = c(20, 50),
     xlim = c(10.40835, 10.4103), ylab = "Conductance in pS", xlab = "Time in s")

# deconvolution
lines(deconvolution, col = "red", lwd = 3)

# deconvolution convolved with the filter
ind <- seq(10.408, 10.411, 1e-6)
convolvedSignal <- lowpassFilter::getConvolution(ind, deconvolution, filter)
lines(ind, convolvedSignal, col = "blue", lwd = 3)

# for comparison, fit prior to the deconvolution step
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

# deconvolution
lines(deconvolution, col = "red", lwd = 3)

# deconvolution convolved with the filter
ind <- seq(9.647, 9.65, 1e-6)
convolvedSignal <- lowpassFilter::getConvolution(ind, deconvolution, filter)
lines(ind, convolvedSignal, col = "blue", lwd = 3)

# deconvolution with a wrong filter
# does not fit the recorded data points appropriately
wrongFilter <- lowpassFilter(type = "bessel",
                             param = list(pole = 6L, cutoff = 0.2),
                             sr = 1e4)
deconvolutionWrong <- deconvolveLocally(fit, data = gramA, filter = wrongFilter,
                                        startTime = 9)

# deconvolution
lines(deconvolutionWrong, col = "orange", lwd = 3)

# ideconvolution convolved with the filter
ind <- seq(9.647, 9.65, 1e-6)
convolvedSignal <- lowpassFilter::getConvolution(ind, deconvolutionWrong, filter)
lines(ind, convolvedSignal, col = "darkgreen", lwd = 3)

# with less regularisation of the correlation matrix
deconvolveLocally(fit, data = gramA, filter = filter, startTime = 9,
                  regularization = 0.5)

# with estimation of the level of long segments by the mean
# but requiring 30 observations for it
deconvolveLocally(fit, data = gramA, filter = filter, startTime = 9,
                  localEstimate = mean, thresholdLongSegment = 30)

# with one refinement step less, but with a larger grid
# progress of the deconvolution is reported
# potential warning for no deconvolution is suppressed
deconvolveLocally(fit, data = gramA, filter = filter, startTime = 9,
                  gridSize = c(1 / filter$sr, 1 / 10 / filter$sr),
                  windowFactorRefinement = 2, report = TRUE,
                  suppressWarningNoDeconvolution = TRUE)
}}

\keyword{nonparametric}
