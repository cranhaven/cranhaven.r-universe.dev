\name{lowpassFilter}
\alias{lowpassFilter}
\alias{print.lowpassFilter}

\title{Lowpass filtering}

\description{
Reexported from \code{\link[lowpassFilter]{lowpassFilter}}. Creates lowpass filters.
}

\usage{
lowpassFilter(type = c("bessel"), param, sr = 1, len = NULL, shift = 0.5)
}

\arguments{
\item{type}{a string specifying the type of the filter, currently only Bessel filters are supported}
\item{param}{a \code{\link{list}} specifying the parameters of the filter depending on \code{type}. For \code{"bessel"} the entries \code{pole} and \code{cutoff} have to be specified and no other named entries are allowed. \code{pole} has to be a single integer giving the number of poles (order). \code{cutoff} has to be a single positive numeric not larger than \code{1} giving the normalized cutoff frequency, i.e. the cutoff frequency (in the temporal domain) of the filter divided by the sampling rate}
\item{sr}{a single numeric giving the sampling rate}
\item{len}{a single integer giving the filter length of the truncated and digitised filter, see \cite{Value} for more details. By default (\code{NULL}) it is chosen such that the autocorrelation function is below \code{1e-3} at \code{len / sr} and at all lager lags \code{(len + i) / sr}, with \code{i} a positive integer}
\item{shift}{a single numeric between \code{0} and \code{1} giving a shift for the digitised filter, i.e. kernel and step are obtained by evaluating the corresponding functions at \code{(0:len + shift) / sr}}
}

\value{
An object of \code{\link{class}} \code{lowpassFilter}, i.e. a \code{\link{list}} that contains
\describe{
\item{\code{"type"}, \code{"param"}, \code{"sr"}, \code{"len"}}{the corresponding arguments}
\item{\code{"kernfun"}}{the kernel function of the filter, obtained as the Laplace transform of the corresponding transfer function}
\item{\code{"stepfun"}}{the step-response of the filter, i.e. the antiderivative of the filter kernel}
\item{\code{"acfun"}}{the autocorrelation function, i.e. the convolution of the filter kernel with itself}
\item{\code{"acAntiderivative"}}{the antiderivative of the autocorrelation function}
\item{\code{"truncatedKernfun"}}{the kernel function of the at \code{len / sr} truncated filter, i.e. \code{kernfun} truncated and rescaled such that the new kernel still integrates to \code{1}}
\item{\code{"truncatedStepfun"}}{the step-response of the at \code{len / sr} truncated filter, i.e. the antiderivative of the kernel of the truncated filter}
\item{\code{"truncatedAcfun"}}{the autocorrelation function of the at \code{len / sr} truncated filter, i.e. the convolution of the kernel of the truncated filter with itself}
\item{\code{"truncatedAcAntiderivative"}}{the antiderivative of the autocorrelation function of the at \code{len / sr} truncated filter}
\item{\code{"kern"}}{the digitised filter kernel normalised to one, i.e. \code{kernfun((0:len + shift) / sr) / sum(kernfun((0:len + shift) / sr))}}
\item{\code{"step"}}{the digitised step-response of the filter, i.e. \code{stepfun((0:len + shift) / sr)}}
\item{\code{"acf"}}{the discrete autocorrelation, i.e. \code{acfun(0:len / sr)}}
\item{\code{"jump"}}{the last index of the left half of the filter, i.e. \code{min(which(ret$step >= 0.5)) - 1L}, it indicates how much a jump is shifted in time by a convolution of the signal with the digitised kernel of the lowpassfilter; if all values are below \code{0.5}, \code{len} is returned with a \code{warning}}
\item{\code{"number"}}{for developers; an integer indicating the type of the filter}
\item{\code{"list"}}{for developers; a list containing precomputed quantities to recreate the filter in C++}
}
}

\encoding{utf8}
\references{
Pein, F., Bartsch, A., Steinem, C., Munk, A. (2021) Heterogeneous Idealization of Ion Channel Recordings - Open Channel Noise. \emph{IEEE Transactions on NanoBioscience} \bold{20}(1), 57--78.

Pein, F., Tecuapetla-Gómez, I., \enc{Schütte}{Schuette}, O., Steinem, C., Munk, A. (2018) Fully-automatic multiresolution idealization for filtered ion channel recordings: flickering event detection. IEEE Trans. Nanobioscience, 17(3):300-320.

Pein, F. (2017) Heterogeneous Multiscale Change-Point Inference and its Application to Ion Channel Recordings.
PhD thesis, Georg-August-Universität Göttingen. http://hdl.handle.net/11858/00-1735-0000-002E-E34A-7.

Hotz, T., \enc{Schütte}{Schuette}, O., Sieling, H., Polupanow, T., Diederichsen, U., Steinem, C., and Munk, A. (2013) Idealizing
ion channel recordings by a jump segmentation multiresolution filter. IEEE Trans. Nanobioscience, 12(4):376-386.
}

\author{This function is a modified and extended version of \code{\link[stepR]{dfilter}} written by Thomas Hotz. New code is written by Florian Pein and Inder Tecuapetla-Gómez.}

\examples{
# the filter used for the gramicidin A recordings given by gramA
filter <- lowpassFilter(type = "bessel", param = list(pole = 4L, cutoff = 1e3 / 1e4),
                        sr = 1e4)

# filter kernel, truncated version
plot(filter$kernfun, xlim = c(0, 20 / filter$sr))
t <- seq(0, 20 / filter$sr, 0.01 / filter$sr)
# truncated version looks very similar
lines(t, filter$truncatedKernfun(t), col = "red")

# filter$len (== 11) is chosen such that filter$acf < 1e-3 for it and all larger lags
plot(filter$acfun, xlim = c(0, 20 / filter$sr), ylim = c(-0.003, 0.003))
abline(h = 0.001, lty = "22")
abline(h = -0.001, lty = "22")

abline(v = (filter$len - 1L) / filter$sr, col = "grey")
abline(v = filter$len / filter$sr, col = "red")
\donttest{
## zoom into a single jump of the idealisation
## we suggest to do this for every new measurement setup once 
## to control whether the correct filter is assumed
# idealisation by JULES (might take some time if not called somewhere before,
# please see its documentation for more details)
idealisation <- jules(gramA, filter = filter, startTime = 9, messages = 100)

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
}

# filter with sr == 1
filter <- lowpassFilter(type = "bessel", param = list(pole = 4L, cutoff = 1e3 / 1e4))

# filter kernel and its truncated version
plot(filter$kernfun, xlim = c(0, 20 / filter$sr))
t <- seq(0, 20 / filter$sr, 0.01 / filter$sr)
# truncated version, looks very similar
lines(t, filter$truncatedKernfun(t), col = "red")
# digitised filter
points((0:filter$len + 0.5) / filter$sr, filter$kern, col = "red", pch = 16)

# without a shift
filter <- lowpassFilter(type = "bessel", param = list(pole = 4L, cutoff = 1e3 / 1e4),
                        shift = 0)
# filter$kern starts with zero
points(0:filter$len / filter$sr, filter$kern, col = "blue", pch = 16)

# much shorter filter
filter <- lowpassFilter(type = "bessel", param = list(pole = 4L, cutoff = 1e3 / 1e4),
                        len = 4L)
points((0:filter$len + 0.5) / filter$sr, filter$kern, col = "darkgreen", pch = 16)
}

\keyword{ts}
