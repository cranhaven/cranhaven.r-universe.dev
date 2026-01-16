
\newcommand{\opt}{\ifelse{latex}{\code{"#1"}}{\verb{"#1"}}}
\newcommand{\nl}{\ifelse{latex}{ }{\ifelse{html}{ }{ \cr}}}

\name{Dimodal Model Tests}
\alias{Dipeak.test}
\alias{Dipeak.critval}
\alias{Diflat.test}
\alias{Diflat.critval}
\title{
Significance models of features in the low-pass spacing.
}
\description{
Return the probability of the characteristic feature value, the height of a
peak or length of a flat, using parametric models developed for the low-pass
spacing, or determine the feature value at some significance level.
}

\usage{
Dipeak.test(ht, n, flp, filter, lower.tail=TRUE)
Dipeak.critval(pval, n, flp, filter)
Diflat.test(len, n, flp, filter, basedist, lower.tail=TRUE)
Diflat.critval(pval, n, flp, filter, basedist)
}

\arguments{
\item{ht}{
  difference(s) between the standardized data value at the peak and
  deepest minimum to either side
}
\item{len}{
  length(s) of flat in data points
}
\item{pval}{
  the significance level(s) to find the corresponding height or length,
  the quantile of the feature value is \code{1-pval}
}
\item{n}{
  number of data points before filtering
}
\item{flp}{
  the size of the FIR kernel, either as a fraction of \code{n} or as an
  integer
}
\item{filter}{
  the FIR kernel used to smooth the spacing
}
\item{basedist}{
  for the flat models, the distribution used to generate the length quantiles
}
\item{lower.tail}{
  a boolean, if TRUE the test returns the probability the null distribution
  is less than or equal to the feature value, if FALSE greater than
}
}

\details{
The test functions convert the feature value into a quantile or significance
level based on null distribution models.  The critval functions do the
opposite.  The models are parametric because they are built on draws of
specifically chosen variates and the size of features that appear after
low-pass filtering the data.  The features depend on the size of the
draw \code{n} and the smoothing done, set by the Finite Impulse Response
(FIR) \code{filter} and the size \code{flp} of the kernel.  Implicitly they
depend on the feature detectors, but variations in the parameters controlling
those have neither been studied nor incorporated in the model.

The peak height model comes from draws of an asymmetric Weibull variate with
scale 2 and shape 4, which proved to give reasonable, conservative quantiles
against other distributions.  The preferred filter uses a Kaiser kernel.
The other filters available, the Bartlett or triangular (synonyms), Hanning,
Hamming, Gaussian or normal (synonyms), and Blackman kernel, are handled by
scaling the Kaiser model.  The filter size is typically expressed as a
fraction of the draw size, with \code{flp=0.15} a good default; spans in
data points are also accepted.  Smaller kernels will produce rougher data
with more peaks and fewer flats and can be tolerated if the spacing is
already smooth, as happens with very large data sets.  The test height for
the model is scaled by the standard deviation of the total signal.

The peak test models the distribution of heights with an inverse Gaussian,
a.k.a. Wald distribution.  The height is corrected for the filter and its
size, and the inverse Gaussian location and scale parameters depend on the
data and filter sizes.  These values are provided in the returned list.

The flat length model varies much more with the parametric distribution
chosen as the base, and the recommended \code{basedist}, a logistic variate,
is a compromise.  Models for normal or Gaussian (synonyms), Gumbel, and
Weibull distributions are also available, but there is little overlap
between the quantiles of lengths within them; the logistic falls in the
middle.  The Weibull variant is more liberal, accepting lengths that are
two-thirds those needed to pass at the same level as the logistic.  The
Gumbel lengths are four-thirds longer.  The filter type, size, and draw
size are the same as for the peak height model.  Unlike the peak model,
different filters require different models internally.

The length distribution varies smoothly with the data size and filter, and
the flat model can calculate the probability directly without going through
a distribution function.

The models come from simulations over the ranges \code{n = 50 \dots 500} and
\code{flp = 0.05 \dots 0.5}, measuring quantiles between \code{q = 0.90 \dots
0.99999}.  They fit the critical values within 5\% over most of these values,
degrading to 10\% at the edges.  The spread in the reported probability also
increases at the edges of the parameter space.  In particular, data sets of
less than 60 points or windows larger than 30\% are less trustworthy, as are
quantiles beyond 0.9999.  The models will generate a warning under these
conditions and a tighter significance level should be used to judged the
results.  For data sizes much beyond 500, it is better to switch to the
normal or Weibull base distribution when testing flats.

Bad values passed for the draw and LP kernel sizes will raise errors.  The
filter name will default to Kaiser if the argument does not match a supported
kernel or if it is a bad value (NA, empty, or non-character).  The base
distribution similarly defaults to the logistic.  The arguments correspond
to options \opt{lp.kernel}, \opt{lp.window} or \opt{diw.window}, and
\opt{flat.distrib}.  The probabilities should be evaluated against
\opt{alpha.ht} and \opt{alpha.len} for the minimum passing level.

All four functions can take vectors as their first argument, which are
evaluated one by one for the given filter and draw set-up.
}

\value{
\code{Dipeak.test} and \code{Diflat.test} return lists of class \opt{Ditest}
with elements
\item{method}{a string describing the test}
\item{statfn}{function used to evaluate significance level/probability}
\item{statistic}{what is tested, the height of the peak or length of the flat}
\item{statname}{text string describing the statistic}
\item{parameter}{distributional arguments, for the peak the corrected height
  \code{corrht} and the \code{mu} and \code{lambda} for the inverse Gaussian;
  omitted for flats}
\item{p.value}{probability of feature}
\item{alternative}{a string describing the direction of the test vs. the
  null distribution}
\item{model}{parameters for the feature model, \code{n} the data size,
  \code{flp} the low-pass filter size as a fraction of \code{n},
  \code{filter} the low-pass kernel, and for flats \code{basedist} the
  distribution used to build the model
  \nl }

\code{statistic} and \code{p.value} will have the length of the \code{ht}
or \code{len} argument.  NA and NaN values in the first argument will
propagate to \code{p.value}, NULL produces an empty vector, and non-numeric
values an NA.  If \code{pval} is less than 0 or greater than 1 the
\code{p.value} is NaN.
}

\seealso{
 \code{\link{Dimodal}},
 \code{\link{Diopt}},
 \code{\link{find.peaks}},
 \code{\link{find.flats}}
}

\examples{
pval <- Dipeak.test(0.25*(1:16), 200, 0.15,'kaiser', lower.tail=FALSE)
pval$p.value
## Recovers pval.
Dipeak.critval(pval$p.value, 200, 0.15,'kaiser')

pval <- Diflat.test(10*(1:12), 200, 0.15,'kaiser', 'logistic', lower.tail=FALSE)
pval$p.value
Diflat.critval(pval$p.value, 200, 0.15,'kaiser', 'logistic')
}

\keyword{Dimodal}
\keyword{modality}
\keyword{low-pass spacing}
