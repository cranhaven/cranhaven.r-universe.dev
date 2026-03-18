\name{slp.gam}
\alias{slp.gam}
\alias{slp}
\title{Generate a Basis Matrix for Discrete Prolate Spheroidal (Slepian) Sequences}
\description{
  Generate the basis matrix for a particular \code{N, W} Slepian sequence
  family member, with the additional property that the smoother passes constants
  without distortion. Can be quite slow execution due to the latter property.

  Based on \code{\link[splines]{ns}} for implementation with \code{\link[gam]{gam}}.

  Parallel implementation for \code{mgcv} included in package as
  \code{\link{slp.mgcv}}.
}
\usage{
slp(x, W = NA, K = NA, deltat = 1, naive = FALSE, intercept = FALSE, 
    customSVD = TRUE, forceC = FALSE, returnS = FALSE)
}
\arguments{
  \item{x}{the predictor variable.  Missing values are allowed. Assumed to be contiguous;
      if not, then converted to a contiguous series to determine appropriate \code{N},
      \code{K} and \code{W}, then the basis vectors are back-converted at the termination
      of the routine. Should be in units of \code{deltat}. }
  \item{W}{the time bandwidth. Computed as the frequency domain analogue of the maximum period of
      interest for a time series-regression problem using ``smooth functions of time''. For example,
      a period choice of 2 months converts to 60 days and \code{W = 1/60} cycles per day. Alternatively,
      if the interest is in a period of \code{7} cycles per year, then \code{W = 7 / 365.2425 = 0.0192}
      cycles per day.}
  \item{K}{the number of basis vectors requested. If not provided, then \code{W} must be, and
      \code{K} is set to approximately \code{floor(2 * N * W - 1)}. This parameter is approximately equivalent
      to \code{df} for \code{\link[splines]{ns}} with fixed dimension. Note: if you specify \code{K} higher
      than \code{2 * N * W + 1} performance will suffer significantly. The actual number of basis vectors
      returned is \code{K-1} for the case of \code{intercept = FALSE}, and \code{K} for \code{intercept = TRUE}.}
  \item{deltat}{the time step for the input \code{x}. Restricted to \code{1} and \code{6} days
      for ease of logic checking, as these are the most traditional choices. Assumes that 
      \code{W}  is in the same units, and has no real impact beyond this, so it is trivial to 
      make \code{deltat} symbolically equal an arbitrary choice and convert \code{W} to match.}
  \item{naive}{a flag for returning the naive (default) Slepian basis vectors \code{v} (\code{TRUE}) rather than
      the mean-passing SLP2 or SLP3 variants (\code{FALSE}). }
  \item{intercept}{a flag for choosing between a SLP2 or SLP3 basis. Type-2 bases capture (absorb) 
      means of target series, while Type-3 bases ignore (pass) means. }
  \item{customSVD}{a flag for using the built-in \code{\link{svd}} (case \code{FALSE}) or a 
      modified version of \code{DGESDD} LAPACK 3.5.0. The modified version provides significant
      speed improvements as it skips a number of unnecessary steps for the particular edge
      case needed by \code{slp}.}
  \item{forceC}{a flag for forced computation of the basis vectors. Several combinations of commonly
      used \code{N, W, K} parameters have been pre-computed and included with the package. If this 
      parameter is set to \code{TRUE}, the routine will compute the basis vectors regardless of whether
      they are available in pre-computed form. See \code{\link{checkSaved}{checkSaved}} for further details. }
  \item{returnS}{a flag for returning the projection matrix \code{S} rather than the basis vectors that form the same.
      Intended to be used outside of model-fitting environments. }
}
\details{
  \code{slp} is based around the routine \code{.dpss}, which generates a family of Discrete
  Prolate Spheroidal (Slepian) Sequences. These vectors are orthonormal, have alternating
  even/odd parity, and form the optimally concentrated basis set for the subspace of 
  \code{R^N} corresponding to the bandwidth \code{W}. Full details are given
  in Slepian (1978).  These basis functions have natural boundary conditions, and lack any form of 
  knot structure. This version is returned for \code{naive = TRUE}.

  The \code{dpss} basis vectors can be adapted to provide the additional
  useful property of capturing or passing constants perfectly. That is, the smoother matrix
  \code{S} formed from the returned rectangular matrix will either reproduce constants
  at near round-off precision, i.e., \code{S \%*\% rep(1, N) = rep(1, N)}, 
  for \code{naive = FALSE} with \code{intercept = TRUE}, or will pass constants, 
  i.e., \code{S \%*\% rep(1, N) = rep(0, N)}, for \code{naive = FALSE} with \code{intercept = FALSE}. 

  The primary use is in modeling formula to directly specify a Slepian time-based smoothing
  term in a model: see the examples.

  For large \code{N} this routine can be \bold{very} slow. If you are computing models with
  large \code{N}, we highly recommend pre-computing the basis object, then using it
  in your models without recomputation. The third example below demonstrates this approach.
}
\value{
  A matrix of dimension \code{length(x) * K} or \code{length(x) * (K-1)} where 
  either \code{K} was supplied, or \code{W} was supplied and \code{K} converted. Note that the
  basis vectors are computed on a contiguous grid based on \code{x}, and then
  back-converted to the time structure of \code{x}.

  Attributes are returned that correspond to the arguments to \code{ns},
  and explicitly give \code{K}, \code{W}, etc.
}
\seealso{
  \code{\link[splines]{ns}} 
}
\references{
  Thomson, D.J (1982)
  Spectrum estimation and harmonic analysis. \emph{Proceedings of the IEEE}.
  Volume \bold{70}, number 9, pp. 1055-1096.

  Slepian, David (1978)
  Prolate Spheroidal Wave Functions, Fourier Analysis, and Uncertainty V: the Discrete Case.
  \emph{Bell System Technical Journal}. Volume \bold{57}, pp. 1371-1429.
}
\examples{
    # Examples using pkg:gam
    library("gam")
    library("slp")
    N <- 730
    W <- 14 / N
    K <- 28         # will actually use 27 df when intercept = FALSE
    x <- rnorm(n = N, sd = 1)
    y <- x + rnorm(n = N, sd = 2) + 5.0
    t <- seq(1, N)    

    # note: all three examples share identical results

    # example with in-call computation, using K (df)
    fit1 <- gam(y ~ x + slp(t, K = K, forceC = TRUE), family = gaussian) 

    # example with in-call computation, using W
    fit2 <- gam(y ~ x + slp(t, W = W, forceC = TRUE), family = gaussian)

    # example with out-of-call computation, using K
    timeBasis <- slp(t, K = K, forceC = TRUE)
    fit3 <- gam(y ~ x + timeBasis, family = gaussian)

    # the same computations can be done using pre-computed basis vectors
    # for significant speed-ups, especially for large N - see `checkSaved'
    # for more details
    fit4 <- gam(y ~ x + slp(t, W = W, forceC = FALSE))
}
\keyword{models}
\keyword{smooth}
\keyword{regression}
\keyword{Slepian}
\keyword{gam}
