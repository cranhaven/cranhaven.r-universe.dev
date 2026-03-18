%
%     Written by Karim Rahim and Wesley Burr.
%
%     This file is part of the 'slp' package for R.
%
%     If you wish to report bugs please contact the maintainer:
%     Wesley Burr
%     <wesley.burr@gmail.com>
%

\name{.dpss}
\alias{.dpss}
\alias{dpss}
\title{Compute Discrete Prolate Spheroidal Sequences}
\description{
   Compute Discrete Prolate Spheroidal (Slepian) Sequences for use as
   time-based smoother. This approach uses the tridiagonal method and exploits
   symmetry. Note the odd order tapers are normalized so that the slope at the
   centre is positive, in accordance with Slepian (1978) and Thomson (1982). This
   differs from Percival and Walden (1993). This code follows Chapter
   8.3 of Percival and Walden (1993) using LAPACK function calls,  Anderson (1999).
}
\usage{
.dpss(n,k,nw)
}
\arguments{
  \item{n}{A positive integer, typically the length of the time series-regression data set.}
  \item{k}{A positive integer, the number of basis vectors for the smoother, often 2*nw - 2. }
  \item{nw}{A positive double-precision number, the time-bandwidth parameter. The frequency domain
      analogue of the maximum period of interest.}
}
\value{
  \item{v}{A 'n' by 'k' matrix of basis vectors of class 'dpss'. Each column is the appropriate
    Slepian sequence of order 'k-1'.}
}
\references{
Anderson, E. (1999).
\emph{LAPACK Users' guide (Vol. 9).}
SIAM.

Percival, D.B. and Walden, A.T. (1993)
\emph{Spectral analysis for physical applications.}
Cambridge University Press.

Slepian, D. (1978)
Prolate spheroidal wave functions, Fourier analysis, and
uncertainty. V--The discrete case. \emph{Bell System Technical Journal}
Volume \bold{57}, pp. 1371--1430

Thomson, D.J (1982)
Spectrum estimation and harmonic analysis. \emph{Proceedings of the IEEE}
Volume \bold{70}, number 9, pp. 1055--1096.
}
\keyword{math}
