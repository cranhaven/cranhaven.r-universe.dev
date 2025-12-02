\name{plottarget3d}
\alias{plottarget3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{3D plot of a two-dimensional  MCMC target, or any function}

\description{Utility function for the package \pkg{EntropyMCMC}, to visualize a 
2-dimensional target of a MCMC algorithm, mostly for testing purpose.
This uses the function \code{persp} from package \pkg{graphics}.}
\usage{
plottarget3d(zft, l, r, ms, theta, phi, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{zft}{a function, typically a 2-dimensional target of a MCMC.}
  \item{l, r, ms}{mesh boundaries and size.}

  \item{theta, phi}{angles defining the viewing direction. \cite{theta} 
  gives the azimuthal direction and \cite{phi} the colatitude.}
  \item{...}{additional graphical parameters.}
}

\value{Returns a 3D plot on a mesh of size \code{(l, r, ms)}.}

\author{
Didier Chauveau.}


%% ~Make other sections like Warning with \section{Warning }{....} ~

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{file} % use one of  RShowDoc("KEYWORDS")
