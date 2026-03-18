\name{hierarch.bs}
\alias{hierarch.bs}
\title{Construction of the hierarchical B-spline density basis.}

\description{Construction of the hierarchical B-spline density basis.}

\usage{
hierarch.bs(x, d, plot.bsp, typ, penden.env, int=FALSE)
}

\arguments{
  \item{x}{Marginal data for construction.}
  \item{d}{Hierarchy level of the marginal hierarchical B-spline density.}
  \item{plot.bsp}{Default = FALSE. If TRUE, each B-spline basis is plotted.}
  \item{typ}{typ==1 without open B-splines at the boundary
    typ==2 with open B-splines at the boundary.}
  \item{penden.env}{Containing all information, environment of pencopula().}
  \item{int}{Default = FALSE. If TRUE, the integral of the hierarchical
    B-spline density basis is calculated (used for the distribution
    function of the estimation).}
}

\details{
  First, the marginal hierarchical B-spline density basis is constructed for each
  covariate 'B.tilde'.

  After the construction of each marginal basis, the
  hierarchical B-spline density basis is built in the main program
  pencopula(), using an object  'Index.basis.D' (saved in the
  environment). 'Index.basis.D' notes which compenent of the marginal
  basis has to be selected. In the main program the object
  'tilde.Psi.d.D' is constructed. D refers to the maximum hierachy level
  and 'd' is the hierarchy level of the marginal hierarchical B-spline.
}

\value{
  \item{B.tilde}{'B.tilde' is the hierarchical B-spline density basis,
    returned by 'hierarch.bs'.}
  \item{int.B.tilde}{If 'int=TRUE', the integral of the hierachical
    B-spline density basis is calculated and returned by 'hierarch.bs'.}
}

\author{Christian Schellhase <cschellhase@wiwi.uni-bielefeld.de>}
\references{Estimating Non-Simplified Vine Copulas Using Penalized Splines, Schellhase, C. and Spanhel, F. (2017), Statistics and Computing.}
\keyword{nonparametric}

