\name{interpol.area}
\alias{interpol.area}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Interpolate missing intensities and calculate the area for a single EIC. 
}
\description{
	This is an internal function that's not supposed to be called directly by the user.
}
\usage{
interpol.area(x, y, all.x, all.w)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the positions of x(retention time) where non-NA y is observed.}
  \item{y}{the observed intensities.}
  \item{all.x}{all possible x(retention time) in the LCMS profile.}
  \item{all.w}{the "footprint" of each measured retention time, used as weight for the corresponding y.}
}
\details{
	This is an internal function. It interpolates missing y using linear interpolation, and then calculates the area under the curve.
}
\value{
	The area is returned.
}
\author{
	Tianwei Yu <tyu8@emory.edu>
}
\keyword{ models }