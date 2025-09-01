\name{present.cdf.3d}
\alias{present.cdf.3d}
\title{ Generates 3 dimensional plots for LCMS data. }
\description{
  This function takes the matrix output from proc.cdf() and generates a 3D plot of the data. It relies on the rgl library. 
}
\usage{
present.cdf.3d(prof, fill.holes = TRUE, transform = "none", time.lim = NA, 
mz.lim = NA, box = TRUE, axes = TRUE)
}
\arguments{
  \item{prof}{ The matrix output from the proc.cdf() function. }
  \item{fill.holes}{ A lot of peaks have missing values at some time points. If fill.holes is TRUE, the function will fill in the missing values by interpolation.}
    \item{transform}{ If the value is "sqrt", the values are transformed by taking square root. If "cuberoot", the values are transformed by taking cubic root.  }
  \item{time.lim}{ The limit in retention time for the area of spectrum to be plotted. It should be either NA or a vector of two values: the lower limit and the upper limit. }
  \item{mz.lim}{ The limit in m/z value for the area of spectrum to be plotted.  It should be either NA or a vector of two values: the lower limit and the upper limit.}
  \item{box}{ If a box should be drawn.}
  \item{axes}{ If the axes should be drawn.}
}
\details{
  The function calls the rgl library. Spectrum values within the time.lim and mz.lim range is plotted in 3D. 
}
\value{
There is no return value from this function.
}
\references{ http://rgl.neoscientists.org/about.shtml }
\author{ Tianwei Yu <tyu8@emory.edu> }
\examples{
data(prof)
present.cdf.3d(prof[[2]],time.lim=c(250,400), mz.lim=c(400,500))
}
