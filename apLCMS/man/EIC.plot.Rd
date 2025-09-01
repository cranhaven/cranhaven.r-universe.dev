\name{EIC.plot}
\alias{EIC.plot}
\title{ Plot extracted ion chromatograms }
\description{
  Given an output object from the function cdf.to.ftr(), this function plots the EICs selected by the user. 
}
\usage{
EIC.plot(aligned, rows = NA, colors = NA, transform = "none",
                 subset = NA, min.run, min.pres, max.spline.time.points
                 = 1000)
}
\arguments{
  \item{aligned}{ An output object from cdf.to.ftr(). }
  \item{rows}{ A numeric vector selecting which rows of the aligned feature table to be plotted. }
  \item{colors}{ The colors (one per profile) the user wishes to use for the plots. The default is NA, in which case a default color set is used. }
  \item{transform}{ There are four possible values. "none": the original intensity data is plotted; "log": the intensity data is transformed by log(x+1); "sqrt": the intensity data is square root transformed; "cuberoot": the intensity data is cube root transformed. }
  \item{subset}{ The user can choose a subset of the profiles for which the EICs are plotted. It is given as a vector of profile indecies. The default is NA, in which case the EICs from all the profiles are plotted. }
  \item{min.run}{ The min.run parameter used in the proc.cdf() step. }
  \item{min.pres}{ The min.pres parameter used in the proc.cdf() step. }
  \item{max.spline.time.points}{ The maximum time points to use in spline fit. }
}
\details{
  The EICs are plotted as overlaid line plots. 
  The graphic device is divided into four parts, each of which is used to plot one EIC. When all four parts are occupied, the function calls x11() to open another graphic device.
  The colors used (one per profile) is printed in the command window.
}
\value{
  There is no return value.
}
\references{
Bioinformatics. 25(15):1930-36. 
BMC Bioinformatics. 11:559.
}
\author{ Tianwei Yu <tyu8@emory.edu> }
\keyword{ models }