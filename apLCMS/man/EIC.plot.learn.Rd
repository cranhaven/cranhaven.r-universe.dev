\name{EIC.plot.learn}
\alias{EIC.plot.learn}
\title{ Plot extracted ion chromatograms based on the machine learning method output }
\description{
  Given an output object from the function semi.sup.learn(), this function plots the EICs selected by the user. 
}
\usage{
EIC.plot.learn(aligned, rows = NA, colors = NA, transform = "none",
                 subset = NA, tol = 2.5e-05, ridge.smoother.window =
                 50, baseline.correct = 0, max.spline.time.points =
                 1000)
}
\arguments{
  \item{aligned}{ An output object from cdf.to.ftr(). }
  \item{rows}{ A numeric vector selecting which rows of the aligned feature table to be plotted. }
  \item{colors}{ The colors (one per profile) the user wishes to use for the plots. The default is NA, in which case a default color set is used. }
  \item{transform}{ There are four possible values. "none": the original intensity data is plotted; "log": the intensity data is transformed by log(x+1); "sqrt": the intensity data is square root transformed; "cuberoot": the intensity data is cube root transformed. }
  \item{subset}{ The user can choose a subset of the profiles for which the EICs are plotted. It is given as a vector of profile indecies. The default is NA, in which case the EICs from all the profiles are plotted. }
  \item{tol}{ The mz tolerance level used in learn.cdf(). }
  \item{ridge.smoother.window}{ The ridge.smoother.window parameter value used in learn.cdf(). }
  \item{baseline.correct}{ The baseline.correct parameter value used in learn.cdf(). }
  \item{max.spline.time.points}{ The maximum number of points to use in the spline fit along the retention time axis. }
}
\details{ The function plots a single EIC. It plots intensity against retention time. It uses different color for different profiles. }
\value{ There is no return value. }
\references{
Bioinformatics. 25(15):1930-36. 
BMC Bioinformatics. 11:559.
}
\author{ Tianwei Yu <tyu8@emory.edu> }
\keyword{ models }
