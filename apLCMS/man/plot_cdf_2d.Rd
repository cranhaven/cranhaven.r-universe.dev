\name{plot_cdf_2d}
\alias{plot_cdf_2d}
\title{ Plot the data in the m/z and retention time plane. }
\description{ This is a diagnostic function. It takes the original CDF file, as well as the detected feature table, and plots the data in the m/z - retention time plane, using a user-defined range. The entire data is too big to plot, thus the main purpose is to focus on small subregions of the data and check the peak detection results. }
\usage{
plot_cdf_2d(rawname, f, mzlim, timelim, lwd = 1)
}
\arguments{
  \item{rawname}{ The CDF file name. }
  \item{f}{ The output object of prof.to.feature(). }
  \item{mzlim}{ The m/z range to plot. }
  \item{timelim}{ The retention time range to plot. }
  \item{lwd}{ Line width parameter, to be passed on to the function line(). }
}
\value{ There is no return value. }
\references{
Bioinformatics. 25(15):1930-36. 
BMC Bioinformatics. 11:559.
}
\author{ Tianwei Yu <tyu8@emory.edu> }
\keyword{ models }