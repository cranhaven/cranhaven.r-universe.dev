\name{plot_txt_2d}
\alias{plot_txt_2d}
\title{ Plot the data in the m/z and retention time plane. }
\description{ This is a diagnostic function. It takes the original text file, as well as the detected feature table, and plots the data in the m/z - retention time plane, using a user-defined range. The entire data is too big to plot, thus the main purpose is to focus on small subregions of the data and check the peak detection results. }
\usage{
plot_txt_2d(rawname, f, mzlim, timelim, lwd = 1)
}
\arguments{
  \item{rawname}{ The text file name. }
  \item{f}{ The output object of prof.to.feature(). }
  \item{mzlim}{ The m/z range to plot. }
  \item{timelim}{ The retention time range to plot. }
  \item{lwd}{ Line width parameter, to be passed on to the function line(). }
}
\details{  
  The columns in the text file need to be separated by tab. The first column needs to be the retention time, the second column the m/z values, and the third column the intensity values. The first row needs to be the column labels, rather than values. 
}
\value{ There is no return value. }
\references{
Bioinformatics. 25(15):1930-36. 
BMC Bioinformatics. 11:559.
}
\author{ Tianwei Yu <tyu8@emory.edu> }
\keyword{ models }