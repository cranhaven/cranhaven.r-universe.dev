\name{load.lcms}
\alias{load.lcms}
\title{ Loading LC/MS data. }
\description{ This is an internal function. It loads LC/MS data into memory. }
\usage{
load.lcms(filename)
}
\arguments{
  \item{filename}{ The CDF file name. }
}
\details{ The function uses functionality provided by the mzR package from Bioconductor. }
\value{
A list is returned.
  \item{masses}{ The vector of m/z values. }
  \item{labels}{ The vector of retention times. }
  \item{intensi}{ The vector of intensity values. }
  \item{times}{ The vector of unique time points. }
}
\references{
Bioinformatics. 25(15):1930-36. 
BMC Bioinformatics. 11:559.
}
\author{ Tianwei Yu <tyu8@emory.edu> }
\keyword{ models }
