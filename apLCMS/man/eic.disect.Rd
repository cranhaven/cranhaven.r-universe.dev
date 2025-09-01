\name{eic.disect}
\alias{eic.disect}
\title{ Internal function: Extract data feature from EIC. }
\description{ The function extracts data features after applying different smoother settings. }
\usage{
eic.disect(raw.prof, smoother.window = c(1, 5, 10))
}
\arguments{
  \item{raw.prof}{ The data after adaptive binning, i.e. the output from adaptive.bin.2(). }
  \item{smoother.window}{ The smoother window sizes to use for data feature extraction. }
}
\details{ We take a number of data characteristic measurements from each EIC, including m/z span, m/z standard deviation, retention time (RT) span, RT peak location, and summary statistics on the raw intensity values of the EIC. We also centroid the data in each EIC such that it becomes two-dimensional data (intensity v.s. RT). We then apply different smoothers (shape/window size) in combination of different weighting schemes (unweighted, weighted with intensity, weighted with log intensity) to each EIC. At each smoothing setting, we record summary statistics of smoothed data.  }
\value{ A matrix. Every row corresponds to an EIC. Every column corresponds to a data feature. }
\references{Bioinformatics. 30(20):  2941-2948.}
\author{ Tianwei Yu <tyu8@emory.edu> }
\keyword{ models }