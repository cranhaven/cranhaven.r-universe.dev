\name{spikedApples}
\alias{spikedApples}
\docType{data}
\title{Metabolomics data on spiked apples}
\description{An data set of LC-MS features, obtained from twenty
  apples. The last ten apples are spiked with known compounds. This set
  provides a test case for biomarker selection methods: the task is to
  retrieve the true biomarker variables. The raw LC-MS data have been
  converted to CDF format and processed with XCMS to obtain the features.
}
\usage{data(spikedApples)}
\format{
  The format is a list of four elements:
  \describe{
    \item{mz}{the m/z values of the features (rounded)}
    \item{rt}{the retention times of the features}
    \item{dataMatrix}{the intensities of the features in the individual samples}
    \item{biom}{the indices of the "true" biomarkers}
  }
}
\author{Pietro Franceschi}
\references{
  P. Franceschi, D. Masuero, U. Vrhovsek, F. Mattivi and R. Wehrens: A
  benchmark spike-in data set for biomarker identification in
  metabolomics. J. Chemom. 26, 16-24 (2012)

  R. Wehrens, P. Franceschi, U. Vrhovsek and F. Mattivi. Stability-based
  biomarker selection. Analytica Chimica Acta (2011), 705, 15-23.
  http://dx.doi.org/10.1016/j.aca.2011.01.039.
}
\examples{
data(spikedApples)
## show features identified in all apples
plot(spikedApples$rt, spikedApples$mz,
     xlab = "Retention time (s)", ylab = "m/z",
     main = "Spiked apples - subset")
}
\keyword{datasets}
