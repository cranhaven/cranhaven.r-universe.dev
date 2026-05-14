\name{hvwi}
\alias{hvwi}
\docType{data}
\title{Hepatic vein waveform index data}
\description{
This data collected by Pedersen et al. (2008) was taken from Terpstra et al. (2011). The data are calculated from doppler waveforms corresponding to 66 patients who were scheduled for a percutaneous liver needle biopsy.}
\usage{data("hvwi")}
\format{
  A data frame with 66 observations on the following 2 variables.
  \describe{
    \item{\code{Group}}{Fibrosis groups: \code{A}:cirrhosis, \code{B}:severe, \code{C}:moderate, \code{D}:mild and \code{E}:none}
    \item{\code{Values}}{HVWI values for the five fibrosis groups}
  }
}

\source{
Terpstra, J. T., Chang, C. H. and Magel, C. M. (2011). On the use of Sperman's correlation coefficient for testing ordered alternatives. \emph{Journal of Statistical Computation and Simulation}, \bold{81:11}, 1381-1392.
}
\references{
Pedersen, J. F., Madsen, L. G., Vibeke, A. L. Hamberg, O., Horn, T., Federspiel, B. and Bytzer, P. (2008). A doppler waveform index to characterize hepatic vein velocity pattern and evaluate hepatic fibrosis. \emph{J. Clin. Ultrasound}, \bold{36:4}, 208-211.
}
\examples{
data(hvwi)
levels(hvwi$Group) <- c("cirrhosis", "severe", "moderate", "mild", "none")
boxplot(Values~Group,data=hvwi,xlab="Fibrosis group",ylab="HVWI value")
}
\keyword{datasets}
