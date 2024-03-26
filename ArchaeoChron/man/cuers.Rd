\name{cuers}
\alias{cuers}
\docType{data}
\title{Dating the Last Firing of the Medieval or Modern Lime Kiln of Cuers (Provence, France)}
\description{
Radiocarbon dates and errors associated with the last firing of the kiln. These dates are measured on two charcaol found in the kiln and assumed to have burnt during the last firing. 
}
\usage{data("cuers")}
\format{
  A data frame with 2 observations on the following 3 variables.
  \describe{
    \item{\code{SampleName}}{ name of the charcoal}
    \item{\code{Age}}{a numeric vector corresponding to the radiocarbon measurement made on each charcaol}
    \item{\code{Error}}{a numeric vector corresponding to the error on the radiocarbon measurement made on each charcaol}
  }
}
\details{
The last firing date of lime kiln of Cuers (Provence, France), Pas-Redon site (Vaschalde et al. (2014)), has been determined using walls baked clay (AM dating) and charcoals (14C dating). Here the dating is only based on the 2 radiocarbon datings (Poz-42876 and Ly-16086) only.}
\source{
Vaschalde, C., Herve, G., Lanos, P., and Thiriot, J. (2014). La datation des structures de cuisson: integration de l'archeomagnetisme et du radiocarbone, apports de l'anthrazcologie. Archeologie Medievale, 44:1-16.
P. Lanos and A. Philippe. Hierarchical Bayesian Modeling for Combining Dates in Archaeological Context. Journal de la SFdS, Vol. 158 (2) pp 72-88 2017.
}
\examples{
data(cuers)
}
\keyword{datasets}
