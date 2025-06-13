\name{ttScreening-package}
\alias{ttScreening-package}
\docType{package}
\title{
Genome-Wide DNA Methylation Sites Screening by Use of Training and Testing Samples
}
\description{
 A screening process to filter out non-informative DNA methylation sites by applying (ordinary or robust) linear regressions to training data, and the results are further examined using testing samples. Surrogate variables are included to account for unknown factors.
}
\details{
\tabular{ll}{
Package: \tab ttScreening\cr
Type: \tab Package\cr
Version: \tab 1.6\cr
Date: \tab 2018-09-18\cr
License: \tab Artistic-2.0\cr
}
This package utilizes training and testing samples to filter out uninformative DNA methylation
sites. Surrogate variables (SVs) of DNA methylation are included in the filtering process
to explain unknown factor effects.
}
\author{
Meredith Ray, Xin Tong, Hongmei Zhang

Maintainer: Meredith Ray <maray@memphis.edu>
}
\references{
Ray MA, Tong X, Lockett GA, Zhang H, and Karmaus WJJ. (2016) ``An Efficient Approach to Screening Epigenome-Wide Data'', BioMed Research International.

Leek JT and Storey JD. (2007) ``Capturing heterogeneity in gene expression studies by `Surrogate Variable Analysis'.'' PLoS Genetics, 3: e161.
}


\keyword{ ttScreening }
\seealso{
\code{\link{sva}}
}
