\name{TS_signature}
\alias{TS_signature}
\docType{data}
\title{
A signature constructed with a set of tumor suppressor genes
}
\description{
TS_signature was constructed with a set of 26 tumor suppressor genes screened
from microarray data GSE19804 and cohorts GSE18842, GSE40419, and GSE21933
using differential analysis and bioinformatics methods.

}
\usage{data("TS_signature")}
\format{
  A data frame with 28 observations on the following 2 variables.
  \describe{
    \item{\code{probeid}}{a character vector}
    \item{\code{gene}}{a character vector}
  }
}
\details{
TS signature has 26 tumor suppressor genes (26 probeids). These genes are
normally expressed but repressed in cancer or tumor cells. Therefore, a TS
signature also has low expression in normal tissues but high expression in
cancer cells of patients.
}

\references{
Zhang, C., Jiang, M., Zhou, N. et al. Use tumor suppressor genes as biomarkers
for diagnosis of non-small cell lung cancer. Sci Rep 11, 3596 (2021).
https://doi.org/10.1038/s41598-020-80735-x.
}
\examples{
data(ST_signature)
}
\keyword{datasets}
