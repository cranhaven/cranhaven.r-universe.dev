\name{TCGA_forestplt}
\alias{TCGA_forestplt}
\docType{data}
\title{
Data for forestplot
}
\description{
The data are derived from results of performing multivariate Cox proportional
hazard survival analysis on patients with ADC in TCGA lung cancer.
}
\usage{data("TCGA_forestplt")}
\format{
  A data frame with 28 observations on the following 5 variables.
  \describe{
    \item{\code{stage}}{a character vector}
    \item{\code{variate}}{a character vector}
    \item{\code{beta}}{a numeric vector}
    \item{\code{se}}{a numeric vector}
    \item{\code{p_value}}{a numeric vector}
  }
}
\details{
The dataset was constructed with 28 observations for up and down signatures,
sex and age in stages 1A, 1B, stage2 and stage3 and 5 variables stage, variate,
beta, se and p_value.
}
\examples{
data(TCGA_forestplt)

}
\keyword{datasets}
