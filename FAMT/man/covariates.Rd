\name{covariates}
\alias{covariates}
\docType{data}
\title{Covariates data frame
}
\description{A data frame with 6 covariates in columns and 43 arrays in rows, describing the arrays of the expression dataset. See also \code{\link{expression}}, \code{\link{annotations}}
}
\usage{data(covariates)}
\format{
  A data frame with 43 observations on the following 6 variables.
  \describe{
    \item{\code{AfClass}}{a factor with levels \code{F} (Fat) \code{L} (Lean) \code{NC} (Intermediate) giving the abdominal fatness class}
    \item{\code{ArrayName}}{Identifying the arrays (character)}
    \item{\code{Mere}}{a factor with 8 levels giving the dam of the offsprings}
    \item{\code{Lot}}{a factor with 4 levels giving the hatch}
    \item{\code{Pds9s}}{a numeric vector giving the body weight}
    \item{\code{Af}}{a numeric vector giving the abdominal fatness, the experimental condition of main interest in this example}
  }
}
%\details{}


\source{UMR Genetique Animale - INRA/AGROCAMPUS OUEST - Rennes, France.}
\references{Blum Y., Le Mignon G., Lagarrigue S. and Causeur S. (2010) - A factor model to analyze heterogeneity in gene expression, BMC Bioinformatics, 11:368.

Le Mignon, G. and Desert, C. and Pitel, F. and Leroux, S. and Demeure, O. and Guernec, G. and Abasht, B. and Douaire, M. and Le Roy, P. and Lagarrigue S. (2009) - Using trancriptome profling to characterize QTL regions on chicken chromosome 5. BMC Genomics, 10:575.}
\examples{
data(covariates)
dim(covariates) 
summary(covariates)
}
\keyword{datasets}
