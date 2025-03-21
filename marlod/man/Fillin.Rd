\name{Fillin}
\alias{Fillin}
\title{Fill-in or Substitution Methods}
\description{Uses substitution methods, including single and multiple value imputation techniques, such that any measurements less than the limit of detection (LOD).}
\usage{Fillin(y, lod, substitue)}
\arguments{
  \item{y}{A list of numeric values or a vector of the observed values.}
  \item{lod}{A numeric value of limit of detection (LOD).}
  \item{substitue}{A character string specifying the substitution approach, including "None", "LOD", "LOD2", "LODS2", "BetaMean", "BetaGM", and "QQplot".}
}
\details{Single value imputation techniques, such as LOD/2 or LOD/\eqn{\sqrt2} ("LOD2" or "LODS2") (Hornung and Reed, 1990; Burstyn and Teschke, 1999), and \eqn{\beta}-substitution method ("BetaMean" and "BetaGM") (Ganser and Hewett, 2010), are used to assign a value to a range between 0 and the LOD. "QQplot" represents the multiple order value imputation technique that depicts the natural logarithm of the uncensored or detected observed values versus the Z-scores and fits a linear regression presented in a quantile-quantile (QQ) plot (Pleil, 2016).}
\value{A list of numeric values or a vector with imputed values that are assigned to non-detects.}

\references{Burstyn, I., Teschke, K. (1999). Studying the determinants of exposure: a review of methods. \emph{American Industrial Hygiene Association Journal}, 60, 57--72.

Ganser, G. H., Hewett, P. (2010). An accurate substitution method for analyzing censored data. \emph{Journal of Occupational and Environmental Hygiene}, 7, 233--44.

Hornung, R. W., Reed, L. D. (1990). Estimation of average concentration in the presence of nondetectable values. \emph{Applied Occupational and Environmental Hygiene}, 5, 46--51.

Pleil, J. D. (2016). QQ-plots for assessing distributions of biomarker measurements and generating defensible summary statistics. \emph{Journal of Breath Research}, 10, 035001.
}
\author{I-Chen Chen}

\examples{
## Uses an example from Ganser and Hewett (2010).
library(marlod)

y <- c(0,0,0,3.06,4.41,7.23,8.29,9.52,19.94,20.25) #LOD=3
lod <- 3

\donttest{Fillin(y, lod, "None")}

\donttest{Fillin(y, lod, "LOD")}

\donttest{Fillin(y, lod, "LOD2")}

\donttest{Fillin(y, lod, "LODS2")}

Fillin(y, lod, "BetaMean")

\donttest{Fillin(y, lod, "BetaGM")}

\donttest{Fillin(y, lod, "QQplot")}
}
