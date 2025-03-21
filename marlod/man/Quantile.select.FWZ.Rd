\name{Quantile.select.FWZ}
\alias{Quantile.select.FWZ}
\title{Function to Select a Type of Time-Dependent Covaraite Through a Quantile Regression Model}
\description{Selects a type of time-dependent covaraite through a marginal quantile regression model for longitudinal exposure data with values less than the limit of detection (LOD).}
\usage{Quantile.select.FWZ(y, x, lod, substitue, tau, data)}
\arguments{
  \item{y}{A column matrix of the observed outcome values or responses.}
  \item{x}{A matrix of covariate values, for which the number of columns is the number of covariates.}
  \item{lod}{A numeric value of limit of detection (LOD).}
  \item{substitue}{A character string specifying the substitution approach, including "None", "LOD", "LOD2", "LODS2", "BetaMean", "BetaGM", "MIWithID", "MIWithIDRM", and "QQplot".}
  \item{tau}{A numeric value of quantile level, e.g., tau=0.25 for 25th quantile and tau=0.5 for median.}
  \item{data}{A data frame that originazes the given data into two-dimensional structure of rows and columns.}
}

\details{The function modifies the R functions provided by Dr. Liya Fu and based on the manuscript of Fu \emph{et al.} (2015). Chen \emph{et al.} (2024) further applied the Gaussian pseudolikelihood approach for quantile regression to environmental exposure and biomonitoring longitudinal data with values less than the limit of detection (LOD) and time-dependent covaraites. The work to select a working type of time-dependent covaraite is based on the manuscript of Chen and Westgate (2021).

Fill-in or substitution methods, including single and multiple value imputation techniques, were used to assign values for non-detects. See the Details of the "Fillin" function for introduction of the available substitution methods. For a multiple random value imputation technique, it provides an alternative for environmental exposure and biomonitoring data with non-detects, in which the imputed values can be generated using a regression of an exposure measurement on covariate(s) ("MIWithID" and "MIWithIDRM") (Lubin \emph{et al.}, 2004). Information of identification (ID) would be included in "MIWithID" as the covariate, e.g., "id in "simdata58", while ID and order of cluster size or time points would be treated as the covariates in "MIWithIDRM", e.g. "id" and "visit" in "simdata58".}
\value{An object of class "Quantile.select.FWZ" representing the fit.}

\references{Chen, I-C., Bertke, S. J., Dahm, M. M. (2024). Quantile regression for longitudinal data with values below the limit of detection and time-dependent covariates -- application to modeling carbon nanotube and nanofiber exposures. \emph{Annals of Work Exposures and Health}. \doi{10.1093/annweh/wxae068}

Chen, I-C., Westgate, P. M. (2021). Marginal quantile regression for longitudinal data analysis in the presence of time-dependent covariates. \emph{The International Journal of Biostatistics}, 17, 267--282.

Fu, L., Wang, Y.-G., Zhu, M. (2015). A Gaussian pseudolikelihood approach for quantile regression with repeated measurements. \emph{Computational Statistics and Data Analysis}, 84, 41--53.

Lubin, J. H., Colt, J. S., Camann, D., \emph{et al.} (2004). Epidemiologic evaluation of measurement data in the presence of detection limits. \emph{Environmental Health Perspectives}, 112, 1691--6.
}
\author{Liya Fu and I-Chen Chen}
\seealso{\code{\link{Quantile.FWZ}}.}

\examples{
## Uses the simdata58 to run the marginal models.
library(marlod)
library(MASS)
library(miWQS)
library(quantreg)

data(simdata58)

y=as.matrix(as.vector(t(simdata58$y)))
x1=as.matrix(as.vector(t(simdata58$x1)))
x=cbind(matrix(1,length(x1),1),x1)

## LOD=0.5 is equivalent to detection proportion=50.7% (censoring proportion=49.3%).
lod=0.5

## Median or 50th quantile is given.
tau=0.5

## Examples to perform the function
\donttest{Quantile.select.FWZ(y, x, lod, "BetaMean", tau, simdata58)}

\donttest{Quantile.select.FWZ(y, x, lod, "QQplot", tau, simdata58)}

\donttest{Quantile.select.FWZ(y, x, lod, "MIWithID", tau, simdata58)}
}
