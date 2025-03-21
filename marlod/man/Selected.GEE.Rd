\name{Selected.GEE}
\alias{Selected.GEE}
\title{Function to Select a Type of Time-Dependent Covaraite Through a Generalized Estimating Equation Model}
\description{Selects a type of time-dependent covaraite through a marginal mean regression model using generalized estimating equation (GEE) estimation method for longitudinal exposure data with values less than the limit of detection (LOD).}
\usage{Selected.GEE(id, y, x, lod, substitue, corstr, maxiter)}
\arguments{
  \item{id}{A column matrix of subject IDs. The number of rows is the total number of observations. Data must be sorted by IDs.}
  \item{y}{A column matrix of the observed outcome values or responses.}
  \item{x}{A matrix of covariate values, for which the number of columns is the number of covariates.}
  \item{lod}{A numeric value of limit of detection (LOD).}
  \item{substitue}{A character string specifying the substitution approach, including "None", "LOD", "LOD2", "LODS2", "BetaMean", "BetaGM", "MIWithID", "MIWithIDRM", and "QQplot".}
  \item{corstr}{A character string specifying the working correlation structure, given by either "exchangeable" or "AR-1".}
  \item{maxiter}{The maximum number of iterations.}
}

\details{The function modifies the supplementary R function for GEE in Westgate (2014). With the marginal modeling, Chen \emph{et al.} (2024) incorporate the fill-in methods, including single and multiple value imputation techniques, such that any measurements less than the limit of detection (LOD) are assigned values. Based on the manuscripts of Chen and Westgate (2017, 2019), this function also enable to use a empirical MSE minimization criterion (EMMC) to select a working type of time-dependent covaraite.

See the Details of the "Fillin" function for introduction of the available fill-in or substitution methods. For a multiple random value imputation technique, it provides an alternative for environmental exposure and biomonitoring data with non-detects, in which the imputed values can be generated using a regression of an exposure measurement on covariate(s) ("MIWithID" and "MIWithIDRM") (Lubin \emph{et al.}, 2004). Information of identification (ID) would be included in "MIWithID" as the covariate, e.g., "id in "simdata58", while ID and order of cluster size or time points would be treated as the covariates in "MIWithIDRM", e.g. "id" and "visit" in "simdata58".}
\value{An object of class "Selected.GEE" representing the fit.}

\references{Chen, I-C., Bertke, S. J., Estill, C. F. (2024). Compare the Marginal Effects for Environmental Exposure and Biomonitoring Data with Repeated Measurements and Values Below the Limit of Detection. \emph{Journal of Exposure Science and Environmental Epidemiology}. \doi{10.1038/s41370-024-00640-7}

Chen, I-C., Westgate, P. M. (2017). Improved methods for the marginal analysis of longitudinal data in the presence of time-dependent
covariates. \emph{Statistics in Medicine}, 36, 2533--46.

Chen, I-C., Westgate, P. M. (2019). A novel approach to selecting classification types for time-dependent covariates in the marginal
analysis of longitudinal data. \emph{Statistical Methods in Medical Research}, 28, 3176--86.

Lubin, J. H., Colt, J. S., Camann, D., \emph{et al.} (2004). Epidemiologic evaluation of measurement data in the presence of detection limits. \emph{Environmental Health Perspectives}, 112, 1691--6.

Westgate, P. M. (2014). Criterion for the simultaneous selection of a working correlation structure and either generalized estimating equations or the quadratic inference function approach. \emph{Biometrical Journal}, 56, 461--476.
}
\note{The function is capable of analyzing one measurement or more than one repeated measurements per subject. Unbalanced repeated measurements are also permittable.}
\author{Philip M. Westgate and I-Chen Chen}
\seealso{\code{\link{Modified.GEE}}, \code{\link{MGEE}}.}

\examples{
## Uses the simdata58 to run the marginal models.
library(marlod)
library(MASS)
library(miWQS)

data(simdata58)

id=as.matrix(as.vector(t(simdata58$id)))
y=as.matrix(as.vector(t(simdata58$y)))
x1=as.matrix(as.vector(t(simdata58$x1)))

## LOD=0.5 is equivalent to detection proportion=50.7% (censoring proportion=49.3%).
lod=0.5

## Intercept is not included in the "x1"
\donttest{Selected.GEE(id, y, x1, lod, "None", "exchangeable", 1000)}

\donttest{Selected.GEE(id, y, x1, lod, "LOD", "AR-1", 1000)}

\donttest{Selected.GEE(id, y, x1, lod, "LOD2", "exchangeable", 1000)}

\donttest{Selected.GEE(id, y, x1, lod, "LODS2", "AR-1", 1000)}

\donttest{Selected.GEE(id, y, x1, lod, "BetaMean", "exchangeable", 1000)}

\donttest{Selected.GEE(id, y, x1, lod, "BetaGM", "AR-1", 1000)}

Selected.GEE(id, y, x1, lod, "MIWithID", "exchangeable", 1000)

Selected.GEE(id, y, x1, lod, "MIWithIDRM", "AR-1", 1000)

Selected.GEE(id, y, x1, lod, "QQplot", "exchangeable", 1000)
}
