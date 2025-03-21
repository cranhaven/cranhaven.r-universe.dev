\name{Modified.QIF}
\alias{Modified.QIF}
\title{Function of a Quadratic Inference Function (QIF) Model}
\description{Runs a marginal mean regression model using quadratic inference function (QIF) estimation method for repeated measures data with values less than the limit of detection (LOD).}
\usage{Modified.QIF(id, y, x, lod, substitue, corstr, beta, typetd, maxiter)}
\arguments{
  \item{id}{A column matrix of subject IDs. The number of rows is the total number of observations. Data must be sorted by IDs.}
  \item{y}{A column matrix of the observed outcome values or responses.}
  \item{x}{A matrix of covariate values, for which the number of columns is the number of covariates.}
  \item{lod}{A numeric value of limit of detection (LOD).}
  \item{substitue}{A character string specifying the substitution approach, including "None", "LOD", "LOD2", "LODS2", "BetaMean", "BetaGM", "MIWithID", "MIWithIDRM", and "QQplot".}
  \item{corstr}{A character string specifying the working correlation structure, given by either "exchangeable" or "AR-1".}
  \item{beta}{A matrix of initial parameter estimates, e.g., these estimates could be from general linear model or generalized estimating equation (GEE) using independence working structure.}
  \item{typetd}{An atomic vector specifies the types of time-dependent covaraites, with the length of the vector equal to the number of regression parameters, excluding the intercept. For time-independent covariates or those in a cluster study, "1" is assigned.}
  \item{maxiter}{The maximum number of iterations.}
}

\details{The function modifies the supplementary R function for GEE in Westgate (2014a), in whcih small-sample standard error corrections are applied (Kauermann and Carroll, 2001; Mancl and DeRouen, 2001; Westgate and Braun, 2012; Westgate, 2012, 2014b). With the marginal modeling, Chen \emph{et al.} (2024) incorporate the fill-in methods, including single and multiple value imputation techniques, such that any measurements less than the limit of detection (LOD) are assigned values. This function also presents the results of the "trace of the empirical covariance matrix" (TECM) (Westgate, 2014c) and the "correlation information criterion" (CIC) (Hin and Wang, 2009). Both criteria have been shown to be preferable to other criteria in choosing an analysis method and corresponding structure (Westgate, 2014a).

See the Details of the "Fillin" function for introduction of the available fill-in or substitution methods. For a multiple random value imputation technique, it provides an alternative for environmental exposure and biomonitoring data with non-detects, in which the imputed values can be generated using a regression of an exposure measurement on covariate(s) ("MIWithID" and "MIWithIDRM") (Lubin \emph{et al.}, 2004). Information of identification (ID) would be included in "MIWithID" as the covariate, e.g., "id in "simdata15", while ID and order of cluster size or time points would be treated as the covariates in "MIWithIDRM", e.g. "id" and "visit" in "simdata15".}
\value{An object of class "Modified.QIF" representing the fit.}

\references{Chen, I-C., Bertke, S. J., Estill, C. F. (2024). Compare the Marginal Effects for Environmental Exposure and Biomonitoring Data with Repeated Measurements and Values Below the Limit of Detection. \emph{Journal of Exposure Science and Environmental Epidemiology}. \doi{10.1038/s41370-024-00640-7}

Kauermann, G., Carroll, R. J. (2001). A note on the efficiency of sandwich covariance matrix
estimation. \emph{Journal of the American Statistical Association}, 96, 1387--96.

Lubin, J. H., Colt, J. S., Camann, D., \emph{et al.} (2004). Epidemiologic evaluation of measurement data in the presence of detection limits. \emph{Environmental Health Perspectives}, 112, 1691--6.

Mancl, L. A., DeRouen, T. A. (2001). A covariance estimator for GEE with improved small-sample properties. \emph{Biometrics}, 57, 126--134.

Westgate, P. M., Braun, T. M. (2012). The effect of cluster size imbalance and covariates on the estimation performance of quadratic inference functions. \emph{Statistics in Medicine}, 31, 2209--2222.

Westgate, P. M. (2012). A bias-corrected covariance estimate for improved inference with quadratic inference functions. \emph{Statistics in Medicine}, 31, 4003--4022.

Westgate, P. M. (2014a). Criterion for the simultaneous selection of a working correlation structure and either generalized estimating equations or the quadratic inference function approach. \emph{Biometrical Journal}, 56, 461--476.

Westgate, P. M. (2014b). A comparison of utilized and theoretical covariance weighting matrices on the estimation performance of quadratic inference functions. \emph{Communications in Statistics -- Simulation and Computation}, 43, 2432--2443.

Westgate, P. M. (2014c). Improving the correlation structure selection approach for generalized estimating equations and balanced longitudinal data. \emph{Statistics in Medicine}, 33, 2222--2237.
}
\note{The function is capable of analyzing one measurement or more than one repeated measurements per subject. Unbalanced repeated measurements are also permittable.}
\author{Philip M. Westgate and I-Chen Chen}
\seealso{\code{\link{Selected.QIF}}.}

\examples{
## Uses the simdata15 to run the marginal models.
library(marlod)
library(MASS)
library(miWQS)

data(simdata15)

id=as.matrix(as.vector(t(simdata15$id)))
y=as.matrix(as.vector(t(simdata15$y)))
x1=as.matrix(as.vector(t(simdata15$x1)))
x2=as.matrix(as.vector(t(simdata15$x2)))
x=cbind(x1,x2)

## LOD=2 is equivalent to detection proportion=56.3% (censoring proportion=43.7%).
lod=2

## Gets initial estimates for the QIF approach through independence structure
initial=glm(y ~ x1 + x2, data=simdata15, family=gaussian)
beta_initial=as.matrix(initial$coefficients)

## Intercept is not included in the "x"
Modified.QIF(id, y, x, lod, "None", "exchangeable", beta_initial, c(1,1), 1000)

Modified.QIF(id, y, x, lod, "LOD", "AR-1", beta_initial, c(1,1), 1000)

Modified.QIF(id, y, x, lod, "LOD2", "exchangeable", beta_initial, c(1,1), 1000)

Modified.QIF(id, y, x, lod, "LODS2", "AR-1", beta_initial, c(1,1), 1000)

Modified.QIF(id, y, x, lod, "BetaMean", "exchangeable", beta_initial, c(1,1), 1000)

Modified.QIF(id, y, x, lod, "BetaGM", "AR-1", beta_initial, c(1,1), 1000)

Modified.QIF(id, y, x, lod, "MIWithID", "exchangeable", beta_initial, c(1,1), 1000)

Modified.QIF(id, y, x, lod, "MIWithIDRM", "AR-1", beta_initial, c(1,1), 1000)

Modified.QIF(id, y, x, lod, "QQplot", "exchangeable", beta_initial, c(1,1), 1000)
}
