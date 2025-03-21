\name{Modified.GEE}
\alias{Modified.GEE}
\title{Function of a Generalized Estimating Equation (GEE) Model}
\description{Runs a marginal mean regression model using generalized estimating equation (GEE) estimation method for repeated measures data with values less than the limit of detection (LOD).}
\usage{Modified.GEE(id, y, x, lod, substitue, corstr, typetd, maxiter)}
\arguments{
  \item{id}{A column matrix of subject IDs. The number of rows is the total number of observations. Data must be sorted by IDs.}
  \item{y}{A column matrix of the observed outcome values or responses.}
  \item{x}{A matrix of covariate values, for which the number of columns is the number of covariates.}
  \item{lod}{A numeric value of limit of detection (LOD).}
  \item{substitue}{A character string specifying the substitution approach, including "None", "LOD", "LOD2", "LODS2", "BetaMean", "BetaGM", "MIWithID", "MIWithIDRM", and "QQplot".}
  \item{corstr}{A character string specifying the working correlation structure, given by either "exchangeable" or "AR-1".}
  \item{typetd}{An atomic vector specifies the types of time-dependent covaraites, with the length of the vector equal to the number of regression parameters, excluding the intercept. For time-independent covariates or those in a cluster study, "1" is assigned.}
  \item{maxiter}{The maximum number of iterations.}
}

\details{The function modifies the supplementary R function for GEE in Westgate (2014a), in whcih small-sample standard error corrections are applied (Kauermann and Carroll, 2001; Mancl and DeRouen, 2001; Westgate, 2013). More discussions about the use of covariance corrections can be found in Westgate (2016), and Ford and Westgate (2017, 2018). With the marginal modeling, Chen \emph{et al.} (2024) incorporate the fill-in methods, including single and multiple value imputation techniques, such that any measurements less than the limit of detection (LOD) are assigned values. This function also presents the results of the "trace of the empirical covariance matrix" (TECM) (Westgate, 2014b) and the "correlation information criterion" (CIC) (Hin and Wang, 2009). Both criteria have been shown to be preferable to other criteria in choosing an analysis method and corresponding structure (Westgate, 2014a).

See the Details of the "Fillin" function for introduction of the available fill-in or substitution methods. For a multiple random value imputation technique, it provides an alternative for environmental exposure and biomonitoring data with non-detects, in which the imputed values can be generated using a regression of an exposure measurement on covariate(s) ("MIWithID" and "MIWithIDRM") (Lubin \emph{et al.}, 2004). Information of identification (ID) would be included in "MIWithID" as the covariate, e.g., "id in "simdata15", while ID and order of cluster size or time points would be treated as the covariates in "MIWithIDRM", e.g. "id" and "visit" in "simdata15".}
\value{An object of class "Modified.GEE" representing the fit.}

\references{Chen, I-C., Bertke, S. J., Estill, C. F. (2024). Compare the Marginal Effects for Environmental Exposure and Biomonitoring Data with Repeated Measurements and Values Below the Limit of Detection. \emph{Journal of Exposure Science and Environmental Epidemiology}. \doi{10.1038/s41370-024-00640-7}

Ford, W. P., Westgate, P. M. (2017). Improved standard error estimator for maintaining the validity of inference in cluster randomized trials with a small number of clusters. \emph{Biometrical Journal}, 59, 478--95.

Ford, W. P., Westgate, P. M. (2018). A comparison of bias-corrected empirical covariance
estimators with generalized estimating equations in small-sample longitudinal study settings. \emph{Statistics in Medicine}, 37, 4318--29.

Hin, L. Y., Wang, Y.-G. (2009). Working-correlation-structure identification in generalized estimating equations. \emph{Statistics in Medicine}, 28, 642--658.

Kauermann, G., Carroll, R. J. (2001). A note on the efficiency of sandwich covariance matrix
estimation. \emph{Journal of the American Statistical Association}, 96, 1387--96.

Lubin, J. H., Colt, J. S., Camann, D., \emph{et al.} (2004). Epidemiologic evaluation of measurement data in the presence of detection limits. \emph{Environmental Health Perspectives}, 112, 1691--6.

Mancl, L. A., DeRouen, T. A. (2001). A covariance estimator for GEE with improved small-sample properties. \emph{Biometrics}, 57, 126--134.

Westgate, P. M. (2013). A bias correction for covariance estimators to improve inference with generalized estimating equations that use an unstructured correlation matrix. \emph{Statistics in Medicine}, 32, 2850--2858.

Westgate, P. M. (2014a). Criterion for the simultaneous selection of a working correlation structure and either generalized estimating equations or the quadratic inference function approach. \emph{Biometrical Journal}, 56, 461--476.

Westgate, P. M. (2014b). Improving the correlation structure selection approach for generalized estimating equations and balanced longitudinal data. \emph{Statistics in Medicine}, 33, 2222--2237.

Westgate, P. M. (2016). A covariance correction that accounts for correlation estimation to improve finite-sample inference with generalized estimating equations: a study on its applicability with structured correlation matrices. \emph{Journal of Statistical Computation and Simulation}, 86, 1891--1900.
}
\note{The function is capable of analyzing one measurement or more than one repeated measurements per subject. Unbalanced repeated measurements are also permittable.}
\author{Philip M. Westgate and I-Chen Chen}
\seealso{\code{\link{Selected.GEE}}.}

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

## Intercept is not included in the "x"
Modified.GEE(id, y, x, lod, "None", "exchangeable", c(1,1), 1000)

Modified.GEE(id, y, x, lod, "LOD", "AR-1", c(1,1), 1000)

Modified.GEE(id, y, x, lod, "LOD2", "exchangeable", c(1,1), 1000)

Modified.GEE(id, y, x, lod, "LODS2", "AR-1", c(1,1), 1000)

Modified.GEE(id, y, x, lod, "BetaMean", "exchangeable", c(1,1), 1000)

Modified.GEE(id, y, x, lod, "BetaGM", "AR-1", c(1,1), 1000)

Modified.GEE(id, y, x, lod, "MIWithID", "exchangeable", c(1,1), 1000)

Modified.GEE(id, y, x, lod, "MIWithIDRM", "AR-1", c(1,1), 1000)

Modified.GEE(id, y, x, lod, "QQplot", "exchangeable", c(1,1), 1000)
}
