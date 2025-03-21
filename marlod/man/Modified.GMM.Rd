\name{Modified.GMM}
\alias{Modified.GMM}
\title{Function of a Generalized Method of Moments Model}
\description{Runs a marginal mean regression model using generalized method of moments (GMM) estimation method for repeated measures data with values less than the limit of detection (LOD).}
\usage{Modified.GMM(id, y, x, lod, substitue, beta, maxiter)}
\arguments{
  \item{id}{A column matrix of subject IDs. The number of rows is the total number of observations. Data must be sorted by IDs.}
  \item{y}{A column matrix of the observed outcome values or responses.}
  \item{x}{A matrix of covariate values, for which the number of columns is the number of covariates.}
  \item{lod}{A numeric value of limit of detection (LOD).}
  \item{substitue}{A character string specifying the substitution approach, including "None", "LOD", "LOD2", "LODS2", "BetaMean", "BetaGM", "MIWithID", "MIWithIDRM", and "QQplot".}
  \item{beta}{A matrix of initial parameter estimates, e.g., these estimates could be from general linear model or generalized estimating equation (GEE) using independence working structure.}
  \item{maxiter}{The maximum number of iterations.}
}

\details{The modified GMM approach was originally proposed by Chen and Westgate (2017), in whcih a linear shrinkage method of Han and Song (2011) was incorporated to resolve potential singularity problems. The method should be utilized when the Moore–Penrose generalized inverse fails to solve the weighting matrix. Small-sample standard error corrections were also applied to the modified GMM (Mancl and DeRouen, 2001; Westgate, 2012). With the marginal modeling, Chen \emph{et al.} (2024) incorporate the fill-in methods, including single and multiple value imputation techniques, such that any measurements less than the limit of detection (LOD) are assigned values. This function also presents the results of the "trace of the empirical covariance matrix" (TECM) (Westgate, 2014a) and the "correlation information criterion" (CIC) (Hin and Wang, 2009). Both criteria have been shown to be preferable to other criteria in choosing an analysis method and corresponding structure (Westgate, 2014b).

See the Details of the "Fillin" function for introduction of the available fill-in or substitution methods. For a multiple random value imputation technique, it provides an alternative for environmental exposure and biomonitoring data with non-detects, in which the imputed values can be generated using a regression of an exposure measurement on covariate(s) ("MIWithID" and "MIWithIDRM") (Lubin \emph{et al.}, 2004). Information of identification (ID) would be included in "MIWithID" as the covariate, e.g., "id in "simdata15", while ID and order of cluster size or time points would be treated as the covariates in "MIWithIDRM", e.g. "id" and "visit" in "simdata15".}
\value{An object of class "Modified.GMM" representing the fit.}

\references{Chen, I-C., Bertke, S. J., Estill, C. F. (2024). Compare the Marginal Effects for Environmental Exposure and Biomonitoring Data with Repeated Measurements and Values Below the Limit of Detection. \emph{Journal of Exposure Science and Environmental Epidemiology}. \doi{10.1038/s41370-024-00640-7}

Chen, I-C., Westgate, P. M. (2017). Improved methods for the marginal analysis of longitudinal data in the presence of timedependent covariates. \emph{Statistics in Medicine}, 36, 2533--2546.

Han, P., Song, P. X. K. (2011). A note on improving quadratic inference functions using a linear shrinkage approach. \emph{Statistics and Probability Letters}, 81, 438--445.

Lubin, J. H., Colt, J. S., Camann, D., \emph{et al.} (2004). Epidemiologic evaluation of measurement data in the presence of detection limits. \emph{Environmental Health Perspectives}, 112, 1691--6.

Mancl, L. A., DeRouen, T. A. (2001). A covariance estimator for GEE with improved small-sample properties. \emph{Biometrics}, 57, 126--134.

Westgate, P. M. (2012). A bias-corrected covariance estimate for improved inference with quadratic inference functions. \emph{Statistics in Medicine}, 31, 4003--4022.

Westgate, P. M. (2014a). Improving the correlation structure selection approach for generalized estimating equations and balanced longitudinal data. \emph{Statistics in Medicine}, 33, 2222--2237.

Westgate, P. M. (2014b). Criterion for the simultaneous selection of a working correlation structure and either generalized estimating equations or the quadratic inference function approach. \emph{Biometrical Journal}, 56, 461--476.
}
\author{I-Chen Chen}

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

## Gets initial estimates for the GMM approach through independence structure
initial=glm(y ~ x1 + x2, data=simdata15, family=gaussian)
beta_initial=as.matrix(initial$coefficients)

## Intercept is not included in the "x"
\donttest{Modified.GMM(id, y, x, lod, "None", beta_initial, 1000)}

\donttest{Modified.GMM(id, y, x, lod, "LOD", beta_initial, 1000)}

\donttest{Modified.GMM(id, y, x, lod, "LOD2", beta_initial, 1000)}

\donttest{Modified.GMM(id, y, x, lod, "LODS2", beta_initial, 1000)}

\donttest{Modified.GMM(id, y, x, lod, "BetaMean", beta_initial, 1000)}

\donttest{Modified.GMM(id, y, x, lod, "BetaGM", beta_initial, 1000)}

Modified.GMM(id, y, x, lod, "MIWithID", beta_initial, 1000)

Modified.GMM(id, y, x, lod, "MIWithIDRM", beta_initial, 1000)

Modified.GMM(id, y, x, lod, "QQplot", beta_initial, 1000)
}
