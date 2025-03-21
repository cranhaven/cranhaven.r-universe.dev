\name{Quantile.FWZ}
\alias{Quantile.FWZ}
\title{Function of a Quantile Regression Model}
\description{Runs a marginal quantile regression model for repeated measures data with values less than the limit of detection (LOD).}
\usage{Quantile.FWZ(y, x, lod, substitue, tau, corstr, typetd, data)}
\arguments{
  \item{y}{A column matrix of the observed outcome values or responses.}
  \item{x}{A matrix of covariate values, for which the number of columns is the number of covariates.}
  \item{lod}{A numeric value of limit of detection (LOD).}
  \item{substitue}{A character string specifying the substitution approach, including "None", "LOD", "LOD2", "LODS2", "BetaMean", "BetaGM", "MIWithID", "MIWithIDRM", and "QQplot".}
  \item{tau}{A numeric value of quantile level, e.g., tau=0.25 for 25th quantile and tau=0.5 for median.}
  \item{corstr}{A character string specifying the working correlation structure, given by either "exchangeable" or "AR-1".}
  \item{typetd}{An atomic vector specifies the types of time-dependent covaraites, with the length of the vector equal to the number of regression parameters, excluding the intercept. For time-independent covariates or those in a cluster study, "1" is assigned.}
  \item{data}{A data frame that originazes the given data into two-dimensional structure of rows and columns.}
}

\details{This function modifies the R functions provided by Dr. Liya Fu and based on the manuscript of Fu \emph{et al.} (2015). Chen \emph{et al.} (2021) further applied the Gaussian pseudolikelihood approach for quantile regression to environmental exposure and biomonitoring repeated measures data with values less than the limit of detection (LOD). Fill-in or substitution methods, including single and multiple value imputation techniques, were used to assign values for non-detects.

See the Details of the "Fillin" function for introduction of the available substitution methods. For a multiple random value imputation technique, it provides an alternative for environmental exposure and biomonitoring data with non-detects, in which the imputed values can be generated using a regression of an exposure measurement on covariate(s) ("MIWithID" and "MIWithIDRM") (Lubin \emph{et al.}, 2004). Information of identification (ID) would be included in "MIWithID" as the covariate, e.g., "id in "simdata15", while ID and order of cluster size or time points would be treated as the covariates in "MIWithIDRM", e.g. "id" and "visit" in "simdata15".}
\value{An object of class "Quantile.FWZ" representing the fit.}

\references{Chen, I-C., Bertke, S. J., Curwin, B. D. (2021). Quantile regression for exposure data with repeated measures in the presence of non-detects. \emph{Journal of Exposure Science and Environmental Epidemiology}, 31, 1057--1066.

Fu, L., Wang, Y.-G., Zhu, M. (2015). A Gaussian pseudolikelihood approach for quantile regression with repeated measurements. \emph{Computational Statistics and Data Analysis}, 84, 41--53.

Lubin, J. H., Colt, J. S., Camann, D., \emph{et al.} (2004). Epidemiologic evaluation of measurement data in the presence of detection limits. \emph{Environmental Health Perspectives}, 112, 1691--6.
}
\author{Liya Fu and I-Chen Chen}
\seealso{\code{\link{Quantile.select.FWZ}}.}

\examples{
## Uses the simdata15 to run the marginal models.
library(marlod)
library(MASS)
library(miWQS)
library(quantreg)

data(simdata15)

y=as.matrix(as.vector(t(simdata15$y)))
x1=as.matrix(as.vector(t(simdata15$x1)))
x2=as.matrix(as.vector(t(simdata15$x2)))
x=cbind(matrix(1,length(x1),1),x1,x2)

## LOD=2 is equivalent to detection proportion=50% (censoring proportion=50%).
lod=2

## Median or 50th quantile is given.
tau=0.5

## Examples to perform the function
\donttest{Quantile.FWZ(y, x, lod, "BetaGM", tau, "AR-1", c(1,1), simdata15)}

\donttest{Quantile.FWZ(y, x, lod, "QQplot", tau, "exchangeable", c(1,1), simdata15)}

\donttest{Quantile.FWZ(y, x, lod, "MIWithID", tau, "exchangeable", c(1,1), simdata15)}
}
