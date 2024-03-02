\name{roc}
\alias{roc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Time-Dependent ROC Curves With Right Censored Data
}
\description{
This function allows for the estimation of time-dependent ROC curve by considering possible confounding factors. This method is implemented by standardizing and weighting based on an IPW estimator.
}
\usage{
roc(times, failures, variable, confounders, data,
 pro.time, precision=seq(.01, .99, by=.01))
}
\arguments{
  \item{times}{A character string with the name of the variable in \code{data} which represents the follow up times.}
  \item{failures}{A character string with the name of the variable in \code{data} which represents the event indicator (0=right censored, 1=event).}
  \item{variable}{A character string with the name of the variable in \code{data} which represents the prognostic variable under interest. This variable is collected at the baseline. The variable must be previously standardized according to the covariates among the controls as proposed by Le Borgne et al. (2017).}
  \item{confounders}{An object of class "formula". More precisely only the right part with an expression of the form \code{~ model}, where \code{model} is the linear predictor of the logistic regressions performed for each cut-off value. The user can use \code{~1} to obtain the crude estimation.}
  \item{data}{An object of the class \code{data.frame} containing the variables previously detailed.}
  \item{pro.time}{The value of prognostic time represents the maximum delay for which the capacity of the variable is evaluated. The same unit than the one used in the argument \code{times}.}
  \item{precision}{The quintiles (between 0 and 1) of the prognostic variable used for computing each point of the time dependent ROC curve. 0 (min) and 1 (max) are not allowed.}
}
\details{
This function computes confounder-adjusted time-dependent ROC curve with right-censored data. We adapted the naive IPCW estimator as explained by Blanche, Dartigues and Jacqmin-Gadda (2013) by considering the probability of experiencing the event of interest before the fixed prognostic time, given the possible confounding factors.
}
\value{
\item{table}{This data frame presents the sensitivities and specificities associated with the cut-off values. \code{J} represents the Youden index.}
\item{auc}{The area under the time-dependent ROC curve for a prognostic up to \code{pro.time}.}
}
\references{
Blanche et al. (2013) Review and comparison of roc curve estimators for a time-dependent outcome with marker-dependent censoring. Biometrical Journal, 55, 687-704. <doi:10.1002/ bimj.201200045>

Le Borgne et al. Standardized and weighted time-dependent ROC curves to evaluate the intrinsic prognostic capacities of a marker by taking into account confounding factors. Stat Methods Med Res. 27(11):3397-3410, 2018. <doi: 10.1177/ 0962280217702416>.
}

\examples{
# import and attach the data example
data(dataDIVAT3)

# A subgroup analysis to reduce the time needed for this example

dataDIVAT3 <- dataDIVAT3[1:400,]

# The standardized and weighted time-dependent ROC curve to evaluate the
# capacities of the recipient age for the prognosis of post kidney
# transplant mortality up to 2000 days by taking into account the
# donor age and the recipient gender.

# 1. Standardize the marker according to the covariates among the controls
lm1 <- lm(ageR ~ ageD + sexeR, data=dataDIVAT3[dataDIVAT3$death.time >= 2500,])
dataDIVAT3$ageR_std <- (dataDIVAT3$ageR - (lm1$coef[1] + lm1$coef[2] * dataDIVAT3$ageD +
 lm1$coef[3] * dataDIVAT3$sexeR)) / sd(lm1$residuals)

# 2. Compute the sensitivity and specificity from the proposed IPW estimators
roc2 <- roc(times="death.time", failures="death", variable="ageR_std",
confounders=~bs(ageD, df=3) + sexeR, data=dataDIVAT3, pro.time=2000,
precision=seq(0.1,0.9, by=0.2))

# The corresponding ROC graph
plot(roc2, col=2, pch=2, lty=1, type="b", xlab="1-specificity", ylab="sensibility")

# The corresponding AUC
roc2$auc
}

\keyword{Metrics}
