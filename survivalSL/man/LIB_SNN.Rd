\name{LIB_SNN}
\alias{LIB_SNN}

\title{
Library of the Super Learner for Survival Neural Network
}

\description{
Fit a 1-layer neural network based on the partial likelihood from a Cox proportional hazards model.
}

\usage{
LIB_SNN(times, failures, group=NULL, cov.quanti=NULL, cov.quali=NULL,
data, n.nodes, decay, batch.size, epochs)
}

\arguments{
  \item{times}{The name of the variable related the numeric vector with the follow-up times.}
  \item{failures}{The name of the variable related the numeric vector with the event indicators (0=right censored, 1=event).}
  \item{group}{The name of the variable related to the exposure/treatment. This variable shall have only two modalities encoded 0 for the untreated/unexposed patients and 1 for the treated/exposed ones. The default value is NULL: no specific exposure/treatment is considered. When a specific exposure/treatment is considered, it will be forced in the algorithm or related interactions will be tested when possible.}
  \item{cov.quanti}{The name(s) of the variable(s) related to the possible quantitative covariates. These variables must be numeric.}
  \item{cov.quali}{The name(s) of the variable(s) related to the possible qualitative covariates. These variables must be numeric with two levels: 0 and 1. A complete disjunctive form must be used for covariates with more levels.}
  \item{data}{A data frame for training the model in which to look for the variables related to the status of the follow-up time (\code{times}), the event (\code{failures}), the optional treatment/exposure (\code{group}) and the covariables included in the previous model (\code{cov.quanti} and \code{cov.quali}).}
  \item{n.nodes}{The number of hidden nodes.}
  \item{decay}{The value of the weight decay.}
  \item{batch.size}{The value of batch size.}
  \item{epochs}{The value of epochs.}
 }


\details{
This function is based is based on the \code{deepsurv} from the \code{survivalmodels} package. You need to call Python using \code{reticulate}. In order to use it, the required Python packages must be installed with \code{reticulate::py_install}. Therefore, before running the present \code{LIB_SNN} function, you must install  and call for the \code{reticulate} and \code{survivalmodels} packages, and install \code{pycox} by using the following command: \code{install_pycox(pip = TRUE, install_torch = FALSE)}.  The \code{survivalSL} package functions without these supplementary installations if this learner is not included in the library.
}

\value{
\item{model}{The estimated model.}
\item{group}{The name of the variable related to the exposure/treatment.}
\item{cov.quanti}{The name(s) of the variable(s) related to the possible quantitative covariates.}
\item{cov.quali}{The name(s) of the variable(s) related to the possible qualitative covariates.}
\item{data}{The data frame used for learning. The first column is entitled \code{times} and corresponds to the observed follow-up times. The second column is entitled \code{failures} and corresponds to the event indicators. The other columns correspond to the predictors.}
\item{times}{A vector of numeric values with the times of the \code{predictions}.}
\item{predictions}{A matrix with the predictions of survivals of each subject (lines) for each observed time (columns).}
}

\references{
Katzman, J. L., Shaham, U., Cloninger, A., Bates, J., Jiang, T., & Kluger, Y. (2018). DeepSurv: personalized treatment recommender system using a Cox proportional hazards deep neural network. BMC Medical Research Methodology, 18(1), 24. https://doi.org/10.1186/s12874-018-0482-1
}


%\examples{ }

\keyword{Super Learner Library}
