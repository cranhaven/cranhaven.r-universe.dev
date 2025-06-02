\name{LMest-package}
\alias{LMest-package}
\alias{LMest}
\docType{package}
\title{Overview of the Package LMest}

\description{The package \code{LMest} is a framework for specifying and fitting Latent (or Hidden) Markov (LM) models for the analysis of longitudinal continuous and categorical data. Covariates are also included in the model specification through suitable parameterizations.}

\details{
Different LM models are estimated through specific functions requiring a data frame in long format. Responses are mainly categorical, the functions referred to continous responses are specified with \code{Cont}. When responses are continuos, the (multivariate) Gaussian distribution, conditional to the latent process, is assumed. 
The functions are the following:
\describe{
\item{\code{\link{lmest}}}{Function to estimate LM models for categorical responses generating the following classes:
\itemize{
\item{\code{\link{LMbasic-class}} for the basic LM model without covariates.}
\item{\code{\link{LMmanifest-class}} for the LM  model with covariates in the measurement submodel.}
\item{\code{\link{LMlatent-class}} for the LM model with covariates in the latent model.}
}}
\item{\code{\link{lmestCont}}}{Function to estimate LM models for continuous outcomes generating the following classes:
\itemize{
\item{\code{\link{LMbasiccont-class}} for the  basic LM model for continuous responses without covariates.}
\item{\code{\link{LMlatentcont-class}} for the  LM model for continuous responses with covariates in the latent model.}
}}
\item{\code{\link{lmestMixed}}}{Function to estimate Mixed LM models for categorical responses with discrete random effects in the latent model generating the following class:
\itemize{
\item{\code{\link{LMmixed-class}} for the mixed LM model.}
}}
\item{\code{\link{lmestMc}}}{Function to estimate Markov Chain models for categorical responses generating the following classes:
\itemize{
\item{\code{\link{MCbasic-class}} for the Markov Chain (MC) model without covariates.}
\item{\code{\link{MCcov-class}} for the MC model with covariates.}
}}
}


Maximum likelihood estimation of model parameters is performed through the Expectation-Maximization algorithm, which is implemented by relying on Fortran routines.

Model selection is provided by \code{\link{lmest}} and \code{\link{lmestCont}} functions. In addition, function \code{\link{lmestSearch}} allows us to deal with both model selection and multimodality of the likelihood function. Two main criteria are provided to select the number of latent states: the Akaike Information Criterion and the Bayesian Information Criterion.

Prediction of the latent states is performed by the function \code{\link{lmestDecoding}}: for local and global decoding (Viterbi algorithm) from the output of functions lmest, lmestCont and lmestMixed.

The package allows us to deal with missing responses, including drop-out and non-monotonic missingness, under the missing-at-random assumption.

Standard errors for the parameter estimates are obtained by the function \code{\link{se}} through exact computation of the information matrix or by reliable numerical approximations of this matrix.

The \code{\link{print}} method shows some convergence information, and the \code{\link{summary}} method shows the estimation results.

The package also provides some real and simulated data sets that are listed using the function \code{data(package = "LMest")}.

}


\author{
Francesco Bartolucci [aut,cre], Silvia Pandolfi [aut], Fulvia Pennoni [aut], Alessio Farcomeni [ctb], and Alessio Serafini [ctb]

Maintainer: Francesco Bartolucci <francesco.bartolucci@unipg.it>
}

\references{
Bartolucci, F., Pandolfi, S. and Pennoni, F. (2017). LMest: An R Package for Latent Markov Models for Longitudinal Categorical Data, \emph{Journal of Statistical Software}, \bold{81}, 1-38, doi:10.18637/jss.v081.i04.

Bartolucci, F., Farcomeni, A. and Pennoni, F. (2013). \emph{Latent Markov Models for Longitudinal Data}, Chapman and Hall/CRC press.

Bartolucci, F., Farcomeni, A., and Pennoni, F. (2014). Latent Markov models: A review of a general framework for the analysis of longitudinal data with covariates (with discussion). \emph{TEST}, \bold{23}, 433-465.


}

\seealso{\code{\link{lmest}}, \code{\link{lmestCont}}, \code{\link{lmestMc}}, \code{\link{lmestMixed}}, \code{\link{LMmixed-class}}, \code{\link{LMbasic-class}},\code{\link{LMbasiccont-class}}, \code{\link{LMlatent-class}},\code{\link{LMlatentcont-class}}, \code{\link{LMmanifest-class}}}
