#' MEteorits: Mixtures-of-ExperTs modEling for cOmplex and non-noRmal dIsTributions
#'
#' @description `meteorits` is a package containing several original and
#' flexible mixtures-of-experts models to model, cluster and classify
#' heteregenous data in many complex situations where the data are distributed
#' according to non-normal and possibly skewed distributions, and when they
#' might be corrupted by atypical observations. The toolbox also contains
#' sparse mixture-of-experts models for high-dimensional data.
#'
#' `meteorits` contains the following Mixture-of-Experts models:
#'
#' \itemize{
#'   \item NMoE (Normal Mixtures-of-Experts) provides a flexible framework for
#'   heterogenous data with Normal expert regressors network;
#'   \item SNMoE (Skew-Normal Mixtures-of-Experts) provides a flexible
#'   modeling framework for heterogenous data with possibly skewed
#'   distributions to generalize the standard Normal mixture of expert model;
#'   \item tMoE (t Mixtures-of-Experts) provides a flexible and robust
#'   modeling framework for heterogenous data with possibly heavy-tailed
#'   distributions and corrupted by atypical observations;
#'   \item StMoE (Skew t Mixtures-of-Experts) provides a flexible and robust
#'   modeling framework for heterogenous data with possibly skewed,
#'   heavy-tailed distributions and corrupted by atypical observations.
#' }
#'
#' For the advantages/differences of each of them, the user is referred to our
#' mentioned paper references.
#'
#' To learn more about `meteorits`, start with the vignettes:
#' `browseVignettes(package = "meteorits")`
#'
#' @references
#'
#' Chamroukhi, F. 2017. \emph{Skew-T Mixture of Experts.} Neurocomputing - Elsevier 266: 390--408. \url{https://chamroukhi.com/papers/STMoE.pdf}.
#'
#' Chamroukhi, F. 2016a. \emph{Robust Mixture of Experts Modeling Using the T-Distribution.} Neural Networks - Elsevier 79: 20--36. \url{https://chamroukhi.com/papers/TMoE.pdf}.
#'
#' Chamroukhi, F. 2016b. \emph{Skew-Normal Mixture of Experts.} In The International Joint Conference on Neural Networks (IJCNN). Vancouver, Canada. \url{https://chamroukhi.com/papers/Chamroukhi-SNMoE-IJCNN2016.pdf}.
#'
#' Chamroukhi, F. 2015a. \emph{Non-Normal Mixtures of Experts.} \url{http://arxiv.org/pdf/1506.06707.pdf}.
#'
#' Chamroukhi, F. 2015b. \emph{Statistical Learning of Latent Data Models for Complex Data Analysis.} Habilitation Thesis (HDR), Universite de Toulon. \url{https://chamroukhi.com/FChamroukhi-HDR.pdf}.
#'
#' Chamroukhi, F. 2010. \emph{Hidden Process Regression for Curve Modeling, Classification and Tracking.} Ph.D. Thesis, Universite de Technologie de Compiegne. \url{https://chamroukhi.com/FChamroukhi-PhD.pdf}.
#'
#' Chamroukhi, F., A. Same, G. Govaert, and P. Aknin. 2009. \emph{Time Series Modeling by a Regression Approach Based on a Latent Process.} Neural Networks 22 (5-6): 593--602. \url{https://chamroukhi.com/papers/Chamroukhi_Neural_Networks_2009.pdf}.
#'
#' @import methods
#' @import pracma
#' @import MASS
## usethis namespace: start
#' @useDynLib meteorits, .registration = TRUE
## usethis namespace: end
## usethis namespace: start
#' @importFrom Rcpp sourceCpp
## usethis namespace: end
"_PACKAGE"
