#' Inference in Randomized Clinical Trials with Death and Missingness
#'
#' @docType package
#' @name idem-package
#' @aliases idem
#' @useDynLib idem, .registration = TRUE
#'
#' @importFrom rstan sampling extract stanc
#' @importFrom rstantools rstan_config
#' @importFrom RcppParallel RcppParallelLibs
#' @importFrom grDevices colors pdf dev.off grey
#' @importFrom graphics axis box legend lines par plot points text contour
#'     filled.contour grid rect abline
#' @importFrom utils read.table tail
#' @importFrom sqldf sqldf
#' @importFrom parallel detectCores mclapply
#' @importFrom mice mice mice.impute.mnar.norm complete
#'
#' @import stats
#' @import survival
#' @import Rcpp
#' @import methods
#'
#' @description
#'
#' This package contains the functions for drawing inference in randomized
#' clinical trials with death and intermittent missingness.
#'
#' @section Notation:
#'
#' Consider a two-arm randomized study. Let \eqn{Y_k} denote outcome measured at
#' time \eqn{t_k} and \eqn{Z} denote a functional endpoint that is a function of
#' \eqn{Y}. Let \eqn{L} denote the survival time. Let \eqn{X} denote the
#' baseline covariates and \eqn{T} denote the treatment assignment.
#'
#' @section Ranking:
#'
#' If two subject were both alive at the end of the study, they are ranked based
#' on functional outcome \eqn{Z}. If at least one subject was dead at the end of
#' the study, they are ranked based on survival time \eqn{L}.
#'
#' Treatment effect, \eqn{\theta} is defined as the probability that the outcome
#' for a random individual randomized to treatment \eqn{T=0} is less than the
#' outcome of a random individual randomized to treatment \eqn{T=1} minus the
#' probability that the outcome for a random individual randomized to treatment
#' \eqn{T=0} is greater than the outcome of a random individual randomized to treatment
#' \eqn{T=1}.
#'
#' @section Missingness:
#'
#' In order to estimate \eqn{\theta} in the presence of missing data, we need to
#' impute \eqn{Z} for subjects alive at the end of the study with \eqn{Y_k} missing
#' for some \eqn{k}.
#'
#' The benchmark assumption we consider for the imputation is the complete case
#' missing value (CCMV) restrictions. We then consider exponential tilting
#' models for introducing sensitivity parameters for evaluating the robustness
#' of the findings with regards to different missing data mechanism assumptions.
#' The models are as follows:
#'
#' \deqn{ f(Y^{(s)}_{mis} | Y^{(s)}_{obs}, Y_0, X, T,S=s) \propto \exp(
#' \beta_T Z) f(Y^{(s)}_{mis} | Y^{(s)}_{obs}, Y_0, X, T,S=1)
#' }
#'
#' where \eqn{S} denotes the missingness patterns, \eqn{S=1} denotes the
#' completers and \eqn{\beta_T} denotes the sensitivity parameter for arm \eqn{T}.
#'
#' @section Graphical user interface (GUI):
#'
#' This package provides a web-based GUI. See \code{\link{imShiny}} for
#' details.
#'
#' @references
#'
#' Wang C, Scharfstein DO, Colantuoni E, Girard T, Yan Y (2016). Inference in
#' Randomized Trials with Death and Missingness. <DOI:10.1111/biom.12594>
#'
#' Wang C, Colantuoni E, Leroux A, Scharfstein DO (2020). idem: An R Package for
#' Inferences in Clinical Trials with Death and Missingness.
#' <DOI:10.18637/jss.v093.i12>
#'
#'
NULL


#' Example dataset
#'
#' @description The Awakening and Breathing Controlled (ABC) trial randomized critically ill
#' patients receiving mechanical ventilation 1:1 within each study site to
#' management with a paired sedation plus ventilator weaning protocol involving
#' daily interruption of sedative through spontaneous awakening trials (SATs)
#' and spontaneous breathing trials (SBTs) or sedation per usual care
#' (UC) and SBTs.
#'
#' The example dataset is from a single site substudy in ABC. The researchers
#' assessed differences in cognitive, psychological and functional outcomes at 3
#' and 12 months after randomization.
#'
#' @name abc
#'
#' @format A dataframe with 5 variables:
#' \describe{
#'   \item{AGE}{Age}
#'   \item{TRT}{Treatment assignment. 0: UC + SBT, 1: SAT + SBT}
#'   \item{SURV}{Survival days}
#'   \item{Y2}{Cognitive score at 12 months}
#'   \item{Y1}{Cognitive score at 3 months}
#' }
#'
#' @references
#'
#' T. D. Girard, J. P. Kress, B. D. Fuchs, J. W. W. Thomason, W. D. Schweickert,
#' B. T. Pun, D. B. Taichman, J. G. Dunn, A. S. Pohlman, P. A. Kinniry, J. C.
#' Jackson, A. E. Canonico, R. W. Light, A. K. Shintani, J. L. Thompson, S. M.
#' Gordon, J. B. Hall, R. S. Dittus, G. R. Bernard, and E. W. Ely. Efficacy and
#' safety of a paired sedation and ventilator weaning protocol for mechanically
#' ventilated patients in intensive care (awakening and breathing controlled
#' trial): a randomised controlled trial. Lancet, 371:126-134, 2008.
#'
#'
NULL
