#' Fits Joint Latent Class Tree (JLCT) model.
#' 
#' Fits Joint Latent Class Tree (JLCT) model. 
#' The main function of this package is \code{jlctree}.
#' \subsection{Problem setup}{
#' The dataset contains three types of variables about each subject:
#' the time-to-event, the longitudinal outcome, and additional covariates.
#' The goal is to jointly model the time-to-event by a survival model
#' and the longitudinal outcomes by a linear mixed-effects model,
#' and using the additional covariates.
#' The longitudinal outcomes consist of repeated measurements, thus 
#' are expected to be time-varying for a given subject. 
#' The additional covariates can be either time-invariant or time-varying.
#' Nevertheless, \code{jlctree} also allows data with time-invariant longitudinal outcome
#' and covariates.}
#' \subsection{JLCT model}{
#' This package implements the Joint Latent Class Tree (JLCT) modeling approach.
#' JLCT assumes that the population consists of homogeneous latent classes; 
#' within a latent class subjects follow the same survival and linear mixed-effects model,
#' but those differ from class to class.
#' In addition, JLCT assumes that conditioning on latent class membership, 
#' time-to-event and longitudinal outcomes are independent.
#' JLCT looks for a tree-based partitioning such that
#' within each estimated latent class defined by a terminal node,
#' the time-to-event and longitudinal responses display a lack of association.
#' Once the tree is constructed, JLCT assigns each observation to a latent class
#' (i.e. terminal node), and independently fits survival and linear mixed-effects models,
#' using the class membership information.}
#' \subsection{Time-to-event data format}{
#' The time-to-event data format required by \code{jlctree} depends on the 
#' time-varying nature of the variables to use:
#' if longitudinal outcome, or any of the covariates 
#' specified in \code{survival}, \code{classmb}, \code{fixef}, and \code{ranef}
#' is time-varying, then the time-to-event data must be in left-truncated right-censored (LTRC) format.
#' Otherwise, when longitudinal outcome and all of the covariates are time-invariant,
#' there should be only one observation per subject, and the time-to-event data 
#' can either be in LTRC format (when there exits subject-specific entry time) or in 
#' standard right-censored format.\cr\cr
#' To construct time-to-event data in left-truncated right-censored format, consider using function
#' \code{tmerge} in \code{R} package \code{survival}. 
#' See the simulated \code{data_timevar} and \code{data_timeinv} for examples
#' of LTRC format and right-censored format respectively.
#' }
#' 
#' @seealso \code{\link{jlctree}, \link{data_timeinv}, \link{data_timevar}}
#' @docType package
#' @keywords package
#' @name jlctree-package
#' @references {Ningshan Zhang and Jeffrey S. Simonoff: Joint Latent Class Trees: A Tree-Based Approach to Joint Modeling of Time-to-event and Longitudinal Data. arXiv:1812.01774 (2018).}
NULL

