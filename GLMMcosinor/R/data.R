#' Vitamin D dataset for cosinor modeling examples.
#'
#' Simulated data set to illustrate the cosinor model. The \code{vit_d} column
#' contains the blood vitamin D levels which vary over time (\code{time}).
#' The rhythm of the vitamind D fluctuations follows a cosine function and can
#' be modeled with a cosinor model. The \code{X} column is a binary covariate
#' representing two groups of patients and is associated with the
#' characteristics of the rhythm. The rhythm has a period of about 12 hours.
#'
#' @format A \code{data.frame} with 3 variables: \code{vit_d}, \code{time}, and
#' \code{X}.

"vitamind"

#' cosinor_mixed dataset for cosinor modeling examples.
#'
#' Simulated data set to illustrate a mixed cosinor model. The \code{Y} column
#' contains a simulated outcome variable that varies over the time variable
#' (\code{times}). The \code{subject} column is a grouping variable that can be
#' used as a random effect. The rhythm has a period of 24 hours. Data was
#' simulated using \code{simulate_cosinor}.
#'
#' @format A \code{data.frame} with 3 variables: \code{Y}, \code{times}, and
#' \code{subject}.

"cosinor_mixed"
