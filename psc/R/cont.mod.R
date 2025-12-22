#' Example model for a survival outcome
#'
#' A generated model with a continuous data endpoint and a identity link function.
#' Data for the model were synthetically generated and are based on a dataset to
#' evaulate the use of Sorafenib in HCC akin to the PROSASH model
#' (see ?psc::surv.mod for more details)
#'
#' @format A model of class 'glm':
#' \describe{
#'  \item{ecog}{ECOG performance Status}
#'  \item{logafp}{AFP - log scale}
#'  \item{alb}{albumin}
#'  \item{logcreat}{Creatinine - log scale}
#' }
#' @source simulated
"cont.mod"
