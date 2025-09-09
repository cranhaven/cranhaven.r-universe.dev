#' Model for a survival outcome based on Gemcitbine patietns from ESPAC-3
#'
#' A generated model with a survival endpoint and a cuymulative hazard function
#' estimated using flexible parametric splines. Data for the model were obtained
#' from the ESPAC-3 trials
#'
#' @format A model of class 'pscCFM' containg a 'flexsurvreg' model:
#' \describe{
#'  \item{gamma}{cumulative baseline hazard parameters}
#'  \item{nodes}{negative (n=1) or positive (n=2) lymph nodes}
#'  \item{grade}{tumour grade (1,2 or 3)}
#'  \item{lca199}{log transformed ca19.9}
#'  \item{t}{T-stage (1,2 or 3)}
#' }
#' @source simulated
"gemCFM"
