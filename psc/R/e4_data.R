#' Example Dataset of patients treated with GemCap in the ESPAC-4 trial
#'
#' A dataset containing 346 simulated patients.  Data are based on the patietns
#' randomised to revceive GemCap in the ESPAC-4 trial
#'
#' @format A model of class 'flezsurvreg':
#' \describe{
#'  \item{time}{survival time}
#'  \item{cen}{censoring indicator}
#'  \item{nodes}{negative (n=1) or positive (n=2) lymph nodes}
#'  \item{grade}{tumour grade (1,2 or 3)}
#'  \item{lca199}{log transformed ca19.9}
#'  \item{t}{T-stage (1,2 or 3)}
#' }
#' @source simulated
"e4_data"

