#' IAT reliability
#'
#' Compute the practice -- test IAT reliability.
#'
#' @param data dataframe with class \code{"dscore"} (Gawronski et al., 2017).
#'
#' @return List of two objects:
#'    \describe{
#'     \item{\code{Test-practice reliability}}{contains the IAT reliability.}
#'     \item{\code{Number of Participants}}{Contains the number of participants
#'       on which the reliability was computed.}
#'     }
#' @export
#'
#'
#' @examples
#' # compute D-score 2 for the IAT data ###
#'   data("raw_data") # import data
#'   iat_cleandata <- clean_iat(raw_data, sbj_id = "Participant",
#'                           block_id = "blockcode",
#'                           mapA_practice = "practice.iat.Milkbad",
#'                           mapA_test = "test.iat.Milkbad",
#'                           mapB_practice = "practice.iat.Milkgood",
#'                           mapB_test = "test.iat.Milkgood",
#'                           latency_id = "latency",
#'                           accuracy_id = "correct",
#'                           trial_id = "trialcode",
#'                           trial_eliminate = c("reminder", "reminder1"),
#'                           demo_id = "blockcode",
#'                           trial_demo = "demo")
#'   iat_data <- iat_cleandata[[1]]
#' # calculate D-score
#'   iat_dscore <- compute_iat(iat_data,
#'                          Dscore =  "d2")
#'   IAT_rel(iat_dscore)
IAT_rel <- function(data){
  # check dataset class --------------------------
  if(is.na(class(data)[2]) | class(data)[2] != "dscore"){
    stop("data is not an object of class dscore")
  }
  # compute realiability --------------------------
  IAT_reliability <- list(rel = round(
    cor(data[ , c(grep("d_practice", colnames(data)),
                  grep("d_test",
                       colnames(data)))])[2, 1],
    2),
    number = round(nrow(data)))
  names(IAT_reliability) <- c("Test-pratice Reliability",
                             "Number of participants")
  class(IAT_reliability) <- "IAT_rel"
  return(IAT_reliability)
}
