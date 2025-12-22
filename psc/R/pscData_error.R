#' A function which performs error checks between the DC and CFM
#'
#' The purpose of this function is check that terms included in the Data Cohort
#' match those used within the Counter Factual Model. This acts as a
#' sub-function to the pscData.R function.
#'
#' @param term.nm Term names from the CFM
#' @param DC a data cohort to be 'cleaned'
#' @return a 'stop' command when errors are detected
pscData_error <- function(term.nm,DC){

  ## removing 'weights' column if there
  wid <- which(term.nm=="(weights)");wid
  if(length(wid)>0) term.nm <- term.nm[-wid]

  ## id for unavailable terms
  data_unavail_id <- which(!term.nm %in% names(DC))

  if (length(data_unavail_id) != 0){
    data_unavail <- term.nm[data_unavail_id]
    stop(paste("Covariate '", data_unavail, "' is included in the model but not
               the dataset",sep = ""))
  }
}

