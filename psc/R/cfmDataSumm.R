#' Summarising data within a CFM
#'
#' The pscCFM creates a model object which is stripped of identifiable
#' information.  The cfmDataSumm fucntion supplies a tabulated form of the dataset
#' for summary information
#'
#' @param cfm a 'glm' or 'flexsurvreg' model object
#' @return a summary table
#' @import gtsummary
#' @export
cfmDataSumm <- function(cfm){


  ### Getting data from object
  data <- model.frame(cfm)

  ## removing outcome
  out <- data[,1]
  data<- data[,-1]

  ## removing "weights" column
  w.id <- which(names(data)=="(weights)")
  if(length(w.id)>0) data <- data[,-w.id]

  ### Table summary
  tb_summ <- tbl_summary(data,missing="ifany")
  ret <- list(tb_summ)
  names(ret) <- "summ_Table"
  class(ret) <- c("quiet_gtsumm",class(ret))
  ret
}


