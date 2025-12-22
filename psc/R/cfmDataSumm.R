#'  Summarising data within a Counter Factual Model (CFM)
#'
#' The pscCFM creates a model object which is stripped of identifiable
#' information.  The cfmDataSumm function supplies a tabulated form of the
#' dataset used in the CFM for summary information.  Information returned in
#' the form of a table
#'
#' @param cfm a 'glm' or 'flexsurvreg' model object
#' @return a summary table
#' @import gtsummary
cfmDataSumm <- function(cfm){

  ### Getting data from object
  data <- model.frame(cfm);data

  ## Formula
  fix.form <- formula(cfm,fixed.only=T)
  fix.term <- terms(fix.form)
  terms <- attributes(fix.term)$term.labels

  ### Selecting variables which are fixed effects
  data <- data[,which(names(data)%in%terms)]
  if(length(terms)==1){
    data <- data.frame(data)
    names(data) <- terms
  }

  ### Table summary
  tb_summ <- tbl_summary(data,missing="ifany")
  ret <- list(tb_summ)
  names(ret) <- "summ_Table"
  class(ret) <- c("quiet_gtsumm",class(ret))
  ret
}




