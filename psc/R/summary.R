#' Personalised Synthetic Controls - summary
#'
#' A generic function to provide a summary of a 'psc' object obtained from
#' pscfit.R
#'
#' @param object an object of class 'psc'
#' @param ... not used
#' @return A summary of a psc object obtained using pscSumm and a copy of the pscfit object
#' @import survival
#' @examples
#' e4_data <- psc::e4_data
#' gemCFM <- psc::gemCFM
#' psc <- pscfit(gemCFM,e4_data,nsim=1500,nchain=1)
#' summary(psc)
#' @export
summary.psc <- function(object,...){

  #### CFM details
  frm <- object$formula;frm
  fam <- object$family;fam
  mt <- object$mod_class;mt

  ### Summary of the cfm
  if("glm"%in%object$mod_class){
    cfm.summ <-cfmSumm.glm(object)
  }

  if("flexsurvreg"%in%object$mod_class){
    options(warn=-1) ## removing warning for unobtained med surv
    cfm.summ <- cfmSumm.flexsurvreg(object)
    cfms.id <- lapply(cfm.summ[,c(2,4,5)],function(x) cfm.summ$time[min(which(x<0.5))])
    cfm.summ <-list(cfm.summ,"summ"=unlist(cfms.id))
    options(warn=1)
  }

  ### Outcome summary
  if("glm"%in%object$mod_class){
    out.summ <- mean(object$DC$Y)
    if(!is.null(object$DC$trt)){
      out.summ <- tapply(object$DC$Y,object$DC$trt,mean)
    }
  }

  if("flexsurvreg"%in%object$mod_class){
    sfit <- survfit(Surv(object$DC$Y$time,object$DC$Y$cen)~1)
    out.summ <- summary(sfit)$table[7:9];out.summ
    if(!is.null(object$DC$trt)){
      sfit <- survfit(Surv(object$DC$Y$time,object$DC$Y$cen)~object$DC$trt)
      out.summ <- summary(sfit)$table[,7:9]
    }
  }


  ### Model
  cat("Counterfactual Model (CFM): \n")

  if("glm"%in%mt){
    cat(
      paste("A model of class 'GLM'"," \n",sep=""),
      paste("Family: ",fam$family," \n",sep=""),
      paste("Link: ",fam$link," \n",sep=""))
  }

  if("flexsurvreg"%in%mt){
    cat(
      paste("A model of class 'flexsurvreg'"," \n",sep=""),
      paste("Fit with ",object$k," internal knots","\n",sep=""))
  }

  cat("\n")
  cat("CFM Formula: \n")
  print(frm)
  cat("\n")

  ### CFM summary
  cat("CFM Summary: \n")
  cat("Expected response for the outcome under the CFM:")
  cat("\n")

  print.default(format(cfm.summ$summ,digits = max(3L, getOption("digits") - 3L)), print.gap = 2L,
                quote = FALSE)
  cat("\n")
  cat("Observed outcome from the Data Cohort:")
  cat("\n")
  print.default(format(as.matrix(out.summ),digits = max(3L, getOption("digits") - 3L)), print.gap = 2L,
                quote = FALSE)
  cat("\n")

  ### Fit summary
  cat("MCMC Fit: \n")
  cat("Posterior Distribution obtaine with fit summary:")
  cat("\n")
  pf <- as.matrix(object$postFit)
  print.default(format(pf,digits = max(3L, getOption("digits") - 3L)), print.gap = 2L,
                quote = FALSE)
  cat("\n")

  ### Fit summary
  cat("Summary: \n")
  cat("Posterior Distribution for beta:")

  print(object)

}
