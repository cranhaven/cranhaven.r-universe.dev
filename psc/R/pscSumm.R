#' Personalised Synthetic Controls - summary
#'
#' A function which provides a summary of a DC_clean object.
#' To be used either in conjunction with dataComb.R or summary.psc.R
#'
#' @param DC_clean a cleaned dataset ontained using dataComb().
#' @return psc summary results including an estimate of the linear predictor
#' combing the data and the model, an estimate of patient level response and
#' summary statistics of the average responses for the sythenthic and observed
#' populations
#'
#' @export
pscSumm <- function(DC_clean){

  lp <- linPred(DC_clean,resp=F)
  resp <- linPred(DC_clean,resp=T)
  mtc.cond <- "trt"%in%colnames(DC_clean$cov)
  trt <- rep(1,nrow(DC_clean$cov))
  if(mtc.cond) trt <- factor(DC_clean$cov[,which(colnames(DC_clean$cov)=="trt")])
  lev <- levels(trt)
  lp_ret <- tapply(lp,trt,median)



  if("glm" %in% DC_clean$model_extract$mod_class){
    #exp_resp <- mean(resp)
    exp_resp <- tapply(resp,trt,median)
    ob_resp <- mean(unlist(DC_clean$out))
  }


  if("flexsurvreg" %in% DC_clean$model_extract$mod_class){
    s.ob <- Surv(DC_clean$out$time,DC_clean$out$cen)
    sfit <- survfit(s.ob~1)
    sfit.tab <- summary(sfit)$table


    #exp_resp <- median(resp)
    exp_resp <- tapply(resp,trt,median)
    ob_resp <- sfit.tab[7]
  }

  cat(paste(nrow(DC_clean$cov),"observations selected from the data cohort for comparison"),"\n")
  cat("CFM of type",DC_clean$model_extract$mod_class,"identified"," \n")
  cat("linear predictor succesfully obtained with median: \n ")
  cat(paste("trt",lev,": ",round(lp_ret,3),"\n",sep=""))
  cat("Average expected response: \n ")
  cat(paste("trt",lev,": ",round(exp_resp,3),"\n",sep=""))
  cat(paste("Average observed response:",round(ob_resp,3)),"\n")

  ret <- list("linpred"=lp,"E(patResp)"=resp,"expResp"=exp_resp,"obResp"=ob_resp)
  return(ret)
}
