

LIB_COXall<- function(times, failures, group=NULL, cov.quanti=NULL, cov.quali=NULL, data){
  .outcome <- paste("Surv(", times, ",", failures, ")")
  if(!(is.null(group))){
    if(is.null(cov.quanti)==F & is.null(cov.quali)==F){
      .f <- as.formula( paste(.outcome, "~", group, "+", paste( cov.quanti,  collapse = " + "), " + ", paste(cov.quali, collapse = " + "),
                              collapse = " ") )
    }
    if(is.null(cov.quanti)==F & is.null(cov.quali)==T){
      .f <- as.formula( paste(.outcome, "~", group, "+", paste( cov.quanti, collapse = " + "),collapse = " ") )
    }
    if(is.null(cov.quanti)==T & is.null(cov.quali)==F){
      .f <- as.formula( paste(.outcome, "~", group, "+",paste(cov.quali, collapse = " + "),collapse = " ") )
    }
    if(is.null(cov.quanti)==T & is.null(cov.quali)==T){
      .f <- as.formula( paste(.outcome, "~", group) )
    }
  }
  else{
    if(is.null(cov.quanti)==F & is.null(cov.quali)==F){
      .f <- as.formula( paste(.outcome, "~", paste( cov.quanti,  collapse = " + "), " + ", paste(cov.quali, collapse = " + "),
                              collapse = " ") )
    }
    if(is.null(cov.quanti)==F & is.null(cov.quali)==T){
      .f <- as.formula( paste(.outcome, "~", paste( cov.quanti, collapse = " + "),collapse = " ") )
    }
    if(is.null(cov.quanti)==T & is.null(cov.quali)==F){
      .f <- as.formula( paste(.outcome, "~",  paste(cov.quali, collapse = " + "),collapse = " ") )
    }
  }

  .coxph <- coxph(.f, data=data)


  .coxphsurv<-survfit(.coxph, newdata = data,se.fit = F)


  .lp.coxph <- predict(.coxph, newdata = data, type="lp")
  .b <- glmnet_basesurv(data[,times], data[,failures], .lp.coxph, centered = FALSE)
  .H0 <- data.frame(value = .b$cumulative_base_hazard, time = .b$times)

  .sumcoxphsurv<-summary(.coxphsurv, times=sort(unique(data[,times])))
  .pred.temp <- t(.sumcoxphsurv$surv)
  .time.temp <- .sumcoxphsurv$time
  .obj <- list(model=.coxph,
               library="LIB_COXaic",
               group=group, cov.quanti=cov.quanti, cov.quali=cov.quali,
               data=data.frame(times=data[,times], failures=data[,failures],
                               data[, !(dimnames(data)[[2]] %in% c(times, failures))]),
               times=.time.temp,  hazard=.H0$value, predictions=.pred.temp)

  class(.obj) <- "libsl"

  return(.obj)
}

