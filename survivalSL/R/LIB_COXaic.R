

LIB_COXaic<- function(times, failures, group=NULL, cov.quanti=NULL, cov.quali=NULL, data, final.model){

  .outcome <- paste("Surv(", times, ",", failures, ")")

  .f<-as.formula( paste(.outcome, "~",  paste(final.model, collapse = " + "), collapse = " ") )

  .coxph <- coxph(.f, data=data)


  .coxphsurv<-survfit(.coxph, newdata = data, se.fit = F)

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

