

LIB_AFTggamma <- function(times, failures, group=NULL, cov.quanti=NULL,
                       cov.quali=NULL, data){
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

  .flex<-flexsurvreg(.f, data = data,
                     dist = "gengamma",
                     hessian=F, method="Nelder-Mead")

  .flex.cum=summary(.flex, type="cumhaz")

  .H0 <- data.frame(value = .flex.cum$est, time = .flex.cum$time)

  .predlist<-summary(.flex, type = "survival", newdata=data, ci = F, se=F )
  .time.temp=.predlist[[1]]$time

  .pred=matrix(nrow=length(.predlist), ncol=length(.predlist[[1]]$time))

  for (i in 1:length(.predlist)){
    .pred[i,]=.predlist[[i]]$est
  }

  .obj <- list(model=.flex,
               library="LIB_AFTggamma",
               group=group, cov.quanti=cov.quanti, cov.quali=cov.quali,
               data=data.frame(times=data[,times], failures=data[,failures], data[, !(dimnames(data)[[2]] %in% c(times, failures))]),
               times=.time.temp, hazard=.H0$value, predictions=.pred)

  class(.obj) <- "libsl"

  return(.obj)
}
