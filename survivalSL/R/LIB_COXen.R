

LIB_COXen <- function(times, failures, group=NULL, cov.quanti=NULL, cov.quali=NULL, data, alpha, lambda){
  .outcome <- paste("Surv(", times, ",", failures, ")")
  if(!(is.null(group))){
    if(is.null(cov.quanti)==F & is.null(cov.quali)==F){
      .f <- as.formula( paste(.outcome, "~", group, "*(",paste("bs(", cov.quanti, ", df=3)",
                                                               collapse = " + "), " + ",
                              paste(cov.quali, collapse = " + "),  ")",collapse = " ") )
    }
    if(is.null(cov.quanti)==F & is.null(cov.quali)==T){
      .f <- as.formula( paste(.outcome, "~", group, "*(",paste("bs(", cov.quanti, ", df=3)",
                                                               collapse = " + "),")" ))
    }
    if(is.null(cov.quanti)==T & is.null(cov.quali)==F){
      .f <- as.formula( paste(.outcome, "~", group, "*(",paste(cov.quali, collapse = " + "),
                              ")",collapse = " ") )
    }
    if(is.null(cov.quanti)==T & is.null(cov.quali)==T){
      .f <- as.formula( paste(.outcome, "~", group) )

      .coxph <- coxph(.f, data=data)

      .coxphsurv<-survfit(.coxph, newdata = data,se.fit = F)

      .lp.coxph <- predict(.coxph, newdata = data, type="lp")
      .b <- glmnet_basesurv(data[,times], data[,failures], .lp.coxph, centered = FALSE)
      .H0 <- data.frame(value = .b$cumulative_base_hazard, time = .b$times)

      .sumcoxphsurv<-summary(.coxphsurv, times=sort(unique(data[,times])))
      .pred.temp <- t(.sumcoxphsurv$surv)
      .time.temp <- .sumcoxphsurv$time
      .obj <- list(model=.coxph,
                   library="LIB_COXen",
                   group=group, cov.quanti=cov.quanti, cov.quali=cov.quali,
                   data=data.frame(times=data[,times], failures=data[,failures],
                                   data[, !(dimnames(data)[[2]] %in% c(times, failures))]),
                   times=.time.temp,  hazard=.H0$value, predictions=.pred.temp)

      class(.obj) <- "libsl"

      return(.obj)
    }

    .full <- coxph( .f,  data = data)
    .l <- length(.full$coefficients)
    .bs=NULL
    .bin=NULL
    if(is.null(cov.quanti)==F){
      .bs <- eval(parse(text=paste("cbind(",
                                   paste("bs(data$", cov.quanti,",df=3)", collapse = ", ")
                                   ,")") ) )
    }
    if(is.null(cov.quali)==F){
      .bin <- eval(parse(text=paste("cbind(",  paste("data$", cov.quali, collapse = ", "), ")") ) )
    }

    .cov <- cbind(.bs,.bin)
    .x <- cbind(data[,group], .cov, .cov * data[,group])
    .y <- Surv(data[,times], data[,failures])

    .en<- glmnet(x = .x, y = .y, lambda = lambda,
                 type.measure = "deviance", family = "cox",
                 alpha = alpha, penalty.factor = c(0, rep(1, .l-1)))
  }
  else{
    if(is.null(cov.quanti)==F & is.null(cov.quali)==F){
      .f <- as.formula( paste(.outcome, "~", paste("bs(", cov.quanti, ", df=3)", collapse = " + "),
                              " + ", paste(cov.quali, collapse = " + "),
                              collapse = " ") )
    }
    if(is.null(cov.quanti)==F & is.null(cov.quali)==T){
      .f <- as.formula( paste(.outcome, "~", paste("bs(", cov.quanti, ", df=3)", collapse = " + ")))
    }
    if(is.null(cov.quanti)==T & is.null(cov.quali)==F){
      .f <- as.formula( paste(.outcome, "~",  paste(cov.quali, collapse = " + "),collapse = " ") )
    }
    .full <- coxph( .f,  data = data)
    .l <- length(.full$coefficients)

    .bs=NULL
    .bin=NULL
    if(is.null(cov.quanti)==F){
      .bs <- eval(parse(text=paste("cbind(",
                                   paste("bs(data$", cov.quanti,",df=3)", collapse = ", ")
                                   ,")") ) )
    }
    if(is.null(cov.quali)==F){
      .bin <- eval(parse(text=paste("cbind(",  paste("data$", cov.quali, collapse = ", "), ")") ) )
    }
    .cov <- cbind(.bs,.bin)
    .x <- .cov
    .y <- Surv(data[,times], data[,failures])

    .en<- glmnet(x = .x, y = .y, lambda = lambda,
                 type.measure = "deviance", family = "cox",
                 alpha = alpha)
  }

  .lp.en <- predict(.en, newx = .x)
  .b <- glmnet_basesurv(data[,times], data[,failures], .lp.en, centered = FALSE)
  .H0 <- data.frame(value = .b$cumulative_base_hazard, time = .b$times)


  .pred.temp <- exp(matrix(exp(.lp.en)) %*% t(as.matrix(-1*.H0$value)))
  .time.temp <- .H0$time


  .obj <- list(model=.en,
               library="LIB_COXen",
               group=group, cov.quanti=cov.quanti, cov.quali=cov.quali,
               data=data.frame(times=data[,times], failures=data[,failures],
                               data[, !(dimnames(data)[[2]] %in% c(times, failures))]),
               times=.time.temp,  hazard=.H0$value, predictions=.pred.temp)

  class(.obj) <- "libsl"

  return(.obj)
}

