
LIB_RSF <- function(times, failures, group=NULL, cov.quanti=NULL, cov.quali=NULL,
                    data, nodesize, mtry, ntree){

  .outcome <- paste("Surv(", times, ",", failures, ")")

  if(!(is.null(group))){
    if(is.null(cov.quanti)==F & is.null(cov.quali)==F){
      .f <- as.formula( paste(.outcome, "~", group, "+", paste( cov.quanti,  collapse = " + "),
                              " + ", paste(cov.quali, collapse = " + "),
                              collapse = " ") )
    }
    if(is.null(cov.quanti)==F & is.null(cov.quali)==T){
      .f <- as.formula( paste(.outcome, "~", group, "+",
                              paste( cov.quanti, collapse = " + "),collapse = " ") )
    }
    if(is.null(cov.quanti)==T & is.null(cov.quali)==F){
      .f <- as.formula( paste(.outcome, "~", group, "+",
                              paste(cov.quali, collapse = " + "),collapse = " ") )
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
  options(rf.cores=1, mc.cores=1)
  .rf <- rfsrc(.f, data = data, nodesize = nodesize, mtry = mtry, ntree = ntree, splitrule="logrank")

  .time <- sort(unique(data[,times]))

  .pred.rf <- predict(.rf)
  .survival <- cbind(rep(1, dim(.pred.rf$survival.oob)[1]), .pred.rf$survival.oob)
  .time.interest <- c(0, .pred.rf$time.interest)

  idx=findInterval(.time,.time.interest)
  .pred=.survival[,pmax(1,idx)]

  .obj <- list(model=.rf,
               library="LIB_RSF",
               group=group, cov.quanti=cov.quanti, cov.quali=cov.quali,
               data=data.frame(times=data[,times], failures=data[,failures],
                               data[, !(dimnames(data)[[2]] %in% c(times, failures))]),
               times=.time,  predictions=.pred)

  class(.obj) <- "libsl"

  return(.obj)
}
