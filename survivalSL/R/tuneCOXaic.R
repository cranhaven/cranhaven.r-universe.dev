
tuneCOXaic<- function(times, failures, group=NULL, cov.quanti=NULL, cov.quali=NULL,
                        data, model.min=NULL, model.max=NULL){

  .outcome <- paste("Surv(", times, ",", failures, ")")

  if(is.null(model.min)==TRUE){
    if(!(is.null(group))){
      .f0<-as.formula( paste(.outcome, "~",group ))
    }
    else{
      .f0<-as.formula( paste(.outcome, "~1" ))
    }
  }
  else{
    .f0<-as.formula( paste(.outcome, "~",  paste(model.min, collapse = " + "),collapse = " ") )
  }
  if(is.null(model.max)==TRUE){
    if(!(is.null(group))){
      if(is.null(cov.quanti)==F & is.null(cov.quali)==F){
        .f <- as.formula( paste(.outcome, "~", group, "*(",paste(cov.quanti, collapse = " + "), " + ",
                                paste(cov.quali, collapse = " + "),  ")",collapse = " ") )
      }
      if(is.null(cov.quanti)==F & is.null(cov.quali)==T){
        .f <- as.formula( paste(.outcome, "~", group, "*(",paste(cov.quanti, collapse = " + "),")" ))
      }
      if(is.null(cov.quanti)==T & is.null(cov.quali)==F){
        .f <- as.formula( paste(.outcome, "~", group, "*(",paste(cov.quali, collapse = " + "),  ")",collapse = " ") )
      }
      if(is.null(cov.quanti)==T & is.null(cov.quali)==T){
        .f <- as.formula( paste(.outcome, "~", group) )
      }
    }
    else{
      if(is.null(cov.quanti)==F & is.null(cov.quali)==F){
        .f <- as.formula( paste(.outcome, "~", paste(cov.quanti, collapse = " + "), " + ", paste(cov.quali, collapse = " + "),
                                collapse = " ") )
      }
      if(is.null(cov.quanti)==F & is.null(cov.quali)==T){
        .f <- as.formula( paste(.outcome, "~", paste( cov.quanti, collapse = " + ")))
      }
      if(is.null(cov.quanti)==T & is.null(cov.quali)==F){
        .f <- as.formula( paste(.outcome, "~",  paste(cov.quali, collapse = " + "),collapse = " ") )
      }
    }

  }
  else{
    .f<-as.formula( paste(.outcome, "~",  paste(model.max, collapse = " + "),collapse = " ") )
  }

  .fit0<-coxph(.f0, data=data)

  .fit<-coxph(.f, data=data)

  .res.step<-stepAIC(.fit0, scope=formula(.fit), direction="forward", k=2, trace=FALSE)

  return(list(optimal=list(final.model=names(.res.step$assign)),
              results = list(res.step=.res.step))
  )
}



