
tuneCOXen<- function(times, failures, group=NULL, cov.quanti=NULL, cov.quali=NULL, data, cv=10,
                       parallel=FALSE, alpha, lambda){

  .outcome <- paste("Surv(", times, ",", failures, ")")

  if(!(is.null(group))){
    if(is.null(cov.quanti)==F & is.null(cov.quali)==F){
      .f <- as.formula( paste(.outcome, "~", group, "*(",paste("bs(", cov.quanti, ", df=3)", collapse = " + "), " + ",
                              paste(cov.quali, collapse = " + "),  ")",collapse = " ") )
    }
    if(is.null(cov.quanti)==F & is.null(cov.quali)==T){
      .f <- as.formula( paste(.outcome, "~", group, "*(",paste("bs(", cov.quanti, ", df=3)", collapse = " + "),")" ))
    }
    if(is.null(cov.quanti)==T & is.null(cov.quali)==F){
      .f <- as.formula( paste(.outcome, "~", group, "*(",paste(cov.quali, collapse = " + "),  ")",collapse = " ") )
    }
    if(is.null(cov.quanti)==T & is.null(cov.quali)==T){
      .f <- as.formula( paste(.outcome, "~", group) )
      return(list(optimal=list(alpha=NA,
                               lambda=NA),
                  results = data.frame(alpha=NA,lambda=NA, cvm=NA)))
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
  }

  else{
    if(is.null(cov.quanti)==F & is.null(cov.quali)==F){
      .f <- as.formula( paste(.outcome, "~", paste("bs(", cov.quanti, ", df=3)", collapse = " + "), " + ", paste(cov.quali, collapse = " + "),
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
  }
  .results<-c()
  for( a in 1:length(alpha)){
    .cv.en<-glmnet::cv.glmnet(x=.x, y=.y, family = "cox",  type.measure = "deviance",
                              foldsid="folds", parallel = parallel, alpha=alpha[a],
                              # penalty.factor = c(0, rep(1, .l-1)),
                              lambda=lambda)
    .results<-rbind(.results,
                    cbind(rep(alpha[a],length(.cv.en$lambda)),.cv.en$lambda,.cv.en$cvm))
  }

  colnames(.results)=c("alpha","lambda","cvm")
  .results=data.frame(.results)


  return(list(optimal=list(alpha=.results[which(.results$cvm==min(.results$cvm)),1] ,
                           lambda=.results[which(.results$cvm==min(.results$cvm)),2] ),
                           results = .results))
}



