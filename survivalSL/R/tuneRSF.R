
tuneRSF <- function(times, failures, group=NULL, cov.quanti=NULL,
                         cov.quali=NULL, data, nodesize, mtry, ntree){

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

  old <- options()
  on.exit(options(old))

  options(rf.cores=1, mc.cores=1)

  find.tune.rf.fast<-function(param.test, f, data ){

    res.rsf <- rfsrc(f, data = data, nodesize = param.test[1], mtry = param.test[2],
                          ntree = param.test[3], splitrule="logrank")

    res<-c(param.test[1], param.test[2], param.test[3],  tail(res.rsf$err.rate, 1))

    return(res)
  }

  .grid <-  expand.grid(nodesize=nodesize, mtry=mtry, ntree=ntree)
  .grid=cbind(.grid[,1],.grid[,2], .grid[,3])


  .tune.rf<-apply(.grid,MARGIN=1, FUN=find.tune.rf.fast, f=.f, data=data)

  .res=t(.tune.rf)
  colnames(.res)=c("nodesize","mtry","ntree","error")
  .res=data.frame(.res)

  .mini<-.res[which(.res$error==min(.res$error, na.rm=TRUE) & is.na(.res$error)==FALSE),]
  .mini<-.mini[1,]

  .optimal=list(nodesize=as.numeric(.mini$nodesize),
               mtry=as.numeric(.mini$mtry),
               ntree=as.numeric(.mini$ntree))

  return(list(optimal=.optimal, results = .tune.rf))
}

