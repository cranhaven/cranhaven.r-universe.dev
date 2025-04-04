conv.softsplits <- function(fit,varnames) {
  varindexes <- as.double(factor(fit$var,levels=varnames))
  leaves <- fit$var=="<leaf>"
  varindexes[leaves] <- 0L
  if(any(is.na(varindexes))) {
    stop("the tree requires variables not contained in the data")
  }
  fit$splits[leaves] <- 0
  fit$lb[leaves] <- 0
  fit$ub[leaves] <- 0
  fit$ncat[leaves] <- 0L
  fit$childref[leaves] <- 0L
  nclass <- ncol(fit$yval)
  varindexes <- varindexes-1
  return(list(
    varindexes=as.integer(varindexes),
    splits=as.double(fit$splits),
    ncat=as.integer(fit$ncat),
    lb=as.double(fit$lb),
    ub=as.double(fit$ub),
    childref=as.integer(fit$childref),
    yval=as.double(fit$yval),
    nclass=as.integer(nclass)
  ))
}

