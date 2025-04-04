# Boundaries determined by given data to the splits in the tree.
#
# @param fit The soft tree
# @param d The data set
bounds.by.esd <- function(fit, d) {
  if (any(fit$ncat >= 2)) {
    stop("the 'bounds by ESD' softening is not supported on trees with categorial splits")
  }
  varindexes <- factor(fit$var,levels=names(d))
  #needed.data <- as.matrix(newdata[sort(unique(varindexes))])
  ndata <- as.integer(nrow(d))
  class.column.name <- all.vars(terms(fit))[1] # Name of the dependent variable
  varnames <- colnames(d)
  varnames <- varnames[varnames!=class.column.name]
  varindexes <- as.double(factor(fit$var,levels=varnames))
  leaves <- fit$var=="<leaf>"
  varindexes[leaves] <- 0
  if(any(is.na(varindexes))) {
    stop("the tree requires variables not contained in the data")
  }
  fit$splits[leaves] <- 0
  fit$lb[leaves] <- 0
  fit$ub[leaves] <- 0
  fit$ncat[leaves] <- 0
  nclass <- as.integer(ncol(fit$yval))
  data.matrix <- as.double(as.matrix(d[,varnames]))
  class.names <- c("0","1")  #todo: get it from names(fit$yval)
  class <- as.integer(as.double(factor(d[[class.column.name]],levels=class.names))-1)
  dim <- as.integer(ncol(d))
  treesize <- as.integer(nrow(fit))
  varindexes <- as.integer(varindexes-1)
  splits <- as.double(fit$splits)
  ncat <- as.integer(fit$ncat)
  childref <- as.integer(fit$childref)
  yval <- as.double(as.matrix(fit$yval))
  bounds <- as.double(c(fit$lb,fit$ub))
  temp <- .C("bounds_by_esd",
    data.matrix,
    class,
    ndata,
    dim,
    treesize,
    varindexes,
    splits,
    ncat,
    childref,
    yval,
    nclass,
    bounds=bounds,
    PACKAGE="SplitSoftening",
    NAOK=TRUE)
  temp <- as.data.frame(matrix(temp$bounds,ncol=2))
  names(temp) <- c("lb","ub")
  return(temp)
}

#' Soften splits separately using error standard deviation.
#'
#' Set boundaries determined by given data to the splits in the tree,
#' such that in any inner node if its splitting value would be moved there,
#' then the number of misclassified cases in this node would be one standard deviation over the actual misclassification.
#'
#' This is the same approach as C4.5 uses for "probabilistic splits"
#'
#' @references
#' {Quinlan, J. Ross} {(1993)}, \emph{C4.5: programs for machine learning}, {San Francisco, CA, USA}: {Morgan Kaufmann Publishers Inc.} 
#' 
#' @param fit The soft tree
#' @param d The data set
#'
#' @export
softening.by.esd <- function(fit, d) {
  b <- bounds.by.esd(fit, d)
  splits <- !is.na(fit$splits)
  #fix illegal values (inroduced probably by rounding errors)
  fit$lb[splits] <- apply(rbind(b$lb[splits],fit$splits[splits]),2,min)
  fit$ub[splits] <- apply(rbind(b$ub[splits],fit$splits[splits]),2,max)
  return(fit)
}

