#' @importFrom stats median

#' @export

Prototypes <- function(label, x, prox, nProto = 5, nNbr = floor((min(table(label))-1)/nProto)) {
  foo <- function(xx) {
    ifelse(is.numeric(xx),median(xx,na.rm=TRUE),names(which.max(table(xx))))
  }
  getProto <- function(whichLab) {
    l_prox <- prox
    l_label <- label
    xd <- list()
    for(i in 1:nProto) {
      ## Find the nearest nNbr neighbors of each case
      ## (including the case itself). 
      idx <- t(apply(l_prox, 1, order, decreasing=TRUE)[1:nNbr,])
      ## Find the class labels of the neighbors.
      cls <- l_label[idx]
      dim(cls) <- dim(idx)
      ## Count the number of neighbors in the class for each case.
      ncls <- rowSums(cls==whichLab)
      ## Find the case with most neighbors in that class.
      clsMode <- sample(which(ncls == max(ncls)),size=1)
      ## Identify the neighbors of the class mode that are of the target class.
      nbrList <- idx[clsMode,][label[idx[clsMode,]]==whichLab]
      ## Get the X data for the neighbors of the class mode.
      xd[[i]] <- character()
      for(k in 1:ncol(x)) xd[[i]][k] <- foo(x[nbrList,k])
      l_prox <- l_prox[-nbrList,-nbrList]
      l_label <- l_label[-nbrList]
    }
    res <- do.call('rbind',xd)
    colnames(res) <- colnames(x)
    return(res)
  }
  set.seed(1)
  label <- as.character(label)
  clsLabel <- unique(label)
  xdat <- lapply(clsLabel,getProto)
  names(xdat) <- clsLabel
  xdat
}