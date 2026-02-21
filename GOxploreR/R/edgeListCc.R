#' Title
#'
#' @param x The GO terms
#' @keywords internal
#' @return return the edgelist of the GO term
#'
#'
edgelistCC <- function(x){
  matrix.el1 <- data.frame()
  l <- lapply(x, function(t){
    chil.x <- xx.ch2[[t]]
    if(length(chil.x) != 0){
      matrix.el <- data.frame()
      for(i in 1:length(chil.x)){
        matrix.el[i,1] <- chil.x[i]
        matrix.el[i,2] <- t
      }
      matrix.el1 <- rbind(matrix.el1,matrix.el)
    }
  })
  for (j in 1:length(l)) {
    matrix.el1 <- rbind(matrix.el1,l[[j]])

  }
  return(matrix.el1)
}


#' GO cellular component (CC) descendant GO-terms as an Edgelist
#'
#' @param goterm A string object of CC GO-terms
#'
#' @return A vector comprising of GO-terms and the nodes they are linked to
#' @export
#'
#' @description The GO2DecCC function provides a simple way to represent the cellular component (CC) GO-terms.
#'              The function returns the descendant child nodes of a GO-term. In other words we begin from an
#'              ancestor term and find it's descendant child terms.
#'
#'
#' @examples
#' v <- "GO:0000799" # CC GO term
#' GO2DecCC(v)
#' x <- "GO:0043231"
#' GO2DecCC(x)
#'
# GO2DecCC <- function(goterm){
#   x <- goterm
#   go <- 1
#   finalmat <- data.frame()
#   while(go >= 1){
#     dat <- edgelistCC(x)
#     finalmat <- rbind(finalmat,dat)
#     x <- dat[,1]
#     x <- x[!is.na(x)]
#     go <- length(x)
#   }
#   finalmat <- unique(finalmat)
#   finalmat <- finalmat[-which(is.na(finalmat[,1])),]
#   rownames(finalmat) <- NULL
#   colnames(finalmat) <- NULL
#
#   if(all(is.na(finalmat))){
#     return(NULL)
#   }
#   else{
#     return(as.vector(finalmat[,c(2,1)]))
#   }
#
# }

GO2DecCC <- function(goterm){
  x <- goterm
  terms <- lapply(x, function(x){
    offspringcc[[x]]
  })
  return(unlist(terms))
}




