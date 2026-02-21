

#' Title
#'
#' @param x The GO-terms
#' @keywords internal
#' @return return the edgelist of the GO term

edgelistBP <- function(x){
  matrix.el1 <- data.frame()
  l <- lapply(x, function(t){
    chil.x <- xx.ch[[t]]
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

#' GO biological process (BP) descendant GO-terms as an Edgelist
#'
#' @param goterm A string object of BP GO-term
#'
#' @return A vector comprising of GO-terms and the nodes they are linked to
#' @export
#'
#' @description The GO2DecBP function provides a simple way to represent the biological process (BP) GO term.
#'              The function returns the descendant child nodes of a GO-term. In other words we begin from an
#'              ancestor term and find it's descendant child terms.
#'
#'
#' @examples
#' v <- "GO:0000070" # BP GO term
#' GO2DecBP(v)
#' x <- "GO:0006725"
#' GO2DecBP(x)
#'
# GO2DecBP <- function(goterm){
#   x <- goterm
#   go <- 1
#   finalmat <- data.frame()
#   while(go >= 1){
#     dat <- edgelistBP(x)
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
#
# }

GO2DecBP <- function(goterm){
  x <- goterm
  terms <- lapply(x, function(x){
    offspringbp[[x]]
  })
  return(unlist(terms))
}




