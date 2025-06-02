#' Creation of a "credpart" object from a from a fuzzy or possibilistic partition matrix
#'
#' \code{create_fuzzy_credpart} creates a "credpart" object from a fuzzy or possibilistic partition matrix.
#'
#' @param U A fuzzy or possibilistic partition matrix of size n*c, wheer c is the nmber of clusters.
#'
#' @return An object of class "credpart".
#' 
#' @export
#'
#' @seealso \code{\link{extractMass}},\code{\link{create_hard_credpart}}
#'
#' @references
#'  T. Denoeux, S. Li and S. Sriboonchitta. Evaluating and Comparing Soft Partitions: an 
#'  Approach Based on Dempster-Shafer Theory. IEEE Transactions on Fuzzy Systems, 
#'  26(3):1231-1244, 2018.
#' 
#' @examples
#' \dontrun{
#' library(fclust)
#' U<-FKM(fourclass[,1:2],4)$U
#' clus<-create_fuzzy_credpart(U)
#' summary(clus)
#' }

create_fuzzy_credpart<-function(U){
  # creates a credpart object from a fuzzy or possibilistic partition matrix
  c<-ncol(U)
  F<-rbind(rep(0,c),diag(c))
  mass<-cbind(1-rowSums(U),U)
  clus<-extractMass(mass,F,method='create_fuzzy_credpart')
  return(clus)
}