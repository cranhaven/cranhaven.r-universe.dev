#' Creation of a "credpart" object from a vector of class labels
#'
#' \code{create_hard_credpart} creates a "credpart" object from a vector of class labels.
#'
#' @param y A vector of class labels.
#'
#' @return An object of class "credpart".
#' 
#' @export
#'
#' @seealso \code{\link{extractMass}}, \code{\link{create_fuzzy_credpart}}
#'
#' @references
#'  T. Denoeux, S. Li and S. Sriboonchitta. Evaluating and Comparing Soft Partitions: an 
#'  Approach Based on Dempster-Shafer Theory. IEEE Transactions on Fuzzy Systems, 
#'  26(3):1231-1244, 2018.
#' 
#' @examples
#' \dontrun{
#' data(fourclass)
#' y<-kmeans(fourclass[,1:2],4)$cluster
#' clus<-create_hard_credpart(y)
#' summary(clus)
#' }

create_hard_credpart<-function(y){
  # creates a credalpart object from a vector of class labels
  y<-as.numeric(y)
  c<-max(y)
  n<-length(y)
  F<-diag(c)
  mass<-F[y,]
  clus<-extractMass(mass,F,method='create_hard_credpart')
  return(clus)
}
