#' @title Generate path matrix from data file
#' @name gen_path_mat
#' @description Helper function to generate an empty similarity matrix based on 
#' header of individual data file
#' @param x The data file
#' @param diag Whether diagonal elements should exist 
#' @return Empty matrix with named rows and columns corresponding to individual data files
#' @examples gen_path_mat(data.frame(V1 = rnorm(10, 0, 1), V2 = rnorm(10, 0, 1)))
#' @export
gen_path_mat <- function(x, diag = T){
  mat <- matrix(FALSE, nrow = ncol(x), ncol = ncol(x))
  rownames(mat) <- colnames(mat) <- colnames(x)
  if (diag == F) diag(mat) <- NA
  return(mat)
}