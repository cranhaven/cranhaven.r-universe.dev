#' Valid counts
#'
#' Enumerate matrix of valid counts for a vector of values
#'
#' @param x Vector
#' @param c.prev Calculated chat from layer l-1
#'
#' @return NULL
#'
valid.counts <- function(x,c.prev){
  #Outputs a 2-column matrix of valid counts
  vec = seq(from = 0, to = c.prev)
  mat = matrix(vec, ncol = 1)

  mat  = expand.mat(mat, vec)
  mat = mat[rowSums(mat) %in% x, ]

  if(!is.matrix(mat)){
    return(mat)
  }
  else if(is.matrix(mat) & nrow(mat)<1){
    return(NULL)
  }
  else{
    #mat = mat[rowSums(mat) %in% x, ]
    idx = split(seq(nrow(mat)), rowSums(mat))
    return(lapply(idx, function(i, x) x[i, ], mat))
  }
}


