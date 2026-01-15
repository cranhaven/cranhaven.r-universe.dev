#' @title Calculate XRsX and XRsY
#' @description internal function to calculate crossproducts within pleioR.
#' @param id_matrix matrix of IDs
#' @param sets_rs list of inverses of matrix R
#' @param xx numeric vector with crossproducts of the X matrix
#' @param xy matrix with X transpose Y products.
#' @export
xrsx_xrsy <- function(id_matrix, sets_rs, xx, xy){
  trait_labels <- colnames(id_matrix)[-ncol(id_matrix)]
  xrsx <- diag(ncol(id_matrix) - 1)
  colnames(xy) <- colnames(xrsx) <- rownames(xrsx) <- trait_labels
  xrsy <- xrsx[1,]
  onetrait_sets <- rowSums(id_matrix[, -ncol(id_matrix), drop = F]) == 1

  for (trait_i in trait_labels){
    xrsyij <- 0
    for (trait_j in trait_labels){
      xrsxij <- 0
      id_i <- id_matrix[, trait_i] == 1
      id_j <- id_matrix[, trait_j] == 1
      id_1 <- id_i & onetrait_sets
      if (trait_i == trait_j & any(id_1)){
        xrsxij <- xx[which(id_1)] * as.numeric(sets_rs[which(id_1)])
        xrsyij <- xrsyij + xy[id_1, trait_i] * as.numeric(sets_rs[which(id_1)])
      }
      for (i in unname(which(id_j & id_i & !id_1))){
        rs_i <- sets_rs[[i]][trait_i, trait_j]
        xrsxij <- xrsxij + xx[i] * rs_i
        xrsyij <- xrsyij + xy[i, trait_j] * rs_i
      }
      xrsx[trait_i, trait_j] <- xrsxij
    }
    xrsy[trait_i] <- xrsyij
  }

  return(list('xrsx' = xrsx, 'xrsy' = xrsy))
}
