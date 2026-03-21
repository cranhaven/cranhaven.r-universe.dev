#' @title filmtrust small dataset
#'
#' @description filmtrust small dataset
#'
#' @format A sparse matrix  of class \code{dgCMatrix} with 100 users and 1000
#'   items. A (i,j) th element is 1 only if user i has rated item j. A (i,j) th
#'   element is zero if there is no rating value. There are 7780 ratings (
#'   sparsity = 0.778 ). This is a simply \code{ft_implicit[1:100,1:1000]}.
#'   Source: \href{http://www.librec.net/datasets/filmtrust.zip}{librec
#'   datasets}
#'
"ft_small"