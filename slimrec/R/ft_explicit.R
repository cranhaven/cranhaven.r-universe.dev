#' @title filmtrust explicit dataset
#'
#' @description filmtrust explicit dataset
#'
#' @format A sparse matrix  of class \code{dgCMatrix} with 1508 users and 2071
#'   items. A (i,j) th element is rating provided by user i for item j. The
#'   possible ratings are \code{(0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0)}. A
#'   (i,j) th element is zero if there is no rating value. There are 35494
#'   ratings ( sparsity = 0.0114 ). Source:
#'   \href{http://www.librec.net/datasets/filmtrust.zip}{librec datasets}
#'
"ft_explicit"