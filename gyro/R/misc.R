#' @title Changes of sign
#' @description Sometimes, the coordinates of the vertices of a polyhedron are
#'   given with changes of sign (with a symbol \strong{+/-}). This function
#'   performs the changes of sign.
#'
#' @param M a numeric matrix of coordinates of some points (one point per row)
#' @param changes either the indices of the columns of \code{M} where the
#'   changes of sign must be done, or \code{"all"} to select all the indices
#'
#' @return A numeric matrix, \code{M} transformed by the changes of sign.
#' @export
#'
#' @importFrom purrr imap
#'
#' @examples library(gyro)
#' library(rgl)
#' ## ~~ rhombicosidodecahedron ~~##
#' phi <- (1 + sqrt(5)) / 2
#' vs1 <- rbind(
#'   c(1, 1, phi^3),
#'   c(phi^2, phi, 2 * phi),
#'   c(2 + phi, 0, phi^2)
#' )
#' vs2 <- rbind(vs1, vs1[, c(2, 3, 1)], vs1[, c(3, 1, 2)]) # even permutations
#' vs <- changesOfSign(vs2)
#' \donttest{open3d(windowRect = c(50, 50, 562, 562), zoom = 0.65)
#' plotGyrohull3d(vs)}
changesOfSign <- function(M, changes = "all"){
  if(!is.matrix(M)) M <- rbind(M)
  if(identical(changes, "all")) changes <- 1L:ncol(M)
  `colnames<-`(as.matrix(do.call(rbind, apply(M, 1L, function(row){
    expand.grid(
      imap(row, ~ (if(.x == 0 || !.y %in% changes) .x else c(-.x, .x)))
    )
  }))), NULL)
}
