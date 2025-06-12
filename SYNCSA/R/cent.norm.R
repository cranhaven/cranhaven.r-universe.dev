#' @title Matrix centralization and standardization
#'
#' @description Internal function for centralization and standardization in a matrix.
#'
#' @encoding UTF-8
#' @param x A matrix
#' @param na.rm Logical argument (TRUE or FALSE) to specify if missing
#' observations are removed (Default na.rm = FALSE).
#' @author Vanderlei Julio Debastiani <vanderleidebastiani@@yahoo.com.br>
#' @seealso \code{\link{syncsa}}
#' @keywords SYNCSA
#' @export
cent.norm <- function (x, na.rm = FALSE)
{
  x.cent <- sweep(x, 2, colMeans(x, na.rm = na.rm), "-")
  x.norm <- apply(x.cent^2, 2, sum, na.rm = na.rm )
  x.cent.norm <- sweep(x.cent, 2, sqrt(x.norm), "/")
  return(x.cent.norm)
}

