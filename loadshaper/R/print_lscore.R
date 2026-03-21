#'
#' @title Print Summary of Load Shape Score
#'
#'
#'
#'
#'
#' @param x An object of class \code{lscore},
#' created by the function \code{\link{lscore}}.
#'
#' @param ... \code{NULL}. Used for S3 generic/method consistency.
#'
#'
#' @return \code{NULL}. Prints the summary of the load shape score.
#'
#'
#'
#'
#' @seealso \code{\link{lslin}}, \code{\link{lscore}},
#' \code{\link{lscore}}
#'
#' @note Same as \code{\link{summary.lscore}}
#'
#'
#' @examples
#' loads <- ercot[ercot$Year == 2019, ]$SOUTH
#' # --------------
#' log_loadshape <- lslog(loads, target_lf = 0.5)
#' print(lscore(log_loadshape, type = "acf"))
#' print(lscore(log_loadshape, type = "pacf"))
#' # --------------
#' lin_loadshape <- lslin(loads, target_lf = 0.5)
#' print(lscore(lin_loadshape, type = "acf"))
#' print(lscore(lin_loadshape, type = "pacf"))
#'
#'
#'
#'
#'
#'
#'
#' @method print lscore
#' @export
print.lscore <- function(x, ... = NULL)
{
  score <- x
  row1 <- paste("Max. Lag : ", max(score$lag))
  row2 <- paste("Type : ", score$type)
  row3 <- paste("Weighted MAPE : ",
                round(score$wmape * 100, 2), "%")
  tmp <- data.frame(rbind(row1, row2, row3))
  tmp <- format(tmp, justify = "left")
  names(tmp) <- NULL
  rownames(tmp) <- NULL
  print(tmp, row.names = FALSE)
}
