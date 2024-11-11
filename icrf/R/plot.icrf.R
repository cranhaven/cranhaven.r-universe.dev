#' @rdname plot
#' @name plot
#'
#' @title icrf IMSE rate plot
#'
#' @description 'Plot the error rates or MSE of a randomForest object'
#' (Quoted statements are from
#' \code{randomForest} by Liaw and Wiener unless otherwise mentioned.)
#'
#' @param x an object of \code{icrf} class.
#' @param type 'type of plot.'
#' @param main 'main title of the plot.'
#' @param oob Whether the out-of-bag error should be returned?
#' @param ... 'other graphical parameters.'
#'
#' @return The IMSE (integrated mean squared error) of the \code{icrf} object
#' is invisibly returned. 'If the object has a non-null test component, then the returned
#' object is a matrix where the first' (two) column is the IMSE measure (types 1 and 2), 'and
#' the second column is for the test set.'
#' The rows represent the forest iterations.
#'
#' @note 'If the \code{x} has a non-null \code{test} component, then the test set errors are
#' also plotted.'
#'
#'
#' @examples
#' # rats data example
#' # Note that this is a toy example. Use a larger ntree and nfold in practice.
#' data(rat2)
#' set.seed(1)
#' samp <- sample(1:dim(rat2)[1], 200)
#' rats.train <- rat2[samp, ]
#' rats.test <- rat2[-samp, ]
#' # Note that this is a toy example. Use a larger ntree and nfold in practice.
#' set.seed(2)
#' \dontshow{
#' # 1. formula (currentstatus)
#'  rats.icrf.small <-
#'    icrf(~ dose.lvl + weight + male + cage.no, data = rat2,
#'         data.type = "currentstatus", currentstatus.label = c("survtime", "tumor"),
#'         returnBest = TRUE, ntree=2, nfold=1)
#'  plot(rats.icrf.small)
#' }
#' \donttest{
#'  rats.icrf.small <-
#'    icrf(~ dose.lvl + weight + male + cage.no, data = rat2,
#'         data.type = "currentstatus", currentstatus.label = c("survtime", "tumor"),
#'         returnBest = TRUE, ntree=10, nfold=3)
#'  plot(rats.icrf.small)
#' }
#' @author Hunyong Cho, Nicholas P. Jewell, and Michael R. Kosorok.
#'
#' @references
#' \href{https://arxiv.org/abs/1912.09983}{Cho H., Jewell N. J., and Kosorok M. R. (2020+). "Interval censored
#'  recursive forests"}
#'
#' @export
#' @useDynLib icrf
#'
plot.icrf <- function(x, type="l", main = deparse(substitute(x)), oob = FALSE, ...) {
  test <- !(is.null(x$test$testerror))
  err <- if (oob) x$imse.oob else x$imse.NO
  measure <- colnames(err)
  if(test) err <- cbind(err, x$test$testerror)
  # if(test) colnames(err) <- c("Train", "Test")

  matplot(1:x$nfold, err, type = type, xlab="forests",
          ylab = if(oob) "IMSE (oob)" else "Error (non-oob)",
          main = main, lty = 1:ncol(err),  col = 1:ncol(err), ... = ...)
  legend(1, max(err)*0.9,
         c(paste0(measure, " (train)"),
           if (test) paste0(measure, " (test)")),
         lty = 1:ncol(err), col = 1:ncol(err))
  invisible(err)
}


