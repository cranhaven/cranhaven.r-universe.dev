#' plot booster
#'
#' plots errors of booster model iterations
#'
#' @param x booster object
#' @param y ignored
#' @param ... additional arguments.
#'
#' @return Summary of "booster" object.
#' @importFrom utils tail
#'
#' @rdname plot.booster
#' @keywords internal
#' @export

plot.booster <- function(x, y, ...){
  if (class(x) != "booster") {
    stop("object class must be 'booster'")
  }

  pp <- get(paste0("plot.", x$method, "_adaboost"))
  pp(x)
  return(invisible(x))
}

#' @rdname plot.booster
#' @keywords internal
#' @export
plot.discrete_adaboost <- function(x, y, ...){

    models <- x$models
    err_train <- x$err_train
    err_test <- x$err_test

    if (length(models) > 1) {
      if (!is.null(err_test)) {
        plot(err_train, xlab = "Iteration", ylab = "Error",
             ylim = c(0, max(c(err_train, err_test)))*1.1)
        graphics::lines(err_train)
        graphics::points(err_test, col = "red", pch = 2)
        graphics::lines(err_test, col = "red")
        graphics::legend("topright", legend = c("Train", "Test"), lty = c(1,1),
                         col = c("black", "red"), pch = c(1,2))
      } else {
        plot(err_train, xlab = "Iteration", ylab = "Error",
             ylim = c(min(c(err_train)), max(c(err_train))))
        graphics::lines(err_train)
        graphics::legend("topright", legend = c("Train"), lty = c(1),
                         col = c("black"),
                         pch = c(1))
      }
    } else{
      stop(paste("Not enough iteration for plotting."))
    }
  return(invisible(x))
}


#' @rdname plot.booster
#' @keywords internal
#' @export
plot.real_adaboost <- function(x, y, ...){

    models <- x$models
    err_train <- x$err_train
    err_test <- x$err_test

    if (length(models) > 1) {
      if (!is.null(err_test)) {
        plot(err_train, xlab = "Iteration", ylab = "Error",
             ylim = c(0, max(c(err_train, err_test)))*1.1)
        graphics::lines(err_train)
        graphics::points(err_test, col = "red", pch = 2)
        graphics::lines(err_test, col = "red")
        graphics::legend("topright", legend = c("Train", "Test"), lty = c(1,1),
                         col = c("black", "red"), pch = c(1,2))
      } else {
        plot(err_train, xlab = "Iteration", ylab = "Error",
             ylim = c(min(c(err_train)), max(c(err_train))))
        graphics::lines(err_train)
        graphics::legend("topright", legend = c("Train"), lty = c(1),
                         col = c("black"),
                         pch = c(1))
      }
    } else{
      stop(paste("Not enough iteration for plotting."))
    }

  return(invisible(x))
}


