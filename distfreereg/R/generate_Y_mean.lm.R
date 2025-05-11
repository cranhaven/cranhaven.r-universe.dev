generate_Y_mean.lm <- function(true_mean, theta, ...){
  stopifnot(is(true_mean, "lm"))
  if(is.null(true_mean[["x"]]) || is.null(true_mean[["y"]]))
    true_mean <- update(true_mean, x = TRUE, y = TRUE)
  tX <- t(true_mean[["x"]])
  return(colSums(theta*tX))
}
