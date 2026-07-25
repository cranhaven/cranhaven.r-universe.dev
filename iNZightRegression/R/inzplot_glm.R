#' @describeIn inzplot.lm Method for GLMs
#' @export
inzplot.glm <- function(x, ..., env = parent.frame()) {
    inzplot.lm(x, ..., env = env)
}
