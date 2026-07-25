#' Summary method for linear models
#'
#' @param x an `lm`, `glm`, or `svyglm` object
#' @param ... additional arguments passed to `iNZightSummary`
#' @param env the environment for evaluating things (e.g., bootstraps)
#' @return An object of class `summary.lm`, `summary.glm`, or
#'         `summary.svyglm`.
#'
#' @seealso [iNZightSummary]
#' @md
#' @export
inzsummary.lm <- function(x, ..., env = parent.frame()) {
    iNZightSummary(x, ..., env = env)
}
