#' @title Easily get the matrix \code{A} and the vector \code{b}
#' @description Get the matrix \code{A} and the vector \code{b} representing
#'  the linear inequalities with a user-friendly syntax.
#'
#' @param model a "MIP model"; see the example
#'
#' @return A list with the matrix \code{A} and the vector \code{b} for usage in
#'   \code{\link{integrateOverPolyhedron}}.
#' @export
#' @importFrom ompr extract_constraints
#'
#' @examples
#' library(ompr)
#' model <- MIPModel() %>%
#'   add_variable(x) %>% add_variable(y) %>% add_variable(z) %>%
#'   add_constraint(-5 <= x) %>% add_constraint(x <= 4) %>%
#'   add_constraint(-5 <= y) %>% add_constraint(y <= 3 - x) %>%
#'   add_constraint(-10 <= z) %>% add_constraint(z <= 6 - x - y)
#' getAb(model)
getAb <- function(model) {
  ineqs <- extract_constraints(model)
  signs <- ifelse(ineqs[["sense"]] == "<=", 1, -1)
  list(
    "A" = as.matrix(signs * ineqs[["matrix"]]),
    "b" = signs * ineqs[["rhs"]]
  )
}
