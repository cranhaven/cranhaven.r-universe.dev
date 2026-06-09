#' Apply Formulas to a Model
#'
#' `fapply` returns a list of the same length as `formulas`. Each element is
#'    the result of applying `model`the the corresponding element of `formulas`.
#'
#' This is a member of the [`apply`][base::lapply] family. It is similar to
#'    [`lapply`][base::lapply], but handles the [`call`][base::call] slightly
#'     differently. This makes the output prettier.
#'
#' @export
#' @param formulas a list of formulas or objects coercible to formula by
#'    [stats::as.formula].
#' @param model a function taking a [`formula`][stats::formula] as its first
#'    argument.
#' @param ... additional arguments to be passed to \code{model}.
#' @return \code{fapply} returns a list of evaluated function calls.
#' @examples
#' formulas = subformula(mpg ~ cyl + disp, protected = ~ cyl)
#' fapply(formulas, lm, data = mtcars) # Pretty output.
#' lapply(formulas, lm, data = mtcars) # Less pretty output.

fapply = function(formulas, model, ...) {

  model = substitute(model)

  formulas = lapply(formulas, stats::as.formula)

  models = lapply(formulas, function(f) formula_to_call(f, model, ...))

  stats::setNames(models, sapply(formulas, formula_to_character))

}
