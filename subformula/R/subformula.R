#' Calculate Subformulas
#'
#' A formula \code{sub} is a subformula of \code{formula} if *(i)* all the terms
#'    on the right hand side of \code{sub} are terms of \code{formula} and *(ii)*
#'    their left hand sides are identical. \code{subformula} finds every
#'    subformula of \code{formula} that contains each term in \code{protected}.
#'
#' Protected terms will appear in every subformula. If the supplied formula
#'    includes the term \code{0} or \code{-1}, none of the subformulas will
#'    include the intercept. Otherwise, the intercept will be interpreted as
#'    being protected. If \code{formula} is is coerced to a \code{formula}
#'    object, its associated [environment][base::environment] will be
#'    \code{NULL}. All subformulas will inherit their \code{.Environment}
#'    attribute from \code{formula}.
#'
#' @export
#' @param formula an object of class "[`formula`][stats::formula]" (or one
#'    that can be coerced to that class via [`formula`][stats::formula]).
#' @param protected a vector or formula specifying which covariates are
#'    \emph{protected}. Protected formulas appear in all subformulas.
#' @param data an optional data frame (or object coercible by
#'   [`as.data.frame`][base::as.data.frame] to a data frame). Used to fill
#'   out formulas as \code{y ~ .}.
#'
#' @return \code{subformula} returns a list of \code{formula} objects.
#' @examples
#' subformula(z ~ x + y)
#' subformula(y ~ x + y + y^2, protected = ~ x)
#' subformula(y ~ x + y + t + I(t^2), protected = c("x","I(t^2)"))

subformula = function(formula,
                       protected = NULL,
                       data = NULL) {

  env = if (inherits(formula, "formula"))
    attr(formula, ".Environment") else NULL

  formula = stats::as.formula(formula)
  response = get_formula_response(formula)
  terms = get_formula_terms(formula, data)
  protected = get_protected(protected, terms)
  intercept = get_intercept(formula, data)

  term_matrix_ = terms_matrix(formula = formula,
                              protected = protected,
                              data = data)

  if (intercept == 0) {
    term_matrix_ = cbind(term_matrix_, rep(TRUE, nrow(term_matrix_)))
    colnames(term_matrix_)[length(terms) + 1] = intercept
    forms = apply(term_matrix_, 1, function(x) c(terms, 0)[x])
  } else {
    forms = apply(term_matrix_, 1, function(x) terms[x])
  }


  forms = lapply(forms, function(x)
    do.call(paste, as.list(c(unlist(x), sep = " + "))))
  forms = lapply(forms, function(x) paste0(response, " ~ ", x))

  if (is.null(protected))
    forms[[1]] = if (intercept == 1) paste0(response, " ~ 1") else NULL

  sapply(forms, function(form) stats::as.formula(form, env = env))

}
