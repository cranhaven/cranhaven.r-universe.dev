get_intercept = function(formula, data)
  attr(stats::terms(formula, data = data), "intercept")

get_formula_response = function(formula) {

  if (class(formula) != "formula") stop("formula must be of class 'formula'")

  utils::head(all.vars(stats::terms(formula)), n = 1)

}

get_formula_terms = function(formula, data = NULL) {

  if (class(formula) != "formula")  stop("formula must be of class 'formula'")

  attr(stats::terms(formula, data = data), "term.labels")

}

get_protected = function(protected, terms) {

  if (class(protected) == "formula")  protected = get_formula_terms(protected)

  protected = protected[protected %in% terms]

  if (length(protected) == 0) protected = NULL

  protected

}

terms_matrix = function(formula, protected = NULL,
                        keep_intercept = TRUE,
                        keep_interactions = TRUE,
                        data = NULL) {

  formula = stats::as.formula(formula)
  terms = get_formula_terms(formula, data)
  protected = get_protected(protected, terms)

  prot_len = length(protected)
  mat_len = length(terms) - prot_len
  kept_terms = setdiff(terms, protected)

  protected_matrix = matrix(data = rep(TRUE, prot_len * 2 ^ mat_len),
                            nrow = 2 ^ mat_len)

  term_matrix = expand.grid(lapply(rep(FALSE, mat_len), function(x) c(x, TRUE)))
  term_matrix = cbind(protected_matrix, term_matrix)
  colnames(term_matrix) = c(protected, kept_terms)
  term_matrix = term_matrix[, match(terms, names(term_matrix))]
  term_matrix

}

formula_to_call = function(formula, model, data, ...) {

  eval(as.call(c(model,
                 formula,
                 data = substitute(data),
                 alist2(...))))

}

alist2 = function(...) as.list(substitute((...)))[-1]

formula_to_character = function(formula) {
  form = as.character(formula)
  paste(form[[2]], form[[1]], form[[3]])
}
