#' Model distribution family display-er
#'
#' Prints the family and link functions of a model in a short way
#' @keywords internal
f_disp <- function(model) {
  fam_name <- fam_obtainer(model)
  links <- link_printer(model)
  coefs <- names(links)
  return(paste0("Family: ", fam_name, "\n",
                "Links: ", paste(coefs, links, sep = ": ", collapse = ", ")))
}

#' Model formulas printer
#'
#' Prints the model formulas of all parameters
#' @importFrom methods is
#' @keywords internal
formula_printer <- function(model) {

  # GAMLSS
  if (is(model, "gamlss")) {
    form_names <- names(model)
    form_names <- form_names[grepl("formula", form_names)]
    all_forms <- model[form_names]
    return(all_forms)
  }

  # BAMLSS
  if (is(model, "bamlss")) {
    return(model$formula)
  }

}

#' Model link functions printer
#'
#' Prints the model links of all parameters
#' @importFrom methods is
#' @keywords internal
link_printer <- function(model) {

  # GAMLSS
  if (is(model, "gamlss")) {
    form_names <- names(model)
    form_names <- form_names[grepl("link", form_names)]
    all_links <- model[form_names]
    return(all_links)
  }

  # BAMLSS
  if (is(model, "bamlss")) {
    return(model$family$links)
  }

}
