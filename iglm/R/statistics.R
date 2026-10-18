#' Compute Statistics
#'
#' @description
#' Computes statistics.
#'
#' @param formula A model `formula` object. The left-hand side should be the
#'   name of a \code{\link{iglm.data}} object available in the calling environment.
#'   Alternatively, the left-hand side can be a \code{iglm.data.list} object to compute statistics
#'   for multiple \code{\link{iglm.data}} objects at once (is, e.g., the normal outcome of all simulations).
#'   See \code{\link{iglm-terms}} for details on specifying the right-hand side terms.

#' @param canonical_names (logical) If `TRUE`, returns canonical term names without label substitution.
#'   Default is `FALSE`.
#'
#' @return A named numeric vector (or matrix if a list of data objects is provided). Each element corresponds to a term in the
#'   `formula`, and its value is the calculated observed feature
#'   for that term based on the data in the \code{\link{iglm.data}} object. The names of the
#'   vector match the (optionally labeled) term names derived from the formula terms.
#'
#' @examples
#' # Create a iglm.data object
#' n_units <- 10
#' neighborhood <- matrix(1, nrow = n_units, ncol = n_units)
#' family_x <- "binomial"
#' family_y <- "binomial"
#' x_attr_data <- rbinom(n_units, 1, 0.5)
#' y_attr_data <- rbinom(n_units, 1, 0.5)
#' z_net_data <- matrix(0, nrow = n_units, ncol = n_units)
#' object <- iglm.data(
#'   z_network = z_net_data, x_attribute = x_attr_data,
#'   y_attribute = y_attr_data, neighborhood = neighborhood,
#'   directed = FALSE, family_x = family_x, family_y = family_y,
#'   n_units = n_units,
#'   label_x = "age", label_y = "income"
#' )
#' statistics(object ~ edges(mode = "local") + attribute_y + attribute_x)
#' statistics(object ~ edges(mode = "local") + attribute_y + attribute_x, canonical_names = TRUE)
#' @export
statistics <- function(formula, canonical_names = FALSE) {
  if (!is.logical(canonical_names) || length(canonical_names) != 1 || is.na(canonical_names)) {
    stop("`canonical_names` must be a single non-missing logical value (TRUE or FALSE).", call. = FALSE)
  }
  tmp_obj <- eval(formula[[2]], envir = environment(formula))
  if (inherits(tmp_obj, "iglm.data.list")) {
    k <- length(tmp_obj)
    lhs_call <- formula[[2]]
    rhs_part <- formula[[3]]
    f_env <- environment(formula)
    formula_list <- lapply(1:k, function(i) {
      new_lhs <- call("[[", lhs_call, i)
      new_formula_call <- call("~", new_lhs, rhs_part)
      as.formula(new_formula_call, env = f_env)
    })
    counts <- lapply(formula_list, function(x) {
      statistics(x, canonical_names = canonical_names)
    })
    counts <- do.call("rbind", counts)
    return(counts)
  } else {
    preprocessed <- formula_preprocess(formula)
    if (length(preprocessed$type_list) > 0) {
      counts <- as.vector(xyz_count_statistics(preprocessed))
      names(counts) <- if (canonical_names) {
        preprocessed$coef_names
      } else {
        format_term_names(preprocessed$coef_names, preprocessed$data_object, canonical_names = FALSE)
      }
      return(counts)
    } else {
      warning("No valid terms specified in the formula. Returning an empty vector.")
      return(c())
    }
  }
}


xyz_count_statistics <- function(preprocessed, ...) {
  n_units_tmp <- length(preprocessed$data_object$x_attribute)
  # browser()
  xyz_count_global(
    z_network = preprocessed$data_object$z_network,
    x_attribute = preprocessed$data_object$x_attribute,
    y_attribute = preprocessed$data_object$y_attribute,
    family_x = preprocessed$data_object$family_x,
    family_y = preprocessed$data_object$family_y,
    attr_x_scale = preprocessed$data_object$scale_x,
    attr_y_scale = preprocessed$data_object$scale_y,
    neighborhood = preprocessed$data_object$neighborhood,
    overlap = preprocessed$data_object$overlap,
    directed = preprocessed$data_object$directed,
    terms = preprocessed$term_names,
    data_list = preprocessed$data_list,
    type_list = preprocessed$type_list,
    n_units = n_units_tmp
  )
}
