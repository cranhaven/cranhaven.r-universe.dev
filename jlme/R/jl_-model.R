#' Helpers for converting model specifications in R to Julia equivalents
#'
#' @name jl-helpers-model
#' @keywords internal
#' @return A Julia object of type `<JuliaProxy>`
#'
#' @examplesIf check_julia_ok()
#' \donttest{
#' jlme_setup(restart = TRUE)
#'
#' # Set up model data in R
#' x <- mtcars
#' x$cyl_helm <- factor(x$cyl)
#' contrasts(x$cyl_helm) <- contr.helmert(3)
#' colnames(contrasts(x$cyl_helm)) <- c("4vs6", "4&6vs8")
#'
#' # Formula conversion with
#' julia_formula <- jl_formula(mpg ~ am * cyl_helm)
#' julia_formula
#'
#' # Data frame conversion
#' julia_data <- jl_data(x)
#' julia_data
#'
#' # Contrasts construction (`show_code = TRUE` pretty prints the Julia code)
#' julia_contrasts <- jl_contrasts(x, show_code = TRUE)
#' julia_contrasts
#'
#' # Family conversion
#' julia_family <- jl_family("binomial")
#' julia_family
#'
#' stop_julia()
#' }
NULL

#' @rdname jl-helpers-model
#' @param formula A string or formula object
#' @export
jl_formula <- function(formula) {
  x <- formula
  if (is_jl(x)) return(x)
  x <- JuliaFormulae::julia_formula(x)
  res <- tryCatch(
    jl("@formula(%s)", deparse1(x)),
    error = function(e) {
      sanitize_jl_error(e, sys.call(1))
    }
  )
  if (inherits(res, "condition")) {
    stop(res)
  } else {
    res
  }
}

#' @rdname jl-helpers-model
#' @param df A data frame
#' @param cols A subset of columns to make contrast specifiations for
#' @param show_code Whether to print corresponding Julia code as a side-effect
#' @export
jl_contrasts <- function(df, cols = NULL, show_code = FALSE) {
  if (is_jl(df)) return(NULL)
  dict <- construct_contrasts(df, cols = cols, format = show_code)
  if (show_code) {
    cat(dict)
  }
  if (!is.null(dict)) {
    out <- jl(dict)
  } else {
    out <- NULL
  }
  if (show_code) {
    invisible(out)
  } else {
    out
  }
}

#' @rdname jl-helpers-model
#' @export
jl_data <- function(df) {
  if (is_jl(df)) return(df)
  fct_cols <- Filter(is.factor, df)
  df[, colnames(fct_cols)] <- lapply(fct_cols, as.character)
  JuliaConnectoR::juliaCall("Table", jl_put(df))
}

#' @rdname jl-helpers-model
#' @param family The distributional family as string or `<family>` object
#' @export
jl_family <- function(family = c("gaussian", "binomial", "poisson")) {
  if (is_jl(family)) return(family)
  if (is.function(family)) {
    family <- family()
  }
  if (inherits(family, "family")) {
    family <- family$family
  }
  if (is.character(family)) {
    family <- match.arg(family)
    family <- switch(
      family,
      "gaussian" = "Normal",
      "binomial" = "Bernoulli",
      "poisson"  = "Poisson"
    )
    family <- jl("GLM.%s()", family)
    family
  } else {
    stop("Invalid input to the `family` argument.")
  }
}
