.onLoad <- function(libname, pkgname) {
  # Suppress YAML warning. See https://github.com/rstudio/rstudio/issues/7545
  options("yaml.eval.expr" = TRUE)
}

#' Parse raw and TeX formulae from model formula
#'
#' @param model_formula Model formula.
#'
#' @return List of raw and TeX formulae.
#' @noRd
parse_model_formula <- function(model_formula = c("lin-quad", "lin")) {
  # Validate parameters
  model_formula <- match.arg(model_formula)

  # Parse formula
  if (model_formula == "lin-quad") {
    fit_formula_raw <- "aberr ~ -1 + coeff_C + coeff_alpha + coeff_beta"
    fit_formula_tex <- "\\lambda = C + \\alpha D + \\beta D^{2}"
  } else if (model_formula == "lin") {
    fit_formula_raw <- "aberr ~ -1 + coeff_C + coeff_alpha"
    fit_formula_tex <- "\\lambda = C + \\alpha D"
  }

  formula_list <- list(
    fit_formula_raw = fit_formula_raw,
    fit_formula_tex = fit_formula_tex
  )

  return(formula_list)
}


#' Parse coefficient names from model formula
#'
#' Fix coefficient names (\code{coeff_C}, \code{coeff_alpha}, \code{coeff_beta}) on manual coeffiecient matrix.
#'
#' @param model_formula Model formula.
#'
#' @return Vector of coefficient names
#' @noRd
names_from_model_formula <- function(model_formula = c("lin-quad", "lin")) {
  # Validate parameters
  model_formula <- match.arg(model_formula)

  # Parse formula
  if (model_formula == "lin-quad") {
    names <- c("coeff_C", "coeff_alpha", "coeff_beta")
  } else if (model_formula == "lin") {
    names <- c("coeff_C", "coeff_alpha")
  }

  return(names)
}

#' Fix coefficient matrix names
#'
#' Fix coefficient names (\code{coeff_C}, \code{coeff_alpha}, \code{coeff_beta}) to display properly on reports.
#'
#' @param data Data frame or matrix.
#' @param type Type of name replacement. Either "rows" or "cols".
#' @param output Type of output in which the data is rendered. Either "kable" or "rhot".
#'
#' @return Data frame with fixed rownames and colnames.
#' @noRd
fix_coeff_names <- function(data, type = c("rows", "cols"), output = c("kable", "rhot")) {
  # Validate parameters
  type <- match.arg(type)
  output <- match.arg(output)

  # Select name functions
  if (type == "rows") {
    names_assign <- base::`rownames<-`
    names_read <- base::rownames
  } else if (type == "cols") {
    names_assign <- base::`colnames<-`
    names_read <- base::colnames
  }

  # Select coefficient replacements
  coeffs_old <- c("coeff_C", "coeff_alpha", "coeff_beta")

  if (output == "rhot") {
    coeffs_new <- c("C", rlang::as_utf8_character("\u03B1"), rlang::as_utf8_character("\u03B2"))
  } else if (output == "kable") {
    coeffs_new <- c("$C$", "$\\\\alpha$", "$\\\\beta$")
  }

  # Replace coefficients
  data <- data %>%
    names_assign(
      names_read(.) %>%
        gsub(coeffs_old[[1]], coeffs_new[[1]], .) %>%
        gsub(coeffs_old[[2]], coeffs_new[[2]], .) %>%
        gsub(coeffs_old[[3]], coeffs_new[[3]], .)
    )
  return(data)
}

#' Fix data frame variable names
#'
#' Fix data colnames to display properly on reports.
#'
#' @param data Data frame or matrix.
#' @param type Type of input data. Either "count" and "case".
#' @param output Type of output in which the data is rendered. Only "kable" supported at the moment.
#'
#' @return Data frame with fixed colnames.
#' @noRd
fix_count_data_names <- function(data, type = c("count", "case"), output = "kable") {
  # Validate parameters
  type <- match.arg(type)
  output <- match.arg(output)

  # Parse headers
  col_names <- colnames(data)

  counts_headers <- grep("C[0-9]+", x = col_names, value = TRUE)
  counts_headers <- paste0("$C_{", regmatches(counts_headers, regexpr("[0-9]+", counts_headers)), "}$")

  if (type == "count") {
    other_headers <- grep("C[0-9]+", x = col_names, value = TRUE, invert = TRUE) %>%
      gsub("^D$", "$D$ (Gy)", .) %>%
      gsub("^N$", "$N$", .) %>%
      gsub("^X$", "$X$", .) %>%
      gsub("^mean$", "$\\\\bar{y}$", .) %>%
      gsub("^var$", "$\\\\hat{\\\\sigma}^{2}$", .) %>%
      gsub("^DI$", "$\\\\hat{\\\\sigma}^{2} / \\\\bar{y}$", .) %>%
      gsub("^u$", "$u$", .)

    if (ncol(data) > 3) {
      colnames(data) <- c(other_headers[1:3], counts_headers, other_headers[4:length(other_headers)])
    } else {
      colnames(data) <- c(other_headers[1:3])
    }
  } else if (type == "case") {
    other_headers <- grep("C[0-9]+", x = col_names, value = TRUE, invert = TRUE) %>%
      gsub("^N$", "$N$", .) %>%
      gsub("^X$", "$X$", .) %>%
      gsub("^y$", "$y$", .) %>%
      gsub("^y_err$", "$\\\\hat{\\\\sigma}_{y} / \\\\sqrt{N}$", .) %>%
      gsub("^Fp$", "$F_{P}$", .) %>%
      gsub("^Fp_err$", "$\\\\hat{\\\\sigma}_{P} / \\\\sqrt{N}$", .) %>%
      gsub("^Fg$", "$F_{G}$", .) %>%
      gsub("^Xc$", "$X_{C}$", .) %>%
      gsub("^Fg_err$", "$\\\\hat{\\\\sigma}_{G} / \\\\sqrt{N}$", .) %>%
      gsub("^DI$", "$\\\\hat{\\\\sigma}^{2} / \\\\bar{y}$", .) %>%
      gsub("^u$", "$u$", .)

    colnames(data) <- c(other_headers[1:2], counts_headers, other_headers[3:length(other_headers)])
  }

  return(data)
}

#' Convert case of a string to title case
#'
#' @param string String to modify.
#'
#' @return String as title case.
#' @noRd
to_title <- function(string) {
  split_string <- strsplit(string, " ")[[1]]
  string <- paste(
    toupper(substring(split_string, 1, 1)),
    substring(split_string, 2),
    sep = "",
    collapse = " "
  )

  return(string)
}

#' Global fitting formulas
#'
#' @noRd
list_fitting_formulas <- function() {
  fitting_formulas <- list(
    "Linear quadratic" = c("lin-quad") %>%
      `names<-`(c(rlang::as_utf8_character("\u03BB = C + \u03B1D + \u03B2D\u00B2"))),
    "Linear" = c("lin") %>%
      `names<-`(c(rlang::as_utf8_character("\u03BB = C + \u03B1D")))
  )

  return(fitting_formulas)
}

#' Generalised fit coefficients
#' @noRd
generalise_fit_coeffs <- function(fit_coeffs_vec) {
  general_fit_coeffs <- numeric(length = 3L) %>%
    `names<-`(c("coeff_C", "coeff_alpha", "coeff_beta"))

  for (var in names(fit_coeffs_vec)) {
    general_fit_coeffs[[var]] <- as.numeric(fit_coeffs_vec[[var]])
  }

  return(general_fit_coeffs)
}

#' Generalised variance-covariance matrix
#' @noRd
generalise_fit_var_cov_mat <- function(fit_var_cov_mat) {
  general_fit_var_cov_mat <- matrix(0, nrow = 3, ncol = 3) %>%
    `row.names<-`(c("coeff_C", "coeff_alpha", "coeff_beta")) %>%
    `colnames<-`(c("coeff_C", "coeff_alpha", "coeff_beta"))

  for (x_var in rownames(fit_var_cov_mat)) {
    for (y_var in colnames(fit_var_cov_mat)) {
      general_fit_var_cov_mat[x_var, y_var] <- as.numeric(fit_var_cov_mat[x_var, y_var])
    }
  }

  return(general_fit_var_cov_mat)
}

#' Parse confidence intervals into text form
#' @noRd
parse_conf_int_text <- function(conf_int) {
  if (length(conf_int) == 1) {
    conf_int_text <- paste0("(", round(100 * conf_int, 0), "%", ")")
  } else if (length(conf_int) == 2) {
    conf_int_text <- paste0(
      "(", round(100 * conf_int[["curve"]], 0), "%",
      "-", round(100 * conf_int[["yield"]], 0), "%", ")"
    )
  } else {
    conf_int_text <- NULL
  }
  return(conf_int_text)
}

#' Validate names
#' @noRd
match_names <- function(x, lookup) {
  unmatched <- setdiff(x, lookup)
  if (length(unmatched) > 0) {
    stop("Valid names are ", paste(lookup, collapse = ", "))
  } else {
    return(x)
  }
}
