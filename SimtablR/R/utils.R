# Core Computational and Input Utilities
#
# Shared primitive helpers, type detection, distribution symmetry heuristics,
# numerical safeguards, argument validators, and optional dependency guards.

#########
# OPERATORS AND VARIABLE TYPE DETECTION
# NULL coalescing and heuristic detection of continuous versus categorical variables.

#' Coalesce NULL value with fallback alternative
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a

#' Drop the sign from formatted numbers that round to zero
#'
#' `sprintf("%.2f", -0.001)` and `formatC()` give `"-0.00"`, which reads as a
#' real negative value. Applied to already formatted strings so any grouping or
#' decimal mark is preserved; non-zero digits keep their sign.
#' @keywords internal
#' @noRd
.unsign_zero <- function(txt) {
  zero <- !is.na(txt) & grepl("^-[0.,[:space:]]*$", txt) & grepl("0", txt, fixed = TRUE)
  txt[zero] <- substring(txt[zero], 2L)
  txt
}

#' Format numbers to fixed decimals without a negative zero
#' @keywords internal
#' @noRd
.fmt_fixed <- function(x, digits) {
  .unsign_zero(sprintf(paste0("%.", digits, "f"), x))
}

#' Detect whether variable is summarized as continuous or categorical
#' @keywords internal
#' @noRd
.detect_var_type <- function(x, override = NULL, strict = FALSE) {
  if (!is.null(override)) {
    if (!is.character(override) || length(override) != 1L || is.na(override) || !nzchar(trimws(override))) {
      simtab_abort_input(c(
        "Variable type override must be a single non-missing string.",
        "i" = "Received an object of class {.cls {class(override)[[1]]}} of length {length(override)}.",
        "v" = "Use {.code var.type = \"continuous\"} or {.code var.type = \"categorical\"}."
      ))
    }
    spec <- tolower(trimws(override))
    if (spec %in% c("continuous", "cont", "numeric", "num")) {
      return("continuous")
    }
    if (spec %in% c("categorical", "cat", "factor", "character", "discrete")) {
      return("categorical")
    }
    simtab_abort_input(c(
      "Unknown variable type {.val {override}}.",
      "i" = "Variable types name how a column is summarised, not its storage mode.",
      "v" = "Use {.val continuous} or {.val categorical}."
    ))
  }
  if (is.numeric(x) && !is.factor(x)) {
    if (!strict && .is_low_cardinality_coded(x)) {
      return("categorical")
    }
    return("continuous")
  }
  "categorical"
}

#' Test whether numeric vector represents integer-coded categorical levels
#' @keywords internal
#' @noRd
.is_low_cardinality_coded <- function(x, max_levels = 7L, min_n = 20L) {
  xc <- x[!is.na(x)]
  if (length(xc) == 0 || any(!is.finite(xc)) || any(xc != round(xc))) {
    return(FALSE)
  }
  n_unique <- length(unique(xc))
  n_unique == 2L || (n_unique <= max_levels && length(xc) >= min_n)
}

#' Resolve per-variable argument from scalar, named list, or default value
#' @keywords internal
#' @noRd
.resolve_per_var <- function(arg, var, default) {
  if (is.null(arg)) {
    return(default)
  }
  # A resolved style object is a single global value, never a per-variable
  # mapping. It is itself a (named) list, so it must be short-circuited before
  # the per-variable branch below, otherwise it is misread as a var->value map,
  # `var` is not found among its field names, and it is silently dropped to the
  # default style.
  if (inherits(arg, "simtab_style")) {
    return(arg)
  }
  if (is.list(arg) || (!is.null(names(arg)) && any(nzchar(names(arg))))) {
    if (!is.null(names(arg)) && var %in% names(arg)) {
      return(arg[[var]])
    }
    return(default)
  }
  # Unnamed scalar: applies to all variables.
  arg
}

#########
# DISTRIBUTION SHAPE AND SUMMARY DECISION HEURISTICS
# Sample skewness calculation, numerical stability safeguards, and parametric decision rules.

#' Calculate sample skewness b1 on complete cases
#' @keywords internal
#' @noRd
.sample_skewness_b1 <- function(x) {
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 3) {
    return(NA_real_)
  }
  scale <- max(abs(x))
  if (!is.finite(scale) || scale == 0) {
    return(NA_real_)
  }
  scaled <- x / scale
  centered <- scaled - mean(scaled)
  m2 <- mean(centered^2)
  if (!is.finite(m2) || m2 <= 0) {
    return(NA_real_)
  }
  m3 <- mean(centered^3)
  m3 / (m2^1.5)
}

#' Ensure continuous variable contains no infinite values
#' @keywords internal
#' @noRd
.check_finite_continuous <- function(x, var) {
  if (any(is.infinite(x))) {
    simtab_abort_input(c(
      "Continuous variable {.val {var}} contains non-finite values (Inf or -Inf).",
      "i" = "Summary statistics and tests are undefined on infinite values.",
      "v" = "Recode the infinities to {.code NA}, or exclude the affected rows."
    ))
  }
  invisible(x)
}

#' Calculate numerically stable sample standard deviation
#' @keywords internal
#' @noRd
.sample_sd_stable <- function(x) {
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  if (length(x) < 2L) {
    return(NA_real_)
  }
  scale <- max(abs(x))
  if (scale == 0) {
    return(0)
  }
  stats::sd(x / scale) * scale
}

#' Calculate standard error of sample skewness for sample size n
#' @keywords internal
#' @noRd
.se_skewness <- function(n) {
  n <- as.integer(n)
  if (is.na(n) || n < 3) {
    return(NA_real_)
  }
  sqrt(6 * n * (n - 1) / ((n - 2) * (n + 1) * (n + 3)))
}

#' Evaluate sample size and skewness to choose parametric or non-parametric summary
#' @keywords internal
#' @noRd
.auto_summary_decision <- function(x) {
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  n <- length(x)
  base <- list(
    requested = "auto",
    n = as.integer(n),
    skew = NA_real_,
    se = .se_skewness(n),
    statistic = NA_real_,
    threshold = NA_real_,
    decision = "median",
    reason = NA_character_
  )
  if (n < 3) {
    base$reason <- "N < 3; median [IQR] used because skewness is not estimable."
    return(base)
  }

  skew <- .sample_skewness_b1(x)
  base$skew <- skew
  if (!is.finite(skew)) {
    base$reason <- "zero variance; median [IQR] used because skewness is not estimable."
    return(base)
  }

  se <- .se_skewness(n)
  base$se <- se
  if (n <= 300) {
    threshold <- if (n < 50) 1.96 else 2.58
    statistic <- abs(skew / se)
    is_mean <- statistic <= threshold
    band <- sprintf(
      "%s and |skewness / SE| %s %.2f",
      if (n < 50) "N < 50" else "50 <= N <= 300",
      if (is_mean) "<=" else ">",
      threshold
    )
  } else {
    threshold <- 1
    statistic <- abs(skew)
    is_mean <- statistic < threshold
    band <- sprintf("N > 300 and |skewness| %s 1", if (is_mean) "<" else ">=")
  }
  decision <- if (is_mean) "mean" else "median"
  reason <- paste0(
    band,
    if (is_mean) "; distribution approximately symmetric." else "; median [IQR] used owing to skewness."
  )

  base$statistic <- statistic
  base$threshold <- threshold
  base$decision <- decision
  base$reason <- reason
  base
}

#########
# INPUT GUARDS AND OPTIONAL DEPENDENCY RESOLUTION
# Validations for data frames, confidence levels, decimal digits, and suggested packages.

#' Ensure input object is a valid data frame
#' @keywords internal
#' @noRd
.check_data_frame <- function(data) {
  if (!is.data.frame(data)) {
    simtab_abort_input(c(
      "{.arg data} must be a data frame.",
      "i" = "Received an object of class {.cls {class(data)[[1]]}}.",
      "v" = "Convert it first, e.g. {.code as.data.frame(data)}."
    ))
  }
  invisible(TRUE)
}

#' Validate confidence level is a scalar numeric between 0 and 1
#' @keywords internal
#' @noRd
.check_conf_level <- function(conf.level) {
  if (!is.numeric(conf.level) || length(conf.level) != 1 ||
      is.na(conf.level) || conf.level <= 0 || conf.level >= 1) {
    simtab_abort_input(c(
      "{.arg conf.level} must be between 0 and 1.",
      "i" = "Received: {.val {conf.level}}.",
      "v" = "Use {.code conf.level = 0.95} for conventional 95% intervals."
    ))
  }
  invisible(TRUE)
}

#' Validate decimal digits argument is a scalar integer between 0 and 10
#' @keywords internal
#' @noRd
.check_d <- function(d) {
  if (!is.numeric(d) || length(d) != 1 || is.na(d) || d < 0 || d > 10) {
    simtab_abort_input(c(
      "{.arg d} must be a number between 0 and 10.",
      "i" = "Received: {.val {d}}.",
      "v" = "Use {.code d = 1} for one decimal place."
    ))
  }
  invisible(TRUE)
}

#' Check that suggested package namespace is available
#' @keywords internal
#' @noRd
.require_pkg <- function(pkg, purpose = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    msg <- if (is.null(purpose)) {
      sprintf("Package '%s' needed.", pkg)
    } else {
      sprintf(
        "Package '%s' is required for %s. Install it with install.packages('%s').",
        pkg, purpose, pkg
      )
    }
    simtab_abort_dependency(msg)
  }
  invisible(TRUE)
}
