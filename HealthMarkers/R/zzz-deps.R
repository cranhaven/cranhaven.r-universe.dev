
# List ALL optional (Suggests) namespaces in one place
.optional_deps <- c(
  "CVrisk", "PooledCohort", "QRISK3", "RiskScorescvd",
  "di"
)

# Require exactly one package (runtime gate)
# Emits a typed condition so .hm_safe_call()'s handle_error can read e$package
# without needing regex on the message string.
.need_pkg <- function(pkg) {
  ok <- suppressMessages(suppressWarnings(requireNamespace(pkg, quietly = TRUE)))
  if (!ok) {
    rlang::abort(
      sprintf("Package '%s' is required for this feature. Install it first.", pkg),
      class = "healthmarkers_missing_package",
      package = pkg
    )
  }
}

# Require a set of packages (useful for multi-dep features)
.need_pkgs <- function(pkgs) {
  miss <- pkgs[!vapply(pkgs, requireNamespace, FUN.VALUE = TRUE, quietly = TRUE)]
  if (length(miss)) {
    rlang::abort(sprintf(
      "Missing required package(s): %s. Install them first.",
      paste(miss, collapse = ", ")
    ))
  }
}

# Run an expression only if a package is available; otherwise return NA or a fallback
.with_pkg <- function(pkg, expr, otherwise = NULL) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    force(expr)
  } else {
    if (!is.null(otherwise)) otherwise else rlang::abort(
      sprintf("Optional package '%s' not installed.", pkg)
    )
  }
}

.onLoad <- function(libname, pkgname) {
  # Initialize verbosity option if unset
  if (is.null(getOption("healthmarkers.verbose"))) {
    # allowed: "none" | "inform" | "debug"; TRUE treated as "inform"
    options(healthmarkers.verbose = "none")
  }
  invisible(TRUE)
}

utils::globalVariables(c(
  ":=",
  "chol_total","chol_ldl","chol_hdl","triglycerides","age_year","z_HOMA",
  "glucose","HbA1c","bp_sys_z","bp_dia_z","weight_kg","height_m",
  "BMI","Avignon_Si0","Avignon_Si120"
))
