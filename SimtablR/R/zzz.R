# Package Lifecycle and Initialization Hooks
#
# Startup banner formatting, extension and journal style registry seeding,
# and runtime registration of S3 methods for soft dependencies.

#########
# PACKAGE ATTACHMENT AND STARTUP BANNER
# Publication-ready welcome message and workflow feature highlights.

#' Display publication-ready workflow banner on package attachment
#' @keywords internal
#' @noRd
.onAttach <- function(libname, pkgname) {
  ver <- utils::packageVersion(pkgname)
  msg <- cli::rule(
    left = paste0("SimtablR ", ver),
    right = "Publication-ready epidemiological tables"
  )
  packageStartupMessage(cli::col_cyan(msg))
  packageStartupMessage(
    cli::col_green(cli::symbol$tick),
    " Describe: ",
    cli::col_blue("tb(), table1()"),
    cli::col_white("  Estimate: "),
    cli::col_blue("regtab(), survtab()"),
    "\n",
    cli::col_green(cli::symbol$tick),
    " Diagnose: ",
    cli::col_blue("diag_test(), roc()"),
    cli::col_white("  Build: "),
    cli::col_blue("simtab() |> verbs, simtablr()"),
    "\n",
    cli::col_green(cli::symbol$tick),
    " Revise: ",
    cli::col_blue("adjust(), sensitivity(), e_value()"),
    "\n",
    cli::col_green(cli::symbol$tick),
    " Report: ",
    cli::col_blue("as_methods(), strobe(), codebook(), why()"),
    "\n",
    cli::col_green(cli::symbol$tick),
    " Export: ",
    cli::col_blue("export_docx(), as_flextable(), as_gt(), autoplot()"),
    "\n",
    cli::col_green(cli::symbol$info),
    " Learning SimtablR? ",
    cli::col_cyan("Use browseVignettes(package = 'SimtablR') for guides")
  )
  packageStartupMessage(
    cli::col_silver("Use suppressPackageStartupMessages() to silence.")
  )
}

#########
# PACKAGE LOADING AND REGISTRY INITIALIZATION
# Seed built-in journals, engine registries, advice rules, and soft S3 methods.

#' Initialize registries and register S3 methods on package load
#' @keywords internal
#' @noRd
.onLoad <- function(libname, pkgname) {
  # Seed the built-in journal style presets.
  tryCatch(.seed_builtin_styles(), error = function(e) NULL)

  # Seed computational registries and methodological advice rules.
  tryCatch(.seed_builtin_registries(), error = function(e) NULL)
  tryCatch(.seed_builtin_rules(), error = function(e) NULL)

  # External S3 Method Registration for Soft Dependencies (flextable)
  if (requireNamespace("flextable", quietly = TRUE)) {
    tryCatch(
      {
        flextable_env <- asNamespace("flextable")
        namespace_env <- asNamespace(pkgname)
        flextable_methods <- c(
          "as_flextable.simtab_result",
          "as_flextable.simtab_spec",
          "as_flextable.simtab_report",
          "as_flextable.simtab_rbind_tb"
        )
        for (m in flextable_methods) {
          registerS3method(
            genname = "as_flextable",
            class = sub("^as_flextable\\.", "", m),
            method = get(m, envir = namespace_env),
            envir = flextable_env
          )
        }
      },
      error = function(e) NULL
    )
  }
}
