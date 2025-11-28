#' Packages' sets
#'
#' These are presets of packages which can be used to easily prepare
#' systems installing them in groups or all together (using `pkg_all`).
#'
#' Packages imported by the mentioned packages are not included into
#' the sets (that is the reason why, e.g., you do not see `{shiny}` or
#' `knitr`, ...).
#'
#' @name pkg_sets
#' @seealso install_pkg_set
NULL


#' @describeIn pkg_sets Packages for side-interest purposes, including
#'   `{beepr}`, `{fortunes}`, and `{telegram.bot}`.
#' @export
pkg_misc <- purrr::set_names(c(
  "beepr",
  "fortunes",
  "telegram.bot"
))

#' @describeIn pkg_sets Utilities for data analyses, including `{here}`,
#'   `{lobstr}`, `{progress}`, `{progressr}`, `{readxl}`, `{tidyverse}`,
#'   and `{writexl}`.
#' @export
pkg_utils <- purrr::set_names(c(
  "here",
  "lobstr",
  "progress",
  "progressr",
  "readxl",
  "tidyverse",
  "writexl"
))

#' @describeIn pkg_sets Are you in a hurry?! This includes `{furrr}`,
#'   `{parallel}`, `snow`, and `{tidyfast}`.
#' @export
pkg_speed <- purrr::set_names(c(
  "furrr",
  "parallel",
  "snow",
  "tidyfast"
))

#' @describeIn pkg_sets Machine Learning in R (we do not include
#'  `{keras}` nor `{tensorflow}` which require _ad hoc_ configuration).
#'   This includes `{caret}`, `{glmnet}`, `{mlr3}`, `{snow}`,
#'   `{tidymodels}`, `{tidytext}`, and `{tm}`.
#' @export
pkg_mlt <- purrr::set_names(c(
  "caret",
  "glmnet",
  "mlr3",
  "tidymodels",
  "tidytext",
  "tm"
))

#' @describeIn pkg_sets (Clinical oriented) statistical analyses in R.
#'   This includes `{CBPS}`, `{cobalt}`, `{dlnm}`, `{ggeffects}`,
#'   `{MatchIt}`, `{mice}`, `{rms}`, `{twang}`, and `{WeightIt}`.
#' @export
pkg_stat <- purrr::set_names(c(
  "CBPS",
  "cobalt",
  "dlnm",
  "ggeffects",
  "MatchIt",
  "mice",
  "rms",
  "twang",
  "WeightIt"
))

#' @describeIn pkg_sets Stan analyses in R (Alert: these packages
#'   require `stan` be installed into the system. They are not included
#'   into `pkg_all`). This includes `{rstanarm}`, and `{brms}`.
#' @export
pkg_stan <- purrr::set_names(c(
  "rstanarm",
  "brms"
))

#' @describeIn pkg_sets Develop stuff in R. This includes `{assertive}`,
#'   `{checkmate}`, `{covr}`, `{lintr}`, `{profvis}`, `{pryr}`,
#'   `{renv}`, `{roxygen2}`, `{styler}`, `{tarchetypes}`, `{testthat}`,
#'   `{spelling}`, and `{usethis}`.
#' @export
pkg_devel <- purrr::set_names(c(
  "assertive",
  "checkmate",
  "covr",
  "lintr",
  "profvis",
  "pryr",
  "renv",
  "roxygen2",
  "styler",
  "tarchetypes",
  "testthat",
  "spelling",
  "usethis"
))

#' @describeIn pkg_sets Write and render things in R. This includes
#'   `{blogdown}`, `{DT}`, `{pander}`, and `{pkgdown}`.
#' @export
pkg_docs <- purrr::set_names(c(
  "blogdown",
  "DT",
  "pander",
  "pkgdown",
  "ggpubr"
))

#' @describeIn pkg_sets for develop and put in production stuffs
#'   (`{shiny}` app included). This includes `{golem}`, `{docopt}`,
#'   `{plumbr}`, and `{shinyjs}`.
#' @export
pkg_prod <- purrr::set_names(c(
  "golem",
  "docopt",
  "plumbr",
  "shinyjs"
))

#' @describeIn pkg_sets Union of the `pkg_misc`, `pkg_utils`, `pkg_speed`,
#'   `pkg_mlt`, `pkg_stat`, `pkg_devel`, `pkg_docs`, and `pkg_prod`
#'    pkg_* sets.
#' @export
pkg_all <- sort(unique(c(
  pkg_misc, pkg_utils, pkg_speed, pkg_mlt, pkg_stat, pkg_devel,
  pkg_docs, pkg_prod
)))
