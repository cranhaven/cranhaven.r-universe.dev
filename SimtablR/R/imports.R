# Package Imports and Global Variables
#
# Centralized namespace import declarations and tidy-evaluation global variables
# to ensure clean CRAN check validation and consistent symbols.

#' @importFrom stats addmargins anova as.formula binom.test binomial chisq.test
#' @importFrom stats coef confint fisher.test glm kruskal.test lm mcnemar.test
#' @importFrom stats pnorm poisson qnorm quantile relevel sd setNames t.test
#' @importFrom stats update vcov wilcox.test
#' @importFrom utils write.csv
#' @importFrom generics tidy glance
#' @importFrom rlang as_label call2 enquo enquos ensym is_quosure new_quosures
#' @importFrom rlang quo_get_env quo_get_expr quo_is_missing quo_text
#' @importFrom tidyselect eval_select
#' @keywords internal
"_PACKAGE"

#########
# GLOBAL VARIABLE REGISTRATIONS
# Tidy-evaluation column names and pipeline symbols registered for R CMD check.

utils::globalVariables(c(
  "Variable", "Result", "Outcome", "P_Value",
  ".data", "false_positive_rate", "sensitivity", "marker",
  "marker1", "marker2", "marker3", "outcome"
))
