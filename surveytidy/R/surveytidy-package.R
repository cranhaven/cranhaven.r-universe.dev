# R/surveytidy-package.R
#
# Package-level documentation and imports.

#' surveytidy: Tidy dplyr/tidyr Verbs for Survey Design Objects
#'
#' Provides dplyr and tidyr verbs for survey design objects created with the
#' `surveycore` package. The key statistical feature is **domain-aware
#' filtering**: `filter()` marks rows as in-domain rather than removing them,
#' which is essential for correct variance estimation of subpopulation
#' statistics.
#'
#' ## Key verbs
#'
#' * [filter()] — domain estimation (marks rows, never removes them)
#' * [select()] — column selection preserving design variables
#' * [mutate()] — add/modify columns with weight-change warnings
#' * [rename()] — auto-updates design variable names and metadata
#' * [group_by()] / [ungroup()] — grouped analysis support
#' * [arrange()] — row sorting preserving domain membership
#' * [subset()] — physical row removal with a strong warning
#'
#' ## Domain estimation vs. physical subsetting
#'
#' `filter()` and `subset()` have fundamentally different statistical meanings:
#'
#' * `filter(.data, condition)` — sets `..surveycore_domain..` to `TRUE` for
#'   matching rows. All rows are retained. Variance estimation correctly uses
#'   the full design.
#'
#' * `subset(.data, condition)` — physically removes non-matching rows.
#'   Variance estimates will be biased unless the design was explicitly built
#'   for the subset. Use only when you understand the statistical implications.
#'
#' @keywords internal
#'
#' @importFrom dplyr dplyr_reconstruct
#' @importFrom tidyselect eval_select
"_PACKAGE"
