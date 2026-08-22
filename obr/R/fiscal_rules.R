# UK fiscal rules per the Charter for Budget Responsibility (Autumn 2024,
# updated Autumn 2025) and the Budget Responsibility Act 2024. The rules
# themselves are encoded statically; numerical headroom is intentionally
# not hardcoded because the value changes at every fiscal event and any
# value shipped in the package would go stale within months. Users should
# derive headroom from the current EFO via get_efo_fiscal() or consult the
# OBR's press release for the relevant vintage.

fiscal_rules_table <- function() {
  data.frame(
    rule = c("stability", "investment", "welfare_cap"),
    metric = c(
      "Current budget balance, % of GDP",
      "Public Sector Net Financial Liabilities (PSNFL), % of GDP, year-on-year change",
      "AME-capped welfare spending, GBP bn (cash)"
    ),
    target_description = c(
      "Current budget in balance or surplus by the target year",
      "PSNFL as a share of GDP falling year-on-year by the target year",
      "AME-capped welfare spending below the cap level by the target year"
    ),
    target_year_rule = c(
      "5th forecast year (rolls to 3rd from 2026-27 onwards)",
      "5th forecast year (rolls to 3rd from 2026-27 onwards)",
      "Set in nominal terms for a specific fiscal year by the Treasury"
    ),
    direction_of_pass = c(
      "current budget surplus larger -> pass",
      "PSNFL/GDP falling more steeply -> pass",
      "AME-capped spending further below cap -> pass"
    ),
    source_charter = "Charter for Budget Responsibility, Autumn 2024 (updated Autumn 2025)",
    source_act     = "Budget Responsibility Act 2024 (fiscal lock)",
    stringsAsFactors = FALSE
  )
}

#' Get the current UK fiscal rules
#'
#' Returns a data frame describing the Charter for Budget Responsibility
#' fiscal rules currently in force, as encoded in the package at the time
#' of release. Numerical headroom against each rule is *not* shipped as a
#' constant because it is updated at every fiscal event and would go stale
#' within months; use [obr_headroom()] to derive the stability-rule margin
#' from the current EFO, or consult the OBR's EFO press release for the
#' relevant vintage.
#'
#' The current Charter (Autumn 2024, with an Autumn 2025 update) sets three
#' numerical rules:
#' \itemize{
#'   \item \strong{Stability rule}: current budget in balance or surplus by
#'     the target year. The target year is the 5th forecast year and rolls
#'     to the 3rd forecast year from 2026-27 onwards.
#'   \item \strong{Investment rule}: Public Sector Net Financial Liabilities
#'     (PSNFL) as a share of GDP falling year-on-year by the target year.
#'   \item \strong{Welfare cap}: AME-capped welfare spending below the
#'     statutory cap level by the target year.
#' }
#' The Budget Responsibility Act 2024 also adds a non-numerical
#' \strong{fiscal lock} requiring an OBR forecast for any fiscally
#' significant measure.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{rule}{Short rule code: `"stability"`, `"investment"`, `"welfare_cap"`.}
#'   \item{metric}{The metric the rule binds on.}
#'   \item{target_description}{Plain-English target.}
#'   \item{target_year_rule}{How the target year is set under the Charter.}
#'   \item{direction_of_pass}{Sign convention for headroom.}
#'   \item{source_charter}{Charter version that defines the rule.}
#'   \item{source_act}{Act of Parliament backing the rule.}
#' }
#'
#' @examples
#' obr_fiscal_rules()
#'
#' @family fiscal rules
#' @export
obr_fiscal_rules <- function() {
  fiscal_rules_table()
}
