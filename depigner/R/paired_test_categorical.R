#' Paired test for categorical variables
#'
#' Statistical tests for paired categorical variable.
#'
#' @details If the test is requested for two paired groups, the
#' \code{\link[stats]{mcnemar.test}} is used.
#'
#' If the test is requested for more than two paired groups, the test
#' based on Cochran-Mantel-Haenzen for repeated measures is used
#' (powered by \code{\link[stats]{mantelhaen.test}})
#'
#' @note This function could be used as `catTest` option in the
#' \code{\link[Hmisc]{summary.formula}} with method `reverse`.
#'
#'
#' @param tab a frequency table (an integer `table`, if a `matrix` is
#'   provided, it will be coerced to a `table` internally)
#'
#' @return A list with components
#'         `P` (the computed P-value),
#'         `stat` (the test statistic, either t or F),
#'         `df` (degrees of freedom),
#'         `testname` (test name),
#'         `statname` (statistic name),
#'         `namefun` ("paired_tstat", "rep_aov"),
#'         `latexstat` (LaTeX representation of statname),
#'         `plotmathstat` (for R - the plotmath representation of
#'             `statname`, as a character string),
#'         `note` (contains a character string note about the test).
#' @export
#'
#' @importFrom rlang .data
#' @examples
#' \donttest{
#'   library(Hmisc)
#'
#'   data(Arthritis)
#'
#'   ## two groups
#'   summary(Treatment ~ Sex,
#'     data = Arthritis,
#'     method = "reverse",
#'     test = TRUE,
#'     catTest = paired_test_categorical
#'   )
#'
#'   ## more than two groups
#'   summary(Improved ~ Sex,
#'     data = Arthritis,
#'     method = "reverse",
#'     test = TRUE,
#'     catTest = paired_test_categorical
#'   )
#' }
paired_test_categorical <- function(tab) {


  # input check -----------------------------------------------------
  if (!is_proper_matrix(tab)) return(empty_h_test())

  if (!inherits(tab, "table")) {
    ui_warn("{ui_field(tab)} is not a table.
      It will be coerced to a table by {ui_code('as.table()')}.
      If not sure the coercion is correct, please provide a table directly.
    ")
    tab <- as.table(tab)
  }

  rowcounts <- tab %*% rep(1L, ncol(tab))
  tab <- tab[rowcounts > 0L, ]
  if (!is_proper_matrix(tab)) return(empty_h_test())

  # due = McNemar ---------------------------------------------------

  if (nrow(tab) == 2L && ncol(tab) == 2L) {
    mn_test <- stats::mcnemar.test(tab)

    return(list(
      # values (mandatory)
      P = mn_test[["p.value"]],
      stat = mn_test[["statistic"]],
      df = mn_test[["parameter"]],

      # names (mandatory)
      testname = "McNemar",
      statname = "Chi-square",
      namefun = "mn_chisq",

      # special labels (optional)
      latexstat = "\\mn-chi^{2}_{df}",
      plotmathstat = "mn-chi[df]^2"
    ))
  }


  #  molti = glm ---------------------------------

  dimnames(tab) <- stats::setNames(
    dimnames(tab),
    c("var_levels", "grouping_var")
  )

  group_id <- stats::setNames(seq_along(colnames(tab)), colnames(tab))
  lev_id <- stats::setNames(seq_along(rownames(tab)), rownames(tab))

  tab_df <- dplyr::as_tibble(tab) %>%
    dplyr::mutate(
      lev_id = lev_id[.data[["var_levels"]]],
      group_id = group_id[.data[["grouping_var"]]]
    ) %>%
    dplyr::group_by(.data[["grouping_var"]]) %>%
    dplyr::mutate(prop = .data[["n"]] / sum(.data[["n"]])) %>%
    dplyr::ungroup()

    st <- rms::Glm(
      formula = stats::as.formula("prop ~ var_levels*group_id"),
      data = tab_df,
      family = "quasibinomial"
    )
    cof <- stats::coef(st)
    df <- st[["rank"]] - (names(cof)[[1L]] == "Intercept")
    lr <- st[["null.deviance"]] - st[["deviance"]]
    pval <- 1L - stats::pchisq(lr, df)

  list(
    # values (mandatory)
    P = stats::setNames(pval, "P"),
    stat = stats::setNames(lr, "chi2"),
    df = stats::setNames(df, "df"),

    # names (mandatory)
    testname = "LR test for a group in a GLM model",
    statname = "chi2",
    namefun = "glm_chi2_test",

    # special labels (optional)
    latexstat = "\\t_{df}",
    plotmathstat = "t[df]^2",
    note = paste(
      "Overdispersed binomial GLM is fitted using both ranked groups",
      "and the covariate of interest. Differences are assessed using a",
      "Likelihood-Ratio test that assumes a Chi^2 distribution for the",
      "test statistic."
    )
  )
}
