#' Paired test for continuous variables
#'
#' Statistical tests for paired continuous variable.
#'
#' @details If the test is requested for two paired groups, the
#' \code{\link[stats]{t.test}} is used.
#'
#' If the test is requested for more than two groups, the test based on
#' ANOVA for repeated measures is used (powered by
#' \code{\link[stats]{aov}})
#'
#' @note This function could be used as `conTest` option in the
#' \code{\link[Hmisc]{summary.formula}} with method `reverse`.
#'
#'
#' @param group (fct) vector of groups
#' @param x (num) vector of observations. Note: length of `x` is
#'        considered equal to the number of subjects by the number of
#'        groups. Observation must be provided by subject
#'        (e.g. c(a1, b1, c1, a2, b2, c2, a3, b3, c3, a4, b4, c4), where
#'        the letters, a, b, c, and d represents the groups and the
#'        numbers represents the patients' ids). Note only patient with
#'        observation in all the levels considered will be used.
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
#'
#' @examples
#' \donttest{
#'   library(Hmisc)
#'
#'   ## two groups
#'   summary(Species ~ .,
#'     data = iris[iris[["Species"]] != "setosa", ],
#'     method = "reverse",
#'     test = TRUE,
#'     conTest = paired_test_continuous
#'   )
#'
#'   ## more than two groups
#'   summary(Species ~ .,
#'     data = iris,
#'     method = "reverse",
#'     test = TRUE,
#'     conTest = paired_test_continuous
#'   )
#'
#'   ## without Hmisc
#'   two_obs <- iris[["Sepal.Length"]][iris[["Species"]] != "setosa"]
#'   two_groups <- iris[["Species"]][iris[["Species"]] != "setosa"]
#'   paired_test_continuous(two_groups, two_obs)
#'
#'   obs <- iris[["Sepal.Length"]]
#'   many_groups <- iris[["Species"]]
#'   paired_test_continuous(many_groups, obs)
#' }
paired_test_continuous <- function(group, x) {
  # Imput adjustment and checks -------------------------------------
  len_g <- length(group)
  len_x <- length(x)
  n_lev <- length(levels(group))


  if (len_g != len_x) {
    ui_stop(paste(
      "The length of the variable group has to be the same of",
      "the length of the variable x. They aren't equal."
    ))
  }

  if (!is.factor(group)) {
    ui_warn(
      "Grouping variable converted to factor to compute test."
    )
    # explicit set levels to avoid reordering
    group <- factor(group, levels = unique(group))
  }


  # main constants --------------------------------------------------
  original_levels <- levels(group)


  # Recreate ids (if possible) --------------------------------------
  rle_g <- rle(as.integer(group))[["lengths"]]

  ids <- vector("integer", len_g)
  id <- 0L

  if (length(rle_g) == length(original_levels)) {
    # this means that observation are sorted by group
    if (diff(range(rle_g)) >= .Machine[["double.eps"]]^0.5) {
      ui_warn(paste(
        "Data passed by groups and incomplete:\n",
        "    not same umber of observation among the groups.\n",
        "P returned is the standard F statistics.\n",
        "(9L is added to this P to identify the cases).\n\n"
      ))
      res <- Hmisc::conTestkw(group, x)
      res[["P"]] <- res[["P"]] + 9L
      return(res)
    }
    # observation sorted by groups with the same length
    ids <- rep(seq_len(rle_g[[1L]]), length(rle_g))

  } else {

    # this means observation are sorted by ids
    for (i in seq_along(group)) {
      actual_lev <- which(original_levels == group[[i]])

      is_new_id <- (i == 1L) ||
        (group[[i - 1L]] %in% original_levels[actual_lev:n_lev])

      id <- id + is_new_id
      ids[[i]] <- id
    }
  }


  # main data frame creation ----------------------------------------
  data_db <- dplyr::tibble(ids, x, group) %>%
    dplyr::distinct() %>%
    tidyr::spread("group", "x") %>%
    ggplot2::remove_missing() %>%
    tidyr::gather("group", "x", -ids) %>%
    dplyr::mutate(group = factor(group,
      levels = original_levels[original_levels %in% unique(group)]
    )) %>%
    dplyr::arrange(ids, group)


  group_names <- levels(data_db[["group"]])
  group_n <- length(group_names)
  n_subjects <- length(unique(data_db[["ids"]]))


  # Less Than Two groups --------------------------------------------
  if (group_n < 2L || n_subjects <= group_n) return(fake_h_group_test())


  # Two groups ------------------------------------------------------
  if (group_n == 2L) {
    data_two <- data_db %>%
      tidyr::spread("group", "x")


    test_out <- stats::t.test(data_two[[2L]], data_two[[3L]],
      paired    = TRUE,
      var.equal = TRUE
    )


    # `return()` exits from the function here!
    return(list(
      # values (mandatory)
      P = c(P = test_out[["p.value"]]),
      stat = test_out[["statistic"]],
      df = test_out[["parameter"]],

      # names (mandatory)
      testname = "Paired t-test",
      statname = "t",
      namefun = "paired_tstat",

      # special labels (optional)
      latexstat = "t_{df}",
      plotmathstat = "t[df]",
      note = "Two groups: t-test for paired values is done."
    ))
  }


  # More than two groups --------------------------------------------

  test_out <- summary(
    stats::aov(x ~ group + Error(ids / group), data = data_db)
  )[["Error: Within"]][[1L]]

  list(
    # values (mandatory)
    P = stats::setNames(test_out[1L, "Pr(>F)"], "P"),
    stat = stats::setNames(test_out[1L, "F value"], "F"),
    df = stats::setNames(test_out[1L, "Df"], "df"),

    # names (mandatory)
    testname = "Repeated-measure AOV",
    statname = "F",
    namefun = "rep_aov",

    # special labels (optional)
    latexstat = "F_{df}",
    plotmathstat = "F[df]",
    note = {
      "More than two groups: ANOVA for repeated measure is used."
    }
  )
}
