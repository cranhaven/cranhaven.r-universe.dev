#######################################

# LAPOP Multiple Cross-Country Bar Graph Pre-Processing #

#######################################

#' LAPOP Grouped Bar Graph Pre-Processing
#'
#' This function creates dataframes which can then be input in lapop_ccm for
#' comparing values for multiple variables across countries with a bar graph
#' using LAPOP formatting.
#'
#' @param data A survey object.  The data that should be analyzed.
#' @param outcome_vars Character vector.  Outcome variable(s) of interest to be plotted
#' across country (or other x variable). Max of 4 (four) variables.
#' @param xvar Character string. Outcome variables are broken down by this variable. You can set
#' xvar to "wave" or "year" for cross-time comparisons. Default: pais_lab.
#' @param by Character string. Optional grouping variable used only when
#' `outcome_vars` has length 1. The single outcome will be broken down by the
#' levels of `by`, and those levels will be stored in the `var` column for use
#' in \code{lapop_ccm()}. Default: NULL.
#' @param rec,rec2,rec3,rec4 Numeric. The minimum and maximum values of the outcome variable that
#' should be included in the numerator of the percentage.  For example, if the variable
#' is on a 1-7 scale and rec is c(5, 7), the function will show the percentage who chose
#' an answer of 5, 6, 7 out of all valid answers.  Can also supply one value only,
#' to produce the percentage that chose that value out of all other values.
#' Default: c(1, 1).
#' @param ci_level Numeric. Confidence interval level for estimates.  Default: 0.95
#' @param mean Logical.  If TRUE, will produce the mean of the variable rather than
#' rescaling to percentage.  Default: FALSE.
#' @param filesave Character.  Path and file name to save the dataframe as csv.
#' @param cfmt Character. Changes the format of the numbers displayed above the bars.
#' Uses sprintf string formatting syntax. Default is whole numbers for percentages
#' and tenths place for means.
#' @param sort Character. On what value the bars are sorted.
#' Options are "y" (default; for the value of the first outcome variable), "xv" (for
#' the underlying values of the x variable), "xl" (for the labels of the x variable,
#' i.e., alphabetical).
#' @param order Character.  How the bars should be sorted.  Options are "hi-lo"
#' (default) or "lo-hi".
#' @param ttest Logical.  If TRUE, will conduct pairwise t-tests for difference
#' of means between all outcomes vs. all x-vars and save them in attr(x,
#' "t_test_results"). Default: FALSE.
#' @param keep_nr Logical.  If TRUE, will convert "don't know" (missing code .a)
#' and "no response" (missing code .b) into valid data (value = 99) and use them
#' in the denominator when calculating percentages.  The default is to examine
#' valid responses only.  Default: FALSE.
#'
#' @return Returns a data frame, with data formatted for visualization by lapop_ccm()
#'
#' @examples
#'
#' require(lapop); data(ym23)
#'
#' # Set Survey Context on a small cross-country subset
#' ym23_small <- subset(ym23, pais %in% c(1, 15, 17))
#' ym23lpr <- lpr_data(ym23_small)
#'
#' # Multiple outcomes over countries
#' lpr_ccm(ym23lpr,
#' outcome_vars = c("b12", "b18"),
#' rec = c(1, 3),
#' rec2 = c(5, 7))
#'
#' # Multiple outcomes over years
#' \donttest{
#' lpr_ccm(ym23lpr,
#' outcome_vars = c("b12", "b18"),
#' xvar = "wave",
#' rec = c(1, 3),
#' rec2 = c(5, 7),
#' ttest = TRUE)
#'}
#' # Single outcome broken down by a grouping variable
#' \donttest{
#' lpr_ccm(
#'   ym23lpr,
#'   outcome_vars = "ing4",
#'   xvar = "pais_lab",
#'   by = "pn4",
#'   rec = c(1, 3)
#' )
#' }
#'
#'@export
#'@import dplyr
#'@import srvyr
#'
#'@author Luke Plutowski, \email{luke.plutowski@@vanderbilt.edu} & Robert Vidigal, \email{robert.vidigal@@vanderbilt.edu}


lpr_ccm <- function(data,
                    outcome_vars,
                    xvar = "pais_lab",
                    by = NULL,
                    rec = c(1, 1),
                    rec2 = c(1, 1),
                    rec3 = c(1, 1),
                    rec4 = c(1, 1),
                    ci_level = 0.95,
                    mean = FALSE,
                    filesave = "",
                    cfmt = "",
                    sort = "y",
                    order = "hi-lo",
                    ttest = FALSE,
                    keep_nr = FALSE) {

  if (length(rec2) == 1) {
    rec2 = c(rec2, rec2)
  }
  if (length(rec3) == 1) {
    rec3 = c(rec3, rec3)
  }
  if (length(rec4) == 1) {
    rec4 = c(rec4, rec4)
  }

  if (length(outcome_vars) > 4) {
    stop("`outcome_vars` supports a maximum of 4 variables.")
  }

  if (!is.null(by) && length(outcome_vars) != 1) {
    stop("`by` can only be used when `outcome_vars` has length 1.")
  }

  if (length(rec) == 1) {
    rec <- c(rec, rec)
  }

  # Map rec arguments to outcome variables
  rec_list <- list(rec, rec2, rec3, rec4)
  rec_map <- purrr::map2(outcome_vars, rec_list[1:length(outcome_vars)], ~ list(var = .x, rec = .y))

  # Handle NA recoding if keep_nr is TRUE
  if (keep_nr) {
    data <- data %>%
      mutate(across(all_of(outcome_vars), ~ case_when(
        na_tag(.) %in% c("a", "b") ~ 99,
        TRUE ~ as.numeric(.)
      )))
  }

  # Process each outcome variable with its respective rec
  ccm <- purrr::map_dfr(rec_map, function(mapping) {
    outcome <- mapping$var
    rec <- mapping$rec

    temp <- data %>%
      drop_na(!!sym(xvar)) %>%
      {
        if (!is.null(by)) {
          drop_na(., !!sym(by)) %>%
            group_by(
              pais = as_factor(!!sym(xvar)),
              by_group = as_factor(!!sym(by))
            )
        } else {
          group_by(., pais = as_factor(!!sym(xvar)))
        }
      } %>%
      {
        if (mean) {
          summarize(.,
                    prop = survey_mean(!!sym(outcome),
                                       na.rm = TRUE,
                                       vartype = "ci",
                                       level = ci_level)) %>%
            mutate(proplabel = if (cfmt != "") {
              sprintf(cfmt, prop)
            } else {
              sprintf("%.1f", prop)
            })
        } else {
          summarize(.,
                    prop = survey_mean(between(!!sym(outcome), rec[1], rec[2]),
                                       na.rm = TRUE,
                                       vartype = "ci",
                                       level = ci_level) * 100) %>%
            mutate(proplabel = if (cfmt != "") {
              sprintf(cfmt, round(prop))
            } else {
              sprintf("%.0f%%", round(prop))
            })
        }
      } %>%
      filter(prop != 0) %>%
      rename(lb = prop_low, ub = prop_upp) %>%
      ungroup() %>%
      mutate(var = if (!is.null(by)) as.character(by_group) else outcome) %>%
      {
        if (!is.null(by)) {
          select(., -by_group)
        } else {
          .
        }
      }

    temp
  })

  if (!is.null(by) && length(unique(ccm$var)) > 4) {
    stop("`by` supports a maximum of 4 levels for use with `lapop_ccm()`.")
  }
  # Sorting logic
  ccm = ccm %>%
    {
      if (sort == "y") {
        group_by(., var) %>%
          mutate(rank = rank(-prop)) %>%
          arrange(match(var, unique(var)[1]),
                  if (order == "hi-lo") rank else desc(rank)) %>%
          select(-rank)
      } else if (sort == "xl") {
          arrange(., if (order == "hi-lo") desc(as.character(pais)) else as.character(pais))
      } else if (sort == "xv") {
        arrange(., if (order == "hi-lo") desc(match(pais, levels(pais))) else match(pais, levels(pais)))
      } else {
        .
      }
    }

  # Perform pairwise t-tests if requested
  if (ttest) {
    t_test_results <- data.frame(test = character(),
                                 diff = numeric(),
                                 ttest = numeric(),
                                 pval = numeric(),
                                 stringsAsFactors = FALSE)
    design_test <- data
    x_labels <- as.character(haven::as_factor(design_test$variables[[xvar]]))
    by_labels <- if (!is.null(by)) as.character(haven::as_factor(design_test$variables[[by]])) else NULL
    rec_lookup <- stats::setNames(rec_list[seq_along(outcome_vars)], outcome_vars)

    for (i in seq_len(nrow(ccm))) {
      row_name <- paste0(".ttest_row_", i)
      outcome_name <- if (!is.null(by)) outcome_vars[1] else as.character(ccm$var[i])
      rec_range <- rec_lookup[[outcome_name]]
      outcome_values <- as.numeric(design_test$variables[[outcome_name]])
      value_vector <- if (mean) {
        outcome_values
      } else {
        as.numeric(dplyr::between(outcome_values, rec_range[1], rec_range[2])) * 100
      }

      domain <- x_labels == as.character(ccm$pais[i])
      if (!is.null(by)) {
        domain <- domain & by_labels == as.character(ccm$var[i])
      }

      design_test$variables[[row_name]] <- ifelse(domain, value_vector, NA_real_)
    }

    row_var_names <- paste0(".ttest_row_", seq_len(nrow(ccm)))
    row_formula <- stats::as.formula(paste0("~", paste(row_var_names, collapse = " + ")))
    row_estimates <- survey::svymean(row_formula, design = design_test, na.rm = TRUE)
    design_df <- survey::degf(design_test)

    for (i in 1:(nrow(ccm) - 1)) {
      for (j in (i + 1):nrow(ccm)) {
        contrast_est <- survey::svycontrast(
          row_estimates,
          stats::setNames(c(1, -1), c(row_var_names[i], row_var_names[j]))
        )
        diff_est <- as.numeric(stats::coef(contrast_est))
        diff_se <- sqrt(as.numeric(stats::vcov(contrast_est)))
        diff_t <- diff_est / diff_se
        diff_p <- 2 * stats::pt(-abs(diff_t), df = design_df)

        t_test_results <- rbind(
          t_test_results,
          data.frame(
            test = paste(ccm$pais[i], ccm$var[i], "vs", ccm$pais[j], ccm$var[j]),
            diff = round(diff_est, 3),
            ttest = round(diff_t, 3),
            pval = round(diff_p, 3)
          )
        )
      }
    }

    attr(ccm, "t_test_results") <- t_test_results
  }


  # Save the results to a file if specified
  if (filesave != "") {
    write.csv(ccm, filesave, row.names = FALSE)
  }

  return(ccm)
}
