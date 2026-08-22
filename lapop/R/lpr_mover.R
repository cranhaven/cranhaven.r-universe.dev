######################################################

# LAPOP "Multiple-Over" Breakdown Graph Pre-Processing #

######################################################

#' LAPOP "Multiple-Over" Breakdown Graphs
#'
#' This function creates a dataframe which can then be input in lapop_mover() for
#' comparing means across values of secondary variable(s) using LAPOP formatting.
#'
#' @param data A survey object. The data that should be analyzed.
#' @param outcome Character. Outcome variable(s) of interest to be plotted across secondary
#' variable(s).
#' @param grouping_vars A character vector specifying one or more grouping variables.
#' For each variable, the function calculates the average of the outcome variable,
#' broken down by the distinct values within the grouping variable(s).
#' @param rec Numeric. The minimum and maximum values of the frst outcome variable that
#' should be included in the numerator of the percentage.  For example, if the variable
#' is on a 1-7 scale and rec is c(5, 7), the function will show the percentage who chose
#' an answer of 5, 6, 7 out of all valid answers.  Can also supply one value only,
#' to produce the percentage that chose that value out of all other values.
#' Default: c(1, 1).
#' @param rec2 Numeric. Similar to 'rec' for the second outcome. Default: c(1, 1).
#' @param rec3 Numeric.  Similar to 'rec' for the third outcome. Default: c(1, 1).
#' @param rec4 Numeric.  Similar to 'rec' for the fourth outcome. Default: c(1, 1).
#' @param ci_level Numeric. Confidence interval level for estimates.  Default: 0.95
#' @param mean Logical.  If TRUE, will produce the mean of the variable rather than
#' recoding to percentage.  Default: FALSE.
#' @param filesave Character.  Path and file name to save the dataframe as csv.
#' @param cfmt Changes the format of the numbers displayed above the bars.
#' Uses sprintf string formatting syntax. Default is whole numbers for percentages
#' and tenths place for means.
#' @param ttest Logical.  If TRUE, will conduct pairwise t-tests for difference
#' of means between all individual year-xvar levels and save them in attr(x,
#' "t_test_results"). Default: FALSE.
#' @param keep_nr Logical.  If TRUE, will convert "don't know" (missing code .a)
#' and "no response" (missing code .b) into valid data (value = 99) and use them
#' in the denominator when calculating percentages.  The default is to examine
#' valid responses only.  Default: FALSE.
#'
#' @return Returns a data frame, with data formatted for visualization by lapop_mover
#'
#' @examples
#'\donttest{
#' require(lapop); data(ym23)
#'
#' # Set SUrvey Context
#' ym23lpr<-lpr_data(ym23)
#'
#' # Single DV
#' lpr_mover(data = ym23lpr,
#'  outcome = "ing4",
#'  grouping_vars = c("q1tc_r", "edre"),
#'  rec = c(5, 7), ttest = FALSE)
#'
#' # Multiple DV
#' lpr_mover(data = ym23lpr,
#' outcome = c("ing4", "pn4"),
#' grouping_vars = c("q1tc_r", "edre"),
#' rec = c(5, 7), rec2 = c(1, 2),
#' ttest = FALSE)
#'
#' # Single DV X Single IV
#' lpr_mover(data = ym23lpr,
#' outcome="ing4",
#' grouping_vars="pn4",
#' rec=c(5,7),
#' ttest = FALSE)
#'
#' # Multiple DV X Single IV
#' lpr_mover(data = ym23lpr,
#' outcome=c("ing4", "pn4"),
#' grouping_vars="edre",
#' rec=c(5,7), rec2=c(1,2),
#' ttest = FALSE)
#'
#' # Multiple DV X Multiple IV
#' lpr_mover(data = ym23lpr,
#' outcome=c("ing4", "pn4"),
#' grouping_vars=c("edre", "q1tc_r"),
#' rec=c(5,7), rec2=c(1,2),
#' ttest = FALSE)
#'}
#'
#'@export
#'@import dplyr
#'@import srvyr
#'@import purrr
#'@import haven
#'
#'@author Luke Plutowski, \email{luke.plutowski@@vanderbilt.edu} && Robert Vidigal, \email{robert.vidigal@@vanderbilt.edu}

lpr_mover <- function(data,
                      outcome,
                      grouping_vars,
                      rec = list(c(1, 1)),
                      rec2 = c(1, 1),
                      rec3 = c(1, 1),
                      rec4 = c(1, 1),
                      ci_level = 0.95,
                      mean = FALSE,
                      filesave = "",
                      cfmt = "",
                      ttest = FALSE,
                      keep_nr = FALSE) {

  if (keep_nr) {
    data <- data %>%
      mutate(across(all_of(outcome), ~ case_when(
        na_tag(.) == "a" | na_tag(.) == "b" ~ 99,
        TRUE ~ as.numeric(.)
      )))
  }

  rec_list <- list(rec, rec2, rec3, rec4)
  rec_list <- rec_list[seq_along(outcome)] # Ensure only as many rec values as outcomes

  # Function to calculate means/proportions for a single outcome and grouping variable
  calculate_means <- function(data, outcome_var, grouping_var, rec_range, single_outcome) {
    if (!(grouping_var %in% names(data$variables))) {
      stop(paste("Grouping variable", grouping_var, "not found in data."))
    }
    if (!(outcome_var %in% names(data$variables))) {
      stop(paste("Outcome variable", outcome_var, "not found in data."))
    }

    data %>%
      filter(!is.na(.data[[grouping_var]])) %>%
      group_by(vallabel = haven::as_factor(.data[[grouping_var]])) %>%
      {
        if (mean) {
          summarize(.,
                    prop = survey_mean(.data[[outcome_var]],
                                       na.rm = TRUE,
                                       vartype = "ci",
                                       level = ci_level)
          ) %>%
            mutate(proplabel = sprintf("%.1f", prop))
        } else {
          summarize(.,
                    prop = survey_mean(between(.data[[outcome_var]], rec_range[1], rec_range[2]),
                                       na.rm = TRUE,
                                       vartype = "ci",
                                       level = ci_level) * 100
          ) %>%
            mutate(proplabel = sprintf("%.0f%%", round(prop)))
        }
      } %>%
      mutate(
        outcome = if (!is.null(attributes(data$variables[[outcome_var]])$label)) {
          attributes(data$variables[[outcome_var]])$label
        } else {
          outcome_var
        },
        varlabel = if (single_outcome) {
          if (!is.null(attributes(data$variables[[grouping_var]])$label)) {
            attributes(data$variables[[grouping_var]])$label
          } else {
            grouping_var
          }
        } else {
          paste(grouping_var, outcome_var, sep = " x ")
        },
        vallabel = as.character(vallabel)
      ) %>%
      rename(lb = prop_low, ub = prop_upp) %>%
      mutate(
        .outcome_var = outcome_var,
        .grouping_var = grouping_var,
        .rec_lo = rec_range[1],
        .rec_hi = rec_range[2]
      ) %>%
      select(outcome, varlabel, vallabel, prop, proplabel, lb, ub,
             .outcome_var, .grouping_var, .rec_lo, .rec_hi)
  }

  single_outcome <- length(outcome) == 1

  # Apply function to each combination of outcome and grouping variable
  mover <- map_dfr(grouping_vars, function(gvar) {
    map2_dfr(outcome, rec_list, ~ calculate_means(data, .x, gvar, .y, single_outcome))
  })

  if (filesave != "") {
    write.csv(mover, filesave)
  }

  # Conduct pairwise t-tests if requested
  if (ttest) {
    t_test_results <- data.frame(
      outcome = character(),
      varlabel = character(),
      test = character(),
      diff = numeric(),
      ttest = numeric(),
      pval = numeric(),
      stringsAsFactors = FALSE
    )
    design_test <- data
    mover_test <- mover %>% filter(!is.na(prop))
    for (i in seq_len(nrow(mover_test))) {
      row_name <- paste0(".ttest_row_", i)
      outcome_values <- as.numeric(design_test$variables[[mover_test$.outcome_var[i]]])
      value_vector <- if (mean) {
        outcome_values
      } else {
        as.numeric(dplyr::between(outcome_values, mover_test$.rec_lo[i], mover_test$.rec_hi[i])) * 100
      }
      grouping_labels <- as.character(haven::as_factor(design_test$variables[[mover_test$.grouping_var[i]]]))
      design_test$variables[[row_name]] <- ifelse(
        grouping_labels == as.character(mover_test$vallabel[i]),
        value_vector,
        NA_real_
      )
    }

    row_var_names <- paste0(".ttest_row_", seq_len(nrow(mover_test)))
    row_formula <- stats::as.formula(paste0("~", paste(row_var_names, collapse = " + ")))
    row_estimates <- survey::svymean(row_formula, design = design_test, na.rm = TRUE)
    design_df <- survey::degf(design_test)

    outcomes <- unique(mover_test$outcome)
    for (oc in outcomes) {
      mover_subset <- mover_test %>% filter(outcome == oc)
      varlabels <- unique(mover_subset$varlabel)
      for (vl in varlabels) {
        group_subset <- mover_subset %>% filter(varlabel == vl)
        subset_idx <- match(seq_len(nrow(group_subset)), seq_len(nrow(group_subset)))
        row_idx <- which(mover_test$outcome == oc & mover_test$varlabel == vl)

        for (i in 1:(nrow(group_subset) - 1)) {
          for (j in (i + 1):nrow(group_subset)) {
            contrast_est <- survey::svycontrast(
              row_estimates,
              stats::setNames(c(1, -1), c(row_var_names[row_idx[i]], row_var_names[row_idx[j]]))
            )
            diff_est <- as.numeric(stats::coef(contrast_est))
            diff_se <- sqrt(as.numeric(stats::vcov(contrast_est)))
            diff_t <- diff_est / diff_se
            diff_p <- 2 * stats::pt(-abs(diff_t), df = design_df)

            t_test_results <- rbind(
              t_test_results,
              data.frame(
                outcome = oc,
                varlabel = vl,
                test = paste(group_subset$vallabel[i], "vs", group_subset$vallabel[j]),
                diff = round(diff_est, 3),
                ttest = round(diff_t, 3),
                pval = round(diff_p, 3)
              )
            )
          }
        }
      }
    }

    attr(mover, "t_test_results") <- t_test_results
  }

  mover <- mover %>% select(-any_of(c(".outcome_var", ".grouping_var", ".rec_lo", ".rec_hi")))

  return(mover)
}

