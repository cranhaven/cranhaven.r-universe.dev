######################################################

# LAPOP "Dumbbell" Graph Pre-Processing #

######################################################

#' LAPOP Dumbbell Graphs
#'
#' This function creates dataframes which can then be input in lapop_dumb for
#' comparing means of a variable across countries and two waves using LAPOP formatting.

#' @param data A survey object.  The data that should be analyzed.
#' @param outcome Outcome variable(s) of interest to be plotted across countries
#' and waves, supplied as a character string or vector of strings.
#' @param xvar Character. The grouping variable to be plotted
#' along the x-axis (technically, the vertical axis for lapop_dumb). Usually
#' country (pais). Default: "pais".
#' @param over Numeric. A vector of values for "wave" that specify which two
#' waves should be included in the plot.
#' @param rec Numeric. The minimum and maximum values of the outcome variable that
#' should be included in the numerator of the percentage.  For example, if the variable
#' is on a 1-7 scale and rec is c(5, 7), the function will show the percentage who chose
#' an answer of 5, 6, 7 out of all valid answers.  Can also supply one value only,
#' to produce the percentage that chose that value out of all other values.
#' Default: c(1, 1).
#' @param ci_level Numeric. Confidence interval level for estimates.  Default: 0.95
#' @param mean Logical.  If TRUE, will produce the mean of the variable rather than
#' recoding to percentage.  Default: FALSE.
#' @param filesave Character.  Path and file name to save the dataframe as csv.
#' @param cfmt Character. Changes the format of the numbers displayed above the bars.
#' Uses sprintf string formatting syntax. Default is whole numbers for percentages
#' and tenths place for means.
#' @param sort Character. On what value the bars are sorted.
#' Options are "prop1" (for the value of the outcome variable in wave 1), "prop2"
#' (default; for the value of the outcome variable in wave 2), "xv" (for
#' the underlying values of the x variable), "xl" (for the labels of the x variable,
#' i.e., alphabetical), and "diff" (for the difference between the outcome between
#' the two waves).
#' @param order Character.  How the bars should be sorted.  Options are "hi-lo"
#' (default) or "lo-hi".
#' @param ttest Logical.  If TRUE, will conduct pairwise t-tests for difference
#' of means between all pais-wave combinations and save them in attr(x,
#' "t_test_results"). Default: FALSE.
#' @param keep_nr Logical.  If TRUE, will convert "don't know" (missing code .a)
#' and "no response" (missing code .b) into valid data (value = 99) and use them
#' in the denominator when calculating percentages.  The default is to examine
#' valid responses only.  Default: FALSE.
#'
#' @return Returns a data frame, with data formatted for visualization by lapop_dumb()
#'
#' @examples
#'
#' require(lapop); data(cm23)
#'
#' # Set Survey Context
#' cm23lpr <- lpr_data(cm23)
#'
#' # Single outcome over years
#' lpr_dumb(cm23lpr,
#' outcome = "ing4",
#' rec = c(5, 7),
#' over = c("2018/19", "2023"),
#' sort = "diff")
#'
#' # Multiple outcomes over years
#' lpr_dumb(cm23lpr,
#' outcome=c("b13","b21", "b31"),
#' rec=c(5,7),
#' over=c("2018/19", "2023"))
#'
#'@export
#'@import dplyr
#'@import srvyr
#'@import tibble
#'
#'@author Luke Plutowski, \email{luke.plutowski@@vanderbilt.edu} & Robert Vidigal, \email{robert.vidigal@@vanderbilt.edu}

lpr_dumb <- function(data,
                     outcome,
                     xvar = "pais",
                     over,
                     rec = c(1, 1),
                     ci_level = 0.95,
                     mean = FALSE,
                     filesave = "",
                     cfmt = "",
                     sort = "prop2",
                     order = "hi-lo",
                     ttest = FALSE,
                     keep_nr = FALSE) {

  if (length(rec) == 1) rec <- c(rec, rec)

  if (length(outcome) > 1) {
    results_list <- lapply(outcome, function(out) {
      tmp <- data
      if (keep_nr) {
        tmp <- tmp %>%
          mutate(!!sym(out) := case_when(
            na_tag(!!sym(out)) %in% c("a", "b") ~ 99,
            TRUE ~ as.numeric(!!sym(out))
          ))
      }

      wave1 <- tmp %>%
        filter(wave == over[1]) %>%
        group_by(wave1 = as.character(as_factor(wave))) %>%
        {
          if (mean) {
            summarize(.,
                      prop1 = survey_mean(!!sym(out), na.rm = TRUE, vartype = "ci", level = ci_level)) %>%
              mutate(proplabel1 = if (cfmt != "") sprintf(cfmt, prop1) else sprintf("%.1f", prop1))
          } else {
            summarize(.,
                      prop1 = survey_mean(between(!!sym(out), rec[1], rec[2]), na.rm = TRUE,
                                          vartype = "ci", level = ci_level) * 100) %>%
              mutate(proplabel1 = if (cfmt != "") sprintf(cfmt, round(prop1)) else sprintf("%.0f%%", round(prop1)))
          }
        } %>%
        rename(lb1 = prop1_low, ub1 = prop1_upp)

      wave2 <- tmp %>%
        filter(wave == over[2]) %>%
        group_by(wave2 = as.character(as_factor(wave))) %>%
        {
          if (mean) {
            summarize(.,
                      prop2 = survey_mean(!!sym(out), na.rm = TRUE, vartype = "ci", level = ci_level)) %>%
              mutate(proplabel2 = if (cfmt != "") sprintf(cfmt, prop2) else sprintf("%.1f", prop2))
          } else {
            summarize(.,
                      prop2 = survey_mean(between(!!sym(out), rec[1], rec[2]), na.rm = TRUE,
                                          vartype = "ci", level = ci_level) * 100) %>%
              mutate(proplabel2 = if (cfmt != "") sprintf(cfmt, round(prop2)) else sprintf("%.0f%%", round(prop2)))
          }
        } %>%
        rename(lb2 = prop2_low, ub2 = prop2_upp)

      full <- merge(wave1, wave2, by = character(0))  # no common vars to merge
      full$pais <- attr(tmp[[out]], "label") %||% out
      return(full)
    })

    dumb <- bind_rows(results_list) %>%
      relocate(pais, wave1, prop1, proplabel1, wave2, prop2, proplabel2)

  } else {
    out <- outcome
    if (keep_nr) {
      data <- data %>%
        mutate(!!sym(out) := case_when(
          na_tag(!!sym(out)) %in% c("a", "b") ~ 99,
          TRUE ~ as.numeric(!!sym(out))
        ))
    }

    wave1 <- data %>%
      drop_na(!!sym(xvar)) %>%
      filter(wave == over[1]) %>%
      group_by(pais = as_factor(!!sym(xvar)),
               wave1 = as.character(as_factor(wave))) %>%
      {
        if (mean) {
          summarize(.,
                    prop1 = survey_mean(!!sym(out), na.rm = TRUE, vartype = "ci", level = ci_level)) %>%
            mutate(proplabel1 = if (cfmt != "") sprintf(cfmt, prop1) else sprintf("%.1f", prop1))
        } else {
          summarize(.,
                    prop1 = survey_mean(between(!!sym(out), rec[1], rec[2]), na.rm = TRUE,
                                        vartype = "ci", level = ci_level) * 100) %>%
            mutate(proplabel1 = if (cfmt != "") sprintf(cfmt, round(prop1)) else sprintf("%.0f%%", round(prop1)))
        }
      } %>%
      rename(lb1 = prop1_low, ub1 = prop1_upp)

    wave2 <- data %>%
      drop_na(!!sym(xvar)) %>%
      filter(wave == over[2]) %>%
      group_by(pais = as_factor(!!sym(xvar)),
               wave2 = as.character(as_factor(wave))) %>%
      {
        if (mean) {
          summarize(.,
                    prop2 = survey_mean(!!sym(out), na.rm = TRUE, vartype = "ci", level = ci_level)) %>%
            mutate(proplabel2 = if (cfmt != "") sprintf(cfmt, prop2) else sprintf("%.1f", prop2))
        } else {
          summarize(.,
                    prop2 = survey_mean(between(!!sym(out), rec[1], rec[2]), na.rm = TRUE,
                                        vartype = "ci", level = ci_level) * 100) %>%
            mutate(proplabel2 = if (cfmt != "") sprintf(cfmt, round(prop2)) else sprintf("%.0f%%", round(prop2)))
        }
      } %>%
      rename(lb2 = prop2_low, ub2 = prop2_upp)

    dumb <- merge(wave1, wave2, by = "pais")
  }

  dumb = dumb %>%
    {
      if (sort == "prop1") {
        if (order == "hi-lo") {
          arrange(., desc(prop1))
        } else if (order == "lo-hi") {
          arrange(., prop1)
        }
      } else if (sort == "prop2") {
        if (order == "hi-lo") {
          arrange(., desc(prop2))
        } else if (order == "lo-hi") {
          arrange(., prop2)
        }
      } else if (sort == "xv") {
        if (order == "hi-lo") {
          arrange(., desc(match(pais, levels(pais))))
        } else if (order == "lo-hi") {
          arrange(., match(pais, levels(pais)))
        }
      } else if (sort == "diff") {
        if (order == "hi-lo") {
          mutate(., diff = prop2 - prop1) %>%
            arrange(., desc(diff))
        } else if (order == "lo-hi") {
          mutate(., diff = prop2 - prop1) %>%
            arrange(., diff)
        }
      } else if (sort == "xl") {
        if (order == "hi-lo") {
          arrange(., desc(as.character(xvar)))
        } else if (order == "lo-hi") {
          arrange(., as.character(xvar))
        } else {
          .  # Return unchanged
        }
      }
    }


  if (ttest) {
    t_test_results_df <- data.frame(test = character(),
                                    diff = numeric(),
                                    ttest = numeric(),
                                    pval = numeric(),
                                    stringsAsFactors = FALSE)
    design_test <- data
    if (length(outcome) > 1) {
      if (keep_nr) {
        design_test <- design_test %>%
          mutate(across(all_of(outcome), ~ case_when(
            na_tag(.) %in% c("a", "b") ~ 99,
            TRUE ~ as.numeric(.)
          )))
      }
    } else if (keep_nr) {
      design_test <- design_test %>%
        mutate(!!sym(outcome) := case_when(
          na_tag(!!sym(outcome)) %in% c("a", "b") ~ 99,
          TRUE ~ as.numeric(!!sym(outcome))
        ))
    }

    wave_labels <- as.character(haven::as_factor(design_test$variables$wave))
    x_labels <- if (length(outcome) == 1) as.character(haven::as_factor(design_test$variables[[xvar]])) else NULL

    cell_specs <- data.frame(
      row_id = rep(seq_len(nrow(dumb)), each = 2),
      cell = rep(c("prop1", "prop2"), times = nrow(dumb)),
      outcome_var = if (length(outcome) > 1) rep(outcome, each = 2) else rep(outcome, times = 2 * nrow(dumb)),
      wave_label = c(as.character(dumb$wave1), as.character(dumb$wave2)),
      group_label = if (length(outcome) == 1) rep(as.character(dumb$pais), each = 2) else NA_character_,
      stringsAsFactors = FALSE
    )
    cell_specs$cell_name <- paste0(".ttest_cell_", seq_len(nrow(cell_specs)))

    for (i in seq_len(nrow(cell_specs))) {
      outcome_values <- as.numeric(design_test$variables[[cell_specs$outcome_var[i]]])
      value_vector <- if (mean) {
        outcome_values
      } else {
        as.numeric(dplyr::between(outcome_values, rec[1], rec[2])) * 100
      }

      domain <- wave_labels == cell_specs$wave_label[i]
      if (!is.null(x_labels)) {
        domain <- domain & x_labels == cell_specs$group_label[i]
      }

      design_test$variables[[cell_specs$cell_name[i]]] <- ifelse(domain, value_vector, NA_real_)
    }

    cell_formula <- stats::as.formula(paste0("~", paste(cell_specs$cell_name, collapse = " + ")))
    cell_estimates <- survey::svymean(cell_formula, design = design_test, na.rm = TRUE)
    design_df <- survey::degf(design_test)

    for (i in seq_len(nrow(dumb))) {
      cell1 <- cell_specs$cell_name[cell_specs$row_id == i & cell_specs$cell == "prop1"]
      cell2 <- cell_specs$cell_name[cell_specs$row_id == i & cell_specs$cell == "prop2"]
      contrast_est <- survey::svycontrast(cell_estimates, stats::setNames(c(1, -1), c(cell1, cell2)))
      diff_est <- as.numeric(stats::coef(contrast_est))
      diff_se <- sqrt(as.numeric(stats::vcov(contrast_est)))
      diff_t <- diff_est / diff_se
      diff_p <- 2 * stats::pt(-abs(diff_t), df = design_df)

      t_test_results_df <- rbind(
        t_test_results_df,
        data.frame(
          test = paste(dumb$pais[i], dumb$wave1[i], "vs", dumb$pais[i], dumb$wave2[i]),
          diff = round(diff_est, 3),
          ttest = round(diff_t, 3),
          pval = round(diff_p, 3)
        )
      )
    }

    for (i in 1:(nrow(dumb) - 1)) {
      for (j in (i + 1):nrow(dumb)) {
        cell_i <- cell_specs$cell_name[cell_specs$row_id == i & cell_specs$cell == "prop1"]
        cell_j <- cell_specs$cell_name[cell_specs$row_id == j & cell_specs$cell == "prop1"]
        contrast_est <- survey::svycontrast(cell_estimates, stats::setNames(c(1, -1), c(cell_i, cell_j)))
        diff_est <- as.numeric(stats::coef(contrast_est))
        diff_se <- sqrt(as.numeric(stats::vcov(contrast_est)))
        diff_t <- diff_est / diff_se
        diff_p <- 2 * stats::pt(-abs(diff_t), df = design_df)

        t_test_results_df <- rbind(
          t_test_results_df,
          data.frame(
            test = paste(dumb$pais[i], dumb$wave1[i], "vs", dumb$pais[j], dumb$wave1[j]),
            diff = round(diff_est, 3),
            ttest = round(diff_t, 3),
            pval = round(diff_p, 3)
          )
        )
      }
    }

    for (i in 1:(nrow(dumb) - 1)) {
      for (j in (i + 1):nrow(dumb)) {
        cell_i <- cell_specs$cell_name[cell_specs$row_id == i & cell_specs$cell == "prop2"]
        cell_j <- cell_specs$cell_name[cell_specs$row_id == j & cell_specs$cell == "prop2"]
        contrast_est <- survey::svycontrast(cell_estimates, stats::setNames(c(1, -1), c(cell_i, cell_j)))
        diff_est <- as.numeric(stats::coef(contrast_est))
        diff_se <- sqrt(as.numeric(stats::vcov(contrast_est)))
        diff_t <- diff_est / diff_se
        diff_p <- 2 * stats::pt(-abs(diff_t), df = design_df)

        t_test_results_df <- rbind(
          t_test_results_df,
          data.frame(
            test = paste(dumb$pais[i], dumb$wave2[i], "vs", dumb$pais[j], dumb$wave2[j]),
            diff = round(diff_est, 3),
            ttest = round(diff_t, 3),
            pval = round(diff_p, 3)
          )
        )
      }
    }

    # Store the results as an attribute
    attr(dumb, "t_test_results") <- t_test_results_df
  }


  if (filesave != "") write.csv(dumb, filesave, row.names = FALSE)
  return(dumb)
}
