#######################################

# LAPOP Multi-Line Time Series Graph Pre-Processing #

#######################################

#' LAPOP Multi-Line Time Series Graph Pre-Processing
#'
#' This function creates a dataframe which can then be input in lapop_mline for
#' to show a time series plot with multiple lines.  If one "outcome" variable and an
#' `xvar` variable is supplied, the function produces the values of a single outcome
#' variable, broken down by a secondary variable, across time.  If multiple outcome
#' variables (up to four) are supplied, it will show means/percentages of those
#' variables across time (essentially, it allows you to do lpr_ts for multiple variables).
#'
#' @param data A survey object.  The data that should be analyzed.
#' @param outcome Character vector.  Outcome variable(s) of interest to be plotted
#' across time.  If only one value is provided, the graph will show the outcome
#' variable, over time, broken down by a secondary variable (x-var).
#' If more than one value is supplied, the graph will show each outcome variable
#' across time (no secondary variable).
#' @param rec,rec2,rec3,rec4 Numeric. The minimum and maximum values of the outcome
#' variable that should be included in the numerator of the percentage.
#' For example, if the variable is on a 1-7 scale and rec is c(5, 7), the
#' function will show the percentage who chose an answer of 5, 6, 7 out of
#' all valid answers.  Can also supply one value only, to produce the percentage
#' that chose that value out of all other values. Default: c(1, 1).
#' @param xvar Character. Variable on which to break down the outcome variable.
#' In other words, the line graph will produce multiple lines for each value of
#' xvar (technically, it is the z-variable, not the x variable, which is year/wave).
#' Ignored if multiple outcome variables are supplied.
#' @param use_wave Logical.  If TRUE, will use "wave" for the x-axis; otherwise,
#' will use "year".  Default: FALSE.
#' @param use_cat Logical. If TRUE, will show the percentages of category values
#' of a single variable; otherwise will show percentages of the range of values
#' from rec(). Default FALSE.
#' @param ci_level Numeric. Confidence interval level for estimates.  Default: 0.95
#' @param mean Logical.  If TRUE, will produce the mean of the variable rather than
#' rescaling to percentage.  Default: FALSE.
#' @param filesave Character.  Path and file name to save the dataframe as csv.
#' @param cfmt Character. changes the format of the numbers displayed above the bars.
#' Uses sprintf string formatting syntax. Default is whole numbers for percentages
#' and tenths place for means.
#' @param ttest Logical.  If TRUE, will conduct pairwise t-tests for difference
#' of means between all individual x levels and save them in attr(x,
#' "t_test_results"). Default: FALSE.
#' @param keep_nr Logical.  If TRUE, will convert "don't know" (missing code .a)
#' and "no response" (missing code .b) into valid data (value = 99) and use them
#' in the denominator when calculating percentages.  The default is to examine
#' valid responses only.  Default: FALSE.
#'
#' @return Returns a data frame, with data formatted for visualization by lapop_mline
#'
#' @examples
#'\donttest{
#'require(lapop); data(ym23)
#'
#'# Set Survey Context
#'ym23lpr <- lpr_data(ym23)
#'
#' # Single Variable by Country and Year
#' lpr_mline(ym23lpr,
#' outcome = "ing4",
#' rec = c(5, 7),
#' xvar = "pais",
#' use_wave = TRUE)
#'
#' # Multiple Variables
#' lpr_mline(ym23lpr,
#' outcome = c("b12", "b18"),
#' rec = c(5, 7),
#' rec2 = c(1, 2),
#' rec3 = c(5, 7),
#' use_wave = TRUE)
#'
#' # Binary Single Variable by Category
#' lpr_mline(ym23lpr,
#' outcome = "pn4",
#' use_cat = TRUE,
#' use_wave = TRUE)
#'
#' # Recode Categorical Variable (max 4-categories)
#' lpr_mline(ym23lpr,
#' outcome = "ing4",
#' rec = c(5, 7),
#' use_cat = TRUE,
#' use_wave = TRUE)
#'}
#'@export
#'@import dplyr
#'@import srvyr
#'
#'@author Luke Plutowski, \email{luke.plutowski@@vanderbilt.edu} & Robert Vidigal, \email{robert.vidigal@@vanderbilt.edu}
lpr_mline <- function(data,
                      outcome,
                      rec = c(1, 1),
                      rec2 = c(1, 1),
                      rec3 = c(1, 1),
                      rec4 = c(1, 1),
                      xvar,
                      use_wave = FALSE,
                      use_cat = FALSE,
                      ci_level = 0.95,
                      mean = FALSE,
                      filesave = "",
                      cfmt = "",
                      ttest = FALSE,
                      keep_nr = FALSE) {

  # 1. Input validation
  if (!all(outcome %in% names(data$variables))) {
    missing_vars <- setdiff(outcome, names(data$variables))
    stop(paste("Variable(s) not found in data:", paste(missing_vars, collapse = ", ")))
  }

  if (length(rec) == 1) rec <- c(rec, rec)
  if (length(rec2) == 1) rec2 <- c(rec2, rec2)
  if (length(rec3) == 1) rec3 <- c(rec3, rec3)
  if (length(rec4) == 1) rec4 <- c(rec4, rec4)

  # 2. Store variable labels for all outcomes
  var_labels <- sapply(outcome, function(x) {
    lbl <- attr(data$variables[[x]], "label")
    if (is.null(lbl)) x else lbl
  })

  # 3. Handle categorical case
  if (use_cat) {
    if (length(outcome) > 1) stop("When use_cat=TRUE, only one outcome variable should be provided")

    # Get value labels for categories
    val_labels <- attr(data$variables[[outcome]], "labels")

    # Get categories from rec range or data
    categories <- if (!identical(rec, c(1,1))) {
      rec[1]:rec[2]
    } else {
      sort(unique(na.omit(data$variables[[outcome]])))
    }

    if (!keep_nr) categories <- categories[categories != 99]
    if (length(categories) > 4) stop("Variable has more than 4 categories. Consider recoding.")

    # Create rec_list based on categories
    rec_list <- lapply(categories, function(x) c(x, x))
    outcome_rep <- rep(outcome, length(categories))
    rec <- rec_list[[1]]
    if (length(rec_list) > 1) rec2 <- rec_list[[2]]
    if (length(rec_list) > 2) rec3 <- rec_list[[3]]
    if (length(rec_list) > 3) rec4 <- rec_list[[4]]
  }

  # 4. Process data
  if (length(outcome) > 1 || use_cat) {
    rec_list <- if (use_cat) list(rec, rec2, rec3, rec4)[1:length(categories)]
else list(rec, rec2, rec3, rec4)[1:length(outcome)]

result_list <- lapply(seq_along(if (use_cat) categories else outcome), function(i) {
  temp <- lpr_ts(data = data,
                 outcome = if (use_cat) outcome else outcome[i],
                 rec = rec_list[[i]],
                 use_wave = use_wave,
                 mean = mean,
                 cfmt = cfmt,
                 keep_nr = keep_nr)

  if (use_cat) {
    temp$category <- categories[i]
  } else {
    temp$varlabel <- var_labels[i]
  }
  temp$.outcome_var <- if (use_cat) outcome else outcome[i]
  temp$.rec_lo <- rec_list[[i]][1]
  temp$.rec_hi <- rec_list[[i]][2]
  temp$.category_value <- if (use_cat) categories[i] else NA
  temp$.group_value <- NA_character_
  temp
})

mline <- bind_rows(result_list)

# Handle waves
if (nrow(mline) > 0) {
  all_waves <- unique(mline$wave)
  join_by <- if (use_cat) c("wave", "category") else c("wave", "varlabel")

  full_grid <- expand.grid(
    wave = all_waves,
    if (use_cat) unique(mline$category) else unique(mline$varlabel),
    stringsAsFactors = FALSE
  )
  names(full_grid)[2] <- if (use_cat) "category" else "varlabel"

  mline <- full_grid %>%
    left_join(mline, by = join_by) %>%
    arrange(if (use_cat) category else varlabel, wave)

  # Filter combined waves
  if (any(c("2016","2017") %in% mline$wave)) mline <- filter(mline, wave != "2016/17")
  if (any(c("2018","2019") %in% mline$wave)) mline <- filter(mline, wave != "2018/19")
}

# Apply category labels if use_cat
if (use_cat && exists("val_labels")) {
  mline <- mline %>%
    mutate(varlabel = ifelse(
      category %in% val_labels,
      names(val_labels)[match(category, val_labels)],
      as.character(category)
    ))
}

} else {
  # Single outcome case
  if (keep_nr) {
    data <- data %>%
      mutate(!!outcome := case_when(
        na_tag(!!outcome) == "a" | na_tag(!!outcome) == "b" ~ 99,
        TRUE ~ as.numeric(!!outcome)
      ))
  }

  mline <- data %>%
    drop_na(!!sym(xvar)) %>%
    group_by(varlabel = as_factor(!!sym(xvar)),
             wave = if (use_wave) as.character(as_factor(wave)) else year) %>%
    {
      if (mean) {
        summarize(., prop = survey_mean(!!sym(outcome),
                                        na.rm = TRUE,
                                        vartype = "ci",
                                        level = ci_level)) %>%
          mutate(proplabel = sprintf(if (cfmt != "") cfmt else "%.1f", prop))
      } else {
        summarize(., prop = survey_mean(between(!!sym(outcome), rec[1], rec[2]),
                                        na.rm = TRUE,
                                        vartype = "ci",
                                        level = ci_level) * 100) %>%
          mutate(proplabel = sprintf(if (cfmt != "") cfmt else "%.0f%%", round(prop)))
      }
    } %>%
    filter(prop != 0) %>%
    rename(lb = prop_low, ub = prop_upp) %>%
    mutate(
      .outcome_var = outcome,
      .rec_lo = rec[1],
      .rec_hi = rec[2],
      .category_value = NA,
      .group_value = as.character(varlabel)
    )
}

  # 5. T-tests if requested
  if (ttest && nrow(mline) > 1) {
    design_test <- data
    if (keep_nr) {
      design_test <- design_test %>%
        mutate(across(all_of(unique(stats::na.omit(mline$.outcome_var))), ~ case_when(
          na_tag(.) == "a" | na_tag(.) == "b" ~ 99,
          TRUE ~ as.numeric(.)
        )))
    }

    wave_labels <- if (use_wave) {
      as.character(haven::as_factor(design_test$variables$wave))
    } else {
      as.character(design_test$variables$year)
    }
    group_labels <- if (length(outcome) == 1 && !use_cat) {
      as.character(haven::as_factor(design_test$variables[[xvar]]))
    } else {
      NULL
    }

    mline_test <- mline %>% filter(!is.na(prop))
    for (i in seq_len(nrow(mline_test))) {
      row_name <- paste0(".ttest_row_", i)
      outcome_values <- as.numeric(design_test$variables[[mline_test$.outcome_var[i]]])
      value_vector <- if (mean) {
        outcome_values
      } else if (!is.na(mline_test$.category_value[i])) {
        as.numeric(outcome_values == mline_test$.category_value[i]) * 100
      } else {
        as.numeric(dplyr::between(outcome_values, mline_test$.rec_lo[i], mline_test$.rec_hi[i])) * 100
      }

      domain <- wave_labels == as.character(mline_test$wave[i])
      if (!is.null(group_labels)) {
        domain <- domain & group_labels == as.character(mline_test$.group_value[i])
      }

      design_test$variables[[row_name]] <- ifelse(domain, value_vector, NA_real_)
    }

    row_var_names <- paste0(".ttest_row_", seq_len(nrow(mline_test)))
    row_formula <- stats::as.formula(paste0("~", paste(row_var_names, collapse = " + ")))
    row_estimates <- survey::svymean(row_formula, design = design_test, na.rm = TRUE)
    design_df <- survey::degf(design_test)

    t_test_results <- combn(seq_len(nrow(mline_test)), 2, function(pair) {
      i <- pair[1]
      j <- pair[2]
      contrast_est <- survey::svycontrast(
        row_estimates,
        stats::setNames(c(1, -1), c(row_var_names[i], row_var_names[j]))
      )
      diff_est <- as.numeric(stats::coef(contrast_est))
      diff_se <- sqrt(as.numeric(stats::vcov(contrast_est)))
      diff_t <- diff_est / diff_se
      diff_p <- 2 * stats::pt(-abs(diff_t), df = design_df)

      data.frame(
        test = paste(mline_test$varlabel[i], mline_test$wave[i], "vs",
                     mline_test$varlabel[j], mline_test$wave[j]),
        diff = round(diff_est, 3),
        t_stat = round(diff_t, 3),
        p_value = round(diff_p, 3),
        stringsAsFactors = FALSE
      )
    }, simplify = FALSE) %>% bind_rows()

    attr(mline, "t_test_results") <- t_test_results
  }

  mline <- mline %>% select(-any_of(c(".outcome_var", ".rec_lo", ".rec_hi", ".category_value", ".group_value")))

  # 6. Save if requested
  if (filesave != "") {
    write.csv(mline, filesave, row.names = FALSE)
  }

  return(mline)
  }
