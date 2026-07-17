#' @title Apply sign test
#' @description This function applies the one-sample sign test to input data grouped by specified variables.
#' @param stats_data A data frame containing the calculated growth performance data,
#' e.g. via a function call to \code{calculate_growth_performance}.
#' @param summarized_data A data frame containing corresponding summarized data, e.g. via function call
#' \code{summarize_growth_performance}.
#' @param value The column containing absorption values to be tested. Defaults to 'Value'.
#' @param p.signif The column containing significance denoted in asterisk notation. Defaults
#' to 'p.signif'.
#' @param grouping A character vector specifying the grouping variables.
#' @param na A character value specifying the keyword to display if sign tests cannot be applied
#' on subsets of the data (e.g. because of too small sample sizes). Defaults to "NA".
#' @return A data frame containing the summarized data with sign test results added.
#' @importFrom stats reformulate
#' @importFrom dplyr group_by_at
#' @importFrom dplyr left_join
#' @importFrom rlang .data
#' @importFrom rstatix sign_test adjust_pvalue add_significance
#' @seealso
#' \code{\link{calculate_growth_performance}}, \code{\link{summarize_growth_performance}},
#' \code{\link{plot_growth_performance}},
#' @export
apply_sign_test <- function(stats_data,
                            summarized_data,
                            value = 'Value',
                            p.signif = 'p.signif',
                            grouping = NULL,
                            na = "NA") {
  # Generate formula for one-sample sign test
  f <- reformulate("1", response = value)
  
  # Statistics
  stats_summary <- stats_data %>%
    group_by_at(vars({{ grouping }})) %>% 
    sign_test(f, mu = 0) %>%
    add_significance("p") %>% 
    mutate(p.signif = ifelse(n < 6, na, p.signif))
  
  # Join with summarized data
  joined_data <- left_join(summarized_data, stats_summary,
                           by = grouping)
  
  return(joined_data)
  }
