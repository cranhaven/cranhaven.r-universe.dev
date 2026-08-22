########################################

# LAPOP Stacked Bar Graph Pre-Processing

########################################

#' LAPOP Stacked Bar Graph Pre-Processing
#'
#' This function creates dataframes which can then be input in lapop_stack() for
#' plotting variables categories with a stacked bar graph using LAPOP formatting.
#'
#' @param data  The data that should be analyzed. It requires a survey object from lpr_data() function.
#' @param outcome Vector of variables be plotted.
#' @param xvar Character. Outcome variable will be broken down by this variable. Default is NULL
#' @param sort Character. On what value the bars are sorted: the x or the y.
#' Options are "y" (default; for the value of the outcome variable), "xv" (for
#' the underlying values of the x variable), "xl" (for the labels of the x variable,
#' i.e., alphabetical).
#' @param order Character. How the bars should be sorted. Options are "hi-lo"
#' (default) or "lo-hi".
#' @param filesave Character. Path and file name to save the dataframe as csv.
#' @param keep_nr Logical. If TRUE, will convert "don't know" (missing code .a)
#' and "no response" (missing code .b) into valid data (value = 99) and use them
#' in the denominator when calculating percentages.  The default is to examine
#' valid responses only.  Default: FALSE.
#'
#' @return Returns a data frame, with data formatted for visualization by lapop_stack
#'
#' @examples
#'\donttest{
#' require(lapop); data(ym23)
#'
#' # Set Survey Context
#' ym23lpr<-lpr_data(ym23)
#'
#' # Multiple outcomes stacked
#' lpr_stack(data = ym23lpr,
#' outcome = c("b12", "b18"))
#'
#' # Single outcome over years
#' lpr_stack(data = ym23lpr,
#' outcome = "q14f",
#' xvar="year")
#'}
#'
#'@export
#'@import dplyr
#'@import srvyr
#'@import purrr
#'@import haven
#'
#'@author Robert Vidigal, \email{robert.vidigal@@vanderbilt.edu}

# # -----------------------------------------------------------------------
# LPR_STACK
# # -----------------------------------------------------------------------
lpr_stack <- function(data,
                      outcome,
                      xvar = NULL,
                      sort = "y",
                      order = "hi-lo",
                      filesave = "",
                      keep_nr = FALSE) {

  # Helper function to handle a single variable
  process_outcome <- function(data, outcome_var) {
    outcome_sym <- sym(outcome_var)

    # Handle `keep_nr` logic
    if (keep_nr) {
      data <- data %>%
        mutate(!!outcome_sym := case_when(
          na_tag(!!outcome_sym) %in% c("a", "b") ~ 99,
          TRUE ~ as.numeric(!!outcome_sym)
        ))
    }

    # Perform proportion calculations
    stack <- data %>%
      drop_na(!!outcome_sym) %>%
      {
        if (!is.null(xvar)) {
          group_by(., xvar_label = as_factor(!!sym(xvar)), vallabel = as_factor(!!outcome_sym))
        } else {
          group_by(., vallabel = as_factor(!!outcome_sym))
        }
      } %>%
      summarise(
        prop = survey_mean(proportion = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        varlabel = attributes(data$variables[[outcome_var]])$label,
        prop = prop * 100,
        proplabel = sprintf("%.0f%%", prop)
      ) %>%
      {
        if (!is.null(xvar)) {
          select(., varlabel, vallabel, xvar_label, prop, proplabel)
        } else {
          select(., varlabel, vallabel, prop, proplabel)
        }
      }

    # Sorting logic: prioritize sorting by `xvar_label` if provided
    stack <- stack %>%
      {
        if (!is.null(xvar)) {
          arrange(., xvar_label, desc(prop)) # First sort by xvar_label, then by prop
        } else {
          if (sort == "y") {
            if (order == "hi-lo") {
              arrange(., desc(prop))
            } else if (order == "lo-hi") {
              arrange(., prop)
            } else {
              .
            }
          } else if (sort == "xv") {
            if (order == "hi-lo") {
              arrange(., desc(vallabel))
            } else if (order == "lo-hi") {
              arrange(., vallabel)
            } else {
              .
            }
          } else if (sort == "xl") {
            if (order == "hi-lo") {
              arrange(., desc(as.character(vallabel)))
            } else if (order == "lo-hi") {
              arrange(., as.character(vallabel))
            } else {
              .
            }
          } else {
            .
          }
        }
      }

    return(stack)
  }

  # Apply the purrr helper function to all outcomes and combine the results
  results <- map_dfr(outcome, ~ process_outcome(data, .x))

  # Save to file if required
  if (filesave != "") {
    write.csv(results, filesave, row.names = FALSE)
  }

  return(results)
}
