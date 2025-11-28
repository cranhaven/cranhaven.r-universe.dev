#' Consistency Check Function
#'
#' This function performs a consistency check to ensure that the number of tests
#' is greater than the number of cases for given columns in a dataset. It
#' returns a \code{\link[=ggplot2]{ggplot2}} object visualizing the results.
#'
#' @param data A data frame containing the test and case data.
#' @param tests A character vector specifying the column names for the test
#'  data.
#'
#' @param cases A character vector specifying the column names for the case
#' data. The length of 'tests' and 'cases' must be the same, and each element
#' in 'tests' corresponds to an element in 'cases'.
#'
#' @return A \code{\link[=ggplot2]{ggplot2::ggplot()}} object showing
#' the consistency between the number of tests and cases. The x-axis represents
#' the cases, and the y-axis represents the tests. Each facet represents a
#' disease, and the diagonal line shows where the number of tests equals the
#' number of cases.
#'
#' @examples
#' # check the consistency between malaria tests and cases
#'
#' # get path
#' path <- system.file(
#'         "extdata",
#'         "fake_epi_df_togo.rds",
#'          package = "epiCleanr")
#'
#  # get example data
#' fake_epi_df_togo <- import(path)
#'
#' consistency_check(fake_epi_df_togo,
#'                   tests = c("malaria_tests","cholera_tests"),
#'                   cases = c("malaria_cases", "cholera_cases"))
#'
#'
#' @importFrom ggplot2 ggplot geom_point geom_abline theme_bw facet_wrap labs
#' theme_minimal theme scale_x_continuous scale_y_continuous element_text margin
#' @importFrom glue glue
#' @importFrom crayon green red
#'
#' @export
consistency_check <- function(data, tests, cases) {
  # Check if the length of tests and cases are the same
  if (length(tests) != length(cases)) {
    stop("The length of 'tests' and 'cases' must be the same.")
  }

  # Initialize a data frame to store results
  results <- data.frame(disease = character(),
                        cases = numeric(),
                        tests = numeric())

  # Compute statistics and find rows where tests are less than cases
  inconsistent_rows <- list()
  for (i in seq_along(tests)) {
    test_column <- tests[i]
    case_column <- cases[i]

    # Rows where there are more cases than tests
    inconsistency <- data |>
      filter(!is.na(!!sym(case_column)), !is.na(!!sym(test_column))) |>
      filter(!!sym(case_column) > !!sym(test_column))

    inconsistent_count <- nrow(inconsistency)
    inconsistent_prop <- inconsistent_count / nrow(data) * 100
    inconsistent_rows[[i]] <- inconsistency
    disease_name <- paste(test_column, case_column, sep = " vs ")

    results <- rbind(results, data.frame(disease = disease_name,
                                         cases = data[[case_column]],
                                         tests = data[[test_column]]))

    # Check if there are more tests than cases
    if (inconsistent_count == 0) {
      message(
        crayon::green(
          glue::glue(
            "Consistency test passed for {disease_name}: ",
            "There are more tests than there are cases!")
        )
      )
    } else {
      message(
        crayon::red(
          glue::glue(
            "Consistency test failed for {disease_name}: ",
            "There are {scales::comma(inconsistent_count)} ",
            "({round(inconsistent_prop, 2)}%) ",
            "rows where cases are greater than tests.")
        )
      )
    }
  }

  # Create the plot
  plot <- ggplot2::ggplot(results, ggplot2::aes(y = cases, x = tests)) +
    ggplot2::geom_point(shape = 16, size = 4, show.legend = FALSE, alpha = .5,
               color = "#1e81b0", na.rm = TRUE) +
    ggplot2::geom_abline(intercept = 0, linewidth = 1, alpha = .7, slope = 1,
                color = "darkred") +
    ggplot2::theme_bw() +
    ggplot2::facet_wrap(~ disease, scales = "free") +
    ggplot2::labs(
      y = "Cases",
      x = "Tests",
      title = paste("<span style = 'font-size:10pt'><b style='color",
                    ":#526A83'>Consistency Check</b>: Comparing",
                    "the number of tests and cases</span>")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "top",
      plot.title = ggtext::element_markdown(),
      plot.caption = ggplot2::element_text(size = 8),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(
        color = 'grey90', linetype = 3),
      panel.background = ggplot2::element_rect(color = 'grey10',
                                               linewidth = 0.6),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 12)),
      axis.text.x = ggplot2::element_text(size = 8, angle = 45, hjust = 1,
                                 margin = ggplot2::margin(t = 2, b = 2)),
    ) +
    ggplot2::scale_x_continuous(labels = scales::comma_format(big.mark = ','),
                       limits = c(0, NA), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(labels = scales::comma_format(big.mark = ','),
                       limits = c(0, NA), expand = c(0, 0))

  return(plot)
}
