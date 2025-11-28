#' Detect and Handle Outliers in Dataset
#'
#' This function identifies and handles outliers in a given dataset using
#' various methods including Z-Score, Modified Z-Score, and Inter-Quartile Range
#'  (IQR). It also provides options to treat the identified outliers, using
#'  mean, median, rolling mean by group and inter-quartile range. It also has
#'  the option to generate a summary report and a plot.
#'
#' @param data Dataframe containing the variables to be checked for outliers.
#' @param vars Character vector of variable names to check for outliers. Default
#'            is NULL, which selects all numeric columns.
#' @param method Character indicating the method for outlier detection. Options
#'                  are "zscore", "mod_zscore", and "iqr_method".
#'                  Default is NULL, which applies all methods.
#' @param zscore_threshold Numeric value for Z-Score threshold. Default is 3.
#' @param mod_zscore_threshold Numeric value for Modified Z-Score threshold.
#'         Default is 3.5.
#' @param iqr_k_value Numeric value for IQR multiplier. Default is 1.5.
#' @param treat_method Character indicating how to treat outliers. Options are
#'                    "none", "remove", "mean", "median", "grouped_mean", and
#'                    "quantile". Default is "none".
#' @param grouping_vars Character vector of grouping variables for
#'                     "grouped_mean". Required only if treat_method is
#'                      "grouped_mean".
#' @param report_mode Logical, if TRUE, the function returns a summary report
#'                    and a plot. Default is FALSE.
#'
#' @return
#' If report_mode is TRUE, a list containing a summary dataframe and a ggplot
#' object. Otherwise, a dataframe with outliers treated according to
#' treat_method.
#'
#' @examples
#'
#' # get path
#' path <- system.file(
#'         "extdata",
#'         "fake_epi_df_togo.rds",
#'          package = "epiCleanr")
#'
# # get example data
#' fake_epi_df_togo <- import(path)
#'
#' variables <- c("malaria_tests", "malaria_cases",
#'                  "cholera_tests", "cholera_cases")
#' result <- handle_outliers(fake_epi_df_togo, vars = variables,
#'                method = "zscore", report_mode = TRUE)
#'
#' print(result$report)
#'
#' print(result$plot)
#'
#' @importFrom dplyr select mutate across where row_number summarise left_join
#'             everything all_of arrange group_by bind_cols
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_jitter labs theme_bw theme_minimal theme
#'              scale_color_manual scale_x_continuous guides
#' @importFrom glue glue
#' @importFrom stringr str_replace_all str_replace
#' @importFrom stats mad median quantile
#' @export

handle_outliers <- function(data, vars = NULL, method = NULL,
                            zscore_threshold = 3, mod_zscore_threshold = 3.5,
                            iqr_k_value = 1.5, treat_method = "none",
                            grouping_vars = NULL,
                            report_mode = FALSE) {

  suppressMessages(
    suppressWarnings({
      # select var(s)
      if (is.null(vars)) {
        df <- data |>
          dplyr::select(
            where(~ is.numeric(.)) & !tidyselect::all_of(grouping_vars))
      } else {
        # Select only the specified vars
        df <- data |>
          dplyr::select(tidyselect::all_of(vars))
      }

      list_of_dfs <- list()

      # Detect outlier using Z-Score approach ----------------------------------
      if (is.null(method) || any(method == "zscore")) {
        list_of_dfs$`Z-Score` <- df |>
          dplyr::reframe(
            dplyr::across(
              where(is.numeric),
              list(
                ~ {
                  zscores <- scale(.)
                  result <- abs(zscores) > zscore_threshold
                  result[is.na(result)] <- FALSE
                  return(result)
                }
              ),
              .names = "{.col}"
            )
          )
      }

      # Detect outlier using Modified Z-Score approach -------------------------

      if (is.null(method) || any(method == "mod_zscore")) {
        list_of_dfs$`Modified Z-Score` <- df |>
          dplyr::reframe(
            dplyr::across(
              where(is.numeric),
              list(
                ~ {
                  mad_val <- mad(., na.rm = TRUE)
                  median_val <- median(., na.rm = TRUE)
                  modified_zscores <- 0.6745 * (. - median_val) / mad_val
                  result <- abs(modified_zscores) > mod_zscore_threshold
                  result[is.na(result)] <- FALSE
                  return(result)
                }
              ),
              .names = "{.col}"
            )
          )
      }

      # Detect outlier using Inter-quartile range method (Tukey Fences) approach

      if (is.null(method) || any(method == "iqr_method")) {
        list_of_dfs$`Inter-Quartile Range` <- df |>
          dplyr::reframe(
            dplyr::across(
              where(is.numeric),
              list(
                ~ {
                  Q1 <- quantile(., 0.25, na.rm = TRUE)
                  Q3 <- quantile(., 0.75, na.rm = TRUE)
                  IQR <- Q3 - Q1
                  result <- (. < (Q1 - iqr_k_value * IQR)) |
                    (. > (Q3 + iqr_k_value * IQR))
                  result[is.na(result)] <- FALSE
                  return(result)
                }
              ),
              .names = "{.col}"
            )
          )
      }

      # Make data frame of outlier results -------------------------------------

      # Iterate over the list and create summary for each dataframe
      summary_dfs <- lapply(names(list_of_dfs), function(test_name) {
        df <- list_of_dfs[[test_name]]

        df_summary <- df |>
          dplyr::summarise(
            dplyr::across(
              tidyselect::everything(), ~ sum(.x, na.rm = TRUE)
            )
          ) |>
          tidyr::pivot_longer(
            cols = tidyselect::everything(),
            names_to = "variable", values_to = "outliers"
          )

        df_summary <- df_summary |>
          dplyr::mutate(
            test = test_name,
            prop_outliers = scales::percent(.data$outliers/nrow(df),
                                            accuracy = 2),
            outliers = glue::glue("{.data$outliers}/{scales::comma(nrow(df))}")
          ) |>
          # give any value less than one and not zero '<1%' indicator
          mutate(prop_outliers = ifelse(.data$prop_outliers != 0 &
                                          .data$prop_outliers < 1,
                                        "<1%", .data$prop_outliers)) |>
          dplyr::select("variable", "test", "outliers", "prop_outliers")

        return(df_summary)
      })

      # Combine all the summary dataframes
      final_summary_df <- do.call(rbind, summary_dfs) |>
        dplyr::mutate(
          variable = stringr::str_replace_all(
            .data$variable,
            c(
              "_outliers_z" = "",
              "_outliers_modified_z" = "",
              "_outliers_iqr " = ""
            )
          )
        )

      # Plot -------------------------------------------------------------------

      # do method mapping
      if (is.null(method)) {
        method <- "Modified Z-Score"
      } else if (any(method == "zscore")) {
        method <- "Z-Score"
      } else if (any(method == "mod_zscore")) {
        method <- "Modified Z-Score"
      } else if (any(method == "iqr_method")) {
        method <- "Inter-Quartile Range"
      }


      # Determine which plot to show
      if (is.null(method) || any(method == "mod_zscore")) {
        chosen_df <- list_of_dfs$`Modified Z-Score`
        title_suffix <- "Modified Z-Score"
      } else {
        chosen_df <- list_of_dfs[[method]]
        title_suffix <- method
      }

      # Adding row numbers for joining
      df_id <- df |> dplyr::mutate(id = dplyr::row_number())
      chosen_df <- chosen_df |> dplyr::mutate(id = dplyr::row_number())

      # Joining the data frames
      combined_df <- df_id |>
        dplyr::left_join(chosen_df,
                         by = "id",
                         suffix = c("_df", "_outlier_bool")
        ) |>
        # make sure the group vars dont have df at the end of names
        dplyr::left_join(
          data[grouping_vars] |>
            dplyr::mutate(id = dplyr::row_number()),
          by = "id"
        )


      # Turn it into long format
      combined_df_long <- combined_df |>
        # make the values cols long
        tidyr::pivot_longer(
          cols = tidyselect::contains("_df"),
          names_to = "Variable",
          values_to = "Values"
        ) |>
        mutate(Variable = stringr::str_replace(Variable, "_df", "")) |>
        left_join(
          combined_df |>
            # make the values cols long
            tidyr::pivot_longer(
              cols = contains("_outlier_bool"),
              names_to = "Variable",
              values_to = "is_outlier"
            ) |>
            mutate(Variable = stringr::str_replace(Variable,
                                                   "_outlier_bool", "")),
          by = c("Variable", "id")
        ) |>
        dplyr::select("Variable", "Values", "is_outlier")

      # plot the data
      plot <- combined_df_long |>
        ggplot2::ggplot(
          ggplot2::aes(x = Values, y = Variable)
        ) +
        geom_jitter(aes(color = is_outlier),
                    height = 0.05, size = 4,
                    alpha = 0.5, na.rm = T
        ) +
        ggdist::stat_halfeye(
          adjust = .5,
          width = .6, na.rm = T,
          justification = -.17
        ) +
        ggplot2::scale_color_manual(values = c("#1e81b0", "#b44b1c")) +
        ggplot2::theme_bw() +
        ggplot2::labs(
          title = paste(
            "<span style='font-size:12pt; color:#21130D'><b>Outlier",
            "Plot (Method: ",
            title_suffix,
            ")</b>: Outliers in <span style='color:#B44B1C'>orange</span>,",
            "Non-outliers in <span style='color:#1E81B0'>blue</span></span>"
          )
        ) +
        ggh4x::facet_grid2(
          Variable ~ ., scales = "free", independent  = "all"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          legend.position = "top",
          plot.title = ggtext::element_markdown(),
          plot.caption = ggplot2::element_text(size = 8),
          panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_line(
            color = "grey90", linetype = 3
          ),
          panel.background = ggplot2::element_rect(
            color = "grey10",
            linewidth = 0.6
          ),
          axis.title.y = ggplot2::element_text(
            margin = ggplot2::margin(r = 10)),
          axis.title.x = ggplot2::element_text(
            margin = ggplot2::margin(t = 12)),
          axis.text.x = ggplot2::element_text(
            size = 8, angle = 45, hjust = 1,
            margin = ggplot2::margin(t = 2, b = 2)
          ),
          strip.text.x = ggplot2::element_blank(),
          strip.text.y = ggplot2::element_blank()
        )  +
        ggplot2::guides(color = "none") +
        ggplot2::scale_x_continuous(
          labels = scales::comma_format(big.mark = ","))

      # Deal with the outliers -------------------------------------------------

      # Set up methods to use
      if (treat_method == "remove") {
        na_method <- NA_real_
      } else if (treat_method == "mean") {
        mean_vals <- combined_df |>
          dplyr::summarise(
            dplyr::across(
              tidyselect::ends_with("_df"),
              \(x) mean(x, na.rm = TRUE)
            )
          )
        na_method <- mean_vals
      } else if (treat_method == "median") {
        median_vals <- combined_df |>
          dplyr::summarise(
            dplyr::across(
              tidyselect::ends_with("_df"),
              \(x) median(x, na.rm = TRUE)
            )
          )
        na_method <- median_vals
      } else if (treat_method == "grouped_mean") {
        if (is.null(grouping_vars)) {
          stop("grouping_vars must be provided for grouped mean")
        }
        grouped_mean_vals <- combined_df |>
          dplyr::group_by(across(all_of(grouping_vars))) |>
          dplyr::arrange(across(all_of(grouping_vars))) |>
          dplyr::summarise(
            dplyr::across(
              tidyselect::ends_with("_df"),
              \(x) mean(x, na.rm = TRUE)
            ), .groups = "drop"
          )
        na_method <- grouped_mean_vals
      } else if (treat_method == "quantile") {
        quantile_vals <- combined_df |>
          dplyr::summarise(
            dplyr::across(
              tidyselect::ends_with("_df"),
              ~ quantile(., 0.25, na.rm = TRUE)
            )
          )
        na_method <- quantile_vals
      } else if (treat_method == "none") {
        na_method <- NULL
      } else {
        stop("Unknown treat_method: ", treat_method)
      }


      # Handle outlier values based on the method of interest
      no_outlier_df <- combined_df |>
        dplyr::mutate(
          dplyr::across(
            .cols = tidyselect::ends_with("_df"),
            ~ {
              if (treat_method == "none") {
                return(.)
              }

              outlier_col <- stringr::str_replace(
                dplyr::cur_column(),
                "_df", "_outlier_bool"
              )

              if (treat_method == "remove") {
                return(ifelse(
                  get(outlier_col) == TRUE,
                  NA_real_,
                  .
                ))
              }

              ifelse(
                get(outlier_col) == TRUE,
                na_method[[dplyr::cur_column()]],
                .
              )
            },
            .names = "{str_replace(.col, '_df', '')}"
          )
        ) |>
        select(tidyselect::all_of(colnames(df)))

      # now return the full dataset without outliers
      full_data <- data |>
        select(-tidyselect::all_of(colnames(df))) |>
        dplyr::bind_cols(no_outlier_df) |>
        as.data.frame()

      # Return the appropriate output ------------------------------------------
      if (report_mode) {
        return(list(
          report = final_summary_df,
          plot = plot
        ))
      } else {
        return(full_data)
      }
    }))

}


