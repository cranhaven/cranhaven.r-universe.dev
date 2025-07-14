#' Compute Age Proportions
#'
#' @description
#' Calculates the proportion of population in each age interval using gamma
#' distribution parameters. Handles both regular intervals and the final
#' open-ended interval.
#'
#' @param sim Integer index for current simulation
#' @param scale Matrix of scale parameters for gamma distribution
#' @param shape Matrix of shape parameters for gamma distribution
#' @param run Integer index for current age interval being processed
#' @param limslow Numeric vector of lower age limits for intervals
#' @param limsup Numeric vector of upper age limits for intervals
#'
#' @return Numeric vector of proportions for the given age interval
#'
#' @noRd
compute_age_proportions <- function(sim, scale, shape, run, limslow, limsup) {
  if (run == length(limslow)) {
    # For the last interval (open-ended)
      1 - stats::pgamma(limslow[run],
                        scale = scale[, sim],
                        shape = shape[, sim])
  } else {
    # For regular intervals
      stats::pgamma(limsup[run],
                    scale = scale[, sim],
                    shape = shape[, sim]) -
        stats::pgamma(limslow[run],
                      scale = scale[, sim],
                      shape = shape[, sim])
  }
}

#' Run parallel computation wrapper
#' @noRd
run_parallel_mac <- function(n_sim, scale_pred, shape_pred, runnum, n_cores) {
  pbmcapply::pbmclapply(
    1:n_sim,
    function(sim) {
      compute_age_proportions(sim, scale_pred, shape_pred, runnum,
                              limslow, limsup)
    },
    mc.cores = n_cores,
    mc.preschedule = FALSE
  )
}

#' Run parallel computation wrapper for non-Mac
#' @noRd
run_parallel_other <- function(n_sim, scale_pred, shape_pred, runnum, n_cores) {
  future.apply::future_lapply(
    1:n_sim,
    function(sim) {
      compute_age_proportions(sim, scale_pred, shape_pred, runnum,
                              limslow, limsup)
    }
  )
}

#' Generate Age Population Tables
#'
#' @description
#' Creates age-stratified population tables from predictor data and gamma
#' distribution parameters. Supports parallel processing and caching of results.
#'
#' @param predictor_data Data frame containing population data with columns:
#'    country, region, district, pop
#' @param scale_pred Matrix of scale parameters for gamma distribution
#'    predictions
#' @param shape_pred Matrix of shape parameters for gamma distribution
#'    predictions
#' @param age_range Numeric vector of length 2 specifying min and max ages,
#'    default c(0,99)
#' @param age_interval Numeric interval size between age groups in years,
#'    default 1
#' @param country_code Character ISO3 country code
#' @param ignore_cache Logical whether to ignore cached results, default FALSE
#' @param output_dir Character path to output directory
#' @param n_cores Integer number of cores for parallel processing, default
#'    detectCores()-2
#'
#' @return List containing two data frames:
#'    - prop_df: Age-stratified population proportions with uncertainty intervals
#'    - pop_df: Age-stratified population counts with uncertainty intervals
#'
#' @examples
#'
#' \donttest{
#' predictor_data <- data.frame(
#'  country = rep("ABC", 1100),
#'   region = rep("Region1", 1100),
#'   district = rep("District1", 1100),
#'   pop = rep(1000, 1100)
#' )
#' scale_pred <- matrix(rep(1:10, 1100), nrow = 1100, ncol = 10)
#' shape_pred <- matrix(rep(1:10, 1100), nrow = 1100, ncol = 10)
#' output <- generate_age_pop_table(
#'   predictor_data, scale_pred, shape_pred, age_range = c(0, 99),
#'   age_interval = 10, country_code = "ABC", ignore_cache = TRUE,
#'   output_dir = tempdir(), n_cores = 1
#' )
#' }
#'
#' @export
generate_age_pop_table <- function(predictor_data,
                                   scale_pred,
                                   shape_pred,
                                   age_range = c(0, 99),
                                   age_interval = 1,
                                   country_code,
                                   ignore_cache = FALSE,
                                   output_dir,
                                   n_cores = parallel::detectCores() - 2) {
  # Construct output path
  output_path <- file.path(
    output_dir,
    glue::glue(
      "{country_code}_age_tables_pop_",
      "{age_range[1]}_{age_range[2]}plus_yrs_by_{age_interval}yrs.rds"
    )
  )

  # Check cache
  if (!ignore_cache && file.exists(output_path)) {
    cli::cli_process_start(
      msg = "Importing cached age population data...",
      msg_done = "Successfully imported cached age population data."
    )
    final_list <- readRDS(output_path)
    cli::cli_process_done()
    return(final_list)
  }

  # Ensure output directory exists
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # Initialize data frames
  base_df <- predictor_data |>
    dplyr::group_by(country, region = region, district = district) |>
    dplyr::summarize(popsize = sum(pop, na.rm = TRUE), .groups = "drop") |>
    as.data.frame()

  prop_df <- base_df
  pop_df <- base_df

  # Define age intervals including the final open-ended interval
  limslow <- seq(age_range[1], age_range[2], age_interval)
  limsup <- c(seq(age_range[1] + age_interval,
                  age_range[2] + age_interval, age_interval), Inf)

  n_sim <- ncol(shape_pred)

  # Validate inputs
  if (!identical(dim(scale_pred), dim(shape_pred))) {
    stop("scale_pred and shape_pred must have the same dimensions")
  }

  if (nrow(scale_pred) != nrow(predictor_data)) {
    stop("Number of rows in predictions must match predictor_data")
  }

  # Processing loop
  for (runnum in seq_along(limslow)) {
    cli::cli_process_start(
      msg = glue::glue("Processing interval {runnum}/{length(limslow)}..."),
      msg_done = glue::glue("Completed interval {runnum}/{length(limslow)}.")
    )

    # Run parallel computation based on OS
    if (Sys.info()["sysname"] == "Darwin") {
      # For MacOS
      prop_age_pred <- pbmcapply::pbmclapply(
        1:n_sim,
        function(sim) {
          compute_age_proportions(
            sim = sim, scale = scale_pred, shape = shape_pred,
            run = runnum, limslow = limslow, limsup = limsup
          )
        },
        mc.cores = n_cores
      )
    } else {
      # For Linux/Windows
      future::plan(future::multisession, workers = n_cores)
      prop_age_pred <- future.apply::future_lapply(
        1:n_sim,
        function(sim) {
          compute_age_proportions(
            sim = sim, scale = scale_pred, shape = shape_pred,
            run = runnum, limslow = limslow, limsup = limsup
          )
        }
      )
    }

    # Combine simulation results
    prop_age_pred <- base::do.call(base::cbind, prop_age_pred)
    mean_prop_age <- base::rowMeans(prop_age_pred, na.rm = TRUE)
    quantiles_mean_prop_age <- matrixStats::rowQuantiles(
      prop_age_pred,
      probs = c(0.025, 0.975)
    )

    # Create age class data frames for both proportions and counts
    age_prop_class <- data.frame(
      country = predictor_data$country,
      region = predictor_data$region,
      district = predictor_data$district,
      prop = mean_prop_age,
      lower_quantile = quantiles_mean_prop_age[, 1],
      upper_quantile = quantiles_mean_prop_age[, 2]
    ) |>
      dplyr::group_by(country, region, district) |>
      dplyr::summarize(
        prop = mean(prop, na.rm = TRUE),
        lower_quantile = mean(lower_quantile, na.rm = TRUE),
        upper_quantile = mean(upper_quantile, na.rm = TRUE),
        .groups = "drop"
      )

    age_pop_class <- data.frame(
      country = predictor_data$country,
      region = predictor_data$region,
      district = predictor_data$district,
      pop = mean_prop_age * predictor_data$pop,
      lower_quantile = quantiles_mean_prop_age[, 1] * predictor_data$pop,
      upper_quantile = quantiles_mean_prop_age[, 2] * predictor_data$pop
    ) |>
      dplyr::group_by(country, region, district) |>
      dplyr::summarize(
        pop = sum(pop, na.rm = TRUE),
        lower_quantile = sum(lower_quantile, na.rm = TRUE),
        upper_quantile = sum(upper_quantile, na.rm = TRUE),
        .groups = "drop"
      )

    # Update column names
    interval_name <- if (runnum == length(limslow)) {
      paste0(limslow[runnum], "plus")
    } else {
      paste0(limslow[runnum], "_", limsup[runnum], "y")
    }

    # Update column names for both datasets
    colnames(age_prop_class) <- c(
      "country", "region", "district",
      paste0(interval_name, "_prop_mean"),
      paste0(interval_name, "_prop_low_interval_2.5%"),
      paste0(interval_name, "_prop_upper_interval_97.5%")
    )

    colnames(age_pop_class) <- c(
      "country", "region", "district",
      paste0(interval_name, "_pop_mean"),
      paste0(interval_name, "_pop_low_interval_2.5%"),
      paste0(interval_name, "_pop_upper_interval_97.5%")
    )

    # Merge with final data frames
    prop_df <- prop_df |>
      dplyr::left_join(age_prop_class,
                       by = c("country", "region", "district"))

    pop_df <- pop_df |>
      dplyr::left_join(age_pop_class,
                       by = c("country", "region", "district"))

    # Order columns for both dataframes
    numeric_order <- function(df) {
      order(
        sapply(
          colnames(df),
          function(x) {
            as.numeric(stringr::str_extract(x, "^\\d+"))
          },
          USE.NAMES = FALSE
        ),
        na.last = TRUE
      )
    }

    # Reorder columns for both dataframes
    prop_df <- prop_df[, numeric_order(prop_df)] |>
      dplyr::select(country, region, district, popsize, dplyr::everything())

    pop_df <- pop_df[, numeric_order(pop_df)] |>
      dplyr::select(country, region, district, popsize, dplyr::everything())

    cli::cli_process_done()
  }

  # Create final list
  final_list <- list(
    prop_df = prop_df,
    pop_df = pop_df
  )

  # Save results
  base::saveRDS(final_list, file = output_path)
  cli::cli_alert_success("Final age population data saved to {output_path}")
  return(final_list)
}

#' Process Final Population Data
#'
#' Reads population and proportion data from RDS files, processes the data to
#' generate age-group-based population summaries and proportions at different
#' administrative levels (Country, Region, District), and writes the results to
#' an Excel file with separate sheets for each level and metric.
#'
#' @param input_dir A character string specifying the directory containing RDS
#'   files. Default is "03_outputs/3c_table_outputs" in the project directory.
#' @param excel_output_file A character string specifying the output Excel file
#'   path. Default is "03_outputs/3d_compiled_results/age_pop_denom_compiled.xlsx"
#'   in the project directory.
#' @return None. The function writes an Excel file to the specified output
#'   location with six sheets containing population counts and proportions at
#'   different administrative levels.
#'
#' @export
process_final_population_data <- function(
    input_dir = here::here("03_outputs", "3c_table_outputs"),
    excel_output_file = here::here(
      "03_outputs", "3d_compiled_results",
      "age_pop_denom_compiled.xlsx"
    )) {
  # Ensure output directory exists
  dir.create(dirname(excel_output_file),
             recursive = TRUE,
             showWarnings = FALSE
  )

  # Read and process all files
  files <- list.files(input_dir, pattern = "\\.rds$", full.names = TRUE)

  # Read RDS files and extract both pop_df and prop_df
  data_list <- lapply(files, readRDS)

  # Combine all pop_df and prop_df separately
  pop_df <- lapply(data_list, function(x) x$pop_df) |> dplyr::bind_rows()
  prop_df <- lapply(data_list, function(x) x$prop_df) |> dplyr::bind_rows()

  # Reshape data to long format
  reshape_long <- function(df) {
    df |>
      tidyr::pivot_longer(
        cols = -c(country, region, district, popsize),
        names_to = "age_group",
        values_to = "value"
      )
  }

  pop_long <- reshape_long(pop_df)
  prop_long <- reshape_long(prop_df)

  # Summarize data at different levels
  summarize_by <- function(data, group_vars) {
    summarized_df <- data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      dplyr::summarise(
        value = if (
          any(
            grepl("prop", age_group, ignore.case = TRUE)
          )) {
          mean(value, na.rm = TRUE) |> round(4)
        } else {
          sum(value, na.rm = TRUE) |> round(0)
        },
        .groups = "drop"
      ) |>
      tidyr::pivot_wider(
        names_from = age_group,
        values_from = value
      )

    # Order columns
    numeric_order <- order(
      sapply(
        colnames(summarized_df),
        function(x) {
          as.numeric(stringr::str_extract(x, "^\\d+"))
        },
        USE.NAMES = FALSE
      ),
      na.last = TRUE
    )

    summarized_df[, numeric_order] |>
      dplyr::select(
        dplyr::all_of(setdiff(group_vars, "age_group")),
        dplyr::contains("mean")
      ) |>
      dplyr::rename_with(
        ~ stringr::str_replace_all(
          ., c(
            "_mean" = "", "_pop" = "", "_prop" = "",
            "plus" = "+y"
          )
        ),
        dplyr::contains("mean")
      )
  }

  # Generate summaries for both population and proportions
  pop_adm0 <- summarize_by(pop_long, c("country", "age_group"))
  pop_adm1 <- summarize_by(pop_long, c("country", "region", "age_group"))
  pop_adm2 <- summarize_by(
    pop_long,
    c("country", "region", "district", "age_group")
  )

  prop_adm0 <- summarize_by(prop_long, c("country", "age_group"))
  prop_adm1 <- summarize_by(prop_long, c("country", "region", "age_group"))
  prop_adm2 <- summarize_by(
    prop_long,
    c("country", "region", "district", "age_group")
  )

  # Prepare named list of data frames
  list_output <- list(
    "Country (count)" = pop_adm0,
    "Region (count)" = pop_adm1,
    "District (count)" = pop_adm2,
    "Country (proportion)" = prop_adm0,
    "Region (proportion)" = prop_adm1,
    "District (proportion)" = prop_adm2
  )

  openxlsx2::write_xlsx(
    x = list_output,
    file = excel_output_file
  )

  cli::cli_alert_success(
    glue::glue(
      "Final age-structured population and ",
      "proportions saved to {excel_output_file}."
    )
  )
}

#' Extract Parameters and Optimization Details with Log-Likelihood
#'
#' Reads files matching the pattern "age_param_spatial_urban" in a specified
#' directory, extracts the country name, parameter values, and optimization
#' details, combines the results into a data frame, and optionally saves
#' the output to a file.
#'
#' @param dir_path A character string specifying the directory containing the
#'  files.
#' @param output_file A character string specifying the path to save the output
#'                    data frame. If NULL, the output will not be saved.
#' @return A data frame with the extracted parameters, log-likelihood,
#'         and optimization details.
#' @examples
#'
#' \donttest{
#' # Create temporary directory for dummy parameter files
#' dummy_dir <- tempdir()
#'
#' dummy_params <- list(
#'   par = c(0.5, 1.2, 0.8, log(2), log(3), log(4)),
#'   objective = -123.45,
#'   convergence = 0,
#'   iterations = 10,
#'   evaluations = c("function = 20, gradient = 15"),
#'   message = "Converged"
#' )
#'
#' saveRDS(dummy_params, file = file.path(dummy_dir,
#'                                        "abc_age_param_spatial_urban.rds"))
#'
#' params_df <- extract_age_param(dir_path = dummy_dir,
#'                                output_file = tempdir())
#' }
#'
#' @export
extract_age_param <- function(
    dir_path = here::here("03_outputs", "3a_model_outputs"),
    output_file = here::here("03_outputs", "3d_compiled_results",
                             "model_params.csv")) {

  # List all relevant files
  files <- list.files(dir_path, full.names = TRUE)

  # Filter files matching the desired pattern
  param_files <- grep("age_param_spatial", files, value = TRUE)

  # Initialize a list to store data frames
  param_list <- lapply(param_files, function(file) {
    param <- readRDS(file)

    # Extract parameters into a data frame
    data.frame(
      country = basename(file) |> substr(1, 3) |> toupper(),
      beta1 = param$par[1],
      beta2 = param$par[2],
      gamma = param$par[3],
      log_sigma2 = param$par[4],
      log_phi = param$par[5],
      log_tau1 = param$par[6],
      log_likelihood = param$objective,
      convergence = param$convergence,
      iterations = param$iterations,
      eval_function = param$evaluations["function"],
      eval_gradient = param$evaluations["gradient"],
      message = param$message
    )
  })

  # Combine all data frames into one
  params_df <- do.call(rbind, param_list)

  # remove row names
  rownames(params_df) <- NULL

  # Save to file if output_file is provided
  if (!is.null(output_file)) {
    write.csv(params_df, file.path(output_file, "afro_model_params.csv"),
              row.names = FALSE)
  }

  cli::cli_alert_success(
    "Model paramters extracted and saved to {output_file}.")

  return(params_df)
}
