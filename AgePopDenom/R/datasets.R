#' Main Function to Download DHS Datasets
#'
#' Downloads the latest PR (household) and GE (geographic) DHS datasets for
#' specified countries.
#'
#' @param country_codes A character vector of ISO3 country codes.
#' @param cache_path A character string specifying the cache path for RDHS.
#' @param output_dir_root A character string specifying the root directory for
#'    output.
#' @param survey_id A character vector of survey IDs. If NULL, uses latest
#'    survey.
#' @param email A character string. Email registered with DHS.
#' @param project A character string. Project name as registered with DHS.
#' @param verbose Logical for rdhs setup and messages to be printed.
#' @param clear_cache Logical whether to clear cache before downloading.
#'
#' @return Invisibly returns a list of downloaded dataset filenames.
#' @export
download_dhs_datasets <- function(
    country_codes,
    cache_path = here::here("01_data", "1a_survey_data", "raw"),
    output_dir_root = here::here("01_data", "1a_survey_data", "raw"),
    survey_id = NULL,
    email,
    project,
    verbose = TRUE,
    clear_cache = TRUE) {

  # Get DHS country codes
  dhs_country_code <- countrycode::codelist |>
    dplyr::filter(iso3c %in% country_codes) |>
    dplyr::pull(dhs)

  cli::cli_alert_info("Configuring RDHS settings...")

  # Set RDHS configuration
  rdhs::set_rdhs_config(
    email = email,
    project = project,
    config_path = "rdhs.json",
    cache_path = cache_path,
    data_frame = "data.table::as.data.table",
    global = FALSE,
    verbose_setup = verbose,
    timeout = 120,
    verbose_download = verbose
  )

  cli::cli_alert_success("RDHS configuration complete.")

  # Get PR datasets
  pr_available_data <- rdhs::dhs_datasets(
    countryIds = dhs_country_code,
    surveyType = "DHS",
    fileFormat = "DT",
    fileType = "PR"
  )

  # Check if provided survey_id exists
  if (!is.null(survey_id)) {
    valid_surveys <- survey_id %in% pr_available_data$SurveyId
    if (!all(valid_surveys)) {
      invalid_ids <- survey_id[!valid_surveys]
      cli::cli_alert_warning(
        "Invalid survey IDs provided: {paste(invalid_ids, collapse = ', ')}"
      )
      return(invisible(character(0)))
    }
  }

  # Get PR filenames based on survey_id
  pr_filename <- if (is.null(survey_id)) {
    survey_id <- dplyr::pull(pr_available_data, SurveyId)
    pr_available_data |>
      dplyr::group_by(DatasetType, CountryName) |>
      dplyr::slice_max(SurveyYear) |>
      dplyr::distinct() |>
      dplyr::pull(FileName)
  } else {
    pr_available_data |>
      dplyr::filter(SurveyId %in% survey_id) |>
      dplyr::pull(FileName)
  }

  # Download PR datasets
  pr_output_dir <- file.path(output_dir_root, "pr_records")

  if (length(pr_filename) == 0) {
    cli::cli_alert_warning(
      "No PR datasets found for survey IDs: {survey_id}."
    )
    return(invisible(character(0)))
  }

  pr_datasets <- rdhs::get_datasets(
    pr_filename,
    download_option = "rds",
    output_dir_root = pr_output_dir,
    clear_cache = clear_cache
  )

  # Get and download GE datasets
  ge_filename <- rdhs::dhs_datasets(
    surveyIds = survey_id,
    surveyType = "DHS",
    fileFormat = "FLAT",
    fileType = "GE"
  ) |>
    dplyr::pull(FileName)

  ge_output_dir <- file.path(output_dir_root, "shapefiles")

  if (length(ge_filename) == 0) {
    cli::cli_alert_warning(
      "No GE datasets found for survey IDs: {survey_id}."
    )
    return(invisible(character(0)))
  }

  ge_datasets <- rdhs::get_datasets(
    ge_filename,
    download_option = "rds",
    output_dir_root = ge_output_dir,
    clear_cache = clear_cache
  )

  cli::cli_alert_success("All datasets successfully downloaded and saved.")
  invisible(list(PR = pr_datasets, GE = ge_datasets))
}


#' Aggregate Individual Survey Data and Extract Gamma Parameters by Location
#'
#' This script aggregates the individual Survey data to location level and extracts
#' the gamma parameters for the locations.
#'
#' @param data Data frame containing individual-level age data with
#'        coordinates and urban/rural classification.
#' @param lat_column Column name for latitude coordinates (default: "lat")
#' @param long_column Column name for longitude coordinates (default: "long")
#' @param age_column Column name for age values (default: "ageyrs")
#' @param urban_column Column name for urban/rural classification (default:
#'      "urban")
#' @return List containing:
#'         \itemize{
#'           \item outlier_free_data: Original data with added spatial
#'                  coordinates, outliers removed, and clusters with less than
#'                  10 samples removed
#'           \item age_param_data: Location-level gamma parameters with columns:
#'                 lon, lat, web_x, web_y, log_scale, log_shape, urban,
#'                 b1, c, b2, nsampled
#'         }
#' @export
aggregate_and_extract_gamma <- function(data,
                                        lat_column = "lat",
                                        long_column = "long",
                                        age_column = "ageyrs",
                                        urban_column = "urban") {

  # Validate input column names exist in data
  if (
    !all(
      c(lat_column, long_column,
        age_column, urban_column) %in% names(data))) {
    stop(
      "Missing required input columns. Please ensure data contains: ",
      paste(setdiff(
        c(lat_column, long_column, age_column, urban_column),
        names(data)
      ), collapse = ", ")
    )
  }

  # Convert coordinates
  coords_sf <- data |>
    dplyr::select(
      !!rlang::sym(long_column), !!rlang::sym(lat_column)
    ) |>
    sf::st_as_sf(coords = c(long_column, lat_column), crs = 4236) |>
    sf::st_transform(crs = 3857)

  data <- data |>
    dplyr::mutate(
      web_x = sf::st_coordinates(coords_sf)[, 1],
      web_y = sf::st_coordinates(coords_sf)[, 2]
    ) |>
    # Create groups based on unique coordinate combinations
    dplyr::group_by(web_x, web_y) |>
    dplyr::mutate(id_coords = dplyr::cur_group_id()) |>
    dplyr::ungroup()

  # Remove outliers based on 95th percentile of mean age
  mean_age_threshold <- data |>
    dplyr::group_by(country_code_iso3, web_x, web_y) |>
    dplyr::summarise(mean_age = mean(
      !!rlang::sym(age_column), na.rm = TRUE),
      .groups = "drop") |>
    dplyr::pull(mean_age) |>
    stats::quantile(0.95, na.rm = TRUE)

  outliers <- data |>
    dplyr::group_by(country_code_iso3, web_x, web_y) |>
    dplyr::summarise(
      mean_age = mean(!!rlang::sym(age_column), na.rm = TRUE),
      max_age = max(!!rlang::sym(age_column), na.rm = TRUE),
      sd_age = stats::sd(!!rlang::sym(age_column), na.rm = TRUE),
      n_samples = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::filter(mean_age > mean_age_threshold | n_samples < 10) |>
    dplyr::select(country_code_iso3, web_x, web_y)

  data <- data |>
    dplyr::anti_join(outliers,
                     by = c("country_code_iso3", "web_x", "web_y"))
  # Parameter estimation
  names_dt <- c(
    "lon", "lat", "web_x", "web_y",
    "log_scale", "log_shape", "urban",
    "b1", "c", "b2", "nsampled"
  )

  age_param_data <- matrix(ncol = length(names_dt), nrow = 0) |>
    as.data.frame() |>
    setNames(names_dt)

  loglik_x_i <- function(y, par) {
    sum(dgamma(y, scale = exp(par[1]), shape = exp(par[2]), log = TRUE))
  }

  age_param_data <- data |>
    dplyr::group_by(id_coords) |>
    dplyr::group_modify(~ {
      sub_data <- .x |>
        dplyr::filter(is.finite(!!rlang::sym(age_column)),
                      !!rlang::sym(age_column) != 0)

      age_x_i <- sub_data[[age_column]]
      shape_start <- mean(age_x_i)^2 / var(age_x_i)
      scale_start <- var(age_x_i) / mean(age_x_i)

      estim_x_i <- nlminb(
        log(c(scale_start, shape_start)),
        function(par) -loglik_x_i(age_x_i, par)
      )
      sigma_i <- solve(numDeriv::hessian(
        function(par) -loglik_x_i(age_x_i, par),
        x = estim_x_i$par
      ))

      tibble::tibble(
        lon = dplyr::first(sub_data[[long_column]]),
        lat = dplyr::first(sub_data[[lat_column]]),
        web_x = dplyr::first(sub_data$web_x),
        web_y = dplyr::first(sub_data$web_y),
        log_scale = estim_x_i$par[1],
        log_shape = estim_x_i$par[2],
        urban = dplyr::first(sub_data[[urban_column]]),
        b1 = diag(sigma_i)[1],
        c = sigma_i[1, 2],
        b2 = diag(sigma_i)[2],
        nsampled = nrow(sub_data)
      )
    }) |>
    dplyr::ungroup() |>
    dplyr::left_join(
      data |>
        dplyr::select(dplyr::any_of(c(
          "country", "year_of_survey", "survey_code",
          "country_code_iso3", "country_code_dhs",
          "lat", "long"
        ))) |>
        dplyr::rename(lon = long),
      by = c("lat", "lon")
    ) |>
    dplyr::select(dplyr::any_of(c(
      "country", "country_code_iso3", "country_code_dhs",
      "survey_code", "year_of_survey"
    )), dplyr::everything())


  # Return list of results
  list(
    outlier_free_data = data,
    age_param_data = age_param_data
  )
}


#' Process DHS Data: Merge RDS Files with Shapefiles and Extract Gamma
#' Parameters
#'
#' This function processes DHS (Demographic and Health Survey) data by:
#' 1. Reading RDS files and shapefiles for each country.
#' 2. Merging demographic data with geographic information.
#' 3. Cleaning and aggregating the data.
#' 4. Extracting gamma parameters for age-related analysis.
#'
#' @param rds_dir Character. Path to the directory containing raw RDS files.
#' @param shp_dir Character. Path to the directory containing shapefiles.
#' @param output_path Character. Path to save the final processed dataset as an
#'  RDS file.
#'
#' @details
#' The function loops through RDS files, processes each country's data by
#' merging demographic information with shapefile data, and computes gamma
#' parameters for age-related analysis. The progress is tracked and displayed
#' for each country.
#'
#' The function also filters out incomplete data (e.g., age values of `98`) and
#' handles labelled data using the `haven::zap_labels` function.
#'
#' The final output includes two datasets:
#' 1. Outlier-free data.
#' 2. Aggregated age parameter data.
#'
#' @return None. Saves the final combined dataset to the specified output path.
#'
#' @examples
#' \donttest{
#' tf <- file.path(tempdir(), "test_env")
#' dir.create(tf, recursive = TRUE, showWarnings = FALSE)
#' tmp_rds_dir <- file.path(tf, "rds")
#' tmp_shp_dir <- file.path(tf, "shp")
#' tmp_output <- file.path(tf, "output.rds")
#'
#' dir.create(tmp_rds_dir, recursive = TRUE, showWarnings = FALSE)
#' dir.create(tmp_shp_dir, recursive = TRUE, showWarnings = FALSE)
#'
#' # Create fake DHS data
#' create_fake_dhs_data <- function(country_code) {
#'   set.seed(123) # For reproducibility
#'   n <- 100
#'
#'   # Create labelled vectors
#'   hv007 <- haven::labelled(
#'     sample(c(2015, 2016), n, replace = TRUE),
#'     labels = c("2015" = 2015, "2016" = 2016)
#'   )
#'
#'   hv001 <- haven::labelled(
#'     sample(1:20, n, replace = TRUE),
#'     labels = setNames(1:20, paste("Cluster", 1:20))
#'   )
#'
#'   hv105 <- haven::labelled(
#'     sample(c(1:97, 98), n, replace = TRUE),
#'     labels = c(setNames(1:97, paste("Age", 1:97)), "Don't know" = 98)
#'   )
#'
#'   # Combine into data frame
#'   data.frame(
#'     hv007 = hv007,
#'     hv001 = hv001,
#'     hv105 = hv105
#'   )
#' }
#'
#' # Create fake shapefile data
#' # Create fake shapefile data with explicit CRS
#' create_fake_shapefile <- function(country_code) {
#'   set.seed(123)
#'   n_clusters <- 20
#'
#'   # Create spatial data frame with explicit CRS
#'   sf_data <- sf::st_as_sf(
#'     dplyr::tibble(
#'       DHSCLUST = 1:n_clusters,
#'       URBAN_RURA = sample(c("R", "U"), n_clusters, replace = TRUE),
#'       LATNUM = runif(n_clusters, -10, 10),
#'     LONGNUM = runif(n_clusters, -10, 10)
#'   ),
#'     coords = c("LONGNUM", "LATNUM"),
#'     crs = 4326 # WGS84
#'   ) |>
#'     dplyr::mutate(
#'       LATNUM = runif(n_clusters, -10, 10),
#'       LONGNUM = runif(n_clusters, -10, 10)
#'     )
#' }
#'
#' # Save test data for two countries
#' countries <- c("KE", "TZ")
#' for (country in countries) {
#'   saveRDS(
#'     create_fake_dhs_data(country),
#'     file = file.path(tmp_rds_dir, paste0(country, "HR71FL.rds"))
#'   )
#'   saveRDS(
#'     create_fake_shapefile(country),
#'     file = file.path(tmp_shp_dir, paste0(country, "HR7SHP.rds"))
#'   )
#' }
#'
#' # Run the function
#' process_dhs_data(
#'   rds_dir = tmp_rds_dir,
#'   shp_dir = tmp_shp_dir,
#'   output_path = tmp_output
#' )
#' }
#'
#' @export
process_dhs_data <- function(
    rds_dir = here::here("01_data", "1a_survey_data", "raw", "pr_records"),
    shp_dir = here::here("01_data", "1a_survey_data", "raw", "shapefiles"),
    output_path = here::here("01_data", "1a_survey_data",
                             "processed", "dhs_pr_records_combined.rds")) {

  # Create age-population raster
  cli::cli_h1("Processing DHS data and joining with shapefile")

  # Get a list of RDS and Shapefiles
  rds_files <- list.files(rds_dir, pattern = "\\.rds$", full.names = TRUE)
  shp_files <- list.files(shp_dir, pattern = "\\.rds$", full.names = TRUE)

  # Initialize a list to store processed datasets
  all_data <- list()
  total_countries <- length(rds_files)
  processed_count <- 0

  # Process each RDS file
  for (rds_path in rds_files) {
    rds_file_name <- basename(rds_path)
    country_code <- substr(rds_file_name, 1, 2)

    # Display processing message
    cli::cli_process_start(
      msg = glue::glue("Processing country: {country_code}"),
      msg_done = glue::glue(
        "Processed country: {country_code} ",
        "({processed_count + 1} of {total_countries})"
      )
    )

    # Read the RDS file
    rds_data <- readRDS(rds_path)

    # Select and rename required columns
    rds_processed <- rds_data |>
      dplyr::mutate(country_code_dhs = country_code) |>
      dplyr::select(
        country_code_dhs,
        year_of_survey = hv007,
        dhs_clust_num = hv001,
        ageyrs = hv105
      ) |>
      dplyr::mutate(
        dplyr::across(
          dplyr::where(haven::is.labelled), haven::zap_labels)) |>
      dplyr::mutate(
        ageyrs = as.integer(ageyrs),
        year_of_survey = ifelse(year_of_survey == 94, 1994, year_of_survey),
        year_of_survey = ifelse(year_of_survey == 95, 1995, year_of_survey),
        survey_code = sub("\\.rds$", "", rds_file_name)
      ) |>
      dplyr::filter(ageyrs != 98)

    # Find the matching shapefile - only match by country code
    shp_file <- shp_files[grepl(paste0("^", country_code), basename(shp_files))]

    if (length(shp_file) >= 1) {
      # If multiple matches, take the first one
      shp_file <- shp_file[1]
      shp_data <- readRDS(shp_file) |>
        dplyr::mutate(urban = ifelse(URBAN_RURA == "R", 0, 1)) |>
        dplyr::select(
          dhs_clust_num = DHSCLUST,
          urban,
          lat = LATNUM, long = LONGNUM
        ) |>
        sf::st_drop_geometry()

      merged_data <- rds_processed |>
        dplyr::left_join(shp_data, by = "dhs_clust_num")

      all_data[[rds_file_name]] <- merged_data
      processed_count <- processed_count + 1
    } else {
      cli::cli_alert_warning(
        glue::glue(
          "No matching shapefile found for {country_code} ({rds_file_name})")
      )
    }

    cli::cli_process_done()
  }

  # Bind all datasets together -------------------------------------------------

  binded_dataset <- dplyr::bind_rows(all_data) |>
    dplyr::left_join(
      dplyr::select(countrycode::codelist, country.name.en, iso3c, dhs),
      by = c("country_code_dhs" = "dhs")
    ) |>
    dplyr::select(
      country = country.name.en,
      country_code_iso3 = iso3c,
      country_code_dhs,
      dplyr::everything()
    )

  # Process gamma parameters----------------------------------------------------

  # Create age-population raster
  cli::cli_h1("Process gamma parameters")

  processed_count <- 0

  final_data <- purrr::map(
    unique(binded_dataset$country_code_iso3), function(country_code) {
      processed_count <<- processed_count + 1
      cli::cli_process_start(
        msg = glue::glue(
          "Aggregating and extracting gamma for: {country_code}"),
        msg_done = glue::glue(
          "Aggregated and extracted gamma for: {country_code} ",
          "({processed_count} of {
          length(unique(binded_dataset$country_code_iso3))})"
        )
      )

      processed_data <- binded_dataset |>
        dplyr::filter(
          country_code_iso3 == country_code,
          !is.na(urban) | !is.na(lat) | !is.na(long)
        ) |>
        aggregate_and_extract_gamma()


      # Get the latest year for each survey_code
      outlier_free_data <- processed_data$outlier_free_data |>
        dplyr::filter(long != lat) |>
        dplyr::group_by(survey_code) |>
        dplyr::mutate(
          year_of_survey = max(year_of_survey, na.rm = TRUE)
        ) |>
        dplyr::ungroup() |>
        # Add the combined country and year label
        dplyr::mutate(
          country_year = paste0(
            country, " - ", year_of_survey, " (", survey_code, ")"
          )
        )

      # Get the latest year for each survey_code
      age_param_data <- processed_data$age_param_data |>
        dplyr::filter(lon != lat) |>
        dplyr::group_by(survey_code) |>
        dplyr::mutate(
          year_of_survey = max(year_of_survey, na.rm = TRUE)
        ) |>
        dplyr::ungroup() |>
        # Add the combined country and year label
        dplyr::mutate(
          country_year = paste0(
            country, " - ", year_of_survey, " (", survey_code, ")"
          )
        )

      list(
        outlier_free_data = outlier_free_data,
        age_param_data = age_param_data
      )
    })

  # Combine final data
  final_dataset <- list(
    outlier_free_data = purrr::map_dfr(final_data, "outlier_free_data") |>
      dplyr::distinct(),
    age_param_data = purrr::map_dfr(final_data, "age_param_data") |>
      dplyr::distinct()
  )

  # Save the final dataset
  saveRDS(final_dataset, file = output_path, compress = "xz")

  cli::cli_alert_success(
    "All countries processed. Combined data saved to {output_path}")
}


#' Download population rasters for given country codes.
#'
#' This function attempts to download population rasters from WorldPop for the
#' specified country codes. If `dest_dir` is not provided, file paths will be
#' generated based on the given country codes and saved into the current
#' project directory (using `here::here()`). It first checks if a local file
#' already exists, and if so, it will skip downloading.
#'
#' The function tries a baseline URL (BSGM) for each country code. If the file
#' is not found there, it then tries a secondary URL (maxar_v1). If neither
#' location provides the file, it returns NA and prompts you to check the
#' WorldPop website directly.
#'
#' @param country_codes A character vector of ISO3 country codes.
#' @param dest_dir A character vector of file paths for saving rasters.
#'   If NULL, defaults to "<cc>_ppp_2020_constrained2.tif" in the project dir.
#' @param quiet Logical; if TRUE, suppress status messages.
#'
#' @return Invisibly returns a vector of downloaded file paths
#'  (or NA if not found).
#'
#' @examples
#' \donttest{
#' download_pop_rasters(country_codes = "COM", dest_dir = tempdir())
#' }
#'
#' @export
download_pop_rasters <- function(
    country_codes,
    dest_dir = here::here("01_data", "1b_rasters", "pop_raster"),
    quiet = FALSE) {

  # Ensure destination directory exists
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }

  # Construct destination file paths
  dest_files <- file.path(
    dest_dir,
    paste0(tolower(country_codes), "_ppp_2020_constrained.tif")
  )

  # Check if lengths match
  if (length(country_codes) != length(dest_files)) {
    stop("country_codes and dest_files must have the same length.")
  }

  # Define base URLs
  base_url_bsgm <- paste0(
    "https://data.worldpop.org/GIS/Population/",
    "Global_2000_2020_Constrained/2020/BSGM/"
  )
  base_url_maxar <- paste0(
    "https://data.worldpop.org/GIS/Population/",
    "Global_2000_2020_Constrained/2020/maxar_v1/"
  )

  # Check if a URL exists
  check_url <- function(url) {
    h <- curl::new_handle(nobody = TRUE, timeout = 600)
    res <- tryCatch(
      curl::curl_fetch_memory(url, handle = h),
      error = function(e) NULL
    )
    !is.null(res) && res$status_code == 200
  }

  # Process each country code
  out_files <- mapply(function(cc, df) {
    if (file.exists(df)) {
      if (!quiet) {
        cli::cli_alert_info(
          "Raster file already exists: {crayon::blue(basename(df))}")
      }
      return(df)
    }

    cc_upper <- toupper(cc)
    cc_lower <- tolower(cc)

    url_bsgm <- paste0(base_url_bsgm, cc_upper, "/",
                       cc_lower, "_ppp_2020_constrained.tif")
    url_maxar <- paste0(base_url_maxar, cc_upper, "/",
                        cc_lower, "_ppp_2020_constrained.tif")

    # Determine the final URL to use
    final_url <- if (check_url(url_bsgm)) {
      url_bsgm
    } else if (check_url(url_maxar)) {
      url_maxar
    } else {
      NA_character_
    }

    if (!is.na(final_url)) {
      curl::curl_download(
        url = final_url, destfile = df, quiet = quiet,
        handle = curl::new_handle(timeout = 600)
      )
      df
    } else {
      if (!quiet) {
        cli::cli_alert_warning(
          glue::glue(
            "No file found for {crayon::blue(cc_upper)}. ",
            "Please check the WorldPop site for availability."
          )
        )
      }
      NA_character_
    }
  }, country_codes, dest_files, USE.NAMES = FALSE)

  cli::cli_alert_success("Population raster files successfully processed.")
  invisible(out_files)
}

#' Extract Urban/Rural Extent Raster
#'
#' Extracts the `afurextent.asc` raster file from the package's `inst/extdata`
#' directory to a specified destination.
#'
#' @param dest_dir A character string specifying the directory to save the
#'   extracted raster file.
#' @param overwrite Logical. Whether to overwrite an existing file in the
#'   destination directory. Default is FALSE.
#'
#' @return A character string representing the full path to the extracted raster
#'   file.
#' @details
#' This function extracts the `afurextent.asc` file from the package's
#' `extdata` directory, where it is stored as a compressed `.zip` file. It
#' requires the `raster` package to load the raster file.
#'
#' @examples
#' \donttest{
#'  extract_afurextent(tempdir(), overwrite = TRUE)
#' }
#' @export
extract_afurextent <- function(
    dest_dir = here::here("01_data", "1b_rasters", "urban_extent"),
    overwrite = FALSE) {

  # Ensure the destination directory exists
  if (!dir.exists(dest_dir)) {
    cli::cli_abort(
      glue::glue("Destination directory {dest_dir} does ",
                 "not exist. Please create it first."))
  }

  # Locate the compressed raster file in the package
  raster_zip <- system.file("extdata", "afurextent.asc.zip",
                            package = "AgePopDenom")
  if (raster_zip == "") {
    cli::cli_abort(
      glue::glue("The raster file {raster_zip} ",
                 "is not found in the package."))
  }

  # Define the output path
  raster_file <- file.path(dest_dir, "afurextent.asc")

  # Check if the file already exists
  if (file.exists(raster_file) && !overwrite) {
    cli::cli_alert_info(
      glue::glue("The file already exists at {raster_file} ",
                 "and {overwrite} is FALSE."))
    return(raster_file)
  }

  # Extract the raster file
  cli::cli_alert_info("Extracting raster file to {dest_dir}...")
  utils::unzip(raster_zip, exdir = dest_dir)

  # Validate extraction
  if (!file.exists(raster_file)) {
    cli::cli_abort(
      glue::glue("Extraction failed. The raster file could ",
                 "not be found at {dest_dir}."))
  }


  # Remove any extraneous files in the directory
  dir_files <- list.files(dest_dir, full.names = TRUE)
  extraneous_files <- dir_files[basename(dir_files) != "afurextent.asc"]
  if (length(extraneous_files) > 0) {
    file.remove(extraneous_files)
  }

  cli::cli_alert_success(
    "Raster file successfully extracted to: {raster_file}")
  return(raster_file)
}

#' Download WHO ADM2 Boundaries with Partial Update
#'
#' This function ensures the specified shapefile (`dest_file`) contains
#' boundaries for all requested ISO3 codes (`country_codes`). It dynamically
#' Downloades only the missing codes and appends them to the file, ensuring no
#' duplication.
#'
#' @param country_codes Character vector of ISO3 country codes
#'   (e.g. c("KEN","UGA")).
#' @param dest_file File path where data is saved (default:
#'   "01_data/1c_shapefiles/district_shape.gpkg").
#'
#' @return An `sf` object of combined boundaries for all requested country
#'   codes.
#' @examples
#'
#' \donttest{
#'
#' tf <- file.path(tempdir(), "test_env")
#'
#' # Download population rasters from worldpop
#' download_shapefile(
#'   country_codes = "COM",
#'   dest_file = here::here(tf, "district_shape.gpkg")
#' )
#' }
#' @export
download_shapefile <- function(
    country_codes,
    dest_file = here::here("01_data", "1c_shapefiles", "district_shape.gpkg")) {

  # Initialize existing codes if the file exists
  if (file.exists(dest_file)) {
    existing_sf <- sf::st_read(dest_file, quiet = TRUE)
    existing_codes <- unique(existing_sf$country_code)
  } else {
    existing_codes <- character(0)  # No existing codes if file doesn't exist
  }

  # Calculate missing codes
  diff_codes <- setdiff(country_codes, existing_codes)

  # Assign only the missing codes back to `country_codes`
  country_codes <- diff_codes

  if (length(country_codes) == 0) {
    cli::cli_alert_info(
      glue::glue(
        "All requested country codes are already ",
        "in {dest_file}. No updates needed.")
    )
    return(invisible(NULL))
  }

  # Download missing country codes
  codes_str <- paste0("'", paste(country_codes, collapse = "','"), "'")
  where_clause <- paste0("ISO_3_CODE IN (", codes_str, ")")
  base_url <- paste0(
    "https://services.arcgis.com/5T5nSi527N4F7luB",
    "/arcgis/rest/services/Detailed_Boundary_ADM2/",
    "FeatureServer/0/query"
  )
  params <- list(
    where = where_clause,
    outFields = "ISO_3_CODE,ADM0_NAME,ADM1_NAME,ADM2_NAME,ENDDATE",
    outSR = 4326,
    f = "geojson"
  )


  cli::cli_alert_info(
    glue::glue(
      "Downloading missing WHO ADM2 data for: ",
      "{crayon::blue(paste(country_codes, collapse=', '))}")
  )

  # get shapefile
  new_sf <- base::suppressWarnings(
    base::suppressMessages({
      base::invisible(
        utils::capture.output(
          out <- sf::st_read(
            httr::modify_url(base_url, query = params), quiet = TRUE)
        )
      )
      out
    }
    )
  ) |> dplyr::filter(ENDDATE == "253402214400000") |>
    dplyr::transmute(
      country_code = ISO_3_CODE,
      country      = ADM0_NAME,
      region       = ADM1_NAME,
      district     = ADM2_NAME
    )

  # Append new data to the file or create a new one
  if (file.exists(dest_file)) {

    sf::st_write(sf::st_make_valid(new_sf),
                 dest_file, append = TRUE, quiet = TRUE)
    cli::cli_alert_success(
      glue::glue(
        "Appended missing country codes to existing ",
        "shapefile: {paste(country_codes, collapse=', ')}"
      )
    )
  } else {
    sf::st_write(sf::st_make_valid(new_sf),
                 dest_file, append = FALSE, quiet = TRUE)
    cli::cli_alert_success(
      glue::glue(
        "Created new shapefile with country codes: ",
        "{paste(country_codes, collapse=', ')}"
      )
    )
  }
}

