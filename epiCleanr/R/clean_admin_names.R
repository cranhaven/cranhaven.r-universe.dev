#' Clean and Match Administrative Names
#'
#' This function takes administrative names and cleans them using various
#' matching and string distance algorithms. It can also match the cleaned names
#' with a base list provided by the user or fetched from `GeoNames`, which is a
#' official repository of standard spellings of all foreign geographic names.
#'
#' @param admin_names_to_clean A character vector of administrative names to
#'        clean.
#' @param user_base_admin_names A character of of administrative names that the
#'        use would like to use as reference. This is no necessary, downloaded
#'         `GeoNames` will be used if missing.
#' @param admin_level A character string indicating the administrative level
#'        (e.g., "adm2").
#' @param country_code sed if `use_get_admin_names` is TRUE. A character string
#'       or numerical value of the country code (e.g., "KE"). This can be
#'       in various formats such as country name, ISO codes, UN codes, etc.,
#'       see \code{\link[=countrycode]{countrycode::codelist()}} for the full
#'       list of codes and naming conventions used.
#' @param user_base_only A logical indicating whether to use only the
#'       user-provided base administrative names (`user_base_admin_names`) for
#'       matching. If TRUE, `country_code` and `admin_names_to_clean` are not
#'       required. Default is FALSE.
#' @param report_mode A logical indicating whether to return a detailed report.
#'        Default is FALSE.
#'
#' @return If `report_mode` is set to TRUE, a data frame containing the original
#'        admin names and the matched and cleaned admin names with inormation
#'        of the source of data used to clean including the algorithm used,
#'        else a cleaned list of names is returned.
#'
#' @examples
#'  \donttest{
#' # Example with country code
#' base_names <- c(
#'   "Paris", "Marseille", "Lyon",
#'   "Toulouse", "Nice", "Nantes", "Strasbourg",
#'   "Montpellier", "Bordeaux", "Lille"
#' )
#'
#' unclean_names <- c(
#'   "Pariis", "Marseill", "Lyone",
#'   "Toulous", "Niice", "Nantees", "Strasbourgh",
#'   "Montpeelier", "Bordeuax", "Lilie"
#' )
#'
#' france_new <- clean_admin_names(
#'   country_code = "Fr",
#'   user_base_admin_names = base_names,
#'   admin_names_to_clean = unclean_names
#' )
#'
#' print(france_new)
#'}
#'
#' @seealso \code{\link[=countrycode]{countrycode::codelist()}}
#' for the full list of codes and naming conventions.
#' @importFrom dplyr select filter mutate all_of
#' @importFrom tidyselect contains everything
#' @importFrom tidyr separate pivot_longer pivot_wider
#' @importFrom purrr map
#' @importFrom rlang .data
#' @importFrom tibble rownames_to_column
#' @export

clean_admin_names <- function(admin_names_to_clean, country_code,
                              admin_level = "adm2",
                              user_base_admin_names = NULL,
                              user_base_only = FALSE,
                              report_mode = FALSE) {

  if (!user_base_only) {
    if (is.null(country_code) && is.null(admin_names_to_clean)) {
      stop("Both 'country_code' and 'admin_names_to_clean' must be provided.")
    } else if (is.null(country_code)) {
      stop("'country_code' must be provided.")
    } else if (is.null(admin_names_to_clean)) {
      stop("'admin_names_to_clean' must be provided.")
    }
  } else {
    if (is.null(user_base_admin_names)) {
      stop(
        "'user_base_admin_names' must be provided when ",
        "'user_base_only' is TRUE.")
    }
  }

  # Helper Function to remove specific words from a string
  parse_strings <- function(string) {
    string <- tolower(string)
    string <- gsub(
      paste0(
        "\\bdistrict\\b|\\bcounty\\b|",
        "\\bcity\\b|\\bprovince\\b|\\bregion\\b|",
        "\\bmunicipality\\b|\\btownship\\b|",
        "\\bvillage\\b|\\bward\\b|\\bsubdistrict",
        "\\b|\\bdivision\\b|\\",
        "bstate\\b|\\bprefecture\\b"
      ), "", string
    ) |> clean_names_strings()

    return(string)
  }

  # Get admin names from geonames ---------------------------------------------

  if (!user_base_only) {

    admin_data <- get_admin_names(country_code)[[admin_level]]

    # select relevant cols
    admin_data <- admin_data |>
      dplyr::select(
        -"country_code", tidyselect::all_of(admin_level), -"latitude",
        -"longitude", -"last_updated")

    # Separate 'alternatenames' into multiple columns
    max_cols <- max(stringr::str_count(admin_data$alternatenames, ", ")) + 1
    col_names <- paste0("alt_admin_name_", seq_len(max_cols))

    suppressWarnings({
      admin_data <- admin_data |>
        tidyr::separate(.data$alternatenames,
                        into = col_names, sep = ", ",
                        extra = "merge"
        )
    })

  }

  # Match with base admin names  ----------------------------------------------

  # List of methods to use for matching
  methods <- c("lv", "dl", "lcs", "qgram", "jw", "soundex")

  # Define a function to calculate string distance for a given method
  calculate_user_base_distance <- function(method) {
    suppressWarnings({
      scores <- stringdist::stringdistmatrix(
        # Clean the admin names
        cleaned_admin_names_to_clean,
        cleaned_user_base_admin_names,
        method = method
      )
    })

    # Calculate proportion matched
    matched_indic <- apply(scores, 1, which.min)
    max_distances <- apply(scores, 1, max, na.rm = TRUE)
    min_distances <- apply(scores, 1, min, na.rm = TRUE)
    match_prop <- round((1 - (min_distances / max_distances)) * 100)

    # Extracting the matched cleaned admin names
    matched_cleaned_names <- cleaned_user_base_admin_names[matched_indic]

    # Extracting the matched cleaned admin names
    names_to_clean <- admin_names_to_clean[matched_indic]

    # Extracting the corresponding user-provided base names
    user_base_admin_names <- user_base_admin_names[matched_indic]

    # drop any NA's
    admin_names_to_clean <- admin_names_to_clean[!is.na(admin_names_to_clean)]

    # Combine to a dataset
    data.frame(
      method,
      admin_names_to_clean,
      user_base_admin_names,
      matched_indic, max_distances, min_distances, match_prop
    )
  }

  # Clean the admin names
  cleaned_admin_names_to_clean <-
    parse_strings(admin_names_to_clean[!is.na(admin_names_to_clean)])

  # Match with base admin names (if provided)
  if (!is.null(user_base_admin_names)) {
    # Clean the admin names
    cleaned_user_base_admin_names <- parse_strings(user_base_admin_names)

    user_base_results <- do.call(
      rbind,
      lapply(methods, calculate_user_base_distance)
    )


    # manipulate the results
    user_base_results <- user_base_results |>
      # remove cols Inf values
      dplyr::filter(!is.na(max_distances) | !is.na(min_distances)) |>
      dplyr::select(-"matched_indic", -"max_distances", -"min_distances") |>
      dplyr::distinct() |>
      dplyr::rename(match_prop_user_base_admin_names = match_prop) |>
      tidyr::pivot_wider(
        id_cols = "admin_names_to_clean",
        names_from = "method",
        values_from = c(
          "match_prop_user_base_admin_names",
          "user_base_admin_names"
        )
      )
  }

  # Calculate matching scores for each column of the geonames -----------------
  # Function for the geonames
  if (!user_base_only) {
    calculate_column_distance <- function(column, method) {
      cleaned_column <- clean_names_strings(column, style = "simple_clean")
      suppressWarnings({
        scores <- stringdist::stringdistmatrix(
          cleaned_admin_names_to_clean,
          cleaned_column,
          method = method
        )
      })


      # Calculate proportion matched
      max_distances <- apply(scores, 1, max, na.rm = TRUE)
      min_distances <- apply(scores, 1, min, na.rm = TRUE)
      matched_indices <- apply(scores, 1, which.min)
      match_prop <- round((1 - (min_distances / max_distances)) * 100)
      geo_admins <- column[matched_indices]

      # drop any NA's
      admin_names_to_clean <- admin_names_to_clean[!is.na(admin_names_to_clean)]


      data.frame(method,
                 admin_names_to_clean,
                 country = toupper(country_code),
                 admin_level = toupper(admin_level),
                 geo_admins,
                 match_prop
      )
    }

    # Match with geonames admin names
    calculate_distance_for_column <- function(column) {
      do.call(
        rbind,
        lapply(
          methods,
          function(method) calculate_column_distance(column, method)
        )
      )
    }

    results <- do.call(
      rbind,
      lapply(admin_data, calculate_distance_for_column)
    )

    # Join the results for base datasets
    results_admin_data <- results |>
      # remove index column
      tibble::rownames_to_column("index") |>
      dplyr::rename(column_names = 1) |>
      dplyr::mutate(column_names = stringr::str_replace(
        column_names, "\\..*", ""
      )) |> dplyr::distinct() |>
      dplyr::mutate(column_names = paste(column_names, method, sep = "_")) |>
      dplyr::select(-"method") |>
      tidyr::pivot_wider(
        id_cols = "admin_names_to_clean",
        names_from = "column_names",
        values_from = c("geo_admins", "match_prop")
      )

    # if the user base data is provided then join together
    if (!is.null(user_base_admin_names)) {
      results_admin_data <- results_admin_data |>
        # join the
        dplyr::left_join(
          user_base_results,
          by = "admin_names_to_clean"
        )
    }

  }

    # function to get best match results
    get_best_match_result <-  function(data) {

      # turn matching dataset long
      best_match_result <- data |>
        dplyr::select(
          tidyselect::contains("match_prop"), "admin_names_to_clean"
        ) |>
        tidyr::pivot_longer(
          cols = -("admin_names_to_clean"),
          values_to = "match_prop",
          names_to = "admin_cols"
        ) |>
        dplyr::mutate(
          admin_cols = stringr::str_replace(.data$admin_cols, "match_prop_", "")
        ) |>
        dplyr::group_by(.data$admin_names_to_clean) |>
        # get the best results
        dplyr::slice(which.max(.data$match_prop)) |>
        dplyr::ungroup()

      # take the best admin names as the final columns
      best_names_long <- data |>
        dplyr::select(
          -tidyselect::contains("match_prop"), "admin_names_to_clean"
        ) |>
        tidyr::pivot_longer(
          cols = -("admin_names_to_clean"),
          values_to = "final_cleaned_column",
          names_to = "admin_cols"
        ) |>
        mutate(admin_cols = stringr::str_replace(admin_cols, "geo_admins_", ""))

      # Finalise the best matching results report
      best_match_result <- best_match_result |>
        dplyr::left_join(
          best_names_long,
          by = c("admin_names_to_clean", "admin_cols")
        ) |>
        dplyr::select(
          "admin_names_to_clean",
          "final_cleaned_column",
          "admin_cols",
          "match_prop"
        ) |>
        # rename the matching alorigthem cols
        dplyr::mutate(
          matching_algorithm = dplyr::case_when(
            stringr::str_detect(admin_cols, "qgram") ~ "Q-gram",
            stringr::str_detect(admin_cols, "soundex") ~ "Soundex",
            stringr::str_detect(admin_cols, "osa") ~ "Optimal String Alignment",
            stringr::str_detect(admin_cols, "lv") ~ "Levenshtein Distance",
            stringr::str_detect(admin_cols, "dl") ~ "Damerau-Levenshtein",
            stringr::str_detect(admin_cols, "lcs") ~
              "Longest Common Subsequence",
            stringr::str_detect(admin_cols, "jw") ~ "Jaro-Winkler"
          )
        ) |>
        dplyr::mutate(
          admin_cols = dplyr::case_when(
            stringr::str_detect(admin_cols, "user_base_admin_names") ~
              "User base admin names",
            stringr::str_detect(admin_cols, "alt_admin_name") ~
              "Alternative name from geonames",
            stringr::str_detect(admin_cols, "asciiname") ~
              "Main admin name from geonames",
            stringr::str_detect(
              admin_cols,
              stringr::fixed("ADM2H",
                             ignore_case = TRUE
              )
            ) ~
              "Historical name from geonames",
            TRUE ~ admin_cols
          )
        ) |>
        dplyr::select("names_to_clean" = "admin_names_to_clean",
                      "final_names" = "final_cleaned_column",
                      "source_of_cleaned_name" = "admin_cols",
                      "prop_matched" = "match_prop",
                      "matching_algorithm"
        ) |> dplyr::distinct()

      return(best_match_result)

    }


    if (user_base_only) {
      # Get the best match results if user base only
      best_match_result <- get_best_match_result(user_base_results)
    } else {
      # Get the best match results  if user base and geonames data
      best_match_result <- get_best_match_result(results_admin_data)
    }

    # Calculate matching stats
    total_dist <- nrow(best_match_result[1])
    perfect_match <- sum(best_match_result$prop_matched == 100)
    prop <- round((perfect_match / total_dist) * 100)

    # Common message part
    common_message <- glue::glue(
      crayon::blue(
        "There are {scales::comma(perfect_match)}",
        "out of {scales::comma(total_dist)} ({prop}%)",
        "admins that have been perfectly matched!"
      )
    )

    # Return result based on report_mode
    if (report_mode) {
      message(common_message)

      return(best_match_result)
    } else {
      message(paste(
        common_message,
        "\n Use `report_mode` to double check your matches."
      ))

      # Match the cleaned names with the original order
      ordered_cleaned_names <-
        best_match_result$final_names[match(
          admin_names_to_clean,
          best_match_result$names_to_clean
        )]

      return(ordered_cleaned_names)
    }
}

