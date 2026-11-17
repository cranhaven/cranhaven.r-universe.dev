#' Calculate MME for medication data in long format
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function takes medication data in long format (multiple rows per patient
#' ID), calculates MME using the local calculation method
#' ([calculate_mme_local()]), and adds prescription-level values as new columns.
#' It also returns two additional data frames with patient-level MME summaries
#' (one with buprenorphine included and one without).
#'
#' @param data A `data.frame` or `tibble` in long format with one row per
#'     medication per patient or participant (`id_col`)
#' @param id_col Name of the column containing patient identifier; default is
#'     `"patient_id"`
#' @param medication_col Name of the column containing medication names; default
#'     is `"medication_name"`
#' @param dose_col Name of the column containing dose values; default is
#'     `"dose"`
#' @param doses_per_day_col Name of the column containing doses per 24 hours;
#'     `"doses_per_24_hours"`
#' @param days_col Name of the column containing days of medication; default is
#'     `"days_of_medication"`
#' @param therapy_days_col Name of the column containing therapy days with
#'     buprenorphine (up to one unique value per patient); default is
#'     `"therapy_days"`
#' @param observation_days_col Name of the column containing observation window
#'     days with buprenorphine (up to one unique value per patient); default is
#'     `"observation_window_days"`
#' @param therapy_days_without_col Name of the column containing therapy days
#'     without buprenorphine (up to one unique value per patient). If `NULL`
#'     (default), uses the value from `therapy_days_col`.
#' @param observation_days_without_col Name of the column containing observation
#'     window days without buprenorphine (up to one unique value per patient).
#'     If `NULL` (default), uses the value from `observation_days_col`.
#'
#' @returns A list containing three elements:
#'
#'   * `medications`: The original `data.frame` with added prescription-level
#'   MME columns
#'   * `patient_summary_with_buprenorphine`: Patient-level MME summary including
#'   buprenorphine
#'   * `patient_summary_without_buprenorphine`: Patient-level MME summary
#'   excluding buprenorphine
#'
#' @export
#'
#' @keywords internal
#'
#' @seealso [calculate_mme_local()]
#'
#' @examples
#' library(dplyr)
#' # Calculate MME using long-format data
#' # Subset of opioid_trial data used for speedier example
#' mme <- calculate_mme_df(
#'   data = opioid_trial |> dplyr::filter(patient_id %in% sprintf("P%03d", 1:100)),
#'   therapy_days_without_col = "therapy_days_without",
#'   observation_days_without_col = "observation_window_days_without"
#'   )
#' # ->
#' mme <- calculate_mme(
#'   x = opioid_trial |> dplyr::filter(patient_id %in% sprintf("P%03d", 1:100)),
#'   therapy_days_without_col = "therapy_days_without",
#'   observation_days_without_col = "observation_window_days_without"
#'   )
#'
#' head(mme$medications)
#'
#' head(mme$patient_summary_with_buprenorphine)
#'
#' head(mme$patient_summary_without_buprenorphine)
#'
#' # Cleanup
#' rm(mme)
#'
calculate_mme_df <- function(
  data,
  id_col = "patient_id",
  medication_col = "medication_name",
  dose_col = "dose",
  doses_per_day_col = "doses_per_24_hours",
  days_col = "days_of_medication",
  therapy_days_col = "therapy_days",
  observation_days_col = "observation_window_days",
  therapy_days_without_col = NULL,
  observation_days_without_col = NULL
) {
  lifecycle::deprecate_warn(
    "1.0.0",
    "calculate_mme_df()",
    "calculate_mme()"
  )

  # Must be tibble or data.frame
  if (!inherits(data, "data.frame")) {
    cli::cli_abort(c(
      "{.arg data} must be a {.cls data.frame} or {.cls tbl_df}",
      "x" = "{as.character(substitute(data))} is a {.cls {class(data)}}"
    ))
  }

  # Ensure data is not empty
  if (nrow(data) == 0) {
    cli::cli_abort("{.arg data} must contian at least one row of data")
  }

  # Validate input data
  if (!id_col %in% names(data)) {
    cli::cli_abort(
      "{.arg id_col} column {.val {id_col}} not found in {.arg data}"
    )
  }

  required_cols <- c(
    medication_col,
    dose_col,
    doses_per_day_col,
    days_col,
    therapy_days_col,
    observation_days_col
  )
  if (!is.null(therapy_days_without_col))
    required_cols <- c(required_cols, therapy_days_without_col)
  if (!is.null(observation_days_without_col))
    required_cols <- c(required_cols, observation_days_without_col)

  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "{.data {as.character(substitute(data))}} is missing required columns: {.val {missing_cols}}"
    )
  }

  # Initialize result data frame (copy of original)
  result_data <- data

  # Add columns for prescription-level MME calculations
  result_data$factor <- NA_real_
  result_data$mme <- NA_real_
  result_data$single_day_mme <- NA_real_

  # Create data frames for patient-level summaries
  patient_summary_with <- data.frame()
  patient_summary_without <- data.frame()

  # Process each patient
  patient_ids <- unique(data[[id_col]])

  for (pid in patient_ids) {
    # Get data for this patient
    patient_data <- data[data[[id_col]] == pid, ]

    # Extract parameters for with/without buprenorphine calculations
    therapy_days_with <- patient_data[[therapy_days_col]][1]
    observation_days_with <- patient_data[[observation_days_col]][1]

    # Use specified columns for without bupr, or fall back to default columns
    if (!is.null(therapy_days_without_col)) {
      therapy_days_without <- patient_data[[therapy_days_without_col]][1]
    } else {
      therapy_days_without <- therapy_days_with
    }

    if (!is.null(observation_days_without_col)) {
      observation_days_without <- patient_data[[observation_days_without_col]][
        1
      ]
    } else {
      observation_days_without <- observation_days_with
    }

    # Convert medication data to the format expected by calculate_mme_local
    medications <- lapply(seq_len(nrow(patient_data)), function(i) {
      list(
        medication_name = patient_data[[medication_col]][i],
        dose = patient_data[[dose_col]][i],
        doses_per_24_hours = patient_data[[doses_per_day_col]][i],
        days_of_medication = patient_data[[days_col]][i]
      )
    })

    # Calculate MME using the specified parameters
    withr::local_options(lifecycle_verbosity = "quiet")
    mme_result <- calculate_mme_local(
      therapy_days = c(therapy_days_with, therapy_days_without),
      observation_window_days = c(
        observation_days_with,
        observation_days_without
      ),
      medications = medications
    )

    # Extract prescription-level results and update the result data frame
    med_results <- mme_result$medications

    # Match each medication back to the original rows
    for (i in seq_len(nrow(patient_data))) {
      # Find matching medication in results
      med_name <- patient_data[[medication_col]][i]
      med_dose <- patient_data[[dose_col]][i]
      med_doses_per_day <- patient_data[[doses_per_day_col]][i]
      med_days <- patient_data[[days_col]][i]

      # Find the matching row in med_results
      matched_idx <- which(
        med_results$medication_name == med_name &
          abs(med_results$dose - med_dose) < 1e-6 &
          abs(med_results$doses_per_24_hours - med_doses_per_day) < 1e-6 &
          abs(med_results$days_of_medication - med_days) < 1e-6
      )

      if (length(matched_idx) == 1) {
        # Update result data with prescription-level values
        row_idx <- which(data[[id_col]] == pid)[i]
        result_data$factor[row_idx] <- med_results$factor[matched_idx]
        result_data$mme[row_idx] <- med_results$mme[matched_idx]
        result_data$single_day_mme[row_idx] <- med_results$single_day_mme[
          matched_idx
        ]
      }
    }

    # Extract patient-level summaries
    with_bupr <- data.frame(
      patient_id = pid,
      therapy_days = mme_result$therapy_obs_window_with$therapy_days,
      observation_window_days = mme_result$therapy_obs_window_with$observation_window_days,
      total_mme = mme_result$mme_definitions$with_buprenorphine$total_mme,
      total_days = mme_result$mme_definitions$with_buprenorphine$total_days,
      mme1 = mme_result$mme_definitions$with_buprenorphine$mme1,
      mme2 = mme_result$mme_definitions$with_buprenorphine$mme2,
      mme3 = mme_result$mme_definitions$with_buprenorphine$mme3,
      mme4 = mme_result$mme_definitions$with_buprenorphine$mme4
    )

    without_bupr <- data.frame(
      patient_id = pid,
      therapy_days = mme_result$therapy_obs_window_without$therapy_days,
      observation_window_days = mme_result$therapy_obs_window_without$observation_window_days,
      total_mme = mme_result$mme_definitions$without_buprenorphine$total_mme,
      total_days = mme_result$mme_definitions$without_buprenorphine$total_days,
      mme1 = mme_result$mme_definitions$without_buprenorphine$mme1,
      mme2 = mme_result$mme_definitions$without_buprenorphine$mme2,
      mme3 = mme_result$mme_definitions$without_buprenorphine$mme3,
      mme4 = mme_result$mme_definitions$without_buprenorphine$mme4
    )

    # Add to summary data frames
    patient_summary_with <- rbind(patient_summary_with, with_bupr)
    patient_summary_without <- rbind(patient_summary_without, without_bupr)
  }

  # Rename the patient ID column in summaries to match input
  names(patient_summary_with)[
    names(patient_summary_with) == "patient_id"
  ] <- id_col
  names(patient_summary_without)[
    names(patient_summary_without) == "patient_id"
  ] <- id_col

  # Return results as a list
  list(
    medications = result_data,
    patient_summary_with_buprenorphine = patient_summary_with,
    patient_summary_without_buprenorphine = patient_summary_without
  )
}
