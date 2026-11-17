#' Calculate morphine milligram equivalents (MME) with a `data.frame` or tibble
#'
#' Calculates the single-day MME and total MME for each individual prescription
#'     opioid medication submitted for calculation. Also calculates total MME,
#'     total days of supply, and four distinct Total MME/Day calculations from
#'     the NIH HEAL Online MME Calculator across all prescription medications
#'     for two different medication groupings: 1) opioids without buprenorphine
#'     and 2) opioids with buprenorphine.
#'
#' @details
#' The function will provide the same results regardless of whether the user has
#' specified they want calculation done using the API (`use_api`). Specifying
#' `use_api == FALSE` helps overcome the online calculator API rate limit of 50
#' (patient-level) requests per 15  minutes. In addition to returning
#' user-specified arguments, `calculate_mme()` also returns several  other
#' variables mentioned in the **Description** section. Output variable
#' description details are below; see
#' [Adams, *et al*. (2025)](https://www.doi.org/10.1097/j.pain.0000000000003529)
#' for a comprehensive overview.
#'
#' # Prescription-Level
#'
#' **Conversion Factor for <`medication_name`>** (`factor`): the conversion
#'     factor used for calculating total MME/day.
#'
#' **MME for <`medication_name`>** (`mme`): Morphine milligram equivalent
#'     for the whole prescription specified in `medication_name`, calculated as
#'     `(dose) * (doses_per_24_hours) * (factor) * (days_of_medication)`.
#'
#' **24h MME for <`medication_name`>** (`single_day_mme`): Morphine milligram
#'     equivalent for the prescription specified in `medication_name` for a
#'     single day, calculated as `(dose) * (doses_per_24_hours) * (factor)`.
#'
#' **One day**: Typically, the day with highest opioid exposure is entered, and
#'     the sum of 24-hour MME across the drugs that apply to this day is
#'     calculated. Highest MME in one day is definition 4.
#'
#' # Summary-Level:
#'
#' **On-therapy Days** (`therapy_days`): The sum of prescription duration
#'     (`days_of_medication`) for each medication, but _with each calendar day
#'     counted only ONCE_. User-supplied; this is the denominator for MME/Day
#'     definition 2.
#'
#'   * If there is only one prescription, or if there is no calendar overlap
#'       (no days on which more than one prescription is active), this will be
#'       the same as the total days supply.
#'   * If there are overlapping prescriptions, this is the number of unique
#'       calendar days.
#'
#' **Total MME** (`total_mme`): The MME for each medication, summed across all
#'     prescriptions. This is the numerator for MME/Day definitions 1, 2, and 3.
#'
#' **Total Days Supply** (`total_days`): The sum of the entered prescription
#'     duration (`days_of_medication`) for each of the `medications` (Med 1
#'     duration + med 2 duration...). Automatically calculated. This is the
#'     denominator for MME/Day definition 1.
#'
#' ## MME/Day
#'
#' MME/Day is an aggregate measure, calculating the total MME divided by a
#'     specified time window (a number of days). The MME/Day definitions specify
#'     the number of days:
#'
#' **MME/Day Definition 1 (`mme1`): Total Days Supply**
#'
#' MME Definition 1 = Total MME / Total Days Supply time window (sum of entered
#'     prescription durations).
#'
#' ```r
#' mme1 = total_mme / total_days
#' ```
#'
#' * Note that the same calendar day may contribute multiple times, if
#'     overlapping prescriptions.
#' * Reason to select this definition: This is the least complicated
#'     calculation; appears best suited when immediate-release opioids are
#'     prescribed for short discrete times.
#' * Identified challenge with this definition: It consistently underestimated
#'     MME per day when overlapping prescriptions were present or when
#'     immediate-release and extended release opioids were prescribed
#'     concurrently.
#'
#' **MME/Day Definition 2 (`mme2`): On-therapy Days**
#'
#' MME Definition 2 = Total MME / On-therapy Days time window (sum of entered
#'     prescription durations except each calendar day is counted only ONCE).
#'
#' ```r
#' mme2 = total_mme / therapy_days
#' ```
#' * Note - On-therapy Days unique calendar days.
#' * Reason to select this definition: Provides a smoothed measure useful in
#'     studies of dose-dependent adverse effects, including opioid-induced
#'     constipation or overdose in patients with opioid tolerance or who have
#'     been stable on opioids.
#' * Identified challenge with this definition: The metric is time-varying and
#'     affords the greatest flexibility to define medication gap periods and
#'     leftover/unused medications to improve pharmacoepidemiologic studies.
#'
#' **MME/Day Definition 3 (`mme3`): Fixed Observation Window**
#'
#' Uses the Total MME study-specified fixed observation window. MME Definition
#' 3 = Total MME / Number of days in observation window:
#'
#' ```r
#' mme3 = total_mme / observation_window_days
#' ```
#'
#' * If this definition is selected, it is important to report on the duration
#'     of the fixed window.
#' * Reason to select this definition: Most suitable for studies with a known
#'     or suspected duration of risk during which adverse events are expected
#'     to occur, such as incidence of opioid use disorder. This definition may
#'     be useful when prescriptions are filled at irregular time intervals on a
#'     as needed basis (_pro re nata_, PRN).
#' * Identified challenge with this definition: The definition consistently had
#'     the lowest milligrams per day for immediate-release opioids. It is the
#'     most robust to misspecification, amenable to transformations, and has
#'     the least noise when constructing continuous functions. However, since
#'     it assumes uniform exposure/risk within a window, there is less scope
#'     for time-varying adjustment.
#' * This is the definition recommended by the Department of Health and Human
#'     Services Office of the Inspector General.
#'
#' **MME/Day Definition 4 (`mme4`): Maximum Daily Dose**
#'
#' Uses the sum of 24-hour MME for the day with highest opioid exposure.
#'
#' MME Definition 4 = Drug 1 (dose (mg) x # of doses per day) x conversion
#' factor + Drug 2 (dose (mg) x # of doses per day) x conversion factor + ...
#'
#' ```r
#' mme4 = sum(dose * doses_per_24_hours * factor)
#' ```
#' * Report the highest single-day exposure.
#' * Reason to select this definition: A toxicological perspective may be
#'     appropriate for patients with no opioid tolerance and in the presence
#'     of comorbidities for respiratory depression. It appears to be best
#'     suited for immediate dose-dependent toxic effects, such as respiratory
#'     depression.
#' * Identified challenged with this definition: This definition may have
#'     limited use if it includes opioids where fatal toxicity does not involve
#'     respiratory depression (e.g., tramadol) or have atypical _mu_-opioid
#'     receptor agonism (e.g., tapentadol, buprenorphine).
#' * The definition assumes uniform risk of adverse outcomes regardless of time
#'     on-therapy. More so than the others, this definition is prone to
#'     influence from early refills, unused medication, and how the 90 MME
#'     threshold is operationalized.
#' * This definition underlies the algorithm embedded in the CDC Opioid
#'     Guideline mobile app. There may be difficulty reconciling findings with
#'     studies using the other definitions because it returns a MME per day
#'     that is significantly higher.
#' * This calculator sums the 24-hour MME for every prescription, without
#'     considering calendar dates.
#'
#' @param x (`data.frame` or `tbl_df`)\cr
#'     Object with input data - either a `data.frame` or tibble with data in
#'     long format, with one row per medication per patient or participant
#'     (`id_col`) and including all necessary data for MME calculations
#'     (see [opioid_trial] data) and/or other function arguments
#' @param id_col (`charcter`)\cr
#'     Name of the column containing patient identifier; default is
#'     `"patient_id"`
#' @param medication_col (`charcter`)\cr
#'     Name of the column containing medication names; default is
#'     `"medication_name"`
#' @param dose_col (`charcter`)\cr
#'     Name of the column containing dose values; default is `"dose"`
#' @param doses_per_day_col (`charcter`)\cr
#'     Name of the column containing doses per 24 hours; default is
#'     `"doses_per_24_hours"`
#' @param days_col (`charcter`)\cr
#'     Name of the column containing days of medication; default is
#'     `"days_of_medication"`
#' @param therapy_days_col (`charcter`)\cr
#'     Name of the column containing therapy days with buprenorphine (up to one
#'     unique value per patient); default is `"therapy_days"`
#' @param observation_days_col (`charcter`)\cr
#'     Name of the column containing observation window days with buprenorphine
#'     (up to one unique value per patient); default is
#'     `"observation_window_days"`
#' @param therapy_days_without_col (`charcter`)\cr
#'     Name of the column containing therapy days without buprenorphine (up to
#'     one unique value per patient). If `NULL` (default), uses the value from
#'     `therapy_days_col`.
#' @param observation_days_without_col (`charcter`)\cr
#'     Name of the column containing observation window days without
#'     buprenorphine (up to one unique value per patient). If `NULL` (default),
#'     uses the value from `observation_days_col`.
#' @param use_api (`logical`)\cr
#'     Indicates whether to use the NIH HEAL Online MME Calculator API to
#'     perform calculations or perform them locally instead. For
#'     `calculate_mme.data.frame()` and `calculate_mme.tbl_df()`, the default is
#'     `FALSE`, as the functions assume the user needs to perform the MME
#'     calculations without being restricted by the API rate limit of 50
#'     patient-level calculations per 15 minutes. This also allows the user to
#'     perform the calculations without relying on internet access.
#' @inheritParams rlang::args_dots_empty
#'
#' @returns A list containing three `data.frame` elements:
#'
#'   * `medications`: The original data with added prescription-level MME columns
#'   * `patient_summary_with_buprenorphine`: Patient-level MME summary including
#'   buprenorphine
#'   * `patient_summary_without_buprenorphine`: Patient-level MME summary
#'   excluding buprenorphine
#'
#' @rdname calculate_mme.data.frame
#' @seealso [`calculate_mme.list()`]
#' @export
#'
#' @examples
#' library(dplyr)
#' # Calculate MME using long-format data
#' # Subset of opioid_trial data used for speedier example
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
calculate_mme.data.frame <- function(
  x,
  id_col = "patient_id",
  medication_col = "medication_name",
  dose_col = "dose",
  doses_per_day_col = "doses_per_24_hours",
  days_col = "days_of_medication",
  therapy_days_col = "therapy_days",
  observation_days_col = "observation_window_days",
  therapy_days_without_col = NULL,
  observation_days_without_col = NULL,
  use_api = FALSE,
  ...
) {
  # Ensure data is not empty
  if (nrow(x) == 0) {
    cli::cli_abort("{.arg x} must contian at least one row of data")
  }

  # Validate input x
  if (!id_col %in% names(x)) {
    cli::cli_abort("{.arg id_col} column {.val {id_col}} not found in {.arg x}")
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

  missing_cols <- setdiff(required_cols, names(x))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "{.data {as.character(substitute(x))}} is missing required columns: {.val {missing_cols}}"
    )
  }

  # Check number of unique patients and warn if using API with > 50 patients
  patient_ids <- unique(x[[id_col]])
  n_patients <- length(patient_ids)

  check_unique_pat(n_patients, use_api)
  advise_api_usage(n_patients, use_api)

  # Initialize result data frame (copy of original)
  result_data <- x

  # Add columns for prescription-level MME calculations
  result_data$factor <- NA_real_
  result_data$mme <- NA_real_
  result_data$single_day_mme <- NA_real_

  # Create data frames for patient-level summaries
  patient_summary_with <- data.frame()
  patient_summary_without <- data.frame()

  # Process each patient
  patient_ids <- unique(x[[id_col]])

  for (pid in patient_ids) {
    # Get data for this patient
    patient_data <- x[x[[id_col]] == pid, ]

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
    mme_result <- calculate_mme.list(
      therapy_days = c(therapy_days_with, therapy_days_without),
      observation_window_days = c(
        observation_days_with,
        observation_days_without
      ),
      x = medications,
      use_api = use_api
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
        row_idx <- which(x[[id_col]] == pid)[i]
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
  final_data <- list(
    medications = result_data,
    patient_summary_with_buprenorphine = patient_summary_with,
    patient_summary_without_buprenorphine = patient_summary_without
  )

  return(final_data)
}


#' @rdname calculate_mme.data.frame
#' @export
calculate_mme.tbl_df <- calculate_mme.data.frame
