#' Calculate morphine milligram equivalents (MME)
#'
#' - [`calculate_mme.list()`]
#' - [`calculate_mme.data.frame()`]
#' - [`calculate_mme.tbl_df()`]
#'
#' @param x (`list`, `data.frame`, or `tbl_df`)\cr
#'     Object with input data; either a `list` of medications or long format
#'     data - `data.frame` or tibble.
#' @param ... Additional arguments passed to methods
#'
#' @keywords internal
#'
#' @export
#'
calculate_mme <- function(x, ...) {
  if (rlang::is_list(x) || inherits(x, "data.frame")) {
    UseMethod("calculate_mme")
  } else {
    cli::cli_abort(c(
      "The {.arg x} argument must be a {.cls list}, {.cls data.frame}, or {.cls tbl_df}",
      "x" = "You've specified a {.cls {class(x)}}"
    ))
  }
}


#' Calculate morphine milligram equivalents (MME) for a single study participant
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
#' @param x (`list`)\cr
#'     A `list` of medications. Each element must be a nested `list` containing
#'     each of the following fields:
#'
#'  * `medication_name`: a string matching an API-accepted medication name and
#'      its dosage units. To see a full list of API-accepted values, run
#'      [get_med_list()].
#'  * `dose`: a positive number indicating the dose of the associated opioid
#'      medication listed in the `medication_name` field. Units of `dose` should
#'      match the units listed in `medication_name`.
#'  * `doses_per_24_hours`: a positive number indicating the number of doses in
#'      24 hours.
#'  * `days_of_medication`: a positive number indicating the duration of the
#'      opioid medication prescription listed in the associated
#'      `medication_name` in days.
#' @param therapy_days (`numeric`)\cr
#'     Either a single positive number or a vector of two positive numbers
#'     indicating the sum of prescription duration (days) for each medication,
#'     but _with each calendar day counted only ONCE_. When a single number is
#'     provided, it is used for the both the "with buprenorphine" and "without
#'     buprenorphine" MME calculations; when a vector of 2 numbers is provided
#'     (e.g., `c(10, 18)`) then the first and second numbers in the vector are
#'     used for the "with buprenorphine" and "without buprenorphine" MME
#'     calculations, respectively. This is the denominator for MME/Day
#'     definition 2.
#'
#'  * If there is only one prescription _or_ if there is no calendar overlap
#'      (i.e., no days on which more than one prescription is active) this
#'      will be the same as the total days supply returned by the calculator
#'      API (`total_days`).
#'  * If there are overlapping prescriptions, this is the number of _unique_
#'      calendar days.
#' @param observation_window_days (`numeric`)\cr
#'     Either a single positive number or a vector of two positive numbers
#'     indicating a study-defined fixed observation window of time. Typical
#'     choices are 7 day, 14 day, 30 day, 90 day. When a single number is
#'     provided, it is used for the both the "with buprenorphine" and "without
#'     buprenorphine" MME calculations; when a vector of 2 numbers is provided
#'     (e.g., `c(7, 30)`) then the first and second numbers in the vector are
#'     used for the "with buprenorphine" and "without buprenorphine" MME
#'     calculations, respectively. This is the denominator for MME/Day
#'     definition 3.
#' @param use_api (`logical`)\cr
#'     Indicates whether to use the NIH HEAL Online MME Calculator API (default)
#'     to perform calculations or perform them locally instead (`FALSE`).
#'     Setting to `FALSE` allows the user to perform the same calculations
#'     without being restricted by the Online MME Calculator API rate limit of
#'     50 patient calculations per 15 minutes and also allows the user to
#'     perform the calculations without relying on internet access.
#' @inheritParams rlang::args_dots_empty
#'
#' @returns
#' A `list` of MME calculations. Will error if any medications are invalid or if
#'     any numeric parameters are not positive numbers.
#'
#' @seealso [`calculate_mme.data.frame()`]
#' @export
#'
#' @examples
#' # Recreating example from Adams MCB, et al. 2025 supplement
#' #     https://links.lww.com/PAIN/C213
#' meds_list <- list(
#'   list(
#'     medication_name = "Morphine (mg)",
#'     dose = 5,
#'     doses_per_24_hours = 4,
#'     days_of_medication = 7
#'     ),
#'   list(
#'     medication_name = "Morphine (mg) LA",
#'     dose = 10,
#'     doses_per_24_hours = 3,
#'     days_of_medication = 30
#'     )
#' )
#'
#' # Using API
#' calculate_mme(meds_list, 30, 90)
#'
#' # Not using API
#' calculate_mme(meds_list, 30, 90, use_api = FALSE)
#'
#' # Clean up meds_list
#' rm(meds_list)
#'
calculate_mme.list <- function(
  x,
  therapy_days,
  observation_window_days,
  use_api = TRUE,
  ...
) {
  # Validate inputs
  # ------ Input validation for therapy_days ------
  if (is.numeric(therapy_days) && length(therapy_days) == 1) {
    if (therapy_days <= 0) {
      cli::cli_abort(
        "The {.arg therapy_days} argument must be a positive number"
      )
    }

    # Single value provided - use for both with and without
    therapy_days_with <- therapy_days
    therapy_days_without <- therapy_days
  } else if (is.numeric(therapy_days) && length(therapy_days) == 2) {
    if (therapy_days[1] <= 0 || therapy_days[2] <= 0) {
      cli::cli_abort(
        "All values in {.arg therapy_days} must be positive numbers"
      )
    }

    # Two values provided - use first for with, second for without
    therapy_days_with <- therapy_days[1]
    therapy_days_without <- therapy_days[2]
  } else {
    cli::cli_abort(c(
      "The {.arg therapy_days} argument must be either a single positive number or a vector of 2 positive numbers",
      "i" = "Use a vector of 2 numbers to set different values for MME calculations 'with buprenorphine' and 'without buprenorphine'",
      "i" = "Use a single number to apply the same value to both scenarios"
    ))
  }

  # ------ Input validation for observation_window_days ------
  if (
    is.numeric(observation_window_days) && length(observation_window_days) == 1
  ) {
    if (observation_window_days <= 0) {
      cli::cli_abort(
        "The {.arg observation_window_days} argument must be a positive number"
      )
    }

    # Single value provided - use for both with and without
    observation_window_days_with <- observation_window_days
    observation_window_days_without <- observation_window_days
  } else if (
    is.numeric(observation_window_days) && length(observation_window_days) == 2
  ) {
    if (observation_window_days[1] <= 0 || observation_window_days[2] <= 0) {
      cli::cli_abort(
        "All values in {.arg observation_window_days} must be positive numbers"
      )
    }

    # Two values provided - use first for with, second for without
    observation_window_days_with <- observation_window_days[1]
    observation_window_days_without <- observation_window_days[2]
  } else {
    cli::cli_abort(c(
      "The {.arg observation_window_days} argument must be either a single positive number or a vector of 2 positive numbers",
      "i" = "Use a vector of 2 numbers to set different values for MME calculations 'with buprenorphine' and 'without buprenorphine'",
      "i" = "Use a single number to apply the same value to both scenarios"
    ))
  }

  # Use the internal package data with saved med list
  valid_med_names <- med_list$med_name

  # Validate each medication in the list
  required_fields <- c(
    "medication_name",
    "dose",
    "doses_per_24_hours",
    "days_of_medication"
  )

  if (length(x) == 0) {
    cli::cli_abort(c(
      "When the {.arg x} argument is a {.cls list}, it must have at least one nested {.cls list} element",
      "i" = "Each nested {.cls list} element must have the following fields: {.field {required_fields}}"
    ))
  }

  for (i in seq_along(x)) {
    med <- x[[i]]

    # Check if it's a list
    if (!is.list(med)) {
      cli::cli_abort(c(
        "Element {i} in {.arg x} must be a {.cls list}",
        "x" = "You've supplied a {.cls {class(med)}}"
      ))
    }

    # Check for required fields
    missing_fields <- setdiff(required_fields, names(med))
    if (length(missing_fields) > 0) {
      cli::cli_abort(
        "Element {i} in {.arg x} is missing required fields: {.field {missing_fields}}"
      )
    }

    # Validate field types
    if (
      !is.character(med$medication_name) || length(med$medication_name) != 1
    ) {
      cli::cli_abort(
        "Element {i} in {.arg x}: {.field medication_name} must be a single {.cls character} string"
      )
    }

    if (
      length(valid_med_names) > 0 && !med$medication_name %in% valid_med_names
    ) {
      cli::cli_abort(c(
        "Element {i} in {.arg x}: {.field medication_name} {.val {med$medication_name}} is not a medication name accepted by the API",
        "i" = "Run {.fn get_med_list} to see the list of {.field medication_name}s accepted by the API"
      ))
    }

    if (!is.numeric(med$dose) || length(med$dose) != 1 || med$dose <= 0) {
      cli::cli_abort(
        "Element {i} in {.arg x}: {.field dose} must be a positive number"
      )
    }

    if (
      !is.numeric(med$doses_per_24_hours) ||
        length(med$doses_per_24_hours) != 1 ||
        med$doses_per_24_hours <= 0
    ) {
      cli::cli_abort(
        "Element {i} in {.arg x}: {.field doses_per_24_hours} must be a positive number"
      )
    }

    if (
      !is.numeric(med$days_of_medication) ||
        length(med$days_of_medication) != 1 ||
        med$days_of_medication <= 0
    ) {
      cli::cli_abort(
        "Element {i} in {.arg x}: {.field days_of_medication} must be a positive number"
      )
    }
  }

  if (!rlang::is_logical(use_api)) {
    cli::cli_abort(
      "{.arg use_api} must be either {.code TRUE} or {.code FALSE}"
    )
  }

  if (use_api) {
    # Ensure computer is online for API use
    if (!httr2::is_online()) {
      cli::cli_abort(c(
        "You aren't connected to the internet, which means you can't use the API",
        "i" = "Set {.arg use_api} to {.code FALSE} to calculate MME locally instead"
      ))
    }

    # Base URL for the API
    base_url <- "https://research-mme.wakehealth.edu/api"

    # Create the payload structure
    payload <- list(
      therapy_obs_window_with = list(
        therapy_days = therapy_days_with,
        observation_window_days = observation_window_days_with
      ),
      therapy_obs_window_without = list(
        therapy_days = therapy_days_without,
        observation_window_days = observation_window_days_without
      ),
      medications = x
    )

    req <- httr2::request(glue::glue("{base_url}/mme_definitions")) |>
      httr2::req_headers(
        accept = "application/json",
        "Content-Type" = "application/json"
      ) |>
      httr2::req_body_json(payload) |>
      mmequiv_req_retry()

    resp <- req |>
      httr2::req_perform()

    resp |>
      httr2::resp_body_json(simplifyVector = TRUE)
  } else {
    # Get conversion factors from internal package data
    cf_lookup <- stats::setNames(med_list$cf, med_list$med_name)

    # Calculate per-medication stats
    processed_meds <- lapply(x, function(med) {
      # Get conversion factor for this medication
      factor <- cf_lookup[med$medication_name]

      # Calculate MMEs
      single_day_mme <- med$dose * med$doses_per_24_hours * factor
      mme <- single_day_mme * med$days_of_medication

      # Add calculated fields to the medication
      med$factor <- as.numeric(factor)
      med$mme <- as.numeric(mme)
      med$single_day_mme <- as.numeric(single_day_mme)
      return(med)
    })

    # Convert medications list to data frame for easier processing
    meds_df <- do.call(
      rbind,
      lapply(processed_meds, function(x) {
        data.frame(
          medication_name = x$medication_name,
          dose = x$dose,
          doses_per_24_hours = x$doses_per_24_hours,
          days_of_medication = x$days_of_medication,
          factor = x$factor,
          mme = x$mme,
          single_day_mme = x$single_day_mme,
          stringsAsFactors = FALSE
        )
      })
    )

    # Identify buprenorphine medications
    is_bupr <- grepl(
      "buprenorphine",
      tolower(meds_df$medication_name),
      ignore.case = TRUE
    )

    # Calculate aggregates with buprenorphine
    total_mme_with <- sum(meds_df$mme)
    total_days_with <- sum(meds_df$days_of_medication)

    # Calculate MME definitions with buprenorphine
    mme1_with <- total_mme_with / total_days_with
    mme2_with <- total_mme_with / therapy_days_with
    mme3_with <- total_mme_with / observation_window_days_with
    mme4_with <- sum(meds_df$single_day_mme)

    # Calculate aggregates without buprenorphine (exclude buprenorphine meds)
    if (any(is_bupr)) {
      meds_df_no_bupr <- meds_df[!is_bupr, ]
      total_mme_without <- sum(meds_df_no_bupr$mme)
      total_days_without <- sum(meds_df_no_bupr$days_of_medication)
      mme4_without <- sum(meds_df_no_bupr$single_day_mme)
    } else {
      # If no buprenorphine meds, results are the same
      total_mme_without <- total_mme_with
      total_days_without <- total_days_with
      mme4_without <- mme4_with
    }

    # Calculate MME definitions without buprenorphine
    mme1_without <- total_mme_without / total_days_without
    mme2_without <- total_mme_without / therapy_days_without
    mme3_without <- total_mme_without / observation_window_days_without

    # Create a response object that matches API response structure exactly
    result <- list(
      message = "Processed medications (local calculation)",
      therapy_obs_window_with = list(
        therapy_days = therapy_days_with,
        observation_window_days = observation_window_days_with
      ),
      therapy_obs_window_without = list(
        therapy_days = therapy_days_without,
        observation_window_days = observation_window_days_without
      ),
      medications = meds_df,
      mme_definitions = list(
        with_buprenorphine = data.frame(
          total_mme = total_mme_with,
          total_days = total_days_with,
          mme1 = mme1_with,
          mme2 = mme2_with,
          mme3 = mme3_with,
          mme4 = mme4_with
        ),
        without_buprenorphine = data.frame(
          total_mme = total_mme_without,
          total_days = total_days_without,
          mme1 = ifelse(is.nan(mme1_without), NA_real_, mme1_without),
          mme2 = mme2_without,
          mme3 = mme3_without,
          mme4 = mme4_without
        )
      )
    )

    return(result)
  }
}
