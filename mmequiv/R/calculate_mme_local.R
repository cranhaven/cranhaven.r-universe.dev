#' Calculate morphine milligram equivalents (MME)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Calculates the single-day MME and total MME for each individual prescription
#'     opioid medication submitted for calculation. Also calculates total MME,
#'     total days of supply, and four distinct Total MME/Day calculations from
#'     the NIH HEAL Online MME Calculator across all prescription medications
#'     for two different medication groupings: 1) opioids without buprenorphine
#'     and 2) opioids with buprenorphine.
#'
#' @details
#' `calculate_mme()` and `calculate_mme_local()` produce the same calculation
#' results with and without using the API, respectively. This helps overcome the
#' online calculator API rate limit of 50 (patient-level) requests per 15
#' minutes. In addition to returning user-specified arguments, `calculate_mme()`
#' also returns several  other variables mentioned in the **Description**
#' section, which are described in more detail below. Output variable
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
#' **Total MME** (`total_mme`): The MME for each medication, summed across all prescriptions.
#'     This is the numerator for MME/Day definitions 1, 2, and 3.
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
#' MME Definition 1 = Total MME / Total Days Supply time window (sum of entered prescription durations).
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
#' @param therapy_days Either a single positive number or a vector of two
#'     positive numbers indicating the sum of prescription duration (days) for
#'     each medication, but _with each calendar day counted only ONCE_. When a
#'     single number is provided, it is used for the both the "with
#'     buprenorphine" and "without buprenorphine" MME calculations; when a
#'     vector of 2 numbers is provided (e.g., `c(10, 18)`) then the first and
#'     second numbers in the vector are used for the "with buprenorphine" and
#'     "without buprenorphine" MME calculations, respectively. This is the
#'     denominator for MME/Day definition 2.
#'
#'  * If there is only one prescription _or_ if there is no calendar overlap
#'      (i.e., no days on which more than one prescription is active) this
#'      will be the same as the total days supply returned by the calculator
#'      API (`total_days`).
#'  * If there are overlapping prescriptions, this is the number of _unique_
#'      calendar days.
#' @param observation_window_days Either a single positive number or a vector of
#'     two positive numbers indicating a study-defined fixed observation window
#'     of time. Typical choices are 7 day, 14 day, 30 day, 90 day. When a single
#'     number is provided, it is used for the both the "with buprenorphine" and
#'     "without buprenorphine" MME calculations; when a vector of 2 numbers is
#'     provided (e.g., `c(7, 30)`) then the first and second numbers in the
#'     vector are used for the "with buprenorphine" and "without buprenorphine"
#'     MME calculations, respectively. This is the denominator for MME/Day
#'     definition 3.
#' @param medications A list of medication definitions. Each element must be a
#'     list containing each of the following fields:
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
#'
#' @returns
#' A list of MME calculations from the API. Will error if any medications are invalid
#'     or if any numeric parameters are not positive numbers.
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' meds_list <- list(
#'   list(
#'     medication_name = "Buprenorphine buccal film (mcg) buccal",
#'     dose = 50,
#'     doses_per_24_hours = 2,
#'     days_of_medication = 5
#'     ),
#'   list(
#'     medication_name = "Hydrocodone (mg)",
#'     dose = 75,
#'     doses_per_24_hours = 3,
#'     days_of_medication = 10
#'     )
#' )
#'
#' calculate_mme_local(10, 5, meds_list)
#' # ->
#' calculate_mme(meds_list, 10, 5, use_api = FALSE)
#'
#' # Clean up meds_list
#' rm(meds_list)
calculate_mme_local <- function(
  therapy_days,
  observation_window_days,
  medications
) {
  lifecycle::deprecate_warn(
    "1.0.0",
    "calculate_mme_local()",
    details = "Please use `calculate_mme(use_api = FALSE)` instead"
  )

  # Process therapy_days and observation_window_days to handle both single
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

  if (length(medications) == 0) {
    cli::cli_abort(
      "The {.arg medications} argument must be a non-empty {.cls list}"
    )
  }
  if (!is.list(medications)) {
    cli::cli_abort(c(
      "The {.arg medications} argument must be a {.cls list}",
      "x" = "You've supplied a {.cls {class(medications)}}"
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

  for (i in seq_along(medications)) {
    med <- medications[[i]]

    # Check if it's a list
    if (!is.list(med)) {
      cli::cli_abort(c(
        "Element {i} in {.arg medications} must be a {.cls list}",
        "x" = "You've supplied a {.cls {class(med)}}"
      ))
    }

    # Check for required fields
    missing_fields <- setdiff(required_fields, names(med))
    if (length(missing_fields) > 0) {
      cli::cli_abort(
        "Element {i} in {.arg medications} is missing required fields: {.field {missing_fields}}"
      )
    }

    # Validate field types
    if (
      !is.character(med$medication_name) || length(med$medication_name) != 1
    ) {
      cli::cli_abort(
        "Element {i} in {.arg medications}: {.field medication_name} must be a single {.cls character} string"
      )
    }

    if (
      length(valid_med_names) > 0 && !med$medication_name %in% valid_med_names
    ) {
      cli::cli_abort(c(
        "Element {i} in {.arg medications}: {.field medication_name} {.val {med$medication_name}} is not a medication name accepted by the API",
        "i" = "Run {.fn get_med_list} to see the list of {.field medication_name}s accepted by the API"
      ))
    }

    if (!is.numeric(med$dose) || length(med$dose) != 1 || med$dose <= 0) {
      cli::cli_abort(
        "Element {i} in {.arg medications}: {.field dose} must be a positive number"
      )
    }

    if (
      !is.numeric(med$doses_per_24_hours) ||
        length(med$doses_per_24_hours) != 1 ||
        med$doses_per_24_hours <= 0
    ) {
      cli::cli_abort(
        "Element {i} in {.arg medications}: {.field doses_per_24_hours} must be a positive number"
      )
    }

    if (
      !is.numeric(med$days_of_medication) ||
        length(med$days_of_medication) != 1 ||
        med$days_of_medication <= 0
    ) {
      cli::cli_abort(
        "Element {i} in {.arg medications}: {.field days_of_medication} must be a positive number"
      )
    }
  }

  # Get conversion factors from internal package data
  cf_lookup <- stats::setNames(med_list$cf, med_list$med_name)

  # Calculate per-medication stats
  processed_meds <- lapply(medications, function(med) {
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
