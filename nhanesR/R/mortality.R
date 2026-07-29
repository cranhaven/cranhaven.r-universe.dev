# R/mortality.R

# -- Download -------------------------------------------------------------------

#' Download NHANES Public-Use Linked Mortality Files
#'
#' Downloads the fixed-width `.dat` mortality files from the CDC FTP server
#' for one or more NHANES cycles. Files are cached locally; re-downloading
#' is skipped unless `refresh = TRUE`.
#'
#' The public-use LMF provides mortality follow-up through **December 31, 2019**
#' for NHANES 1999-2018 and NHANES III. Files were released in April 2022 and
#' will not be updated (the 2022-linked restricted-use files require RDC access).
#'
#' @param cycles Character vector of cycle labels. Defaults to all cycles with
#'   a public-use LMF. See [nhanes_cycles()] and [nhanes_lmf_cycles()].
#' @param refresh Logical. Re-download even if a cached file exists? Default
#'   `FALSE`.
#' @param quiet Logical. Suppress download messages? Default uses the
#'   `nhanesR.verbose` option.
#'
#' @return Invisibly, a named character vector of local file paths (one per
#'   cycle). The primary side-effect is writing files to the cache directory
#'   under `mortality/dat/`.
#'
#' @seealso [nhanes_mortality_parse()], [nhanes_mortality_link()],
#'   [nhanes_survival_prep()]
#'
#' @export
#' @examples
#' \donttest{
#' # Download all available cycles
#' nhanes_mortality_download()
#'
#' # Download specific cycles
#' nhanes_mortality_download(c("2013-2014", "2015-2016", "2017-2018"))
#' }
nhanes_mortality_download <- function(cycles = NULL,
                                      refresh = FALSE,
                                      quiet   = !getOption("nhanesR.verbose", TRUE)) {
  if (is.null(cycles)) {
    cycles <- nhanes_lmf_cycles()
  }

  cycles <- .nhanes_validate_lmf_cycles(cycles)
  dest_dir <- .nhanes_cache_subdir("mortality", "dat")

  paths <- character(length(cycles))
  names(paths) <- cycles

  for (cyc in cycles) {
    url      <- .nhanes_lmf_url(cyc)
    filename <- basename(url)
    dest     <- file.path(dest_dir, filename)
    rds_path <- .nhanes_lmf_rds_path(cyc)

    if (!refresh && file.exists(dest)) {
      if (!quiet) cli::cli_inform("Using cached LMF for {cyc}: {.path {dest}}")
      paths[[cyc]] <- dest
      next
    }

    if (!quiet) cli::cli_inform("Downloading LMF for {cyc}")
    .nhanes_download_file(url, dest, desc = paste("LMF", cyc))
    paths[[cyc]] <- dest

    # Invalidate any existing parsed RDS when raw file is refreshed
    if (file.exists(rds_path)) {
      file.remove(rds_path)
      hash_f <- paste0(rds_path, ".md5")
      if (file.exists(hash_f)) file.remove(hash_f)
    }
  }

  invisible(paths)
}

#' List NHANES cycles with a public-use LMF
#'
#' @return Character vector of cycle labels.
#' @seealso [nhanes_mortality_link()], [nhanes_mortality_download()]
#' @export
nhanes_lmf_cycles <- function() {
  .lmf_registry$cycle
}

# -- Parse ----------------------------------------------------------------------

#' Parse NHANES Linked Mortality Files into data frames
#'
#' Reads the fixed-width `.dat` files (downloading them first if needed) and
#' returns a named list of data frames, one per cycle.
#'
#' Variable labels are attached as the `"label"` attribute on each column,
#' following the `haven`/`labelled` convention.
#'
#' @param cycles Character vector of cycle labels. Defaults to all available.
#' @param refresh Logical. Re-parse even if a cached RDS exists? Default `FALSE`.
#' @param download Logical. Auto-download missing `.dat` files? Default `TRUE`.
#'
#' @return A named list of data frames. Each data frame contains:
#'   \describe{
#'     \item{SEQN}{Respondent sequence number (join key to NHANES data).}
#'     \item{ELIGSTAT}{Eligibility: 1=eligible; 2=under 18; 3=insufficient data.}
#'     \item{MORTSTAT}{Vital status: 0=assumed alive; 1=assumed deceased.}
#'     \item{UCOD_LEADING}{Underlying cause of death (11-category ICD-10 recode).}
#'     \item{DIABETES}{Diabetes mentioned on death certificate (1=yes).}
#'     \item{HYPERTEN}{Hypertension mentioned on death certificate (1=yes).}
#'     \item{PERMTH_INT}{Months of follow-up from interview date.}
#'     \item{PERMTH_EXM}{Months of follow-up from examination date.}
#'   }
#'
#' @note For select records, `PERMTH_INT`, `PERMTH_EXM`, and `UCOD_LEADING`
#'   contain **synthetic (perturbed) values** introduced by CDC to reduce
#'   re-identification risk. `MORTSTAT` and `ELIGSTAT` are not perturbed.
#'
#' @seealso [nhanes_mortality_download()] to download the raw `.dat` files;
#'   [nhanes_mortality_link()] to join parsed mortality data onto an analytic
#'   dataset.
#' @export
#' @examples
#' \donttest{
#' lmf <- nhanes_mortality_parse(c("2015-2016", "2017-2018"))
#' lmf[["2015-2016"]]
#' }
nhanes_mortality_parse <- function(cycles  = NULL,
                                   refresh  = FALSE,
                                   download = TRUE) {
  if (is.null(cycles)) cycles <- nhanes_lmf_cycles()
  cycles <- .nhanes_validate_lmf_cycles(cycles)

  result <- vector("list", length(cycles))
  names(result) <- cycles

  for (cyc in cycles) {
    rds_path <- .nhanes_lmf_rds_path(cyc)

    # Return from RDS cache if valid
    if (!refresh && .nhanes_cache_valid(rds_path)) {
      if (getOption("nhanesR.verbose")) {
        cli::cli_inform("Loading cached LMF for {cyc}")
      }
      result[[cyc]] <- readRDS(rds_path)
      next
    }

    # Ensure .dat file exists
    dat_path <- .nhanes_lmf_dat_path(cyc)
    if (!file.exists(dat_path)) {
      if (download) {
        nhanes_mortality_download(cyc, quiet = FALSE)
      } else {
        cli::cli_abort(
          "LMF .dat file not found for {cyc}. \\
           Run {.fn nhanes_mortality_download} first, or set {.arg download = TRUE}."
        )
      }
    }

    df <- .nhanes_parse_lmf_dat(dat_path, cycle = cyc)

    saveRDS(df, rds_path)
    .nhanes_write_hash(rds_path)

    result[[cyc]] <- df
  }

  result
}

# -- Link -----------------------------------------------------------------------

#' Link mortality data onto an NHANES analytic dataset
#'
#' Performs a left join of the parsed LMF onto a data frame containing NHANES
#' participants, matched on the participant sequence number. Automatically
#' handles multiple cycles by row-binding the appropriate LMF files before
#' joining.
#'
#' Data from any source -- [nhanes_download()], `nhanesA`, or `nhanesdata` --
#' can be linked by supplying the appropriate column name arguments. For
#' example, `nhanesdata` stores the sequence number as `seqn` (integer) and
#' the cycle as `year` (integer start year, e.g. 1999); pass
#' `seqn_col = "seqn", cycle_col = "year"` and both are handled automatically.
#'
#' @param nhanes_data A data frame containing NHANES participants.
#' @param cycles Character vector of `"YYYY-YYYY"` cycle labels present in
#'   `nhanes_data`. Inferred from `cycle_col` when omitted.
#' @param keep_vars Character vector of LMF variables to retain. Defaults to
#'   all: `c("ELIGSTAT", "MORTSTAT", "UCOD_LEADING", "DIABETES", "HYPERTEN",
#'   "PERMTH_INT", "PERMTH_EXM")`.
#' @param download Logical. Download missing LMF files automatically? Default
#'   `TRUE`.
#' @param seqn_col Character. Name of the participant sequence-number column
#'   in `nhanes_data`. Default `"SEQN"` (nhanesR / CDC standard). Use
#'   `"seqn"` for `nhanesdata` output.
#' @param cycle_col Character. Name of the cycle column in `nhanes_data`.
#'   Default `"cycle"` (`"YYYY-YYYY"` labels). Use `"year"` for `nhanesdata`
#'   output, where the column contains integer start years (e.g. 1999, 2001).
#'
#' @return `nhanes_data` with LMF columns appended. Rows with no mortality
#'   record (SEQNs absent from the LMF) will have `NA` for all LMF columns;
#'   this should not occur for continuous NHANES 1999-2018.
#'
#' @seealso [nhanes_survival_prep()] to convert the linked data into a survival
#'   dataset; [nhanes_lmf_cycles()] for cycles with a public-use LMF;
#'   [nhanes_stack()] to row-bind multi-cycle data before linking.
#' @export
#' @examples
#' \donttest{
#' demo <- nhanes_download("DEMO", "2015-2016")
#' demo_mort <- nhanes_mortality_link(demo)
#' }
nhanes_mortality_link <- function(nhanes_data,
                                  cycles    = NULL,
                                  keep_vars = NULL,
                                  download  = TRUE,
                                  seqn_col  = "SEQN",
                                  cycle_col = "cycle") {
  if (!(seqn_col %in% names(nhanes_data))) {
    cli::cli_abort(
      "{.arg nhanes_data} must contain a sequence-number column. \\
       Looking for {.val {seqn_col}}; set {.arg seqn_col} if named differently."
    )
  }

  # Determine cycles, converting integer start-years if needed
  if (is.null(cycles)) {
    if (cycle_col %in% names(nhanes_data)) {
      cycles <- .nhanes_resolve_cycle_col(nhanes_data[[cycle_col]])
    } else {
      cli::cli_abort(
        "Supply {.arg cycles} or ensure {.arg nhanes_data} has a cycle column \\
         (set {.arg cycle_col} if it is not named {.val cycle})."
      )
    }
  }

  cycles <- .nhanes_validate_lmf_cycles(cycles)

  default_keep <- c("SEQN", "ELIGSTAT", "MORTSTAT", "UCOD_LEADING",
                    "DIABETES", "HYPERTEN", "PERMTH_INT", "PERMTH_EXM")
  keep_vars <- keep_vars %||% default_keep[-1L]

  # Parse and row-bind LMFs for all requested cycles
  lmf_list <- nhanes_mortality_parse(cycles, download = download)
  lmf_all  <- do.call(rbind, lapply(names(lmf_list), function(cyc) {
    df <- lmf_list[[cyc]]
    df$cycle <- cyc
    df
  }))

  # Keep only requested LMF variables (SEQN retained only for joining)
  avail    <- intersect(keep_vars, names(lmf_all))
  lmf_join <- lmf_all[, unique(c("SEQN", avail)), drop = FALSE]

  # Match on coerced-integer SEQN to handle character vs integer differences
  idx <- match(as.integer(nhanes_data[[seqn_col]]),
               as.integer(lmf_join$SEQN))

  lmf_cols <- setdiff(names(lmf_join), "SEQN")
  for (col in lmf_cols) {
    nhanes_data[[col]] <- lmf_join[[col]][idx]
  }

  # ELIGSTAT=NA means the participant has no LMF record at all.
  # ELIGSTAT=3 (insufficient identifying data) is expected to have MORTSTAT=NA
  # and should not trigger this warning.
  n_unmatched <- sum(is.na(nhanes_data$ELIGSTAT))
  if (n_unmatched > 0L) {
    cli::cli_warn(
      "{n_unmatched} NHANES participant{?s} had no matching LMF record. \\
       This is unexpected for continuous NHANES 1999-2018 and may indicate \\
       a cycle mismatch."
    )
  }

  nhanes_data
}

# -- Survival prep --------------------------------------------------------------

#' Prepare an NHANES-LMF dataset for survival analysis
#'
#' Takes a linked NHANES-mortality data frame (from [nhanes_mortality_link()])
#' and returns a dataset ready for use with [survival::Surv()], with:
#'
#' - Ineligible participants (`ELIGSTAT != 1`) removed with a warning
#' - A `time` column (in months or years) from the specified follow-up origin
#' - An `event` column (0/1) from `MORTSTAT`
#' - Optional cause-specific event indicator based on `UCOD_LEADING`
#' - Survey weight column renamed and validated
#'
#' @param data A data frame from [nhanes_mortality_link()].
#' @param origin Character. Follow-up origin: `"interview"` (uses `PERMTH_INT`)
#'   or `"exam"` (uses `PERMTH_EXM`). For analyses involving lab or examination
#'   data, use `"exam"`.
#' @param time_unit Character. `"months"` (default, as stored in LMF) or
#'   `"years"` (divides by 12).
#' @param cause Character or `NULL`. If supplied, creates a cause-specific event
#'   indicator `event_cause` that is 1 only when `UCOD_LEADING` matches this
#'   code and `MORTSTAT == 1`. See [nhanes_ucod_labels()] for valid codes.
#'   For all-cause mortality, leave `NULL`.
#' @param weight_var Character. Name of the survey weight column to carry
#'   through. The column is renamed to `survey_weight` in the output.
#'   If `NULL`, no weight is attached.
#' @param seqn_col Character. Name of the participant sequence-number column.
#'   Default `"SEQN"`. Use `"seqn"` for `nhanesdata` output.
#' @param cycle_col Character. Name of the cycle column. Default `"cycle"`.
#'   Use `"year"` for `nhanesdata` output.
#'
#' @return A data frame with additional columns:
#'   \describe{
#'     \item{time}{Follow-up time in specified units.}
#'     \item{event}{All-cause mortality indicator (0/1).}
#'     \item{event_cause}{Cause-specific indicator, if `cause` supplied.}
#'     \item{survey_weight}{Survey weight, renamed from `weight_var`.}
#'     \item{.eligstat_dropped}{Attribute recording how many rows were removed.}
#'   }
#'
#' @section Eligibility filtering:
#'   Participants with `ELIGSTAT != 1` are **automatically removed** with a
#'   warning. This includes those under 18 at time of survey (`ELIGSTAT == 2`)
#'   and those with insufficient identifying data for linkage (`ELIGSTAT == 3`).
#'   The number dropped is reported and attached as an attribute.
#'
#' @section Asymmetric follow-up across cycles:
#'   All public-use LMF files censor at December 31, 2019. Participants
#'   enrolled in later cycles (e.g. 2017-2018) have substantially shorter
#'   maximum follow-up than those enrolled in 1999-2000. This function warns
#'   when multiple cycles are detected, as this asymmetry must be accounted
#'   for in any pooled analysis.
#'
#' @section Survey weights:
#'   NHANES provides three families of survey weight, each correcting for a
#'   different sampling stage. Choosing the wrong weight produces biased
#'   point estimates and incorrect standard errors.
#'
#'   | Weight | When to use |
#'   |--------|-------------|
#'   | `WTINT2YR` | Interview-only data (questionnaires, no lab/exam) |
#'   | `WTMEC2YR` | Any examination or laboratory component |
#'   | `WTSAF2YR` | Analytes from the **fasting subsample** (triglycerides, glucose, insulin, calculated LDL) |
#'
#'   The fasting subsample weight (`WTSAF2YR`) is a *statistical* probability
#'   weight -- not a body-weight measurement -- that accounts for the additional
#'   random subsampling of participants asked to fast before their blood draw.
#'   Fasting participants are a minority of all MEC attendees; using `WTMEC2YR`
#'   for fasting analytes ignores this extra subsampling step and will give
#'   incorrect population estimates.
#'
#'   For pooled multi-cycle analyses divide the 2-year weight by the number of
#'   cycles pooled, or use the pre-computed 4-year weight `WTMEC4YR` where
#'   available. See the NHANES analytic guidelines for details.
#'
#' @section Perturbed variables:
#'   `PERMTH_INT`, `PERMTH_EXM`, and `UCOD_LEADING` contain synthetic values
#'   for select records (CDC data perturbation to reduce re-identification risk).
#'   `MORTSTAT` is not perturbed. Cause-specific analyses using `UCOD_LEADING`
#'   should be interpreted with this in mind.
#'
#' @seealso [nhanes_mortality_link()] which produces the input for this
#'   function; [nhanes_followup_summary()] to check follow-up time by cycle;
#'   [nhanes_ucod_labels()] for cause-of-death codes accepted by `cause`.
#' @export
#' @examples
#' \donttest{
#' demo_list <- nhanes_download("DEMO", c("2013-2014", "2015-2016"))
#' demo <- nhanes_stack(demo_list)
#' demo_mort <- nhanes_mortality_link(demo)
#'
#' # All-cause mortality, exam origin, MEC 2-year weight
#' surv_data <- nhanes_survival_prep(
#'   demo_mort,
#'   origin     = "exam",
#'   time_unit  = "years",
#'   weight_var = "WTMEC2YR"
#' )
#'
#' # Cause-specific: cardiovascular (code "001")
#' surv_data_cvd <- nhanes_survival_prep(
#'   demo_mort,
#'   origin = "exam",
#'   cause  = "001",
#'   weight_var = "WTMEC2YR"
#' )
#'
#' # Use with survival package
#' library(survival)
#' library(survey)
#' design <- svydesign(
#'   id      = ~SDMVPSU,
#'   strata  = ~SDMVSTRA,
#'   weights = ~survey_weight,
#'   nest    = TRUE,
#'   data    = surv_data
#' )
#' }
nhanes_survival_prep <- function(data,
                                 origin     = c("exam", "interview"),
                                 time_unit  = c("months", "years"),
                                 cause      = NULL,
                                 weight_var = NULL,
                                 seqn_col   = "SEQN",
                                 cycle_col  = "cycle") {
  origin    <- match.arg(origin)
  time_unit <- match.arg(time_unit)

  # -- Validate required columns ----------------------------------------------
  required <- c(seqn_col, "ELIGSTAT", "MORTSTAT",
                if (origin == "exam") "PERMTH_EXM" else "PERMTH_INT")
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0L) {
    cli::cli_abort(
      "Required column{?s} missing from {.arg data}: {.val {missing_cols}}.\\n
       Run {.fn nhanes_mortality_link} first."
    )
  }

  # -- Eligibility filtering (always applied, always warned) ------------------
  n_total      <- nrow(data)
  ineligible   <- !is.na(data$ELIGSTAT) & data$ELIGSTAT != 1L

  n_under18  <- sum(!is.na(data$ELIGSTAT) & data$ELIGSTAT == 2L)
  n_insuff   <- sum(!is.na(data$ELIGSTAT) & data$ELIGSTAT == 3L)
  n_dropped  <- sum(ineligible)

  if (n_dropped > 0L) {
    cli::cli_warn(c(
      "!" = "Removed {n_dropped} ineligible participant{?s} \\
             ({n_under18} under 18; {n_insuff} insufficient identifying data).",
      "i" = "These records cannot be used in survival analyses with the \\
             public-use LMF. {n_total - n_dropped} eligible participants remain.",
      "i" = "See the LMF documentation for eligibility criteria: \\
             {.url https://www.cdc.gov/nchs/data/datalinkage/public-use-linked-mortality-file-description.pdf}"
    ))
  }

  data <- data[!ineligible, , drop = FALSE]
  n_after_elig <- nrow(data)

  # -- Warn on asymmetric follow-up -------------------------------------------
  if (cycle_col %in% names(data)) {
    present_cycles <- unique(data[[cycle_col]])
    if (length(present_cycles) > 1L) {
      cli::cli_warn(c(
        "!" = "Data contains {length(present_cycles)} NHANES cycle{?s}: \\
               {.val {present_cycles}}.",
        "i" = "All cycles censor at December 31, 2019. Participants enrolled \\
               in later cycles have shorter maximum follow-up. Consider this \\
               asymmetry in any pooled or stacked analysis."
      ))
    }
  }

  # -- Build time variable ----------------------------------------------------
  time_col <- if (origin == "exam") "PERMTH_EXM" else "PERMTH_INT"
  data$time <- data[[time_col]]

  # Floor at 0.5 months before unit conversion: PERMTH_EXM = 0 means the
  # participant died in the same calendar month as their exam.
  # Cox regression requires time > 0.
  n_floored <- sum(!is.na(data$time) & data$time < 0.5)
  if (n_floored > 0L) {
    cli::cli_inform(
      "{n_floored} record{?s} {?has/have} follow-up time < 0.5 months \\
       (same-month exam and death). Time floored at 0.5 months."
    )
    data$time <- pmax(data$time, 0.5, na.rm = FALSE)
  }

  if (time_unit == "years") {
    data$time <- data$time / 12
  }

  # Flag records where time is NA but MORTSTAT is not (shouldn't happen)
  bad_time <- !is.na(data$MORTSTAT) & is.na(data$time)
  if (any(bad_time)) {
    cli::cli_warn(
      "{sum(bad_time)} record{?s} {?has/have} non-missing MORTSTAT but \\
       missing {time_col}. These will produce NA in the {.val time} column."
    )
  }

  # -- Event indicator --------------------------------------------------------
  data$event <- as.integer(data$MORTSTAT == 1L)

  # -- Cause-specific event ---------------------------------------------------
  if (!is.null(cause)) {
    valid_codes <- .ucod_labels$code
    if (!(cause %in% valid_codes)) {
      cli::cli_abort(c(
        "Invalid {.arg cause} code {.val {cause}}.",
        "i" = "Valid codes: {.val {valid_codes}}",
        "i" = "Use {.fn nhanes_ucod_labels} to see cause labels."
      ))
    }
    if (!("UCOD_LEADING" %in% names(data))) {
      cli::cli_abort(
        "{.val UCOD_LEADING} column not found. Ensure {.fn nhanes_mortality_link} \\
         was run with {.code keep_vars} including {.val 'UCOD_LEADING'}."
      )
    }
    data$event_cause <- as.integer(
      !is.na(data$UCOD_LEADING) &
        data$UCOD_LEADING == cause &
        data$event == 1L
    )
    ucod_label <- .ucod_labels$label[.ucod_labels$code == cause]
    attr(data$event_cause, "cause_label") <- ucod_label
    attr(data$event_cause, "ucod_code")   <- cause
    cli::cli_inform(
      "Cause-specific event {.val event_cause}: {ucod_label} (UCOD {cause}). \\
       {sum(data$event_cause, na.rm = TRUE)} event{?s} among eligible participants."
    )
  }

  # -- Survey weight ----------------------------------------------------------
  if (!is.null(weight_var)) {
    if (!(weight_var %in% names(data))) {
      cli::cli_abort(
        "Weight variable {.val {weight_var}} not found in {.arg data}. \\
         Available columns: {.val {names(data)}}"
      )
    }
    data$survey_weight <- data[[weight_var]]

    # Warn if fasting-subsample weight columns exist but a non-fasting weight
    # was chosen -- common when triglycerides or glucose are in the dataset.
    fasting_cols <- grep("^WTSAF", names(data), value = TRUE,
                         ignore.case = TRUE)
    if (length(fasting_cols) > 0L &&
        weight_var %in% c("WTMEC2YR", "WTINT2YR", "WTMEC4YR")) {
      cli::cli_warn(c(
        "!" = "Fasting-subsample weight column{?s} detected: \\
               {.val {fasting_cols}}.",
        "i" = "If your analysis includes fasting analytes (triglycerides, \\
               glucose, insulin, or Friedewald LDL), use {.val WTSAF2YR} \\
               rather than {.val {weight_var}}.",
        "i" = "{.val WTSAF2YR} is a statistical probability weight for the \\
               fasting subsample -- not a body-weight measurement."
      ))
    }

    # Warn if 2-year weight is used across multiple pooled cycles
    if (cycle_col %in% names(data)) {
      n_cycles <- length(unique(data[[cycle_col]]))
      if (n_cycles > 1L &&
          weight_var %in% c("WTMEC2YR", "WTINT2YR", "WTSAF2YR")) {
        cli::cli_warn(c(
          "!" = "Using 2-year weight {.val {weight_var}} with \\
                 {n_cycles} pooled cycles.",
          "i" = "Divide 2-year weights by the number of cycles pooled, \\
                 or use a pre-computed 4-year weight where available.",
          "i" = "See NHANES analytic guidelines: \\
                 {.url https://wwwn.cdc.gov/nchs/nhanes/analyticguidelines.aspx}"
        ))
      }
    }
  }

  # -- Attach metadata --------------------------------------------------------
  attr(data, ".nhanes_survival") <- list(
    origin        = origin,
    time_unit     = time_unit,
    time_col      = time_col,
    censor_date   = "2019-12-31",
    n_input       = n_total,
    n_elig_dropped = n_dropped,
    n_after_elig  = n_after_elig,
    weight_var    = weight_var,
    cause         = cause,
    created       = Sys.time()
  )

  data
}

# -- Helpers --------------------------------------------------------------------

#' Lookup table for UCOD_LEADING cause-of-death codes
#'
#' Returns the ICD-10 recode table used in the public-use LMF `UCOD_LEADING`
#' variable, including code, plain-language label, and ICD-10 chapter ranges.
#'
#' @return A data frame with columns `code`, `label`, `icd10_range`.
#' @seealso [nhanes_survival_prep()] where the `cause` argument accepts these
#'   codes.
#' @export
#' @examples
#' nhanes_ucod_labels()
nhanes_ucod_labels <- function() {
  .ucod_labels
}

#' Summarize mortality follow-up by cycle
#'
#' Diagnostic helper that reports median follow-up time, event rate, and
#' maximum possible follow-up per cycle. Useful for assessing the asymmetric
#' censoring problem when pooling cycles.
#'
#' @param data A data frame from [nhanes_survival_prep()].
#' @param cycle_col Character. Name of the cycle column. Default `"cycle"`.
#'   Use `"year"` for data originating from `nhanesdata`.
#' @return A data frame with one row per cycle.
#' @seealso [nhanes_survival_prep()] which produces the required input.
#' @export
#' @examples
#' \donttest{
#' demo <- nhanes_download("DEMO", "2015-2016")
#' linked <- nhanes_mortality_link(demo)
#' surv_data <- nhanes_survival_prep(linked, origin = "exam")
#' nhanes_followup_summary(surv_data)
#' }
#' @importFrom stats median
nhanes_followup_summary <- function(data, cycle_col = "cycle") {
  if (!(cycle_col %in% names(data))) {
    cli::cli_abort(
      "{.arg data} must have a cycle column \\
       (set {.arg cycle_col} if not named {.val cycle})."
    )
  }
  if (!("time" %in% names(data))) {
    cli::cli_abort(
      "{.arg data} must have a {.val time} column. \\
       Run {.fn nhanes_survival_prep} first."
    )
  }

  meta  <- attr(data, ".nhanes_survival")
  units <- if (!is.null(meta)) meta$time_unit else "months"

  result <- lapply(split(data, data[[cycle_col]]), function(df) {
    eligible <- df[!is.na(df$ELIGSTAT) & df$ELIGSTAT == 1L, ]
    data.frame(
      cycle           = unique(df[[cycle_col]]),
      n               = nrow(eligible),
      n_events        = sum(eligible$event == 1L, na.rm = TRUE),
      event_rate_pct  = round(
        100 * mean(eligible$event == 1L, na.rm = TRUE), 2
      ),
      median_followup = round(median(eligible$time, na.rm = TRUE), 1),
      max_followup    = round(max(eligible$time,    na.rm = TRUE), 1),
      time_unit       = units,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, result)
  rownames(out) <- NULL
  out
}

# -- Internal helpers -----------------------------------------------------------

# Convert a cycle column to "YYYY-YYYY" labels.
# Accepts either character labels ("1999-2000") or integer start years (1999).
.nhanes_resolve_cycle_col <- function(x) {
  if (is.numeric(x) || is.integer(x)) {
    registered  <- c(.nhanes_cycles$cycle, .nhanes_iii$cycle)
    start_years <- as.integer(sub("-.*", "", registered))
    vals <- unique(x)
    out  <- character(length(vals))
    for (i in seq_along(vals)) {
      idx <- which(start_years == as.integer(vals[i]))
      out[i] <- if (length(idx) == 1L) registered[idx] else
                  paste0(vals[i], "-", vals[i] + 1L)
    }
    out
  } else {
    as.character(unique(x))
  }
}

.nhanes_lmf_rds_path <- function(cycle) {
  dir  <- .nhanes_cache_subdir("mortality", "parsed")
  safe <- gsub("[^A-Za-z0-9]", "_", cycle)
  file.path(dir, paste0("lmf_", safe, ".rds"))
}

.nhanes_lmf_dat_path <- function(cycle) {
  reg      <- .lmf_registry[.lmf_registry$cycle == cycle, ]
  dest_dir <- .nhanes_cache_subdir("mortality", "dat")
  file.path(dest_dir, reg$filename)
}

.nhanes_validate_lmf_cycles <- function(cycles) {
  valid <- nhanes_lmf_cycles()
  bad   <- setdiff(cycles, valid)
  if (length(bad) > 0L) {
    cli::cli_abort(
      "Cycle{?s} not available in the public-use LMF: {.val {bad}}.\\n
       Valid cycles: {.val {valid}}"
    )
  }
  cycles
}

#' Parse a single LMF .dat file using the internal column specification
#' @keywords internal
.nhanes_parse_lmf_dat <- function(dat_path, cycle) {
  spec <- .lmf_colspec

  # Build readr::read_fwf column positions
  col_pos <- readr::fwf_positions(
    start = spec$col_start,
    end   = spec$col_end,
    col_names = spec$variable
  )

  # Column types string for readr
  col_types <- paste(spec$col_type, collapse = "")

  df <- readr::read_fwf(
    dat_path,
    col_positions = col_pos,
    col_types     = col_types,
    na            = c(".", " ", ""),
    show_col_types = FALSE
  )

  # Attach variable labels (haven/labelled compatible)
  for (i in seq_len(nrow(spec))) {
    v <- spec$variable[i]
    if (v %in% names(df)) {
      attr(df[[v]], "label") <- spec$label[i]
      if (spec$perturbed[i]) {
        attr(df[[v]], "perturbed") <- TRUE
      }
    }
  }

  # Attach cycle
  df$cycle <- cycle
  attr(df, "lmf_vintage")   <- "2019"
  attr(df, "censor_date")   <- "2019-12-31"
  attr(df, "source_file")   <- basename(dat_path)

  df
}
