#' FRAX 10-Year Fracture Risk Score (simplified placeholder)
#'
#' Estimates the 10-year probabilities of major osteoporotic and hip fracture
#' using a simplified, non-validated approximation based on FRAX risk factors.
#' This is for development/demo only and does not implement the proprietary FRAX algorithm.
#'
#' @details
#' Important: this function is an educational placeholder and must not be used for
#' clinical decision-making, patient counseling, or guideline-based treatment selection.
#'
#' @param data A data frame or tibble with inputs.
#' @param col_map Named list mapping required and optional inputs:
#'   - Required: age, sex
#'   - Optional binary risk factors: prior_fracture, parent_fracture, steroids,
#'     rheumatoid, secondary_op, smoker, alcohol
#'   - Optional: bmd (T-score; if present and in (-5,0], used to adjust risk)
#' @param country optional country/region code for FRAX calibration (accepted, currently unused).
#' @param na_action One of c("keep","omit","error","ignore","warn").
#' @param verbose Logical; if TRUE, emits progress via rlang::inform.
#' @return Tibble with frax_major_percent and frax_hip_percent.
#'
#' @examples
#' df <- data.frame(Age = c(65, 72, 58), Sex = c("female", "female", "male"))
#' frax_score(df)
#' @export
frax_score <- function(
  data,
  col_map = NULL,
  na_action = c("keep","omit","error","ignore","warn"),
  country = NULL,
  verbose = TRUE
) {
  # Accept but do not use; normalize without emitting warnings
  if (!is.null(country)) {
    country <- as.character(country)[1L]
  }

  data_name <- (function(.e) if (is.symbol(.e)) as.character(.e) else "data")(substitute(data))
  fn_name <- "frax_score"
  .hm_log_input(data, data_name, fn_name, verbose)
  id_col  <- .hm_detect_id_col(data)
  .na <- .hm_normalize_na_action(match.arg(na_action))
  na_action_raw <- .na$na_action_raw
  na_action_eff <- .na$na_action_eff

  # Validate inputs
  if (!is.data.frame(data)) {
    rlang::abort("frax_score(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_frax_error_data_type")
  }
  req <- c("age","sex")
  all_frax_keys <- c("age","sex","bmd_t","prior_fracture","parent_fracture","steroids","smoker","alcohol")
  cm      <- .hm_build_col_map(data, col_map, all_frax_keys, fn = "frax_score")
  data    <- cm$data
  col_map <- cm$col_map
  if (!is.list(col_map) || is.null(names(col_map))) {
    rlang::abort("frax_score(): `col_map` must be a named list.",
                 class = "healthmarkers_frax_error_colmap_type")
  }
  missing_keys <- setdiff(req, names(col_map))
  if (length(missing_keys)) {
    rlang::abort(paste0("frax_score(): missing col_map entries for: ",
                        paste(missing_keys, collapse = ", ")),
                 class = "healthmarkers_frax_error_missing_map")
  }
  mapped_req <- unname(unlist(col_map[req], use.names = FALSE))
  if (any(!nzchar(mapped_req))) {
    bad <- req[!nzchar(unname(unlist(col_map[req])))]
    rlang::abort(paste0("frax_score(): missing col_map entries for: ",
                        paste(bad, collapse = ", ")),
                 class = "healthmarkers_frax_error_bad_map_values")
  }
  missing_cols <- setdiff(mapped_req, names(data))
  if (length(missing_cols)) {
    rlang::abort(paste0("frax_score(): missing required columns in data: ",
                        paste(missing_cols, collapse = ", ")),
                 class = "healthmarkers_frax_error_missing_columns")
  }

  .hm_log_cols(cm, col_map, fn_name, verbose)
  if (isTRUE(verbose))
    hm_inform(sprintf("%s(): computing markers:\n  frax_major_percent [age, sex, risk factors, bmd]\n  frax_hip_percent [age, sex, risk factors, bmd]", fn_name), level = "inform")

  # Build list of used columns that are present in data
  opt_keys <- c("prior_fracture","parent_fracture","steroids","rheumatoid",
                "secondary_op","smoker","alcohol","bmd")
  mapped_all <- unlist(col_map[c(req, opt_keys)], use.names = TRUE)
  mapped_all <- mapped_all[!is.na(mapped_all) & nzchar(mapped_all)]
  used_cols <- intersect(unname(mapped_all), names(data))

  # Coerce numeric-like where appropriate; keep sex as character/factor
  numeric_like_keys <- c("age","prior_fracture","parent_fracture","steroids",
                         "rheumatoid","secondary_op","smoker","alcohol","bmd")
  numeric_like_cols <- unname(unlist(col_map[intersect(names(col_map), numeric_like_keys)]))
  numeric_like_cols <- numeric_like_cols[!is.na(numeric_like_cols) & nzchar(numeric_like_cols)]
  numeric_like_cols <- intersect(numeric_like_cols, used_cols)

  for (cn in numeric_like_cols) {
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(new <- as.numeric(old))
      intro <- sum(is.na(new) & !is.na(old))
      if (intro > 0L) {
        rlang::warn(sprintf("Column '%s' coerced to numeric; NAs introduced: %d", cn, intro),
                    class = "healthmarkers_frax_warn_na_coercion")
      }
      data[[cn]] <- new
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  age <- data[[col_map$age]]
  sex <- data[[col_map$sex]]
  # Map sex
  sex_chr <- tolower(as.character(sex))
  sex_norm <- ifelse(substr(sex_chr, 1, 1) %in% c("m","1"), "male",
                ifelse(substr(sex_chr, 1, 1) %in% c("f","0"), "female", NA_character_))
  if (any(is.na(sex_norm))) {
    rlang::warn("frax_score(): unknown sex codes encountered; related rows set to NA.",
                class = "healthmarkers_frax_warn_unknown_sex")
  }
  # Optional risk factors (numeric 0/1)
  as01 <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.logical(x)) return(as.integer(x))
    if (is.numeric(x)) return(as.integer(x > 0))
    xv <- suppressWarnings(as.numeric(x))
    as.integer(ifelse(is.na(xv), NA, xv > 0))
  }
  getn <- function(key) {
    nm <- col_map[[key]]
    if (!is.null(nm) && nzchar(nm) && nm %in% names(data)) data[[nm]] else NULL
  }
  prior_fx <- as01(getn("prior_fracture")); parent_fx <- as01(getn("parent_fracture"))
  steroids <- as01(getn("steroids")); rheumatoid <- as01(getn("rheumatoid"))
  secondary <- as01(getn("secondary_op")); smoker <- as01(getn("smoker")); alcohol <- as01(getn("alcohol"))
  bmd <- getn("bmd")

  # NA policy on required inputs
  any_na_req <- is.na(age) | is.na(sex_norm)
  if (na_action_raw == "warn" && any(any_na_req)) {
    rlang::warn("Missing age or sex values; FRAX outputs will be NA for those entries.",
                class = "healthmarkers_frax_warn_missing_inputs")
  }
  if (na_action_eff == "error" && any(any_na_req)) {
    rlang::abort("frax_score(): required inputs contain missing values (na_action='error').",
                 class = "healthmarkers_frax_error_missing_values")
  }
  keep <- if (na_action_eff == "omit") !any_na_req else rep(TRUE, length(age))

  # Subset for computation
  d_age <- age[keep]
  d_sex <- sex_norm[keep]
  d_prior <- if (!is.null(prior_fx)) prior_fx[keep] else rep(0L, sum(keep))
  d_parent <- if (!is.null(parent_fx)) parent_fx[keep] else rep(0L, sum(keep))
  d_ster <- if (!is.null(steroids)) steroids[keep] else rep(0L, sum(keep))
  d_rheu <- if (!is.null(rheumatoid)) rheumatoid[keep] else rep(0L, sum(keep))
  d_sec  <- if (!is.null(secondary)) secondary[keep] else rep(0L, sum(keep))
  d_smok <- if (!is.null(smoker)) smoker[keep] else rep(0L, sum(keep))
  d_alc  <- if (!is.null(alcohol)) alcohol[keep] else rep(0L, sum(keep))
  d_bmd  <- if (!is.null(bmd)) bmd[keep] else rep(NA_real_, sum(keep))

  # Domain warnings
  if (any(is.finite(d_age) & (d_age < 40 | d_age > 90))) {
    rlang::warn(
      "frax_score(): age outside typical FRAX range (40-90 years) detected.",
      class = "healthmarkers_frax_warn_age_range"
    )
  }
  if (!is.null(d_bmd) && any(is.finite(d_bmd) & (d_bmd < -6 | d_bmd > 2))) {
    hm_inform(level = "debug", msg = "frax_score(): BMD T-scores outside plausible range (-6 to +2) detected.")
  }

  hm_inform("frax_score(): computing risk", level = "debug")

  n <- length(d_age)
  risk_major <- rep(NA_real_, n)

  # Baseline by sex and age (placeholder logic)
  is_female <- !is.na(d_sex) & d_sex == "female"
  is_male   <- !is.na(d_sex) & d_sex == "male"

  base <- rep(NA_real_, n)
  base[is_female] <- 2 + 0.3 * pmax(d_age[is_female] - 50, 0)
  base[is_male]   <- 1 + 0.2 * pmax(d_age[is_male] - 50, 0)

  # Add risk-factor contributions (binary 0/1; NA contributes 0)
  nz <- function(x) { x[is.na(x)] <- 0L; x }
  contrib <- 5 * nz(d_prior) + 3 * nz(d_parent) + 4 * nz(d_ster) +
            2 * nz(d_rheu) + 2 * nz(d_sec) + 2 * nz(d_smok) + 2 * nz(d_alc)

  risk_major <- base + contrib

  # BMD as T-score in [-5, 0] reduces/increases risk
  if (!all(is.na(d_bmd))) {
    tscore <- ifelse(is.finite(d_bmd) & d_bmd <= 0 & d_bmd >= -5, d_bmd, NA_real_)
    adj <- pmax(-tscore - 1, 0) * 2
    adj[!is.finite(adj)] <- 0
    risk_major <- risk_major + adj
  }

  # Cap major risk and set to NA where sex is unknown
  risk_major[is.na(d_sex)] <- NA_real_
  risk_major <- pmin(risk_major, 95)

  # Hip fracture risk as a fraction of major, increasing with age
  frac <- pmin(0.4 + 0.01 * pmax(d_age - 50, 0), 0.8)
  risk_hip <- pmin(risk_major * frac, 95)

  # Round to one decimal place
  risk_major <- round(risk_major, 1)
  risk_hip <- round(risk_hip, 1)

  out <- tibble::tibble(
    frax_major_percent = risk_major,
    frax_hip_percent   = risk_hip,
    frax_sex_norm      = d_sex,
    frax_age_used      = d_age,
    frax_bmd_tscore    = d_bmd
  )

  if (na_action_eff != "omit") {
    res <- tibble::tibble(
      frax_major_percent = rep(NA_real_, nrow(data)),
      frax_hip_percent   = rep(NA_real_, nrow(data)),
      frax_sex_norm      = rep(NA_character_, nrow(data)),
      frax_age_used      = rep(NA_real_, nrow(data)),
      frax_bmd_tscore    = rep(NA_real_, nrow(data))
    )
    res[keep, ] <- out
    out <- res
  }

  if (!is.null(id_col)) {
    id_vec <- if (na_action_eff == "omit") data[[id_col]][keep] else data[[id_col]]
    out[[id_col]] <- id_vec
    out <- out[, c(id_col, setdiff(names(out), id_col)), drop = FALSE]
    out <- tibble::as_tibble(out)
  }
  if (isTRUE(verbose)) {
    hm_inform(hm_result_summary(out, fn_name), level = "inform")
  }

  out
}

