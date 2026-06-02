#' Appendicular Lean Mass to BMI Index
#'
#' Calculates the ratio of appendicular lean mass (ALM) to body mass index (BMI),
#' and flags low muscle mass based on FNIH Sarcopenia Project cut-points.
#'
#' @details
#' ALM/BMI reflects muscle mass relative to body size. FNIH cut-points:
#' - Men:    ALM/BMI < 0.789
#' - Women:  ALM/BMI < 0.512
#'
#' ALM should be in kilograms and BMI in kg/m^2.
#'
#' @param data A data.frame or tibble with ALM, BMI, and sex columns.
#' @param col_map Named list with:
#'   - alm: appendicular lean mass column name (kg)
#'   - bmi: body mass index column name (kg/m^2)
#'   - sex: sex column name ("Male"/"Female" or m/f; case-insensitive)
#' @param verbose Logical; if TRUE (default), emits progress messages.
#' @param na_action One of c("keep","omit","error","ignore","warn").
#'
#' @return A tibble with:
#'   - alm_bmi_ratio   (numeric)
#'   - low_muscle_mass (logical; TRUE if below sex-specific cut-point;
#'                      NA if sex unknown or ratio NA)
#' @references \insertRef{mclean2014fnih}{HealthMarkers}
#'
#' @examples
#' df <- data.frame(ALM_kg = c(7.2, 5.8, 6.5), BMI = c(24, 28, 22),
#'                  Sex = c("male", "female", "male"))
#' alm_bmi_index(df)
#' @export
alm_bmi_index <- function(
  data,
  col_map = NULL,
  na_action = c("keep","omit","error","ignore","warn"),
  verbose = TRUE
) {
  data_name <- (function(.e) if (is.symbol(.e)) as.character(.e) else "data")(substitute(data))
  fn_name <- "alm_bmi_index"
  .hm_log_input(data, data_name, fn_name, verbose)
  .na <- .hm_normalize_na_action(match.arg(na_action))
  na_action_raw <- .na$na_action_raw
  na_action_eff <- .na$na_action_eff
  id_col <- .hm_detect_id_col(data)

  # --- validate data / mapping ------------------------------------------------
  if (!is.data.frame(data)) {
    rlang::abort(
      "alm_bmi_index(): `data` must be a data.frame or tibble.",
      class = "healthmarkers_alm_bmi_error_data_type"
    )
  }

  req <- c("alm","bmi","sex")
  if (!is.null(col_map) && !is.list(col_map)) {
    rlang::abort(
      "alm_bmi_index(): `col_map` must be a named list.",
      class = "healthmarkers_alm_bmi_error_colmap_type"
    )
  }
  cm      <- .hm_build_col_map(data, col_map, req, fn = "alm_bmi_index")
  data    <- cm$data
  col_map <- cm$col_map

  missing_keys <- setdiff(req, names(col_map))
  if (length(missing_keys)) {
    rlang::abort(
      paste0("alm_bmi_index(): missing col_map entries for: ",
             paste(missing_keys, collapse = ", ")),
      class = "healthmarkers_alm_bmi_error_missing_map"
    )
  }

  mapped <- unname(unlist(col_map[req], use.names = FALSE))
  if (any(!nzchar(mapped))) {
    bad <- req[!nzchar(unname(unlist(col_map[req])))]
    rlang::abort(
      paste0("alm_bmi_index(): missing col_map entries for: ",
             paste(bad, collapse = ", ")),
      class = "healthmarkers_alm_bmi_error_bad_map_values"
    )
  }

  missing_cols <- setdiff(mapped, names(data))
  if (length(missing_cols)) {
    rlang::abort(
      paste0("alm_bmi_index(): missing required columns in data: ",
             paste(missing_cols, collapse = ", ")),
      class = "healthmarkers_alm_bmi_error_missing_columns"
    )
  }

  # --- verbose messages -------------------------------------------------------
  .hm_log_cols(cm, col_map, fn_name, verbose)
  if (isTRUE(verbose))
    hm_inform(sprintf("%s(): computing markers:\n  alm_bmi_ratio    [ALM / BMI]\n  low_muscle_mass  [ratio < sex-specific FNIH cut-point]", fn_name), level = "inform")

  # --- coerce ALM/BMI numeric; sex as character ------------------------------
  num_cols <- unname(unlist(col_map[c("alm","bmi")]))
  for (cn in num_cols) {
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(new <- as.numeric(old))
      intro <- sum(is.na(new) & !is.na(old))
      if (intro > 0L) {
        rlang::warn(
          sprintf("Column '%s' coerced to numeric; NAs introduced: %d", cn, intro),
          class = "healthmarkers_alm_bmi_warn_na_coercion"
        )
      }
      data[[cn]] <- new
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  alm <- data[[col_map$alm]]
  bmi <- data[[col_map$bmi]]
  sex_raw <- data[[col_map$sex]]

  # --- NA policy --------------------------------------------------------------
  any_na_req <- is.na(alm) | is.na(bmi) | is.na(sex_raw)

  if (na_action_raw == "warn" && any(any_na_req)) {
    rlang::warn(
      "Missing ALM, BMI, or sex; outputs will be NA for those entries.",
      class = "healthmarkers_alm_bmi_warn_missing_inputs"
    )
  }

  if (na_action_eff == "error" && any(any_na_req)) {
    rlang::abort(
      "alm_bmi_index(): required inputs contain missing values (na_action='error').",
      class = "healthmarkers_alm_bmi_error_missing_values"
    )
  }

  keep <- if (na_action_eff == "omit") !any_na_req else rep(TRUE, length(alm))

  d_alm <- alm[keep]
  d_bmi <- bmi[keep]
  d_sex_raw <- sex_raw[keep]

  # --- normalize sex ----------------------------------------------------------
  sex_chr <- tolower(as.character(d_sex_raw))
  sex_norm <- ifelse(substr(sex_chr, 1, 1) %in% c("m","1"), "male",
               ifelse(substr(sex_chr, 1, 1) %in% c("f","0"), "female", NA_character_))

  if (any(is.na(sex_norm) & !is.na(d_sex_raw))) {
    rlang::warn(
      "alm_bmi_index(): unrecognized sex entries; expected male/female.",
      class = "healthmarkers_alm_bmi_warn_sex_unknown"
    )
  }

  # --- domain warnings --------------------------------------------------------
  if (any(is.finite(d_bmi) & (d_bmi < 10 | d_bmi > 60))) {
    rlang::warn(
      "alm_bmi_index(): BMI outside realistic range (10-60 kg/m^2) detected.",
      class = "healthmarkers_alm_bmi_warn_bmi_range"
    )
  }
  if (any(is.finite(d_alm) & (d_alm < 5 | d_alm > 40))) {
    rlang::warn(
      "alm_bmi_index(): ALM outside plausible range (5-40 kg) detected.",
      class = "healthmarkers_alm_bmi_warn_alm_range"
    )
  }
  if (any(is.finite(d_bmi) & d_bmi <= 0)) {
    rlang::warn(
      "alm_bmi_index(): nonpositive BMI encountered; ratio set to NA for those rows.",
      class = "healthmarkers_alm_bmi_warn_bmi_nonpositive"
    )
  }

  # --- compute ratio and low-muscle flag -------------------------------------
  hm_inform(level = "debug", msg = "alm_bmi_index(): computing")

  ratio <- d_alm / d_bmi
  ratio[!is.finite(ratio)] <- NA_real_

  thr <- ifelse(sex_norm == "male",   0.789,
         ifelse(sex_norm == "female", 0.512, NA_real_))

  low_flag <- ifelse(is.na(ratio) | is.na(thr), NA, ratio < thr)

  out_core <- tibble::tibble(
    alm_bmi_ratio   = as.numeric(ratio),
    low_muscle_mass = as.logical(low_flag)
  )

  # --- pad back if not omitting ----------------------------------------------
  if (na_action_eff != "omit") {
    out <- tibble::tibble(
      alm_bmi_ratio   = rep(NA_real_, length(alm)),
      low_muscle_mass = rep(NA, length(alm))
    )
    out[keep, ] <- out_core
  } else {
    out <- out_core
  }

  if (!is.null(id_col)) {
    id_vec <- if (na_action_eff == "omit") data[[id_col]][keep] else data[[id_col]]
    out[[id_col]] <- id_vec
    out <- out[, c(id_col, setdiff(names(out), id_col)), drop = FALSE]
    out <- tibble::as_tibble(out)
  }
  if (isTRUE(verbose)) { hm_inform(hm_result_summary(out, fn_name), level = "inform") }

  out
}


