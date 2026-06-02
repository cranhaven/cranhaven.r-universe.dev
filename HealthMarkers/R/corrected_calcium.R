#' Albumin-Corrected Calcium
#'
#' Calculates the albumin-adjusted (corrected) serum calcium level, accounting for hypoalbuminemia,
#' using the Payne formula.
#'
#' @details
#' Payne formula (conventional units): Corrected Ca (mg/dL) = measured Ca (mg/dL) + 0.8 * (4.0 - albumin (g/dL)).
#' If inputs appear to be in SI units (calcium mmol/L, albumin g/L), they are converted to mg/dL and g/dL
#' (using 1 mmol/L ~= 4 mg/dL; 1 g/L = 0.1 g/dL) for the correction and converted back to mmol/L for output.
#'
#' @param data A data.frame or tibble containing serum calcium and albumin.
#' @param col_map Named list with `calcium` and `albumin` indicating column names.
#' @param verbose Logical; if TRUE (default), emits progress via hm_inform().
#' @param na_action One of c("keep","omit","error","ignore","warn").
#' @param units One of c("auto","conventional","si"). "auto" attempts unit detection.
#'
#' @return A tibble with one column: corrected_calcium (numeric, in mg/dL for
#'   conventional input or mmol/L for SI / auto-SI input).
#' @references \insertRef{payne1973}{HealthMarkers}
#'
#' @examples
#' df <- data.frame(Ca = c(2.3, 2.5, 2.1), Alb = c(38, 42, 30))
#' corrected_calcium(df)
#' @export
corrected_calcium <- function(
  data,
  col_map = NULL,
  units = c("auto", "conventional", "si"),
  na_action = c("keep", "omit", "error", "ignore", "warn"),
  verbose = TRUE
) {
  data_name <- (function(.e) if (is.symbol(.e)) as.character(.e) else "data")(substitute(data))
  fn_name <- "corrected_calcium"
  .hm_log_input(data, data_name, fn_name, verbose)
  units         <- match.arg(units)
  .na <- .hm_normalize_na_action(match.arg(na_action))
  na_action_raw <- .na$na_action_raw
  na_action_eff <- .na$na_action_eff
  id_col <- .hm_detect_id_col(data)

  ## --- Basic validation -----------------------------------------------------
  if (!is.data.frame(data)) {
    rlang::abort(
      "corrected_calcium(): `data` must be a data.frame or tibble.",
      class = "healthmarkers_calcium_error_data_type"
    )
  }
  if (!is.null(col_map) && !is.list(col_map)) {
    rlang::abort(
      "corrected_calcium(): `col_map` must be a named list.",
      class = "healthmarkers_calcium_error_colmap_type"
    )
  }
  cm      <- .hm_build_col_map(data, col_map, c("calcium","albumin"), fn = "corrected_calcium")
  data    <- cm$data
  col_map <- cm$col_map

  ca_key  <- intersect(names(col_map), c("calcium", "ca"))
  alb_key <- intersect(names(col_map), c("albumin", "alb"))
  if (length(ca_key) == 0 || length(alb_key) == 0) {
    rlang::abort(
      "corrected_calcium(): missing col_map entries for calcium and/or albumin.",
      class = "healthmarkers_calcium_error_missing_map"
    )
  }

  ca_col  <- as.character(col_map[[ca_key[1]]])
  alb_col <- as.character(col_map[[alb_key[1]]])

  if (!nzchar(ca_col) || !nzchar(alb_col)) {
    rlang::abort(
      "corrected_calcium(): empty mapping values supplied.",
      class = "healthmarkers_calcium_error_bad_map_values"
    )
  }

  missing_cols <- setdiff(c(ca_col, alb_col), names(data))
  if (length(missing_cols) > 0L) {
    rlang::abort(
      paste0(
        "corrected_calcium(): missing required columns in data: ",
        paste(missing_cols, collapse = ", ")
      ),
      class = "healthmarkers_calcium_error_missing_columns"
    )
  }

  .hm_log_cols(cm, col_map, fn_name, verbose)
  if (isTRUE(verbose))
    hm_inform(sprintf("%s(): computing markers:\n  corrected_calcium  [Payne formula: Ca + 0.8 * (4.0 - Alb)]", fn_name), level = "inform")

  ## --- Coercion to numeric --------------------------------------------------
  coerce_num <- function(x, nm) {
    if (!is.numeric(x)) {
      old <- x
      suppressWarnings(xn <- as.numeric(old))
      introduced <- sum(is.na(xn) & !is.na(old))
      if (introduced > 0L) {
        rlang::warn(
          sprintf("Column '%s' coerced to numeric; NAs introduced: %d", nm, introduced),
          class = "healthmarkers_calcium_warn_na_coercion"
        )
      }
      x <- xn
    }
    x[!is.finite(x)] <- NA_real_
    x
  }

  ca_raw  <- coerce_num(data[[ca_col]],  ca_col)
  alb_raw <- coerce_num(data[[alb_col]], alb_col)

  ## --- NA handling ----------------------------------------------------------
  row_missing <- is.na(ca_raw) | is.na(alb_raw)

  if (na_action_raw == "warn" && any(row_missing)) {
    rlang::warn(
      "Missing calcium or albumin values; corrected_calcium will be NA for those rows.",
      class = "healthmarkers_calcium_warn_missing_inputs"
    )
  }

  if (na_action_eff == "error" && any(row_missing)) {
    rlang::abort(
      "corrected_calcium(): required inputs contain missing values (na_action='error').",
      class = "healthmarkers_calcium_error_missing_values"
    )
  }

  keep_idx <- if (na_action_eff == "omit") !row_missing else rep(TRUE, length(row_missing))
  d_ca  <- ca_raw[keep_idx]
  d_alb <- alb_raw[keep_idx]

  ## --- Unit resolution / auto detection ------------------------------------
  med_ca  <- stats::median(d_ca,  na.rm = TRUE)
  med_alb <- stats::median(d_alb, na.rm = TRUE)

  if (units == "conventional") {
    ca_unit  <- "mg/dL"
    alb_unit <- "g/dL"
  } else if (units == "si") {
    ca_unit  <- "mmol/L"
    alb_unit <- "g/L"
  } else { # units == "auto"
    ca_unit  <- if (is.finite(med_ca)  && med_ca  <= 4)  "mmol/L" else "mg/dL"
    alb_unit <- if (is.finite(med_alb) && med_alb > 20) "g/L"    else "g/dL"
    si_inferred <- (ca_unit == "mmol/L" || alb_unit == "g/L")
    if (si_inferred) {
      rlang::warn(
        sprintf(
          "corrected_calcium(): auto-detected SI units Ca=%s, Alb=%s; output standardized to mmol/L.",
          ca_unit, alb_unit
        ),
        class = "healthmarkers_calcium_warn_unit_assumption"
      )
    }
  }

  si_inferred <- (ca_unit == "mmol/L" || alb_unit == "g/L")

  ## --- Convert to working conventional units (mg/dL, g/dL) -----------------
  ca_work_mgdl <- if (ca_unit == "mmol/L") d_ca * 4.0 else d_ca
  alb_work_gdl <- if (alb_unit == "g/L")  d_alb / 10.0 else d_alb

  ## --- Payne correction in mg/dL -------------------------------------------
  corr_mgdl <- ca_work_mgdl + 0.8 * (4.0 - alb_work_gdl)

  ## --- Domain warnings (explicit conventional only, no extreme scan) -------
  if (units == "conventional" && !si_inferred) {
    if (any(is.finite(alb_work_gdl) & (alb_work_gdl < 2 | alb_work_gdl > 5))) {
      rlang::warn(
        "corrected_calcium(): albumin outside typical 2-5 g/dL range; correction accuracy may be affected.",
        class = "healthmarkers_calcium_warn_albumin_range"
      )
    }
    if (any(is.finite(corr_mgdl) & (corr_mgdl < 5 | corr_mgdl > 15))) {
      rlang::warn(
        "corrected_calcium(): corrected calcium outside typical 5-15 mg/dL range detected.",
        class = "healthmarkers_calcium_warn_corrected_range"
      )
    }
  }

  ## --- Output scaling -------------------------------------------------------
  # SI or auto-SI => mmol/L
  corr_out <- if (units == "si" || (units == "auto" && si_inferred)) corr_mgdl / 4.0 else corr_mgdl

  hm_inform(level = "debug", msg = "corrected_calcium(): computing result")

  result <- tibble::tibble(corrected_calcium = corr_out)

  if (na_action_eff != "omit") {
    padded <- tibble::tibble(corrected_calcium = rep(NA_real_, length(ca_raw)))
    padded$corrected_calcium[keep_idx] <- result$corrected_calcium
    result <- padded
  }

  if (!is.null(id_col)) {
    id_vec <- if (na_action_eff == "omit") data[[id_col]][keep_idx] else data[[id_col]]
    result[[id_col]] <- id_vec
    result <- result[, c(id_col, setdiff(names(result), id_col)), drop = FALSE]
    result <- tibble::as_tibble(result)
  }
  if (isTRUE(verbose)) { hm_inform(hm_result_summary(result, fn_name), level = "inform") }

  return(result)
}

