#' Vitamin D Status Category
#'
#' Categorizes vitamin D status based on serum 25-hydroxyvitamin D (25(OH)D) levels.
#'
#' @details
#' Serum 25(OH)D is the standard biomarker for vitamin D status. This function classifies
#' levels (assumed in ng/mL) into categories:
#' - Deficient (< 20 ng/mL)
#' - Insufficient (20-29 ng/mL)
#' - Sufficient (>= 30 ng/mL)
#'
#' Note: Ensure input units are ng/mL. If values appear extremely high (e.g., median > 150),
#' they might be in nmol/L (divide by 2.5 to convert to ng/mL).
#'
#' @param data A data.frame or tibble with a 25-hydroxyvitamin D concentration column.
#' @param col_map A named list with `vitamin_d` giving the column name in `data` for 25(OH)D.
#' @param na_action One of c("keep","omit","error","ignore","warn").
#'   - keep/ignore: compute and propagate NA
#'   - omit: drop rows with NA in required input
#'   - error: abort if required input contains NA
#'   - warn: like keep, but emit missingness warnings
#' @param verbose Logical; if TRUE, emits progress via rlang::inform.
#' @return A tibble with one column: vitamin_d_status (ordered factor with levels "Deficient","Insufficient","Sufficient").
#'
#' @examples
#' df <- data.frame(VitD = c(18, 45, 72))
#' vitamin_d_status(df)
#'
#' @references
#' \insertRef{iomm2011calciumvitd}{HealthMarkers}
#' \insertRef{holick2011vitd}{HealthMarkers}
#'
#' @export
vitamin_d_status <- function(
  data,
  col_map = NULL,
  na_action = c("keep","omit","error","ignore","warn"),
  verbose = TRUE
) {
  .na <- .hm_normalize_na_action(match.arg(na_action))
  na_action_raw <- .na$na_action_raw
  na_action_eff <- .na$na_action_eff

  # Validate data
  if (!is.data.frame(data)) {
    rlang::abort("vitamin_d_status(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_vitd_error_data_type")
  }
  data_name <- (function(.e) if (is.symbol(.e)) as.character(.e) else "data")(substitute(data))
  fn_name <- "vitamin_d_status"
  .hm_log_input(data, data_name, fn_name, verbose)
  id_col  <- .hm_detect_id_col(data)
  # Validate col_map: list vs. missing keys
  if (!is.list(col_map) && !is.null(col_map)) {
    rlang::abort("vitamin_d_status(): `col_map` must be a named list.",
                 class = "healthmarkers_vitd_error_colmap_type")
  }

  cm      <- .hm_build_col_map(data, col_map, c("vitd","vitamin_d"), fn = "vitamin_d_status")
  data    <- cm$data
  col_map <- cm$col_map
  if (is.null(col_map)) col_map <- list()

  if (!is.list(col_map)) {
    rlang::abort("vitamin_d_status(): `col_map` must be a named list.",
                 class = "healthmarkers_vitd_error_colmap_type")
  }

  # Accept either 'vitd' or 'vitamin_d' as the mapping key
  key_opts <- c("vitd", "vitamin_d")
  key_present <- intersect(names(col_map), key_opts)

  # Key missing entirely -> missing_map
  if (length(key_present) == 0L) {
    rlang::abort("vitamin_d_status(): missing col_map entry for vitamin D value (e.g., vitd = 'VitD').",
                 class = "healthmarkers_vitd_error_missing_map")
  }
  key_used <- key_present[1]
  vitd_col <- as.character(col_map[[key_used]])

  if (!nzchar(vitd_col)) {
    rlang::abort(
      sprintf("vitamin_d_status(): empty mapping value for key '%s'.", key_used),
      class = "healthmarkers_vitd_error_bad_map_values"
    )
  }
  if (!(vitd_col %in% names(data))) {
    rlang::abort(
      sprintf("vitamin_d_status(): missing required column in data for key '%s': '%s'", key_used, vitd_col),
      class = "healthmarkers_vitd_error_missing_columns"
    )
  }

  .hm_log_cols(cm, col_map, fn_name, verbose)
  if (isTRUE(verbose))
    hm_inform(sprintf("%s(): computing markers:\n  vitamin_d_status [%s]", fn_name, vitd_col), level = "inform")

  # Coerce numeric
  vitd <- data[[vitd_col]]
  if (!is.numeric(vitd)) {
    old <- vitd
    suppressWarnings(vitd <- as.numeric(old))
    intro <- sum(is.na(vitd) & !is.na(old))
    if (intro > 0) {
      rlang::warn(sprintf("Column '%s' coerced to numeric; NAs introduced: %d", vitd_col, intro),
                  class = "healthmarkers_vitd_warn_na_coercion")
    }
  }
  vitd[!is.finite(vitd)] <- NA_real_

  row_na <- is.na(vitd)
  if (na_action_raw == "warn" && any(row_na)) {
    rlang::warn("Missing 25(OH)D values; status will be NA for those rows.",
                class = "healthmarkers_vitd_warn_missing_inputs")
  }
  if (na_action_eff == "error" && any(row_na)) {
    rlang::abort("vitamin_d_status(): required inputs contain missing values (na_action='error').",
                 class = "healthmarkers_vitd_error_missing_values")
  }
  keep <- if (na_action_eff == "omit") !row_na else rep(TRUE, length(vitd))
  d_vitd <- vitd[keep]

  # Domain warnings (always-on)
  if (any(is.finite(d_vitd) & d_vitd < 0)) {
    rlang::warn("vitamin_d_status(): negative 25(OH)D values detected; check units/lab results.",
                class = "healthmarkers_vitd_warn_negative_values")
  }
  if (is.finite(stats::median(d_vitd, na.rm = TRUE)) && stats::median(d_vitd, na.rm = TRUE) > 150) {
    rlang::warn("vitamin_d_status(): values appear high; ensure units are ng/mL (nmol/L -> divide by 2.5).",
                class = "healthmarkers_vitd_warn_units_suspicious")
  }

  hm_inform("vitamin_d_status(): computing status", level = "debug")

  # Simple status bands (ng/mL): <20 Deficient, 20-29 Insufficient, >=30 Sufficient
  status <- ifelse(is.na(d_vitd), NA_character_,
            ifelse(d_vitd < 20, "Deficient",
            ifelse(d_vitd < 30, "Insufficient", "Sufficient")))
  status <- factor(status, levels = c("Deficient","Insufficient","Sufficient"), ordered = TRUE)

  out <- tibble::tibble(vitamin_d_status = status)

  # Pad back to full row count
  if (na_action_eff != "omit") {
    res <- tibble::tibble(vitamin_d_status = factor(rep(NA_character_, length(vitd)),
                                                    levels = levels(status), ordered = TRUE))
    res$vitamin_d_status[keep] <- out$vitamin_d_status
    out <- res
  }

  if (!is.null(id_col)) {
    id_vec <- if (na_action_eff == "omit") data[[id_col]][keep] else data[[id_col]]
    out[[id_col]] <- id_vec
    out <- out[, c(id_col, setdiff(names(out), id_col)), drop = FALSE]
    out <- tibble::as_tibble(out)
  }
  if (isTRUE(verbose)) hm_inform(hm_result_summary(out, fn_name), level = "inform")
  out
 
}

