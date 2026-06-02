#' Charlson Comorbidity Index (CCI)
#'
#' Computes the Charlson Comorbidity Index by summing weighted comorbidities.
#'
#' @details
#' The Charlson Index predicts 10-year mortality by summing weighted comorbidities.
#' We implement the canonical 19-condition, weight scheme:
#' - 1 point: myocardial infarction, congestive heart failure, peripheral vascular disease,
#'   cerebrovascular disease (stroke), dementia, chronic pulmonary disease (COPD),
#'   rheumatologic disease, peptic ulcer disease
#' - 2 points: hemiplegia/paraplegia, moderate/severe renal disease, any malignancy (non-metastatic),
#'   leukemia, lymphoma, diabetes with complications
#' - 3 points: moderate/severe liver disease
#' - 6 points: metastatic solid tumor, AIDS/HIV
#'
#' To avoid double counting paired conditions, the following use the maximum applicable weight:
#' - Diabetes: max(1 * diabetes without complications, 2 * diabetes with complications)
#' - Liver disease: max(1 * mild liver disease, 3 * moderate/severe liver disease)
#' - Cancer: max(2 * non-metastatic solid tumor, 6 * metastatic solid tumor)
#'
#' Age points are not included here and can be added separately if needed.
#'
#' @param data A data.frame or tibble with binary indicators (0/1) for each comorbidity.
#' @param col_map Named list mapping keys to columns in `data`:
#'   mi, chf, pvd, stroke, dementia, copd, rheum, ulcer, mild_liver, diabetes, diab_comp,
#'   hemiplegia, renal, cancer, leukemia, lymphoma, sev_liver, metastatic_cancer, hiv.
#' @param verbose Logical; if TRUE, emits progress messages.
#' @param na_action One of c("keep","omit","error","ignore","warn").
#'
#' @return A tibble with one column: charlson_index (integer total score; NA if any
#' required input is NA and na_action != "omit").
#'
#' @references \insertRef{charlson1987cci}{HealthMarkers}
#'
#' @examples
#' patient <- tibble::tibble(
#'   mi=0, chf=0, pvd=0, stroke=0, dementia=0, copd=0, rheum=0, ulcer=0,
#'   mild_liver=0, diabetes=0, diab_comp=1, hemiplegia=0, renal=1,
#'   cancer=0, leukemia=0, lymphoma=0, sev_liver=0, metastatic_cancer=0, hiv=0
#' )
#' charlson_index(
#'   patient,
#'   col_map = as.list(stats::setNames(names(patient), names(patient)))
#' )
#'
#' @export
charlson_index <- function(
  data,
  col_map = list(
    mi="mi", chf="chf", pvd="pvd", stroke="stroke", dementia="dementia", copd="copd",
    rheum="rheum", ulcer="ulcer", mild_liver="mild_liver", diabetes="diabetes",
    diab_comp="diab_comp", hemiplegia="hemiplegia", renal="renal", cancer="cancer",
    leukemia="leukemia", lymphoma="lymphoma", sev_liver="sev_liver",
    metastatic_cancer="metastatic_cancer", hiv="hiv"
  ),
  verbose = TRUE,
  na_action = c("keep","omit","error","ignore","warn")
) {
  data_name <- (function(.e) if (is.symbol(.e)) as.character(.e) else "data")(substitute(data))
  fn_name <- "charlson_index"
  .hm_log_input(data, data_name, fn_name, verbose)
  id_col  <- .hm_detect_id_col(data)
  .na <- .hm_normalize_na_action(match.arg(na_action))
  na_action_raw <- .na$na_action_raw
  na_action_eff <- .na$na_action_eff

  keys <- c(
    "mi","chf","pvd","stroke","dementia","copd","rheum","ulcer",
    "mild_liver","diabetes","diab_comp","hemiplegia","renal","cancer",
    "leukemia","lymphoma","sev_liver","metastatic_cancer","hiv"
  )

  # --- validate data and col_map (HM-CS style) ------------------------------
  if (!is.data.frame(data)) {
    rlang::abort(
      "charlson_index(): `data` must be a data.frame or tibble.",
      class = "healthmarkers_cci_error_data_type"
    )
  }

  if (!is.list(col_map)) {
    rlang::abort(
      "charlson_index(): `col_map` must be a named list.",
      class = "healthmarkers_cci_error_colmap_type"
    )
  }

  if (length(col_map) == 0L || is.null(names(col_map))) {
    rlang::abort(
      "charlson_index(): missing or empty `col_map`.",
      class = "healthmarkers_cci_error_missing_map"
    )
  }

  missing_keys <- setdiff(keys, names(col_map))
  if (length(missing_keys)) {
    rlang::abort(
      paste0("charlson_index(): missing col_map entries for: ",
             paste(missing_keys, collapse = ", ")),
      class = "healthmarkers_cci_error_missing_map"
    )
  }

  mapped <- unname(unlist(col_map[keys], use.names = FALSE))
  if (any(!nzchar(mapped))) {
    bad <- keys[!nzchar(unname(unlist(col_map[keys])))]
    rlang::abort(
      paste0("charlson_index(): missing col_map entries for: ",
             paste(bad, collapse = ", ")),
      class = "healthmarkers_cci_error_bad_map_values"
    )
  }

  missing_cols <- setdiff(mapped, names(data))
  if (length(missing_cols)) {
    rlang::abort(
      paste0("charlson_index(): missing required columns in data: ",
             paste(missing_cols, collapse = ", ")),
      class = "healthmarkers_cci_error_missing_columns"
    )
  }

  # --- verbose / debug ------------------------------------------------------
  if (isTRUE(verbose)) {
    map_parts <- vapply(keys, function(k) sprintf("%s -> '%s'", k, col_map[[k]]), character(1))
    hm_inform(sprintf("%s(): col_map: %s", fn_name, paste(map_parts, collapse = ", ")), level = "inform")
    hm_inform(sprintf("%s(): computing markers:\n  charlson_index [mi, chf, pvd, stroke, dementia, copd, rheum, ulcer, mild_liver, diabetes, diab_comp, hemiplegia, renal, cancer, leukemia, lymphoma, sev_liver, metastatic_cancer, hiv]", fn_name), level = "inform")
  }

  # --- coerce to numeric 0/1; track NA coercion -----------------------------
  for (cn in mapped) {
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(new <- as.numeric(old))
      intro <- sum(is.na(new) & !is.na(old))
      if (intro > 0L) {
        rlang::warn(
          sprintf("Column '%s' coerced to numeric; NAs introduced: %d", cn, intro),
          class = "healthmarkers_cci_warn_na_coercion"
        )
      }
      data[[cn]] <- new
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  mat <- as.data.frame(data[mapped], stringsAsFactors = FALSE)
  row_has_na <- Reduce("|", lapply(mat, is.na))

  if (na_action_raw == "warn" && any(row_has_na)) {
    rlang::warn(
      "Missing comorbidity indicators; CCI will be NA for those rows.",
      class = "healthmarkers_cci_warn_missing_inputs"
    )
  }

  if (na_action_eff == "error" && any(row_has_na)) {
    rlang::abort(
      "charlson_index(): required inputs contain missing values (na_action='error').",
      class = "healthmarkers_cci_error_missing_values"
    )
  }

  keep <- if (na_action_eff == "omit") !row_has_na else rep(TRUE, nrow(data))
  d <- as.data.frame(lapply(mat, function(x) x[keep]))

  # --- domain warnings for non-binary values --------------------------------
  is_bad_val <- function(x) is.finite(x) & !(x %in% c(0, 1))
  if (nrow(d) > 0 && any(Reduce("|", lapply(d, is_bad_val)))) {
    rlang::warn(
      "charlson_index(): indicators should be binary 0/1; out-of-range values detected.",
      class = "healthmarkers_cci_warn_out_of_range"
    )
  }

  hm_inform(level = "debug", msg = "charlson_index(): computing")

  # --- scoring --------------------------------------------------------------
  base <- 0
  base <- base +
    1 * d$mi + 1 * d$chf + 1 * d$pvd + 1 * d$stroke + 1 * d$dementia +
    1 * d$copd + 1 * d$rheum + 1 * d$ulcer +
    2 * d$hemiplegia + 2 * d$renal + 2 * d$leukemia + 2 * d$lymphoma +
    6 * d$hiv

  diab   <- pmax(1 * d$diabetes, 2 * d$diab_comp,    na.rm = FALSE)
  liver  <- pmax(1 * d$mild_liver, 3 * d$sev_liver,  na.rm = FALSE)
  cancer <- pmax(2 * d$cancer,     6 * d$metastatic_cancer, na.rm = FALSE)

  score <- base + diab + liver + cancer
  score[!is.finite(score)] <- NA_real_

  out_core <- tibble::tibble(charlson_index = as.integer(round(score, 0)))

  # --- pad back if not omitting ---------------------------------------------
  if (na_action_eff != "omit") {
    res <- tibble::tibble(charlson_index = rep(NA_integer_, nrow(data)))
    res$charlson_index[keep] <- out_core$charlson_index
    out <- res
  } else {
    out <- out_core
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

