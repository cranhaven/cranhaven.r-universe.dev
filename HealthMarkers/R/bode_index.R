#' BODE Index (BMI, Obstruction, Dyspnea, Exercise capacity)
#'
#' Computes the BODE index (0-10) using FEV1 % predicted, 6-minute walk distance (6MWD),
#' mMRC dyspnea scale, and BMI. Higher scores indicate worse prognosis in COPD.
#'
#' @details
#' Scoring components:
#' FEV1 % predicted: >=65 = 0; 50-64 = 1; 36-49 = 2; <=35 = 3
#' 6MWD (meters): >=350 = 0; 250-349 = 1; 150-249 = 2; <=149 = 3
#' mMRC dyspnea: 0-1 = 0; 2 = 1; 3 = 2; 4 = 3
#' BMI: >21 = 0; <=21 = 1
#'
#' @param data data.frame/tibble with required columns.
#' @param col_map named list with keys:
#'   fev1_pct OR (fev1 and fev1_pred) OR fev1_pp; plus sixmwd, mmrc, bmi.
#'   Example minimal: list(fev1_pct="FEV1pct", sixmwd="Walk_m", mmrc="mMRC", bmi="BMI")
#'   Example derive: list(fev1="FEV1", fev1_pred="FEV1_pred", sixmwd="Walk_m", mmrc="mMRC", bmi="BMI")
#'   Example from spirometry_markers: list(fev1_pp="fev1_pp", sixmwd="Walk_m", mmrc="mMRC", bmi="BMI")
#' @param na_action one of c("keep","omit","error","ignore","warn").
#' @param verbose logical; TRUE (default) emits messages.
#' @return tibble with bode_index (integer). NA if any required input missing (unless omitted).
#' @references \insertRef{celli2004bode}{HealthMarkers}
#'
#' @examples
#' df <- data.frame(FEV1pct = c(68, 45, 30), Walk_m = c(400, 280, 140),
#'                  mMRC = c(1, 2, 3), BMI = c(24, 19, 18))
#' bode_index(df, col_map = list(fev1_pct = "FEV1pct", sixmwd = "Walk_m",
#'                               mmrc = "mMRC", bmi = "BMI"))
#' @export
bode_index <- function(
  data,
  col_map = NULL,
  na_action = c("keep","omit","error","ignore","warn"),
  verbose = TRUE
) {
  data_name <- (function(.e) if (is.symbol(.e)) as.character(.e) else "data")(substitute(data))
  fn_name <- "bode_index"
  .hm_log_input(data, data_name, fn_name, verbose)
  .na <- .hm_normalize_na_action(match.arg(na_action))
  na_action_raw <- .na$na_action_raw
  na_action_eff <- .na$na_action_eff
  id_col <- .hm_detect_id_col(data)

  # Validate data
  if (!is.data.frame(data)) {
    rlang::abort("bode_index(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_bode_error_data_type")
  }

  all_bode_keys <- c("fev1_pct","fev1_pp","fev1","sixmwd","mmrc","bmi")
  cm      <- .hm_build_col_map(data, col_map, all_bode_keys, fn = "bode_index")
  data    <- cm$data
  col_map <- cm$col_map
  if (is.null(col_map)) col_map <- list()

  if (!is.list(col_map) || is.null(names(col_map))) {
    rlang::abort("bode_index(): `col_map` must be a named list.",
                 class = "healthmarkers_bode_error_colmap_type")
  }

  # Mandatory non-FEV1 keys
  base_keys <- c("sixmwd","mmrc","bmi")
  missing_base <- setdiff(base_keys, names(col_map))
  if (length(missing_base)) {
    rlang::abort(paste0("bode_index(): missing col_map entries for: ",
                        paste(missing_base, collapse = ", ")),
                 class = "healthmarkers_bode_error_missing_map")
  }

  # Resolve FEV1 percent predicted source
  has_pct <- !is.null(col_map$fev1_pct) && nzchar(col_map$fev1_pct)
  has_raw <- !is.null(col_map$fev1) && nzchar(col_map$fev1)
  has_pred <- !is.null(col_map$fev1_pred) && nzchar(col_map$fev1_pred)
  has_pp <- !is.null(col_map$fev1_pp) && nzchar(col_map$fev1_pp)

  fev1_source <- if (has_pct) "pct" else if (has_raw && has_pred) "derive" else if (has_pp) "pp" else "none"
  if (fev1_source == "none") {
    rlang::abort("bode_index(): must supply either fev1_pct OR (fev1 & fev1_pred) OR fev1_pp.",
                 class = "healthmarkers_bode_error_missing_fev1_source")
  }

  # Collect column names to check exist
  cols_needed <- c(
    if (fev1_source == "pct") col_map$fev1_pct,
    if (fev1_source == "derive") c(col_map$fev1, col_map$fev1_pred),
    if (fev1_source == "pp") col_map$fev1_pp,
    col_map$sixmwd,
    col_map$mmrc,
    col_map$bmi
  )
  cols_needed <- cols_needed[nzchar(cols_needed)]
  miss_cols <- setdiff(cols_needed, names(data))
  if (length(miss_cols)) {
    rlang::abort(paste0("bode_index(): missing required columns in data: ",
                        paste(miss_cols, collapse = ", ")),
                 class = "healthmarkers_bode_error_missing_columns")
  }

  .hm_log_cols(cm, col_map, fn_name, verbose)
  if (isTRUE(verbose))
    hm_inform(sprintf("%s(): computing markers:\n  bode_index  [0-10 COPD severity score]", fn_name), level = "inform")

  # Coercion helper
  coerce_col <- function(cn) {
    if (is.null(cn) || !nzchar(cn)) return(NULL)
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(new <- as.numeric(old))
      intro <- sum(is.na(new) & !is.na(old))
      if (intro > 0) {
        rlang::warn(sprintf("Column '%s' coerced to numeric; NAs introduced: %d", cn, intro),
                    class = "healthmarkers_bode_warn_na_coercion")
      }
      data[[cn]] <- new
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
    data[[cn]]
  }

  fev1_pct <- switch(
    fev1_source,
    pct = coerce_col(col_map$fev1_pct),
    derive = {
      raw <- coerce_col(col_map$fev1)
      pred <- coerce_col(col_map$fev1_pred)
      out <- 100 * raw / pred
      out[!is.finite(out)] <- NA_real_
      out
    },
    pp = {
      pp <- coerce_col(col_map$fev1_pp)
      pp[!is.finite(pp)] <- NA_real_
      pp
    }
  )

  sixmwd <- coerce_col(col_map$sixmwd)
  mmrc   <- coerce_col(col_map$mmrc)
  bmi    <- coerce_col(col_map$bmi)

  row_na <- is.na(fev1_pct) | is.na(sixmwd) | is.na(mmrc) | is.na(bmi)
  if (na_action_raw == "warn" && any(row_na)) {
    rlang::warn("Missing BODE components; bode_index will be NA for those rows.",
                class = "healthmarkers_bode_warn_missing_inputs")
  }
  if (na_action_eff == "error" && any(row_na)) {
    rlang::abort("bode_index(): required inputs contain missing values (na_action='error').",
                 class = "healthmarkers_bode_error_missing_values")
  }
  keep <- if (na_action_eff == "omit") !row_na else rep(TRUE, length(fev1_pct))

  d_fev1 <- fev1_pct[keep]
  d_walk <- sixmwd[keep]
  d_mmrc <- mmrc[keep]
  d_bmi  <- bmi[keep]

  # Domain warnings
  if (any(is.finite(d_mmrc) & (d_mmrc < 0 | d_mmrc > 4))) {
    rlang::warn("bode_index(): mMRC values outside 0-4 detected.",
                class = "healthmarkers_bode_warn_mmrc_range")
  }
  if (any(is.finite(d_fev1) & (d_fev1 < 0 | d_fev1 > 150))) {
    rlang::warn("bode_index(): FEV1 % predicted values outside 0-150 detected.",
                class = "healthmarkers_bode_warn_fev1pct_range")
  }
  if (any(is.finite(d_walk) & (d_walk < 0 | d_walk > 1500))) {
    rlang::warn("bode_index(): 6MWD values outside plausible range detected.",
                class = "healthmarkers_bode_warn_sixmwd_range")
  }
  if (any(is.finite(d_bmi) & (d_bmi < 10 | d_bmi > 80))) {
    rlang::warn("bode_index(): BMI values outside plausible range detected.",
                class = "healthmarkers_bode_warn_bmi_range")
  }

  hm_inform(level = "debug", msg = "bode_index(): computing score")

  fev1_score <- ifelse(is.na(d_fev1), NA_integer_,
                       ifelse(d_fev1 >= 65, 0L,
                       ifelse(d_fev1 >= 50, 1L,
                       ifelse(d_fev1 >= 36, 2L, 3L))))
  walk_score <- ifelse(is.na(d_walk), NA_integer_,
                       ifelse(d_walk >= 350, 0L,
                       ifelse(d_walk >= 250, 1L,
                       ifelse(d_walk >= 150, 2L, 3L))))
  mmrc_score <- ifelse(is.na(d_mmrc), NA_integer_,
                       ifelse(d_mmrc <= 1, 0L,
                       ifelse(d_mmrc == 2, 1L,
                       ifelse(d_mmrc == 3, 2L, 3L))))
  bmi_score  <- ifelse(is.na(d_bmi), NA_integer_,
                       ifelse(d_bmi > 21, 0L, 1L))

  total_score <- fev1_score + walk_score + mmrc_score + bmi_score
  any_comp_na <- is.na(fev1_score) | is.na(walk_score) | is.na(mmrc_score) | is.na(bmi_score)
  total_score[any_comp_na] <- ifelse(na_action_eff == "omit", total_score[any_comp_na], NA_integer_)

  out <- tibble::tibble(
    bode_index = total_score,
    fev1_pct = d_fev1,
    fev1_score = fev1_score,
    walk_score = walk_score,
    mmrc_score = mmrc_score,
    bmi_score  = bmi_score
  )

  if (na_action_eff != "omit") {
    res <- tibble::tibble(
      bode_index = rep(NA_integer_, length(fev1_pct)),
      fev1_pct = rep(NA_real_, length(fev1_pct)),
      fev1_score = NA_integer_,
      walk_score = NA_integer_,
      mmrc_score = NA_integer_,
      bmi_score  = NA_integer_
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
  if (isTRUE(verbose)) { hm_inform(hm_result_summary(out, fn_name), level = "inform") }

  out
}

