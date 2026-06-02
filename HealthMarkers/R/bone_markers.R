
#' Compute Bone Health & Body-Composition Markers (HM-CS v3)
#'
#' Given DXA, anthropometry, and optional bone-turnover markers, computes:
#' - OSTA: (weight - age) x 0.2
#' - ALMI: Appendicular Lean Mass Index = ALM / height^2
#' - FMI: Fat Mass Index = FM / height^2
#' - BMD_Tscore: (BMD - ref_mean) / ref_sd
#' and (if in `col_map` + data) passes through: TBS, HSA, PINP, CTX, BSAP, Osteocalcin.
#'
#' Notes:
#' - Units: height in meters; ALM, FM, weight in kilograms; BMD in g/cm^2; ALMI/FMI in kg/m^2.
#' - Non-finite values are treated as NA; division by zero is prevented by input checks.
#' - `BMD_ref_mean` and `BMD_ref_sd` must be supplied by the user from an appropriate
#'   reference population (for example, study-specific values or external norms such as NHANES).
#'
#' @param data A `data.frame` or tibble with subject-level DXA/anthropometry data.
#' @param col_map Named list mapping keys to column names. Required keys:
#'   - `age`, `weight`, `height`, `ALM`, `FM`, `BMD`, `BMD_ref_mean`, `BMD_ref_sd`
#'   Optional (passed-through if present and found in data): `TBS`, `HSA`, `PINP`, `CTX`, `BSAP`, `Osteocalcin`.
#' @param na_action One of "keep", "omit", or "error" controlling how
#'   missing/non-finite input values are treated.
#' @param verbose Logical; if TRUE (default), emits progress messages via `hm_inform()`.
#'
#' @return A tibble with columns: `OSTA`, `ALMI`, `FMI`, `BMD_Tscore`, and
#'   optionally `TBS`, `HSA`, `PINP`, `CTX`, `BSAP`, `Osteocalcin` (in that order).
#'
#' @examples
#' library(tibble)
#' df <- tibble(
#'   age = c(60, 72), weight = c(65, 50), height = c(1.65, 1.58),
#'   ALM = c(18.2, 14.7), FM = c(22.0, 20.5),
#'   BMD = c(0.95, 0.80), BMD_ref_mean = c(1.00, 1.00), BMD_ref_sd = c(0.12, 0.12)
#' )
#' col_map <- list(
#'   age = "age", weight = "weight", height = "height",
#'   ALM = "ALM", FM = "FM", BMD = "BMD",
#'   BMD_ref_mean = "BMD_ref_mean", BMD_ref_sd = "BMD_ref_sd"
#' )
#' bone_markers(df, col_map)
#'
#' @references
#' \insertRef{woo2002osta}{HealthMarkers}
#' \insertRef{who1994osteoporosis}{HealthMarkers}
#'
#' @importFrom tibble tibble
#' @export
bone_markers <- function(
  data,
  col_map = NULL,
  na_action = c("keep", "omit", "error"),
  verbose = TRUE
) {
  data_name <- (function(.e) if (is.symbol(.e)) as.character(.e) else "data")(substitute(data))
  fn_name <- "bone_markers"
  .hm_log_input(data, data_name, fn_name, verbose)
  na_action <- match.arg(na_action)
  id_col <- .hm_detect_id_col(data)

  required <- c("age", "weight", "height", "ALM", "FM", "BMD", "BMD_ref_mean", "BMD_ref_sd")
  optional <- c("TBS", "HSA", "PINP", "CTX", "BSAP", "Osteocalcin")

  hm_validate_inputs(data, col_map, required_keys = character(0), fn = "bone_markers")
  cm      <- .hm_build_col_map(data, col_map, keys = c(required, optional), fn = fn_name)
  data    <- cm$data
  col_map <- cm$col_map

  # Required columns exist in data
  req_cols <- unname(unlist(col_map[required], use.names = FALSE))
  missing_cols <- setdiff(req_cols, names(data))
  if (length(missing_cols)) {
    rlang::abort(
      paste0("bone_markers(): missing required columns in data: ", paste(missing_cols, collapse = ", ")),
      class = "healthmarkers_bone_error_missing_columns"
    )
  }

  hm_inform(level = "debug", msg = "bone_markers(): computing bone markers")
  .hm_log_cols(cm, col_map, fn_name, verbose)
  if (isTRUE(verbose))
    hm_inform(sprintf("%s(): computing markers:\n  OSTA       [(weight - age) * 0.2]\n  ALMI       [ALM / height^2]\n  FMI        [FM / height^2]\n  BMD_Tscore [(BMD - ref_mean) / ref_sd]", fn_name), level = "inform")

  # Coerce required and present optional columns to numeric; warn if NAs introduced
  present_opt_cols <- intersect(unname(unlist(col_map[intersect(optional, names(col_map))], use.names = FALSE)), names(data))
  for (cn in unique(c(req_cols, present_opt_cols))) {
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old))
      introduced <- sum(is.na(data[[cn]]) & !is.na(old))
      if (introduced > 0) {
        rlang::warn(
          sprintf("bone_markers(): column '%s' coerced to numeric; NAs introduced: %d", cn, introduced),
          class = "healthmarkers_bone_warn_na_coercion"
        )
      }
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  n <- nrow(data)
  # extract required
  age <- data[[col_map$age]]
  weight <- data[[col_map$weight]]
  height <- data[[col_map$height]]
  ALM <- data[[col_map$ALM]]
  FM <- data[[col_map$FM]]
  BMD <- data[[col_map$BMD]]
  ref_mean <- data[[col_map$BMD_ref_mean]]
  ref_sd <- data[[col_map$BMD_ref_sd]]

  # Constraints
  if (any(ref_sd <= 0, na.rm = TRUE)) {
    rlang::abort("bone_markers(): 'BMD_ref_sd' must be positive for non-missing rows.",
                 class = "healthmarkers_bone_error_refsd_positive")
  }
  if (any(height <= 0, na.rm = TRUE)) {
    rlang::abort("bone_markers(): 'height' must be positive for non-missing rows.",
                 class = "healthmarkers_bone_error_height_positive")
  }

  # NA row policy across required variables (NA or non-finite)
  req_df <- data[, unname(unlist(col_map[required])), drop = FALSE]
  rows_with_na <- !stats::complete.cases(req_df)
  if (na_action == "error" && any(rows_with_na)) {
    rlang::abort("bone_markers(): missing/non-finite values present in required inputs (na_action='error').",
                 class = "healthmarkers_bone_error_missing_values")
  } else if (na_action == "omit" && any(rows_with_na)) {
    keep <- !rows_with_na
    data <- data[keep, , drop = FALSE]
    age <- age[keep]; weight <- weight[keep]; height <- height[keep]
    ALM <- ALM[keep]; FM <- FM[keep]; BMD <- BMD[keep]
    ref_mean <- ref_mean[keep]; ref_sd <- ref_sd[keep]
    n <- nrow(data)
  }

  # compute core indices
  OSTA <- (weight - age) * 0.2
  ALMI <- ALM / (height^2)
  FMI <- FM / (height^2)
  BMD_Tscore <- (BMD - ref_mean) / ref_sd

  # helper for optional pass-through
  get_opt <- function(key) {
    if (key %in% names(col_map) && col_map[[key]] %in% names(data)) {
      as.numeric(data[[ col_map[[key]] ]])
    } else {
      rep(NA_real_, n)
    }
  }
  TBS <- get_opt("TBS")
  HSA <- get_opt("HSA")
  PINP <- get_opt("PINP")
  CTX <- get_opt("CTX")
  BSAP <- get_opt("BSAP")
  Osteocalcin <- get_opt("Osteocalcin")

  result <- tibble::tibble(
    OSTA = as.numeric(OSTA),
    ALMI = as.numeric(ALMI),
    FMI = as.numeric(FMI),
    BMD_Tscore = as.numeric(BMD_Tscore),
    TBS = TBS,
    HSA = HSA,
    PINP = PINP,
    CTX = CTX,
    BSAP = BSAP,
    Osteocalcin = Osteocalcin
  )

  if (!is.null(id_col)) {
    result[[id_col]] <- data[[id_col]]
    result <- result[, c(id_col, setdiff(names(result), id_col)), drop = FALSE]
    result <- tibble::as_tibble(result)
  }
  if (isTRUE(verbose)) { hm_inform(hm_result_summary(result, fn_name), level = "inform") }
  result
}

