#######################################

#   LAPOP Extract Response Options    #

#######################################

#' Extract Response Option (RO) values and texts for all variables into a tidy table.
#'
#' Works with:
#'  (a) dataset-level dictionaries (e.g., attr(data, "label.table") is a list keyed by "<VAR>_<lang>"), or
#'  (b) per-variable attributes (e.g., attr(data[[VAR]], "levels") or factor levels).
#'
#' @param data A data.frame read with readstata13/haven/etc.
#' @param lang_id Language code used in label table names ("en", "es", "pt").
#'   If `NULL` or `""`, auto-detect per variable (dataset-level only). Ignored for per-variable `levels`.
#' @param include_special Logical; if FALSE, drop codes >= 1000 when codes are numeric. Default FALSE.
#' @param restrict_to_present Logical; if TRUE, keep only codes that appear in the data. Default TRUE.
#' @param one_row_per_var Logical; if TRUE, return one row per variable_name with concatenated ROs. Default FALSE.
#' @param pair_sep String used to separate each "(value) answer_text" pair when collapsing. Default " | ".
#' @param attr_name Name of the attribute that stores RO info. Default "label.table".
#'
#' @return
#' If `one_row_per_var = FALSE`: tibble with columns `variable_name`, `value`, `answer_text`.
#' If `one_row_per_var = TRUE`: tibble with columns `variable_name`, `answer_text` (collapsed pairs).
#'
#' @examples
#' toy <- data.frame(
#'   ing4 = c(1L, 2L, 1L),
#'   b12 = c(1L, 2L, NA_integer_)
#' )
#' attr(toy, "label.table") <- list(
#'   ing4_pt = c("Apoia muito" = 1L, "Apoia" = 2L, "NS/NR" = 1000L),
#'   b12_pt = c("Muito" = 1L, "Algo" = 2L, "NS/NR" = 1000L)
#' )
#'
#' lpr_extract_ros(toy, lang_id = "pt")
#' lpr_extract_ros(toy, lang_id = "pt", one_row_per_var = TRUE)
#' 
#'@author Robert Vidigal, \email{robert.vidigal@@vanderbilt.edu}
#'
#' @export
#' @import tibble
lpr_extract_ros <- function(data,
                            lang_id = "en",
                            include_special = FALSE,
                            restrict_to_present = TRUE,
                            one_row_per_var = FALSE,
                            pair_sep = " | ",
                            attr_name = "label.table") {

  # ---------- helpers ----------
  is_nonempty_string <- function(x) !is.null(x) && nzchar(x)

  resolve_label_name <- function(VAR, lang_id, lbl_names) {
    # explicit lang
    if (is_nonempty_string(lang_id)) {
      nm <- paste0(VAR, "_", lang_id)
      return(if (nm %in% lbl_names) nm else NA_character_)
    }
    # exact match
    if (VAR %in% lbl_names) return(VAR)
    # candidates
    candidates <- grep(paste0("^", VAR, "(_[A-Za-z0-9]+)?$"), lbl_names, value = TRUE)
    if (length(candidates) == 1) return(candidates)
    if (length(candidates) > 1) {
      pref <- paste0(VAR, "_en")
      if (pref %in% candidates) return(pref)
      warning(sprintf(
        "Multiple label tables found for '%s': %s. Using '%s'.",
        VAR, paste(candidates, collapse = ", "), candidates[1]
      ))
      return(candidates[1])
    }
    NA_character_
  }

  build_df_from_named_codes <- function(VAR, x) {
    # Expect names = labels, values = codes (numeric/integer)
    tibble::tibble(
      variable_name     = VAR,
      value       = suppressWarnings(as.integer(unname(x))),
      answer_text = as.character(names(x))
    )
  }

  build_df_from_named_labels <- function(VAR, x) {
    # names = codes (as character), values = labels
    tibble::tibble(
      variable_name     = VAR,
      value       = suppressWarnings(as.integer(names(x))),
      answer_text = as.character(unname(x))
    )
  }

  build_df_from_levels <- function(VAR, levs) {
    # Fallback when we only have plain levels (no numeric codes available)
    tibble::tibble(
      variable_name     = VAR,
      value       = seq_along(levs),          # positional codes
      answer_text = as.character(levs)
    )
  }

  # ---------- main pathway selection ----------
  dict_at_data <- attr(data, attr_name)
  dict_is_list <- !is.null(dict_at_data) && is.list(dict_at_data)

  vars <- names(data)

  out <- lapply(vars, function(VAR) {
    # Path A: dataset-level dictionary list
    if (dict_is_list) {
      label_table_name <- resolve_label_name(VAR, lang_id, names(dict_at_data))
      if (!is.na(label_table_name)) {
        lt <- dict_at_data[[label_table_name]]
        if (is.null(lt)) return(tibble::tibble(variable_name = VAR,value = NA_integer_,answer_text = NA_character_ ))

        # Try common shapes:
        if (!is.null(names(lt)) && (is.numeric(lt) || is.integer(lt))) {
          df <- build_df_from_named_codes(VAR, lt)
        } else if (!is.null(names(lt)) && is.character(lt)) {
          df <- build_df_from_named_labels(VAR, lt)
        } else {
          warning(sprintf("Unrecognized label table format for '%s'. Skipping.", label_table_name))
          return(tibble::tibble(variable_name = VAR,value = NA_integer_,answer_text = NA_character_))
        }

      } else {
        # No table for this VAR in dataset-level dict; try per-variable attribute below
        df <- NULL
      }

      # If we got df from dataset-level path, continue post-processing
      if (!is.null(df)) {
        # Filter special codes if numeric
        if (!include_special && is.numeric(df$value)) {
          df <- dplyr::filter(df, .data$value < 1000)
        }

        if (restrict_to_present) {
          vals <- unique(stats::na.omit(data[[VAR]]))
          suppressWarnings(vals <- as.integer(vals))
          df <- dplyr::filter(df, .data$value %in% vals)
        }

        return(df)
      }
    }

    # Path B: per-variable attribute or factor levels
    x <- data[[VAR]]
    x_attr <- attr(x, attr_name)

    if (!is.null(x_attr)) {
      # Several possible shapes:
      if (!is.null(names(x_attr)) && (is.numeric(x_attr) || is.integer(x_attr))) {
        df <- build_df_from_named_codes(VAR, x_attr)
      } else if (!is.null(names(x_attr)) && is.character(x_attr)) {
        df <- build_df_from_named_labels(VAR, x_attr)
      } else if (is.character(x_attr) && attr_name == "levels") {
        df <- build_df_from_levels(VAR, x_attr)
      } else {
        # Last resort: if it's a plain character vector, treat as levels
        if (is.character(x_attr)) {
          df <- build_df_from_levels(VAR, x_attr)
        } else {
          warning(sprintf("Attribute '%s' for '%s' has unsupported format; skipping.", attr_name, VAR))
          return(tibble::tibble(variable_name = VAR,value = NA_integer_,answer_text = NA_character_))
        }
      }
    } else {
      # If attr not present but variable is a factor and attr_name == "levels", use factor levels
      if (attr_name == "levels" && is.factor(x)) {
        df <- build_df_from_levels(VAR, levels(x))
      } else {
        return(tibble::tibble(   variable_name = VAR,   value = NA_integer_,   answer_text = NA_character_ ))
      }
    }

    # Post-processing for per-variable path
    # include_special: we only apply if values are numeric and comparable
    if (!include_special && is.numeric(df$value)) {
      df <- dplyr::filter(df, .data$value < 1000)
    } else if (!include_special && !is.numeric(df$value)) {
      # can't apply the >=1000 rule without numeric codes
      # (silently skip; could message if you prefer)
      df <- df
    }

    if (restrict_to_present) {
      vals <- unique(stats::na.omit(data[[VAR]]))
      suppressWarnings(vals <- as.integer(vals))
      if (all(is.na(vals))) {
        # If we couldn't coerce present values to numeric (e.g., character factors),
        # we skip this filter to avoid dropping everything.
        df <- df
      } else {
        df <- dplyr::filter(df, .data$value %in% vals)
      }
    }

    df
  })

  out <- dplyr::bind_rows(out)

  if (nrow(out) == 0) {
    warning("No ROS found; returning NA placeholders for all variables.")
    out <- tibble(
      variable_name = vars,
      value = NA_integer_,
      answer_text = NA_character_
    )
  }

  out <- dplyr::arrange(out, .data$variable_name, suppressWarnings(as.numeric(.data$value)))

  if (one_row_per_var) {
    out <- out |>
      dplyr::mutate(.pair = paste0("(", .data$value, ") ", .data$answer_text)) |>
      dplyr::group_by(.data$variable_name) |>
      dplyr::summarise(answer_text = paste(.data$.pair, collapse = pair_sep), .groups = "drop") |>
      dplyr::arrange(.data$variable_name)
  }

  out
}


