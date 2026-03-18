#' Make an age-length key out of length-at-age data
#'
#' @param laa_data A data.frame with length-at-age data
#' @param sizecol Character string naming the column that holds size data
#' @param autobin Logical. Should the function automatically assign length bins
#' (default is TRUE)
#' @param binwidth Numeric. If \code{autobin = TRUE} this is the width for the
#' size bins
#' @param agecol Character string naming the column that holds age data
#' @param min_age Numeric. All ages less than this value will not be used in
#' ALK
#' @param plus_group Numeric value of the oldest age to include in the ALK. All
#' older individuals will be included in this plus group
#' @param min_age_sample_size Only applicable to alk models. The minimum
#' number of samples that must be in each age group in order to create an alk
#' @param min_total_sample_size Only applicable to alk models. The minimum
#' number of samples that must be in data in order to create an alk
#' @param min_age_groups Only applicable to alk models. The minimum number of
#' age groups that must be in data in order to create an alk
#' @param numcol Character string naming the column that holds numbers data
#' @param warnings Logical. Display warnings (TRUE, default)
#'
#' @export
#'
#' @return A data.frame containing the proportions of records for each size
#' that are at each age.
#'
#' @examples
#' make_alk(laa_data)
make_alk <- function(laa_data,
                     sizecol = "length",
                     autobin = TRUE,
                     binwidth = 1,
                     agecol = "age",
                     min_age = NULL,
                     plus_group = NULL,
                     numcol = NULL,
                     min_age_sample_size = 5,
                     min_total_sample_size =
                       min_age_sample_size * min_age_groups,
                     min_age_groups = 5,
                     warnings = TRUE) {

  laa_data <- check_laa_data(laa_data, quiet = !warnings)
  if (is.null(laa_data)) {
    return(NULL)
  }
  laa_data <-
    laa_data %>%
    rename_laa_cols(size_col = sizecol, age_col = agecol, num_col = numcol) %>%
    sanitize_laa_data() %>%
    adjust_plus_min_ages_df(minage = min_age, pls_grp = plus_group) %>%
    min_age_groups(sub_levels = NULL, min_age_grps = min_age_groups) %>%
    min_count_laa_data(
      sub_levels = NULL,
      min_age_sample_size,
      min_total_sample_size,
      min_age_groups
    )
  laa_data <- check_laa_data(laa_data, quiet = TRUE)
  if (is.null(laa_data)) {
    if (warnings) {
      warning(
        "Your length-at-age data did not have enough samples or age groups.\n",
        "Consider changing min_age_sample_size, min_total_sample_size, or ",
        "min_age_groups."
      )
    }
    return(NULL)
  }
  if (!is.null(min_age)) {
    laa_data <-
      laa_data %>%
      dplyr::filter(.data$age >= min_age)
  }
  if (!is.null(plus_group)) {
    laa_data <-
      laa_data %>%
      dplyr::mutate(age = dplyr::case_when(
        .data$age > plus_group ~ as.integer(plus_group),
        TRUE ~ as.integer(.data$age)
      ))
  }
  if (autobin) {
    if (binwidth <= 1) {
      if (!is.null(numcol)) {
        laa_data <-
          laa_data %>%
          tidyr::uncount(!!rlang::sym(numcol))
      }
      alk_normdist <-
        laa_data %>%
        dplyr::group_by(.data$age) %>%
        dplyr::summarize(
          mean = mean(.data$length, na.rm = TRUE),
          sd = sd(.data$length, na.rm = TRUE)
        ) %>%
        dplyr::mutate(sd = dplyr::case_when(
          .data$sd == 0 ~ 0.3,
          is.na(.data$sd) ~ 0.3,
          TRUE ~ .data$sd
        ))
    } else {
      alk_normdist <- NULL
    }
    laa_data <-
      laa_data %>%
      dplyr::mutate(length = bin_lengths(.data$length, binwidth))
  } else {
    alk_normdist <- NULL
  }
  if (is.null(numcol)) {
    number_at_length <-
      laa_data %>%
      dplyr::count(.data$age, .data$length)
  } else {
    number_at_length <-
      laa_data %>%
      dplyr::group_by(.data$age, .data$length) %>%
      dplyr::summarize(n = sum(!!rlang::sym(numcol)))
  }
  age_proportions <-
    number_at_length %>%
    dplyr::mutate(n = ifelse(is.na(.data$n), 0, .data$n)) %>%
    dplyr::group_by(.data$length) %>%
    dplyr::mutate(prop = .data$n / sum(.data$n)) %>%
    dplyr::select("age", "length", "prop") %>%
    dplyr::ungroup()
  if (!is.null(plus_group)) {
    age_props <-
      age_proportions %>%
      dplyr::mutate(age = dplyr::case_when(
        .data$age >= plus_group ~ paste0("age", plus_group, "+"),
        TRUE ~ paste0("age", .data$age)
      ))
  } else {
    age_props <-
      age_proportions %>%
      dplyr::mutate(age = paste0("age", .data$age))
  }
  alk <-
    age_props %>%
    dplyr::mutate(prop = dplyr::case_when(
      is.na(.data$prop) ~ 0,
      is.nan(.data$prop) ~ 0,
      TRUE ~ .data$prop
    )) %>%
    tidyr::pivot_wider(
      id_cols = "length",
      names_from = "age",
      values_from = "prop",
      values_fill = 0
    ) %>%
    dplyr::arrange(.data$length)
  alk <-
    rename_laa_cols(
      alk, size_col = sizecol, age_col = agecol, num_col = numcol, goback = TRUE
    ) %>%
    assign_alk_attributes(
      size_col = sizecol,
      age_col = agecol,
      autobin = autobin,
      size_bin = binwidth,
      min_age = min_age,
      plus_group = plus_group,
      alk_n = nrow(laa_data),
      classes = "alk",
      dnorm_params = alk_normdist
    )
  return(alk)
}



#' Create a hierarchical age-length key (HALK)
#'
#' This function creates a hierarchically nested age-length key that can be
#' used to estimate age of an organism based on proportion of sampled organisms
#' in each age group.
#'
#' @param data A data.frame with age and size samples
#' @param levels Character vector specifying the levels for HALK creation
#' @param age_col Optional. String of the column name in \code{data} housing
#' age data
#' @param size_col Optional. String of the column name in \code{data} housing
#' size data
#' @param ... Additional arguments passed to \code{\link{make_alk}}
#'
#' @return A \code{\link[tibble]{tibble}} with columns for each level and
#' a column called alk that houses the age-length key for that particular level
#' @export
#'
#' @examples
#' make_halk(spp_data, levels = "spp")
make_halk <- function(data,
                      levels = NULL,
                      age_col = "age",
                      size_col = "length",
                      ...) {
  if (is.null(levels)) {
    if (is_spp_in_data(data)) {
      levels <- add_spp_level(data, levels)
    } else {
      stop("You must provide levels to create a HALK.")
    }
  } else if (xor(is_spp_in_data(data), is_spp_in_levels(levels))) {
    if (is_spp_in_data(data)) {
      levels <- add_spp_level(data, levels)
    } else {
      stop("Each level supplied must match a column in the data")
    }
  } else {
    if (any(!(levels %in% names(data)))) {
      stop("Each level supplied must match a column in the data.")
    }
  }
  drop_na_data <-
    data %>%
    tidyr::drop_na(tidyselect::all_of(levels))
  if (nrow(drop_na_data) != nrow(data)) {
    warning(
      "Some records in `data` contained NA in columns specified in `levels`. ",
      "These records have been removed."
    )
  }
  temp_levels <- levels
  alks <-
    tibble::as_tibble(data[FALSE, ]) %>%
    dplyr::select(tidyselect::all_of(levels))
  for (i in rev(seq_along(temp_levels))) {
    grouping <- rlang::syms(temp_levels)
    level_alks <-
      data %>%
      dplyr::group_by(!!!grouping) %>%
      tidyr::nest() %>%
      dplyr::mutate(alk = purrr::map(
        .data$data,
        make_alk,
        sizecol = size_col,
        agecol = age_col,
        warnings = FALSE,
        ...
      ))
    alks <- dplyr::bind_rows(alks, level_alks)
    temp_levels <- temp_levels[-i]
  }
  alks <-
    alks %>%
    dplyr::filter(!purrr::map_lgl(.data$alk, is.null))
  if (nrow(alks) == 0) {
    warning(
      "Your HALK did not compute. You probably did not have enough \n  ",
      "samples in a particular age group. Either adjust \n  ",
      "min_sample_size or set min_age or plus_group."
    )
    return(NULL)
  } else {
    out <-
      dplyr::select(alks, tidyselect::all_of(levels), "alk") %>%
      dplyr::arrange(!!!rlang::syms(levels)) %>%
      tibble::as_tibble()
    attr(out, "levels") <- levels
    attr(out, "size_col") <- size_col
    attr(out, "age_col") <- age_col
    class(out) <- c("halk_fit", class(out))
    return(out)
  }
}

