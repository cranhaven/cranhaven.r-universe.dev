#' Extract and clean ARU metadata from file names
#'
#' Using regular expressions, metadata is extracted from file names and
#' directory structure, checked and cleaned.
#'
#' Note that times are extracted by first combining the date, date/time
#' separator and the time patterns. This means that if there is a problem with
#' this combination, dates might be extracted but date/times will not. This
#' mismatch can be used to determine which part of a pattern needs to be
#' tweaked.
#'
#' See `vignette("customizing", package = "ARUtools")` for details on
#' customizing `clean_metadata()` for your project.
#'
#' @param file_type Character. Type of file (extension) to summarize. Default
#'   wav.
#' @param pattern_site_id Character. Regular expression to extract site ids. See
#'   `create_pattern_site_id()`. Can be a vector of multiple patterns to match.
#' @param pattern_aru_id Character. Regular expression to extract ARU ids. See
#'   `create_pattern_aru_id()`. Can be a vector of multiple patterns to match.
#' @param pattern_date Character. Regular expression to extract dates. See
#'   `create_pattern_date()`. Can be a vector of multiple patterns to match.
#' @param pattern_time Character. Regular expression to extract times. See
#'   `create_pattern_time()`. Can be a vector of multiple patterns to match.
#' @param pattern_dt_sep Character. Regular expression to mark separators
#'   between dates and times. See `create_pattern_dt_sep()`.
#' @param pattern_tz_offset Character. Regular expression to extract time zone
#'     offsets from file names. See. `create_pattern_tz_offset()`.
#' @param order_date Character. Order that the date appears in. "ymd"
#'   (default), "mdy", or "dmy". Can be a vector of multiple patterns to match.
#'
#' @inheritParams common_docs
#'
#' @return Data frame with extracted metadata
#'
#' @examples
#' clean_metadata(project_files = example_files)
#' clean_metadata(project_files = example_files, subset = "P02")
#'
#' @export
clean_metadata <- function(
    project_dir = NULL,
    project_files = NULL,
    file_type = "wav",
    subset = NULL,
    subset_type = "keep",
    pattern_site_id = create_pattern_site_id(),
    pattern_aru_id = create_pattern_aru_id(),
    pattern_date = create_pattern_date(),
    pattern_time = create_pattern_time(),
    pattern_dt_sep = create_pattern_dt_sep(),
    pattern_tz_offset = create_pattern_tz_offset(), #"[\\+,\\-]\\d{4}"
    order_date = "ymd",
    quiet = FALSE) {
  # Checks
  check_text(project_dir, not_null = FALSE, n = 1)
  check_text(project_files, not_null = FALSE)
  check_text(file_type, n = 1)
  check_text(subset, not_null = FALSE, n = 1)
  check_text(subset_type, n = 1)
  check_text(pattern_site_id)
  check_text(pattern_aru_id)
  check_text(pattern_date)
  check_text(pattern_time)
  check_text(pattern_dt_sep)
  check_text(pattern_tz_offset)
  check_text(order_date)
  check_logical(quiet)


  # Prepare patterns
  file_type_pattern <- stringr::regex(paste0(file_type, "$"), ignore_case = TRUE)

  pattern_site_id <- pat_collapse(pattern_site_id)
  pattern_aru_id <- pat_collapse(pattern_aru_id)
  pattern_date <- pat_collapse(pattern_date)
  pattern_time <- pat_collapse(pattern_time)
  pattern_dt_sep <- pat_collapse(pattern_dt_sep)
  pattern_tz_offset <- pat_collapse(pattern_tz_offset)


  pattern_date_time <- paste0(pattern_date, pattern_dt_sep, pattern_time)

  # Get file lists
  if (!is.null(project_dir)) {
    if (!is.null(project_files)) {
      warn("`project_dir` overrides `project_files`")
    }
    if (!quiet) inform("Fetching file list...")
    project_files <- list_files(project_dir, subset, subset_type,
      type = "file"
    )
  } else if (!is.null(subset)) {
    project_files <- stringr::str_subset(project_files, subset,
      negate = subset_type == "omit"
    )
  } else if (is.null(project_files)) {
    abort("Must provide one of `project_dir` or `project_files`")
  }
  f_ext <- fs::path_ext(project_files)
  # Check for files (either zero or all directories)
  if (length(project_files) == 0 || all(f_ext == "") ) {#all(fs::is_dir(project_files))) {
    if (is.null(subset)) {
      msg <- "`project_dir`"
    } else {
      msg <- "`project_dir`/`subset`/`subset_type` combination"
    }

    abort(c(
      paste0("There are no files in the ", msg, " you have specified. Note:"),
      "i" = "Paths are case-sensitive",
      "i" = "Check folders using `list.dirs(path = PROJECT_DIR)`",
      "i" = "Check for files using `count_files(project_dir = PROJECT_DIR)`"
    ))
  }

  # Check for file types
  n_ext <- sum(stringr::str_detect(f_ext, file_type_pattern))
  if (n_ext == 0) {
    abort(c(glue::glue("Did not find any '{file_type}' files."),
      "i" = "Use `file_type` to change file extension for sound files",
      "i" = "Check `project_dir`/`project_files` are correct"
    ))
  }

  # Collect non-file-type files
  extra <- stringr::str_subset(project_files, file_type_pattern, negate = TRUE)
  log <-  stringr::str_subset(extra, stringr::regex("logfile", ignore_case = TRUE))
  gps <- stringr::str_subset(extra, stringr::regex("gps|summary", ignore_case = TRUE))
  focal <- stringr::str_subset(project_files, file_type_pattern)

  # Set up file path metadata
  meta <- dplyr::tibble(
    dir = fs::path_dir(focal),
    file_name = fs::path_file(focal),
    type = tolower(fs::path_ext(focal))
  )

  if (length(gps) > 0) {
    meta <- meta |>
      dplyr::add_row(
        dir = fs::path_dir(gps),
        file_name = fs::path_file(gps),
        type = "gps"
      )
  }

  if (length(log) > 0) {
    meta <- meta |>
      dplyr::add_row(
        dir = fs::path_dir(log),
        file_name = fs::path_file(log),
        type = "log"
      )
  }


if (!quiet) inform("Extracting ARU info...")

  # Extract ARU metadata -----------------------
  meta <- meta |>
    dplyr::mutate(
      path = file.path(.data$dir, .data$file_name),
      aru_id = stringr::str_extract(.data$file_name, pattern_aru_id),
      aru_id = dplyr::if_else(is.na(.data$aru_id),
        stringr::str_extract(.data$dir, pattern_aru_id),
        .data$aru_id
      ),
      aru_id = dplyr::if_else(is.na(.data$aru_id),
                              stringr::str_extract(.data$path, pattern_aru_id),
                              .data$aru_id
      )
    )

  meta <- dplyr::bind_cols(meta, guess_ARU_type(meta$path))

  meta <- dplyr::mutate(meta, site_id = stringr::str_extract(.data$dir, .env$pattern_site_id))

  pattern_non_date <- paste0(
    "(", pattern_site_id, ")|(",
    pattern_aru_id, ")|(",
    paste0("(", get_pattern("pattern_aru_type"), ")", collapse = "|"),
    ")"
  )


  # Extract Date/time --------------------------
  if (!quiet) inform("Extracting Dates and Times...")
  meta <- meta |>
    dplyr::mutate(
      file_left = stringr::str_remove_all(.data$file_name, pattern_non_date),
      dir_left = stringr::str_remove_all(.data$dir, pattern_non_date),
      # Extract offsets
      tz_offset = stringr::str_extract(.data$file_left, .env$pattern_tz_offset),

      # Try file name
      date_time_chr = stringr::str_extract(.data$file_left, .env$pattern_date_time),
      # Try dir name
      date_time_chr = dplyr::if_else(
        is.na(.data$date_time_chr),
        stringr::str_extract(.data$dir_left, .env$pattern_date_time),
        .data$date_time_chr
      ),
      # date_time_chr = dplyr::if_else(
      #   is.na(.data$tz_offset),
      #   .data$date_time_chr,
      #   paste(.data$date_time_chr, .data$tz_offset, sep = "")
      # ),
      # Get date_times
      # Not implementing at this time as does not work with mix of
      # time zones
      # date_time = lubridate::parse_date_time(
      #   .data$date_time_chr,
      #   orders = dplyr::if_else(is.na(.data$tz_offset),
      #                          paste(order_date, "HMS"),
      #                          paste(order_date, "HMS%z")),
      #   truncated = 1
      # ),
      date_time = lubridate::parse_date_time(
        .data$date_time_chr,
        orders = paste(order_date, "HMS"),
        truncated = 1
      ),



      date = lubridate::as_date(.data$date_time)
    )

  if (any(is.na(meta$date))) {
    missing <- meta |>
      dplyr::filter(is.na(.data$date)) |>
      dplyr::mutate(
        # Try file name
        date_chr = stringr::str_extract(.data$file_left, .env$pattern_date),
        # Try dir name
        date_chr = dplyr::if_else(
          is.na(.data$date_chr),
          stringr::str_extract(.data$dir_left, .env$pattern_date),
          .data$date_chr
        ),
        date = lubridate::parse_date_time(.data$date_chr,
          orders = order_date,
          quiet = TRUE
        ),
        date = lubridate::as_date(.data$date)
      ) |>
      dplyr::select("path", "date")

    if (any(!is.na(missing$date))) {
      # Add dates where missing
      meta <- dplyr::rows_patch(meta, missing, by = "path")
    }
  }

  # Report on details -------------------------
  # Extra files
  if (length(extra[!extra %in% c(gps, log)]) > 0) {
    inform(
      c("!" = paste0(
        "Omitted ", length(extra[!extra %in% c(gps, log)]), " extra, non-",
        file_type, "/GPS files"
      ))
    )
  }

  if (length(log) > 0) {
    inform(c("!" = paste0("Detected ", length(log), " log files")))
  }

  if (length(gps) > 0) {
    inform(c("!" = paste0("Detected ", length(gps), " GPS logs")))
  }

  # Flag problems
  f <- dplyr::filter(meta, .data$type == "wav")
  n <- nrow(f)
  f_d <- sum(is.na(f$date))
  f_dt <- sum(is.na(f$date_time))
  f_type <- sum(is.na(f$aru_type))
  f_id <- sum(is.na(f$aru_id))
  f_site <- sum(is.na(f$site_id))
  f_tz <- sum(is.na(f$tz_offset))

  if (any(c(f_d, f_dt, f_type, f_id, f_site) > 0)) {
    msg <- c("Identified possible problems with metadata extraction:")
    msg <- c(msg, report_missing(f_d, n, "dates"))
    msg <- c(msg, report_missing(f_dt, n, "times"))
    msg <- c(msg, report_missing(f_type, n, "ARU types"))
    msg <- c(msg, report_missing(f_id, n, "ARU ids"))
    msg <- c(msg, report_missing(f_site, n, "sites"))
    # At this point, no need to include time zones in reports
    # as not currently using them.
    # msg <- c(msg, report_missing(f_tz, n, "time zones"))
    if(!quiet & f_type) msg <- c(msg,
      "i" = "Try `clean_logs() to detect aru_type or
          explore the `ARUtoolsExtra` package for more options."
    )
    inform(msg)

  }

  # Arrange ----------------------------------
  # - Match order of starting data
  # - By project_files if provided
  # - Sorted by path name if project_dir
  if (is.null(project_dir) & !is.null(project_files)) {
    meta <- dplyr::arrange(meta, match(.data$path, project_files))
  } else {
    meta <- dplyr::arrange(meta, .data$path)
  }

  dplyr::select(meta, -"file_left", -"dir_left", -"date_time_chr", -"dir")
}
