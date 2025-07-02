#' Prepare and clean site index file
#'
#' A site index file contains information on when specific ARUs were deployed
#' where. This function cleans a file (csv, xlsx) or data frame in preparation
#' for adding these details to the output of `clean_metadata()`. It can be used
#' to specify missing information according to date, such as GPS lon/lats and
#' site ids.
#'
#' Note that times are assumed to be in 'local' time and a timezone isn't used
#' (and is removed if present, replaced with UTC). This allows sites
#' from different timezones to be processed at the same time.
#'
#' @param site_index (Spatial) Data frame or file path. Site index data
#'   to clean. If file path, must be to a local csv or xlsx file.
#' @param name_site_id Character. Name of the column that contains site ids.
#'   Default `"site_id"`.
#' @param name_aru_id Character. Name of the column that contains ARU ids.
#'   Default `"aru_id"`.
#' @param name_date_time Character. Column name that contains dates or
#'   date/times. Can be vector of two names if there are both 'start' and 'end'
#'   columns. Can be `NULL` to ignore dates. Default `"date"`.
#' @param name_coords Character. Column names that contain longitude and
#'   latitude (in that order). Ignored if `site_index` is spatial. Default
#'   `c("longitude", "latitude")`
#' @param name_extra Character. Column names for extra data to include. If a named
#'  vector, will rename the columns (see examples). Default `NULL`.
#' @param resolve_overlaps Logical. Whether or not to resolve date overlaps by
#'   shifting the start/end dates to noon (default `TRUE`). This assumes that
#'   ARUs are generally *not* deployed/removed at midnight (the official
#'   start/end of a day) and so noon is used as an approximation for when an ARU
#'   was deployed or removed. If possible, use specific deployment times to
#'   avoid this issue.
#'
#' @inheritParams common_docs
#'
#' @return Standardized site index data frame
#' @export
#'
#' @examples
#'
#' s <- clean_site_index(example_sites,
#'   name_aru_id = "ARU",
#'   name_site_id = "Sites",
#'   name_date_time = c("Date_set_out", "Date_removed"),
#'   name_coords = c("lon", "lat")
#' )
#'
#' s <- clean_site_index(example_sites,
#'   name_aru_id = "ARU",
#'   name_site_id = "Sites",
#'   name_date_time = c("Date_set_out", "Date_removed"),
#'   name_coords = c("lon", "lat"),
#'   name_extra = c("plot" = "Plots")
#' )
#'
#' # Without dates
#' eg <- dplyr::select(example_sites, -Date_set_out, -Date_removed)
#' s <- clean_site_index(eg,
#'   name_aru_id = "ARU",
#'   name_site_id = "Sites",
#'   name_date_time = NULL,
#'   name_coords = c("lon", "lat"),
#'   name_extra = c("plot" = "Plots")
#' )
#'
clean_site_index <- function(site_index,
                             name_aru_id = "aru_id",
                             name_site_id = "site_id",
                             name_date_time = "date",
                             name_coords = c("longitude", "latitude"),
                             name_extra = NULL,
                             resolve_overlaps = TRUE,
                             quiet = FALSE) {
  # Checks
  check_df_file(site_index)
  check_text(name_aru_id, n = 1)
  check_text(name_site_id, n = 1)
  check_text(name_date_time, n = c(1, 2), not_null = FALSE)
  check_text(name_extra, not_null = FALSE)
  check_logical(resolve_overlaps)

  is_sf <- inherits(site_index, "sf")

  if (!is_sf) {
    check_text(name_coords, n = 2)
  } else {
    check_points(site_index)
    name_coords <- NULL
  }

  # Format different inputs
  if (is_sf) {
    # SF - (create tibble sf https://github.com/r-spatial/sf/issues/951#issuecomment-455735564)
    site_index <- dplyr::as_tibble(site_index) |> sf::st_as_sf()
  } else if (is.data.frame(site_index)) {
    # Data frames
    site_index <- suppressMessages(readr::type_convert(site_index)) |>
      dplyr::as_tibble()
  } else {
    # Files
    type <- fs::path_ext(site_index)
    check_ext(type, c("csv", "xlsx"))

    # Let readr do the index checking
    if (type == "csv") {
      site_index <- readr::read_csv(site_index,
        progress = FALSE,
        show_col_types = FALSE
      )
    } else if (type == "xlsx") {
      if (is_installed("readxl")) {
        site_index <- readxl::read_excel(site_index, progress = FALSE)
      } else {
        abort(c("package \"readxl\" is required to import xlsx files",
          "i" = "Install using \"install.packages(\'readxl\')\"
                     or convert to a 'csv' file and re-import"
        ))
      }
    }
  }

  site_index <- dplyr::rename_with(site_index, tolower)

  # Check cols
  check_names(site_index, c(
    name_site_id, name_date_time, name_aru_id, name_coords,
    name_extra
  ),
  extra = "See ?clean_site_index"
  )

  # Check dates
  check_dates(site_index, name_date_time)

  if (is.null(name_date_time)) {
    d <- dt <- NULL
  } else if (length(name_date_time) == 1) {
    dt <- "date_time"
    d <- "date"
  } else {
    dt <- c("date_time_start", "date_time_end")
    d <- c("date_start", "date_end")
  }

  # Prepare for renaming

  if (length(names(name_extra)) == 0) {
    name_extra <- stats::setNames(nm = name_extra)
  }

  cols <- c(
    "site_id" = name_site_id,
    "aru_id" = name_aru_id,
    stats::setNames(name_date_time, dt),
    if (!is.null(name_coords)) stats::setNames(name_coords, c("longitude", "latitude")),
    name_extra
  ) |>
    tolower()

  # Check and force time zones to UTC if required
  site_index <- check_UTC(site_index, cols[dt])

  # Rename and fix dates
  site_index <- site_index |>
    # Grab and rename columns
    dplyr::select(dplyr::all_of(cols)) |>
    # Get dates
    dplyr::mutate(
      dplyr::across(dplyr::all_of(dt), ~ lubridate::as_datetime(.x) |> lubridate::force_tz("UTC")),
      dplyr::across(dplyr::all_of(dt), lubridate::as_date, .names = "{d}")
    ) |>
    dplyr::relocate(dplyr::any_of(c("longitude", "latitude", "geometry")),
      .after = dplyr::last_col()
    ) |>
    dplyr::relocate(dplyr::any_of(names(name_extra)), .after = dplyr::last_col())

  # For date ranges, check if only using dates
  if (resolve_overlaps &&
    length(dt) == 2 &&
    all(site_index$date_time_start == site_index$date_start) &&
    all(site_index$date_time_end == site_index$date_end)) {
    by_site <- dplyr::group_by(site_index, .data$site_id) |>
      dplyr::filter(.data$date_time_end %in% .data$date_time_start) |>
      nrow()
    by_aru <- dplyr::group_by(site_index, .data$aru_id) |>
      dplyr::filter(.data$date_time_end %in% .data$date_time_start) |>
      nrow()

    if (by_site > 0 | by_aru > 0) {
      lubridate::hour(site_index$date_time_start) <- 12
      lubridate::hour(site_index$date_time_end) <- 12

      if (!quiet) {
        inform(
          c(
            "There are overlapping date ranges",
            "Shifting start/end times to 'noon'",
            # "Use `by_date = \"date_time\"` in `add_sites()`",
            "Skip this with `resolve_overlaps = FALSE`"
          )
        )
      }
    }
  }

  # Arrange
  # - Match order of starting data
  # - No changes from arrange, joins, or groupings, so no need to arrange

  site_index
}

#' Check and clean GPS data
#'
#' Check and clean GPS data from ARU logs. GPS points are checked for obvious
#' problems (expected range, distance cutoffs and timing) then attached to the
#' meta data frame. Note that it is often safer and more reliable to create
#' your own Site Index file including site ids, and GPS coordinates. This file
#' can be cleaned and prepared with `clean_site_index()` instead.
#'
#' If checking for a maximum distance (`dist_cutoff`) among GPS points within a
#' group (`dist_by`), the returned data frame will include a column `max_dist`,
#' which represents the largest distance among points within that group.
#'
#' @param meta Data frame. Output of `clean_metadata()`.
#' @param dist_cutoff Numeric. Maximum distance (m) between GPS points within a
#'   site. Default is 100m but can be set to `Inf` to skip.
#' @param dist_crs Numeric. Coordinate Reference System to use when calculating
#'   distance (should be one with m).
#' @param dist_by Character. Column which identifies sites within which to
#'   compare distance among GPS points. Only valid if `dist_cutoff` is not
#'   `Inf`.
#' @param verbose Logical. Show extra loading information. Default `FALSE`.
#'
#' @inheritParams common_docs
#'
#' @return Data frame of site-level metadata.
#' @export
#'
#' @examplesIf dir.exists("my_project")
#' m <- clean_metadata(project_dir = "my_project")
#' g <- clean_gps(meta = m)
clean_gps <- function(meta = NULL,
                      dist_cutoff = 100, dist_crs = 3161,
                      dist_by = c("site_id", "aru_id"),
                      quiet = FALSE, verbose = FALSE) {
  # Checks
  check_data(meta, type = "meta", ref = "clean_metadata()")
  check_num(dist_cutoff, n = 1)
  check_num(dist_crs, n = 1)
  check_text(dist_by)
  check_logical(verbose)
  # meta <- check_UTC(meta) # Technically not needed at this step... worth doing anyway?


  # Load, combine and clean gps files
  gps <- clean_gps_files(meta, quiet, verbose)

  # Check distances (skips if dist_cutoff = Inf)
  gps <- check_gps_dist(gps,
    crs = dist_crs, dist_cutoff = dist_cutoff,
    dist_by = dist_by, quiet = quiet
  )

  # Flag problems
  n <- nrow(gps)
  f_dt <- sum(is.na(gps$date_time))
  f_coord <- sum(is.na(gps$longitude) | is.na(gps$latitude))
  f_zero <- sum(gps$longitude == 0 | gps$latitude == 0, na.rm = TRUE)
  f_gpx <- sum(gps$gps_ext == "gpx" &
    is.na(gps$date_time) & is.na(gps$date) &
    is.na(gps$longitude) & is.na(gps$latitude))
  f_header <- sum(gps$problems_dt | gps$problems_tm | gps$problems_ll, na.rm = TRUE)

  if (any(c(f_dt, f_coord, f_zero, f_gpx, f_header) > 0)) {
    msg <- c("Identified possible problems with GPS extraction:")
    msg <- c(msg, report_missing(f_dt, n, "date/times"))
    msg <- c(msg, report_missing(f_coord, n, "coordinates"))
    if (f_zero > 0) {
      msg <- c(
        msg,
        "Some coordinates detected as zero (can occur in Song Meters if not set)",
        paste0("Replacing zero coordinates with NA (", f_zero, "/", n, ")")
      )

      gps <- dplyr::mutate(
        gps, dplyr::across(c("longitude", "latitude"), ~ dplyr::na_if(.x, 0))
      )
    }

    msg <- c(msg, report_missing(f_header, n, "headers (in text GPS files)"))
    msg <- c(msg, report_missing(f_gpx, n, "GPX files", "extracted"))

    inform(msg)
  }

  # Arrange
  # - Match order of starting data (meta)
  # - Order by path order, next by date in GPS data (for multiple rows of data)
  dplyr::arrange(
    gps,
    match(.data[["path"]], meta$path[meta$type == "gps"]),
    .data[["date"]]
  ) |>
    dplyr::select(-dplyr::starts_with("problems_"))
}

clean_gps_files <- function(meta, quiet, verbose, call = caller_env()) {
  gps <- dplyr::filter(meta, .data$type == "gps")

  if (nrow(gps) == 0) {
    abort(
      "No GPS data provided and no GPS log files recorded in `meta`",
      call = call
    )
  }

  if (!quiet) {
    inform(
      c(
        "Note: GPS log files can be unreliable... ",
        "Consider supplying your own GPS records and using `clean_site_index()`"
      )
    )
  }

  if (!quiet) {
    p1 <- list(
      show_after = 0,
      format = "Loading GPS files {cli::pb_percent} [{cli::pb_elapsed}]"
    )
    p2 <- list(
      show_after = 0,
      format = "Formating GPS files {cli::pb_percent} [{cli::pb_elapsed}]"
    )
  } else {
    p1 <- p2 <- FALSE
  }

  gps |>
    # Check columns and get skips for non-GPX files
    check_gps_files() |>
    dplyr::mutate(

      # Read files
      gps_data = purrr::pmap(
        list(.data$path, .data$skip, .data$gps_ext),
        ~ load_gps(..1, ..2, ..3, verbose = verbose),
        .progress = p1
      ),

      # Format data
      gps_data = purrr::map2(
        .data$gps_data, .data$gps_ext, fmt_gps,
        .progress = p2
      )
    ) |>
    # Clean up
    dplyr::select(-"date_time", -"date", -"skip") |>
    tidyr::unnest("gps_data", keep_empty = TRUE)
}

#' Load GPS from text or gpx
#'
#' @param path Character. File to load
#' @param skip Numeric. Number of lines to skip in text GPS files
#' @param gps_ext Character. Extension of the GPS file (to identify GPX files)
#' @param verbose Logical. Whether to be extra chatty when loading files
#'
#' @noRd
load_gps <- function(path, skip = NA, gps_ext, verbose = FALSE) {
  if (gps_ext == "gpx") {
    g <- try(sf::st_read(path, layer = "waypoints", quiet = TRUE), silent = TRUE)
  } else {
    if (is.na(skip)) {
      g <- try(stop("No skip", call. = FALSE), silent = TRUE)
    } else {
      g <- try(
        readr::read_csv(path,
          skip = skip - 1, show_col_types = verbose,
          guess_max = Inf,
          name_repair = "unique_quiet",
          progress = FALSE
        ),
        silent = TRUE
      )
    }
  }
  g
}

#' Check text GPS files
#'
#' - Read first 5 lines
#' - Check that can identify column headers
#' - Get the number of lines to skip
#' - Return skips and any problems
#'
#' @param gps_files Character vector. All GPS files (gpx and text)
#'
#' @noRd
check_gps_files <- function(gps_files) {
  # Get text-based GPS logs (i.e. anything but GPX files)
  gps_files <- dplyr::mutate(gps_files,
    gps_ext = tolower(fs::path_ext(.data$path))
  )

  gps_txt <- gps_files |>
    dplyr::filter(.data$gps_ext != "gpx") |>
    dplyr::pull(.data$path)

  if (length(gps_txt) > 0) {
    lines <- stats::setNames(nm = gps_txt) |>
      purrr::imap(~ readr::read_lines(.x, n_max = 5, progress = FALSE))

    # Set patterns
    opts <- getOption("ARUtools")
    pattern_date <- stringr::regex(opts$pat_gps_date, ignore_case = TRUE)
    pattern_time <- stringr::regex(opts$pat_gps_time, ignore_case = TRUE)
    pattern_coords <- stringr::regex(paste0(opts$pat_gps_coords, collapse = "|"),
      ignore_case = TRUE
    )

    # Get skips
    skips <- purrr::map(
      lines,
      ~ stringr::str_which(.x, pattern_coords) |> dplyr::first()
    )

    # Check for columns
    dt <- purrr::map_lgl(lines, ~ !any(stringr::str_detect(.x, pattern_date)))
    tm <- purrr::map_lgl(lines, ~ !any(stringr::str_detect(.x, pattern_time)))
    ll <- is.na(skips) | length(skips) == 0

    # Skip if not detected
    skips[dt | tm | ll] <- NA_integer_

    # Get skips and problems for future reporting
    gps_files <- dplyr::tibble(
      path = names(skips),
      skip = unlist(skips),
      problems_dt = dt,
      problems_tm = tm,
      problems_ll = ll
    ) |>
      dplyr::full_join(gps_files, by = "path")
  } else {
    gps_files <- dplyr::mutate(gps_files,
      skip = NA_integer_,
      problems_dt = FALSE,
      problems_tm = FALSE,
      problems_ll = FALSE
    )
  }
  gps_files
}


#' Check column for evidence of coordinate directions
#'
#' Looks for a single character pattern in the column (usually "N" or "E")
#' If found, returns TRUE to identify column as a coordinate direction column
#'
#' @param col Vector to check
#' @param pattern Character pattern to look for
#'
#' @noRd
coord_dir <- function(col, pattern) {
  # Not all missing
  !all(is.na(col)) &
    # all a direction pattern
    all(stringr::str_detect(col, paste0("^[", pattern, "]{1}$")), na.rm = TRUE)
}

#' Format loaded GPS coordinates
#'
#' - If there was a loading error, return empty data frame
#' - GPX uses `fmt_gps_gpx()`
#' - Otherwise uses `fmt_gps_txt()`
#'
#' @param df GPS data frame to format
#' @param gps_ext GPS file extension used to identify GPX files
#'
#' @noRd
fmt_gps <- function(df, gps_ext) {
  if (inherits(df, "try-error")) {
    g <- fmt_gps_empty()
  } else if (gps_ext == "gpx") {
    g <- fmt_gps_gpx(df)
  } else {
    g <- fmt_gps_txt(df)
  }

  dplyr::select(g, "longitude", "latitude", "date", "date_time")
}

fmt_gps_empty <- function() {
  dplyr::tibble(
    date = lubridate::NA_Date_,
    date_time = lubridate::NA_POSIXct_,
    longitude = NA_real_,
    latitude = NA_real_
  )
}

fmt_gps_gpx <- function(df) {
  df |>
    sf::st_drop_geometry() |>
    dplyr::bind_cols(sf::st_coordinates(df)) |>
    dplyr::select("date_time" = "time", "longitude" = "X", "latitude" = "Y") |>
    dplyr::mutate(
      date_time = lubridate::as_datetime(.data$date_time),
      date = lubridate::as_date(.data$date_time),
      date_time = dplyr::if_else(lubridate::year(.data$date_time) == -1,
        lubridate::NA_POSIXct_,
        .data$date_time
      ),
      date = dplyr::if_else(lubridate::year(.data$date) == -1,
        lubridate::NA_Date_,
        .data$date
      )
    )
}


fmt_gps_txt <- function(df) {
  opts <- getOption("ARUtools")

  df_fmt <- df |>
    # Omit Headings appearing at odd places
    dplyr::filter(.data[[names(df)[1]]] != names(df)[[1]]) |>
    dplyr::rename(
      "longitude" = dplyr::matches(opts$pat_gps_coords[1]),
      "latitude" = dplyr::matches(opts$pat_gps_coords[2]),
      "date" = dplyr::matches(opts$pat_gps_date),
      "time" = dplyr::matches(opts$pat_gps_time)
    ) |>
    # Format times
    dplyr::mutate(
      date_time_chr = paste(.data$date, .data$time),
      date_time = lubridate::parse_date_time(
        .data$date_time_chr,
        orders = c("Ymd HMS", "dmY HMS")
      ),
      date = lubridate::as_date(.data$date_time),
    )

  # Fix coords - Check and apply -/+ if N/S/E/W columns present
  dir <- dplyr::select(df_fmt, dplyr::where(~ coord_dir(.x, "NnSsEeWw")))
  if (ncol(dir) > 0) {
    df_fmt <- df_fmt |>
      dplyr::rename_with(~"ns", .cols = dplyr::where(~ coord_dir(.x, "NnSs"))) |>
      dplyr::rename_with(~"ew", .cols = dplyr::where(~ coord_dir(.x, "EeWw"))) |>
      # Define direction shift
      dplyr::mutate(dplyr::across(
        dplyr::any_of(c("ns", "ew")),
        ~ stringr::str_replace_all(
          tolower(.x),
          c(
            "w" = "-", "e" = "",
            "s" = "-", "n" = ""
          )
        )
      )) |>
      # Apply direction shift (i.e. merge)
      tidyr::unite("longitude", dplyr::any_of(c("ew", "longitude")), sep = "") |>
      tidyr::unite("latitude", dplyr::any_of(c("ns", "latitude")), sep = "")
  }

  # Clean up
  df_fmt |>
    dplyr::mutate(
      longitude = as.numeric(.data$longitude),
      latitude = as.numeric(.data$latitude)
    ) |>
    dplyr::select("longitude", "latitude", "date", "date_time")
}


#' Check distances between points from GPS log
#'
#' @param gps Data frame of gps sites and coordinates. Requires longitude,
#'   latitude, and any columns in `dist_by`.
#' @param crs Numeric. CRS to use for measuring distances. Should be in metres
#' @param dist_cutoff Distance cutoff in meters. Can be set to Inf to avoid this
#'   check.
#' @param dist_by Character. Column names to use in grouping GPS points before
#'   calculating within group distances.
#' @param quiet Logical. Suppress non-essential messages
#'
#' @return Returns data frame with maximum distances between gps points within a
#' group.
#'
#' @noRd
check_gps_dist <- function(gps, crs, dist_cutoff, dist_by, quiet = FALSE) {
  if (dist_cutoff < Inf) {
    max_dist <- gps |>
      dplyr::filter(dplyr::if_all(
        dplyr::any_of(c("longitude", "latitude", dist_by)), ~ !is.na(.)
      ))

    if (nrow(max_dist) == 0) {
      if (!is.null(dist_by)) {
        dist_by <- paste0(", `", paste0(dist_by, collapse = "`, `"), "`")
      } else {
        dist_by <- ""
      }
      if (!quiet) {
        inform(
          c(
            "Skipping distance check:",
            paste0(
              "All records missing at least one of ",
              "`longitude`, `latitude`", dist_by
            )
          )
        )
      }
    } else {
      n <- max_dist |>
        dplyr::select(dplyr::all_of(c("longitude", "latitude", dist_by))) |>
        dplyr::distinct() |>
        dplyr::count(dplyr::across(dplyr::all_of(dist_by)))

      if (all(n$n == 1)) {
        if (!quiet) {
          inform(
            c(
              "Skipping distance check:",
              paste0(
                "No records with more than one set of coordinates per unique `",
                paste0(dist_by, collapse = "`/`"), "`"
              )
            )
          )
        }
      } else {
        max_dist <- max_dist |>
          dplyr::select(dplyr::all_of(c(dist_by, "longitude", "latitude"))) |>
          dplyr::distinct() |>
          sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
          sf::st_transform(crs) |>
          dplyr::group_by(dplyr::across(dplyr::all_of(dist_by))) |>
          dplyr::summarize(
            max_dist = max(sf::st_distance(.data$geometry, .data$geometry)),
            .groups = "drop"
          ) |>
          sf::st_drop_geometry()

        if (any(max_dist$max_dist > units::set_units(dist_cutoff, "m"))) {
          warn(
            c("Within site distances are greater than cutoff",
              "x" = paste0(
                "Distances among ARUs within a site must be less than ",
                "`dist_cutoff` (currently ", dist_cutoff, "m)"
              ),
              "i" = "Set `dist_cutoff` to `Inf` to skip this check (e.g. moving ARUs)"
            )
          )
        }
        gps <- dplyr::left_join(gps, max_dist, by = dist_by)
      }
    }
  }
  gps
}
