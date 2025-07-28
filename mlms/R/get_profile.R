#' Retrieve Profile Data
#'
#' @description Retrieve pressure, temperature, and water-quality data for site visits.
#'
#' @param site_nm 'character' vector.
#'   Local site name for a MLMS well.
#' @param stime_dt 'POSIXct' or 'character' vector.
#'   Start time for field visit.
#' @param poi 'POSIXct' or 'character' vector of length 2.
#'   Start and end limits on the period of interest.
#' @param time_dt 'POSIXct' or 'character' string.
#'   Estimated field visit time, the closest time in the vector of field visit start times.
#' @param pcode 'character' string.
#'   USGS 5-digit parameter code. For example, the parameter code for Tritium is "07000".
#' @param strings_as_factors 'logical' flag.
#'   Whether character vectors should be converted to factor class.
#'
#' @return A data frame.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso [`plot_profile`] function to plot multilevel data for a site visit.
#'
#' @export
#'
#' @examples
#' d <- get_profile(site_nm = "USGS 133", pcode = "07000")
#' str(d)
#'
#' d <- get_profile(poi = c("2023-01-01", NA), strings_as_factors = FALSE)
#' str(d)

get_profile <- function(site_nm = NULL,
                        stime_dt = NULL,
                        poi = NULL,
                        time_dt = NULL,
                        pcode = NULL,
                        strings_as_factors = TRUE) {

  # load datasets
  utils::data("visits", package = "mlms")

  # check arguments
  checkmate::assert_subset(site_nm, choices = unique(visits$site_nm))
  tz <- attr(visits$stime_dt, "tzone")
  if (checkmate::test_character(stime_dt)) {
    stime_dt <- as.POSIXct(stime_dt, tz = tz)
  }
  checkmate::assert_subset(stime_dt, choices = visits$stime_dt)
  if (checkmate::test_character(poi)) {
    poi <- as.POSIXct(poi, tz = tz)
  }
  if (checkmate::test_string(time_dt)) {
    time_dt <- as.POSIXct(time_dt, tz = tz)
  }
  checkmate::assert_posixct(time_dt, len = 1, null.ok = TRUE)
  checkmate::assert_posixct(poi, len = 2, null.ok = TRUE)
  checkmate::assert_string(pcode, null.ok = TRUE)
  checkmate::assert_flag(strings_as_factors)

  # subset columns of visits dataset
  d <- visits[, c("site_nm", "stime_dt")]

  # filter by site name
  if (!is.null(site_nm)) {
    d <- d[d$site_nm %in% site_nm, ]
  }

  # filter by start time
  if (!is.null(stime_dt)) {
    d <- d[d$stime_dt %in% stime_dt, ]
  }

  # filter by period of interest
  if (!is.null(poi)) {
    if (!is.na(poi[1])) {
      d <- d[d$stime_dt >= poi[1], ]
    }
    if (!is.na(poi[2])) {
      d <- d[d$stime_dt <= poi[2], ]
    }
  }

  # filter by closest start time
  if (!is.null(time_dt)) {
    idx <- abs(visits$stime_dt - time_dt) |> which.min()
    is <- d$stime_dt %in% visits$stime_dt[idx]
    d <- d[is, ]
  }

  # bind profile data
  tbl <- do.call(rbind,
    lapply(d$stime_dt,
      FUN = function(x) {
        get_pf_data(stime_dt = x, pcode = pcode)
      }
    )
  )

  # convert character to factor
  if (strings_as_factors) {
    is <- vapply(tbl, FUN = is.character, FUN.VALUE = logical(1))
    tbl[is] <- lapply(tbl[is], FUN = as.factor)
  }

  tbl
}


# Function to retrieve data for a single profile

get_pf_data <- function(stime_dt, pcode = NULL) {

  # load datasets
  utils::data("ports", package = "mlms")
  utils::data("heads", package = "mlms")
  utils::data("samples", package = "mlms")
  utils::data("zones", package = "mlms")

  # check arguments
  if (checkmate::test_string(stime_dt)) {
    tz <- attr(heads$stime_dt, "tzone")
    stime_dt <- as.POSIXct(stime_dt, tz = tz)
  }
  checkmate::assert_posixct(stime_dt, len = 1, any.missing = FALSE)
  stopifnot("Missing start time" = stime_dt %in% heads$stime_dt)
  checkmate::assert_choice(pcode, choices = unique(samples$pcode), null.ok = TRUE)

  # make table with pressure data
  is <- heads$stime_dt == stime_dt
  cols <- c(
    "site_no",
    "site_nm",
    "stime_dt",
    "press_dt",
    "port_nu",
    "temp_va",
    "total_head_va",
    "replicate_fl"
  )
  tbl <- heads[is, cols]

  # get site name
  site_nm <- tbl$site_nm[1]

  # merge in port data
  cols <- c(
    "site_no",
    "zone_nu",
    "port_depth_va",
    "port_alt_va"
  )
  tbl <- merge(tbl, ports[, cols], by = "site_no")

  # merge in zone data
  is <- zones$site_nm == site_nm & zones$zone_nu %in% tbl$zone_nu
  cols <- c(
    "zone_nu",
    "zone_top_va",
    "zone_bot_va",
    "zone_top_alt_va",
    "zone_bot_alt_va"
  )
  tbl <- merge(tbl, zones[is, cols], by = "zone_nu")

  # sort by port number and time
  idxs <- order(tbl$port_nu, tbl$press_dt, decreasing = c(TRUE, FALSE), method = "radix")
  tbl <- tbl[idxs, ]

  # remove records with duplicated port number
  is <- duplicated(tbl$port_nu)
  tbl <- tbl[!is, ]

  # subset columns
  cols <- c(
    "site_nm",
    "site_no",
    "port_nu",
    "port_depth_va",
    "port_alt_va",
    "zone_nu",
    "zone_top_va",
    "zone_bot_va",
    "zone_top_alt_va",
    "zone_bot_alt_va",
    "stime_dt",
    "press_dt",
    "total_head_va",
    "temp_va",
    "replicate_fl"
  )
  tbl <- tbl[, cols]

  # add sample data
  if (!is.null(pcode)) {

    # filter by site, parameter, and time
    is <- samples$site_nm == site_nm &
      samples$pcode == pcode &
      samples$stime_dt %in% stime_dt
    d <- samples[is, ]
    d <- d[order(d$sample_dt), ]
    is <- paste(d$port_nu, d$stime_dt) |> duplicated()
    d <- d[!is, ]

    # sort by port number and time
    idxs <- order(d$port_nu, d$sample_dt, decreasing = c(TRUE, FALSE), method = "radix")
    d <- d[idxs, ]

    # remove records with duplicated port number and subset columns
    is <- duplicated(d$port_nu)
    cols <- c(
      "site_nm",
      "site_no",
      "port_nu",
      "sample_dt",
      "parm_short_nm",
      "pcode",
      "unit_cd",
      "remark_cd",
      "result_va",
      "lab_li_va",
      "lab_ui_va",
      "dqi_cd",
      "sample_type_cd"
    )
    d <- d[!is, cols]

    # convert factor to character
    is <- vapply(d, FUN = is.factor, FUN.VALUE = logical(1))
    d[is] <- lapply(d[is], FUN = as.character)

    # merge water-quality data
    tbl <- merge(tbl, d, by = c("site_nm", "site_no", "port_nu"), all.x = TRUE, sort = FALSE)
  }

  # sort by port
  idxs <- order(tbl$port_nu, decreasing = TRUE)
  tbl <- tbl[idxs, ]
  rownames(tbl) <- NULL

  tbl
}
