#' Summarize DAS effort
#'
#' Chop DAS data into effort segments
#'
#' @param x an object of class \code{das_df},
#'   or a data frame that can be coerced to class \code{das_df}
#' @param method character; method to use to chop DAS data into effort segments
#'   Can be "condition", "equallength", "section",
#'   or any partial match thereof (case sensitive)
#' @param conditions character vector of names of conditions to include in segdata output.
#'   These values must be column names from the output of \code{\link{das_process}},
#'   e.g. 'Bft', 'SwellHght', etc.
#'   If \code{method == "condition"}, then these also are the conditions which
#'   trigger segment chopping when they change. Only the following conditions can be used for chopping:
#'   'Bft', 'SwellHght', 'RainFog', 'HorizSun', 'VertSun', 'Glare', 'Vis', 'Course', 'SpdKt'
#' @param strata.files list of path(s) of the CSV file(s) with points defining each stratum.
#'   The CSV files must contain headers and be a closed polygon.
#'   The list should be named; see the Details section.
#'   If \code{NULL} (the default), then no effort segments are not classified by strata.
#' @param distance.method character;
#'   method to use to calculate distance between lat/lon coordinates.
#'   Can be "greatcircle", "lawofcosines", "haversine", "vincenty",
#'   or any partial match thereof (case sensitive).
#'   Default is "greatcircle"
#' @param seg0.drop logical; flag indicating whether or not to drop segments
#'   of length 0 that contain no sighting (S, K, M, G, t) events.
#'   Default is \code{FALSE}
#' @param comment.drop logical; flag indicating if comments ("C" events)
#'   should be ignored (i.e. position information should not be used)
#'   when segment chopping. Default is \code{FALSE}
#' @param event.touse character vector of events to use to determine
#'   segment lengths; overrides \code{comment.drop}.
#'   If \code{NULL} (the default), then all on effort events are used.
#'   If used, this argument must include at least R, E, S, and A events,
#'   and cannot include ? or 1:8 events
#' @param num.cores Number of CPUs to over which to distribute computations.
#'   Defaults to \code{NULL}, which uses one fewer than the number of cores
#'   reported by \code{\link[parallel]{detectCores}}.
#'   Using 1 core likely will be faster for smaller datasets
#' @param ... arguments passed to the specified chopping function,
#'   such as \code{seg.km} or \code{seg.min.km}
#'
#' @details This is the top-level function for chopping processed DAS data
#'   into modeling segments (henceforth 'segments'), and assigning sightings
#'   and related information (e.g., weather conditions) to each segment.
#'   This function returns data frames with all relevant information for the
#'   effort segments and associated sightings ('segdata' and 'sightinfo', respectively).
#'   Before chopping, the DAS data is filtered for events (rows) where either
#'   the 'OnEffort' column is \code{TRUE} or the 'Event' column "E".
#'   In other words, the data is filtered for continuous effort sections (henceforth 'effort sections'),
#'   where effort sections run from "R" to "E" events (inclusive),
#'   and then passed to the chopping function specified using \code{method}.
#'   Note that while B events immediately preceding an R are on effort,
#'   they are ignored during effort chopping.
#'   In addition, all on effort events (other than ? and numeric events)
#'   with \code{NA} DateTime, Lat, or Lon values are verbosely removed.
#'
#'   If \code{strata.files} is not \code{NULL}, then the effort lines
#'   will be split by the user-provided stratum (strata).
#'   In this case, a column 'stratum' will be added to the end of the segdata
#'   data frame with the user-provided name of the stratum that the segment was in,
#'   or \code{NA} if the segment was not in any of the strata.
#'   If no name was provided for the stratum in \code{strata.files},
#'   then the value will be "Stratum#",
#'   where "#" is the index of the applicable stratum in \code{strata.files}.
#'   While the user can provide as many strata as they want,
#'   these strata can share boundaries but they cannot overlap.
#'   See \code{\link{das_effort_strata}} for more details.
#'
#'   The following chopping methods are currently available:
#'   "condition", "equallength", and "section.
#'   When using the "condition" method, effort sections are chopped
#'   into segments every time a condition changes,
#'   thereby ensuring that the conditions are consistent across the entire segment.
#'   See \code{\link{das_chop_condition}} for more details about this method,
#'   including arguments that must be passed to it via the argument \code{...}
#'
#'   The "equallength" method consists of
#'   chopping effort sections into equal-length segments of length \code{seg.km},
#'   and doing a weighted average of the conditions for the length of that segment.
#'   See \code{\link{das_chop_equallength}} for more details about this method,
#'   including arguments that must be passed to it via the argument \code{...}
#'
#'   The "section" method involves 'chopping' the effort into continuous effort sections,
#'   i.e. each continuous effort section is a single effort segment.
#'   See \code{\link{das_chop_section}} for more details about this method.
#'
#'   The distance between the lat/lon points of subsequent events
#'   is calculated using the method specified in \code{distance.method}.
#'   If "greatcircle", \code{\link{distance_greatcircle}} is used,
#'   while \code{\link[swfscMisc]{distance}} is used otherwise.
#'   See \code{\link{das_sight}} for how the sightings are processed.
#'
#'   The sightinfo data frame includes the column 'included',
#'   which is used in \code{\link{das_effort_sight}} when summarizing
#'   the number of sightings and animals for selected species.
#'   \code{\link{das_effort_sight}} is a separate function to allow users to
#'   personalize the included values as desired for their analysis.
#'   By default, i.e. in the output of this function, 'included' is \code{TRUE} if:
#'   the sighting was made when on effort,
#'   by a standard observer (see \code{\link{das_sight}}),
#'   and in a Beaufort sea state less than or equal to five.
#'
#' @return List of three data frames:
#'   \itemize{
#'     \item segdata: one row for every segment, and columns for information including
#'       unique segment number (segnum), the corresponding effort section (section_id),
#'       the segment index within the corresponding effort section (section_sub_id),
#'       the starting and ending line of the segment in the DAS file (stlin, endlin),
#'       start/end/midpoint coordinates(lat1/lon1, lat2/lon2, and mlat/mlon, respectively),
#'       the start/end/midpoint date/time of the segment (DateTime1, DateTime2, and mDateTime, respectively;
#'       mDateTime is the average of DateTime1 and DateTime2), segment length (dist),
#'       conditions (e.g. Beaufort), and, if applicable, stratum (InStratumName).
#'     \item sightinfo: details for all sightings in \code{x}, including:
#'       the unique segment number it is associated with, segment mid points (lat/lon),
#'       the 'included' column described in the 'Details' section,
#'       and the output information described in \code{\link{das_sight}} for \code{return.format} is "default"
#'     \item randpicks: see \code{\link{das_chop_equallength}};
#'       \code{NULL} if using "condition" method
#'   }
#'
#' @seealso Internal functions called by \code{das_effort}:
#'   \code{\link{das_chop_condition}}, \code{\link{das_chop_equallength}},
#'   \code{\link{das_chop_section}}, \code{\link{das_segdata}}
#'
#' @examples
#' y <- system.file("das_sample.das", package = "swfscDAS")
#' y.proc <- das_process(y)
#'
#' # Using "condition" method
#' das_effort(
#'   y.proc, method = "condition", conditions = c("Bft", "SwellHght", "Vis"),
#'   seg.min.km = 0.05, num.cores = 1
#' )
#'
#' # Using "section" method
#' das_effort(y.proc, method = "section", num.cores = 1)
#'
#' \donttest{
#' # Using "equallength" method
#' y.rand <- system.file("das_sample_randpicks.csv", package = "swfscDAS")
#' das_effort(
#'   y.proc, method = "equallength", seg.km = 10, randpicks.load = y.rand,
#'   num.cores = 1
#' )
#'
#' # Using "section" method and chop by strata
#' stratum.file <- system.file("das_sample_stratum.csv", package = "swfscDAS")
#' das_effort(
#'   y.proc, method = "section", strata.files = list(Poly1 = stratum.file),
#'   num.cores = 1
#' )
#' }
#'
#' @export
das_effort <- function(x, ...) UseMethod("das_effort")


#' @name das_effort
#' @export
das_effort.data.frame <- function(x, ...) {
  das_effort(as_das_df(x), ...)
}


#' @name das_effort
#' @export
das_effort.das_df <- function(x, method = c("condition", "equallength", "section"),
                              conditions = NULL, strata.files = NULL,
                              distance.method = c("greatcircle", "lawofcosines", "haversine", "vincenty"),
                              seg0.drop = FALSE, comment.drop = FALSE, event.touse = NULL,
                              num.cores = NULL, ...) {
  #----------------------------------------------------------------------------
  # Input checks
  if (!(inherits(seg0.drop, "logical") & inherits(comment.drop, "logical")))
    stop("seg0.drop and comment.drop must both be logicals (either TRUE or FALSE)")

  method <- match.arg(method)
  distance.method <- match.arg(distance.method)

  conditions <- .das_conditions_check(conditions, method)

  event.tmp <- c("?", 1:8)
  if (!is.null(event.touse)) {
    if (comment.drop) warning("comment.drop is ignored because event.touse is not NULL")

    if (!all(c("R", "E", "S", "A") %in% event.touse))
      stop("event.use must include at least the following events: ",
           paste(c("R", "E", "S", "A"), collapse = ", "))

    if (any(event.tmp %in% event.touse))
      stop("event.use cannot include the following events: ",
           paste(event.tmp, collapse = ", "))
  }


  #----------------------------------------------------------------------------
  # Prep for chop functions

  # Remove comments if specified
  if (is.null(event.touse) & comment.drop) x <- x %>% filter(.data$Event != "C")

  # Add index column for adding back in ? and 1:8 events, and extract those events
  x$idx_eff <- seq_len(nrow(x))

  # Filter for continuous effort sections; extract ? and 1:8 events
  #   'on effort + 1' is to capture O/E event.
  x.B.preEff <- which(x$Event == "B")
  x.B.preEff <- x.B.preEff[(x.B.preEff %in% c(1, which(!x$OnEffort) + 1))]

  # Don't use Event == "E" in case there are rogue E events
  x.oneff.which <- sort(unique(c(which(x$OnEffort), which(x$OnEffort) + 1)))
  x.oneff.which <- x.oneff.which[!(x.oneff.which %in% x.B.preEff)]
  if (!all(between(x.oneff.which, 1, nrow(x))))
    stop("Error processing index values. ",
         "Please make sure there are no issues flagged by das_check, ",
         "and then report this as an issue.")
  rm(x.B.preEff)

  x.oneff.all <- x[x.oneff.which, ]

  x.oneff <- x.oneff.all %>% filter(!(.data$Event %in% event.tmp) )
  x.oneff.tmp <- x.oneff.all %>%
    filter(.data$Event %in% event.tmp) %>%
    mutate(cont_eff_section = NA, dist_from_prev = NA, seg_idx = NA, segnum = NA)

  rownames(x.oneff) <- rownames(x.oneff.tmp) <- NULL

  if (!all(x.oneff[!x.oneff$OnEffort, "Event"] == "E"))
    stop("Within the continuous effort sections, ",
         "some events other than 'E' events are off effort. ",
         "This may because of time/time zone issues, ",
         "e.g. if continuous effort sections span multiple days ",
         "and das_process was run with 'reset.day = TRUE'")

  if (sum(c(nrow(x.oneff), nrow(x.oneff.tmp))) != nrow(x.oneff.all))
    stop("Error in row numbers - please report this as an issue")

  # Filter for specified events, if applicable
  if (!is.null(event.touse)) x.oneff <- x.oneff %>% filter(.data$Event %in% event.touse)


  # Verbosely remove remaining data without Lat/Lon/DateTime info
  if (any(is.na(x.oneff$Lat) | is.na(x.oneff$Lon) | is.na(x.oneff$DateTime))) {
    x.nacheck <- x.oneff %>%
      mutate(ll_dt_na = is.na(.data$Lat) | is.na(.data$Lon) | is.na(.data$DateTime),
             eff_na = .data$ll_dt_na & (.data$Event %in% c("R", "E")),
             sight_na = .data$ll_dt_na & (.data$Event %in% c("S", "K", "M", "G", "t", "A")))

    # Check that no sightings have NA lat/lon/dt info
    if (any(x.nacheck$sight_na))
      stop("One or more sightings ",
           "have NA Lat/Lon/DateTime values at the following line number(s); ",
           "please fix or remove before processing:\n",
           .print_file_line(x.nacheck$file_das, x.nacheck$line_num, which(x.nacheck$sight_na)))

    # Check that no R/E events have NA lat/lon/dt info
    if (any(x.nacheck$eff_na))
      stop("One or more effort events (R/E events)",
           "have NA Lat/Lon/DateTime values at the following line number(s); ",
           "please fix or remove before processing:\n",
           .print_file_line(x.nacheck$file_das, x.nacheck$line_num, which(x.nacheck$sight_na)))

    # Remove events with NA lat/lon/dt info
    x.oneff <- x.oneff %>% filter(!is.na(.data$Lat) & !is.na(.data$Lon) & !is.na(.data$DateTime))
    message(paste0("There were ", sum(x.nacheck$ll_dt_na), " on effort ",
                   ifelse(comment.drop, "(non-C) ", ""), "events ",
                   "with NA Lat/Lon/DateTime values that will ignored ",
                   "during segment chopping"))

    rm(x.nacheck)
  }

  # Add in 'events' where effort crosses strata boundary
  if (!is.null(strata.files)) {
    # Check names - add as needed
    if (any(names(strata.files) == "")) {
      noname <- names(strata.files) == ""
      names(strata.files)[noname] <- paste0("Stratum", seq_along(strata.files)[noname])
    } else { #is.null(names(strata.files))
      names(strata.files) <- paste0("Stratum", seq_along(strata.files))
    }

    # Call helper, and sanity check
    x.oneff <- das_effort_strata(x.oneff, strata.files)
    stopifnot(
      all(!is.na(x.oneff$Lon[x.oneff$Event == "strata"])),
      all(!is.na(x.oneff$Lat[x.oneff$Event == "strata"]))
    )
  }

  # For each event, calculate distance to previous event
  x.oneff$dist_from_prev <- .dist_from_prev(x.oneff, distance.method)

  # Determine continuous effort sections
  x.oneff$cont_eff_section <- cumsum(x.oneff$Event %in% c("R", "strataR"))

  # If specified, verbosely remove cont eff sections with length 0 and no sighting events
  if (seg0.drop) {
    x.ces.summ <- x.oneff %>%
      group_by(.data$cont_eff_section) %>%
      summarise(dist_sum = sum(.data$dist_from_prev[-1]),
                has_sight = any(c("S", "K", "M", "G", "t") %in% .data$Event),
                line_min = min(.data$line_num))
    ces.keep <- filter(x.ces.summ, .data$has_sight | .data$dist_sum > 0)[["cont_eff_section"]]

    x.oneff <- x.oneff %>% filter(.data$cont_eff_section %in% ces.keep)

    x.oneff$cont_eff_section <- cumsum(x.oneff$Event %in% c("R", "strataR"))

    message(paste("There were", nrow(x.ces.summ) - length(ces.keep),
                  "continuous effort sections removed because they have a",
                  "length of 0 and contain no sighting events"))
    rm(x.ces.summ, ces.keep)
  }

  if (length(unique(x.oneff$cont_eff_section)) != sum(x.oneff$Event %in% c("R", "strataR")) |
      max(x.oneff$cont_eff_section) != sum(x.oneff$Event %in% c("R", "strataR")))
    stop("Error in processing continuous effort sections - ",
         "please report this as an issue")


  #----------------------------------------------------------------------------
  # Chop and summarize effort using specified method
  func.chop <- if (method == "equallength") {
    das_chop_equallength
  } else if (method == "condition") {
    das_chop_condition
  } else if (method == "section") {
    das_chop_section
  } else {
    stop("method is not an accepted value")
  }

  eff.list <- func.chop(
    as_das_df(x.oneff), conditions = conditions, num.cores = num.cores, ...
  )

  x.eff <- eff.list[[1]]
  segdata <- eff.list[[2]]
  randpicks <- eff.list[[3]]

  # Add strata info to segdata as needed - easiest to do this here
  if (!is.null(strata.files)) {
    x.strata.summ <- x.eff %>%
      group_by(.data$segnum) %>%
      summarise(strata_which = unique(.data$strata_which),
                stratum = ifelse(.data$strata_which == 0, NA,
                                 names(strata.files)[.data$strata_which])) %>%
      select(.data$segnum, .data$stratum)
    if (nrow(x.strata.summ) != nrow(segdata))
      stop("Error processing strata and segdata - please report this as an issue")

    segdata <- segdata %>% left_join(x.strata.summ, by = "segnum")
    x.eff <- x.eff %>% select(-!!c(names(strata.files), "strata_which"))
  }

  # Check that things are as expected
  x.eff.names <- c(
    names(x), "dist_from_prev", "cont_eff_section", "seg_idx", "segnum"
    # if (is.null(strata.files)) NULL else c(names(strata.files), "strata_which"),
  )
  if (!identical(names(x.eff), x.eff.names))
    stop("Error in das_effort: names of x.eff. Please report this as an issue")

  if (!all(x.eff$segnum %in% segdata$segnum))
    stop("Error in das_effort(): creating and processing segement numbers. ",
         "Please report this as an issue")

  # Add back in ? and 1:8 (events.tmp) events, if necessary
  # Only for sightinfo groupsizes, and thus no segdata info doesn't matter
  if (nrow(x.oneff.tmp) > 0) x.eff <- bind_rows(x.eff, x.oneff.tmp)

  x.eff.all <- x.eff %>%
    arrange(.data$idx_eff) %>%
    select(-.data$idx_eff)


  #----------------------------------------------------------------------------
  #----------------------------------------------------------------------------
  # Summarize sightings
  sightinfo <- x.eff.all %>%
    left_join(select(segdata, .data$segnum, .data$mlat, .data$mlon),
              by = "segnum") %>%
    das_sight(returnformat = "default") %>%
    mutate(included = (.data$Bft <= 5 & .data$OnEffort & .data$ObsStd),
           included = ifelse(is.na(.data$included), FALSE, .data$included)) %>%
    select(-.data$dist_from_prev, -.data$cont_eff_section)

  # Clean and return
  segdata <- segdata %>% select(-.data$seg_idx)

  sightinfo <- sightinfo %>%
    mutate(year = year(.data$DateTime)) %>%
    select(-.data$seg_idx) %>%
    select(.data$segnum, .data$mlat, .data$mlon, .data$Event,
           .data$DateTime, .data$year, everything())

  list(segdata = segdata, sightinfo = sightinfo, randpicks = randpicks)
}
