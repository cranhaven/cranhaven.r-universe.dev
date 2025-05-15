#' Summarize AirDAS effort
#'
#' Chop AirDAS data into effort segments
#' 
#' @param x \code{airdas_df} object; output from \code{\link{airdas_process}}, 
#'  or a data frame that can be coerced to a \code{airdas_df} object
#' @param method character; method to use to chop AirDAS data into effort segments
#'   Can be "condition", "equallength", "section", 
#'   or any partial match thereof (case sensitive)
#' @param conditions character vector of names of conditions 
#'   to include in segdata output.
#'   These values must be column names from the output of \code{\link{airdas_process}},
#'   e.g. 'Bft', 'CCover', etc. The default is \code{NULL}, 
#'   in which case all relevant conditions will be included.
#'   If \code{method == "condition"}, then these also are the conditions which
#'   trigger segment chopping when they change.
#' @param distance.method character;
#'   method to use to calculate distance between lat/lon coordinates.
#'   Can be "greatcircle", "lawofcosines", "haversine", "vincenty",
#'   or any partial match thereof (case sensitive). Default is "greatcircle"
#' @param num.cores Number of CPUs to over which to distribute computations.
#'   Defaults to \code{NULL}, which uses one fewer than the number of cores
#'   reported by \code{\link[parallel]{detectCores}}
#'   Using 1 core likely will be faster for smaller datasets
#' @param angle.min passed to \code{\link{airdas_sight}}
#' @param bft.max numeric; the maximum Beaufort (column 'Bft') for which to 
#'   mark a sighting as \code{TRUE} in 'included' (see Details). 
#'   Default is 5.
#' @param ... arguments passed to the chopping function specified using \code{method},
#'   such as \code{seg.km} or \code{seg.min.km}
#' 
#' @details This is the top-level function for chopping processed AirDAS data 
#'   into modeling segments (henceforth 'segments'), and assigning sightings 
#'   and related information (e.g., weather conditions) to each segment. 
#'   This function returns data frames with all relevant information for the 
#'   effort segments and associated sightings ('segdata' and 'sightinfo', respectively). 
#'   Before chopping, the AirDAS data is filtered for events (rows) where either
#'   the 'OnEffort' column is \code{TRUE} or the 'Event' column is "E" or "O". 
#'   In other words, the data is filtered for continuous effort sections (henceforth 'effort sections'), 
#'   where effort sections run from "T"/"R" to "E"/"O" events (inclusive), 
#'   and then passed to the chopping function specified using \code{method}. 
#'   All on effort events must not have \code{NA} Lat or Lon values; 
#'   note Lat/Lon values for 1 events were 'filled in' in \code{\link{airdas_process}}.
#' 
#'   The following chopping methods are currently available: 
#'   "condition", "equallength", and "section". 
#'   When using the "condition" method, effort sections are chopped 
#'   into segments every time a condition specified in \code{conditions} changes, 
#'   thereby ensuring that the conditions are consistent across the entire segment.
#'   See \code{\link{airdas_chop_condition}} for more details about this method, 
#'   including arguments that must be passed to it via \code{...}.
#'   
#'   The "equallength" method consists of
#'   chopping effort sections into equal-length segments of length \code{seg.km}, 
#'   and doing a weighted average of the conditions for the length of that segment. 
#'   See \code{\link{airdas_chop_equallength}} for more details about this method, 
#'   including arguments that must be passed to it via \code{...}.
#'   
#'   The "section" method involves 'chopping' the effort into continuous effort sections,
#'   i.e. each continuous effort section is a single effort segment.
#'   See \code{\link{airdas_chop_section}} for more details about this method.
#'   
#'   The distance between the lat/lon points of subsequent events
#'   is calculated using the method specified in \code{distance.method}.
#'   If "greatcircle", \code{\link[swfscDAS]{distance_greatcircle}} is used,
#'   while \code{\link[swfscMisc]{distance}} is used otherwise.
#'   See \code{\link{airdas_sight}} for how the sightings are processed.
#'
#'   The sightinfo data frame includes the column 'included',
#'   which is used in \code{\link{airdas_effort_sight}} when summarizing
#'   the number of sightings and animals for selected species.
#'   \code{\link{airdas_effort_sight}} is a separate function to allow users to
#'   personalize the 'included' values as desired for their specific analysis.
#'   By default, i.e. in the output of this function, 'included' is \code{TRUE} if:
#'   the sighting was a standard sighting (see \code{\link{airdas_sight}}) 
#'   and in a Beaufort sea state less than or equal to 'btf.max'.
#' 
#' @return List of three data frames: 
#'   \itemize{
#'     \item segdata: one row for every segment, and columns for information including
#'       unique segment number, event code that started the associated continuous effort section, 
#'       the starting and ending line of the segment in the DAS file (stlin, endlin),
#'       start/end/midpoint coordinates(lat1/lon1, lat2/lon2, and mlat/mlon, respectively),
#'       the start/end/midpoint date/time of the segment (DateTime1, DateTime2, and mDateTime, respectively;
#'       mDateTime is the average of DateTime1 and DateTime2), segment length (dist),
#'       and conditions (e.g. Beaufort)
#'     \item sightinfo: details for all sightings in \code{x}, including: 
#'       the unique segment number it is associated with, segment mid points (lat/lon), 
#'       the 'included' column described in the Details section,
#'       and the output information described in \code{\link{airdas_sight}}
#'     \item randpicks: see \code{\link{airdas_chop_equallength}}. 
#'       \code{NULL} if using "condition" method.
#'   }
#' 
#' @examples
#' y <- system.file("airdas_sample.das", package = "swfscAirDAS")
#' y.proc <- airdas_process(y)
#' 
#' airdas_effort(
#'   y.proc, method = "condition", conditions = "Bft", seg.min.km = 0.05, 
#'   num.cores = 1
#' )
#' 
#' y.rand <- system.file("airdas_sample_randpicks.csv", package = "swfscAirDAS")
#' airdas_effort(
#'   y.proc, method = "equallength", conditions = c("Bft", "CCover"), 
#'   seg.km = 3, randpicks.load = y.rand, num.cores = 1
#' )
#' 
#' airdas_effort(y.proc, method = "section", num.cores = 1)
#'
#' @export
airdas_effort <- function(x, ...) UseMethod("airdas_effort")


#' @name airdas_effort
#' @export
airdas_effort.data.frame <- function(x, ...) {
  airdas_effort(as_airdas_df(x), ...)
}


#' @name airdas_effort
#' @export
airdas_effort.airdas_df <- function(
    x, method = c("condition", "equallength", "section"), 
    conditions = NULL, 
    distance.method = c("greatcircle", "lawofcosines", "haversine", "vincenty"),
    num.cores = NULL, angle.min = 12, bft.max = 5, ...
) {
  #----------------------------------------------------------------------------
  # Input checks
  
  method <- match.arg(method)
  distance.method <- match.arg(distance.method)
  
  conditions <- .airdas_conditions_check(conditions)
  
  
  #----------------------------------------------------------------------------
  # Prep
  # Filter for and number continuous effort sections
  #   'on effort + 1' is to capture O/E event
  x.oneff.which <- sort(unique(c(which(x$OnEffort), which(x$OnEffort) + 1)))
  stopifnot(all(between(x.oneff.which, 1, nrow(x))))
  
  x.oneff <- x[x.oneff.which, ]
  rownames(x.oneff) <- NULL
  
  x.oneff$dist_from_prev <- .dist_from_prev(x.oneff, distance.method)
  
  # ID continuous effort sections
  x.oneff$cont_eff_section <- cumsum(x.oneff$Event %in% c("T", "R"))
  
  x.oneff.summ <- x.oneff %>% 
    group_by(.data$cont_eff_section) %>% 
    summarise(tr_count = sum(.data$Event %in% c("T", "R")),
              eo_count = sum(.data$Event %in% c("E", "O")))
  if (!(all(x.oneff.summ$tr_count == 1) & all(x.oneff.summ$eo_count == 1)))
    warning("Some continuous effort secitons do not have exactly one ", 
            "T/R event and one O/E event. ", 
            "Please check your data using airdas_check", 
            immediate. = TRUE)
  rm(x.oneff.summ)
  
  
  
  #----------------------------------------------------------------------------
  # Chop and summarize effort using specified method
  eff.list <- if (method == "condition") {
    airdas_chop_condition(as_airdas_df(x.oneff), conditions = conditions, 
                          num.cores = num.cores, ...)
  } else if (method == "equallength") {
    airdas_chop_equallength(as_airdas_df(x.oneff), conditions = conditions, 
                            num.cores = num.cores, ...)
  } else if (method == "section") {
    airdas_chop_section(as_airdas_df(x.oneff), conditions = conditions, 
                        num.cores = num.cores, ...)
  } else {
    stop("method is not an accepted value")
  }
  
  x.eff <- eff.list[[1]]
  segdata <- eff.list[[2]]
  randpicks <- eff.list[[3]]
  
  x.eff.names <- c(
    "Event", "DateTime", "Lat", "Lon", "OnEffort", "Trans", "Bft", 
    "CCover", "Jelly", "HorizSun", "VertSun", 
    "HKR", "Haze", "Kelp", "RedTide", 
    "AltFt", "SpKnot", "ObsL", "ObsB", "ObsR", "Rec", 
    "VLI", "VLO", "VB", "VRI", "VRO", 
    "Data1", "Data2", "Data3", "Data4", "Data5", "Data6", "Data7", 
    "EffortDot", "EventNum", "file_das", "line_num", "file_type", 
    "dist_from_prev", "cont_eff_section", "seg_idx", "segnum"
  )
  if (!identical(names(x.eff), x.eff.names))
    stop("Error in airdas_effort: names of x.eff. ", 
         "Please report this as an issue")
  
  if (!all(x.eff$segnum %in% segdata$segnum))
    stop("Error in airdas_effort(): Error creating and processing ", 
         "segement numbers. Please report this as an issue")
  
  
  #----------------------------------------------------------------------------
  # Summarize sightings (based on sightinfo)
  sightinfo <- x.eff %>% 
    left_join(select(segdata, .data$segnum, .data$mlat, .data$mlon), 
              by = "segnum") %>% 
    airdas_sight(angle.min = angle.min) %>% 
    mutate(included = .data$Bft <= bft.max & .data$SightStd, 
           included = ifelse(is.na(.data$included), FALSE, .data$included)) %>% 
    select(-.data$dist_from_prev, -.data$cont_eff_section)
  
  
  # And return - ready for airdas_effort_sightings
  segdata <- segdata %>% select(-.data$seg_idx)
  
  sightinfo <- sightinfo %>%
    select(-.data$seg_idx) %>%
    select(.data$segnum, .data$mlat, .data$mlon, everything())
  
  list(segdata = segdata, sightinfo = sightinfo, randpicks = randpicks)
} 
