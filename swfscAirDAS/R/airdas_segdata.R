#' Summarize AirDAS data for a continuous effort section
#' 
#' Summarize AirDAS effort data by effort segment, while averaging conditions
#' 
#' @param x \code{airdas_df} object, 
#'   or a data frame that can be coerced to a \code{airdas_df} object.
#'   Must contain a single continuous effort section of AirDAS data; 
#'   see the Details section below
#' @param conditions see \code{\link{airdas_effort}}, or
#'   see Details section for more information
#' @param segdata.method character; either \code{"avg"} or \code{"maxdist"}.
#'   \code{"avg"} means the condition values will be
#'   calculated as a weighted average by distance, while
#'   \code{"maxdist"} means the condition values will be those recorded
#'   for the longest distance during that segment
#' @param seg.lengths numeric; length of the modeling segments 
#'   into which \code{x} will be chopped
#' @param section.id numeric; the ID of \code{x} (the current continuous effort section)
#' @param ... ignored
#' 
#' @details WARNING - do not call this function directly!
#'   It is exported for documentation purposes, but is intended for internal package use only.
#'
#'   This function was designed to be called by one of the airdas_chop_ functions, 
#'   e.g. \code{\link{airdas_chop_equallength}}, and thus 
#'   users should avoid calling it themselves.
#'   It loops through the events in \code{x}, calculating and storing relevant
#'   information for each modeling segment as it goes. 
#'   Because \code{x} is a continuous effort section, it must begin with 
#'   a "T" or "R" event and end with the corresponding "E" or "O" event.
#' 
#'   For each segment, this function reports the 
#'   segment ID, transect code, the start/end/mid coordinates (lat/lon),
#'   start/end/mid date/times (DateTime), segment length, 
#'   year, month, day, time, observers, 
#'   and average conditions (which are specified by \code{conditions}).
#'   The segment ID is designated as \code{section.id} _ index of the modeling segment.
#'   Thus, if \code{section.id} is \code{1}, then the segment ID for 
#'   the second segment from \code{x} is \code{"1_2"}.
#'   
#'   When \code{segdata.method} is \code{"avg"}, the condition values are
#'   calculated as a weighted average by distance.
#'   The reported value for logical columns (e.g. Haze) is the percentage
#'   (in decimals) of the segment in which that condition was \code{TRUE}.
#'   For character columns, the reported value for each segment is
#'   the unique value(s) present in the segment, with \code{NA}s omitted,
#'   pasted together via \code{paste(..., collapse = "; ")}.
#'   When \code{segdata.method} is \code{"maxdist"}, the reported values
#'   are, for each condition, the value recorded for the longest distance
#'   during that segment (with \code{NA}s omitted).
#'
#'   Transect code, file name, and vent code that started the continuous effort section 
#'   are also included in the segdata output.
#'   These values (excluding \code{NA}s) must be consistent across the
#'   entire effort section, and thus across all segments in \code{x};
#'   a warning is printed if there are any inconsistencies.
#'   
#'   \code{\link[swfscMisc]{bearing}} and \code{\link[swfscMisc]{destination}}
#'   are used to calculate the segment start, mid, and end points,
#'   with \code{method = "vincenty"}.
#'   
#' @return Data frame with the segdata information described above
#'   and in \code{\link{airdas_effort}}
#' 
#' @export
airdas_segdata <- function(x, ...) UseMethod("airdas_segdata")


#' @name airdas_segdata
#' @export
airdas_segdata.data.frame <- function(x, ...) {
  airdas_segdata(as_airdas_df(x), ...)
}


#' @name airdas_segdata
#' @export
airdas_segdata.airdas_df <- function(x, conditions, segdata.method = c("avg", "maxdist"),
                                     seg.lengths, section.id, ...) {
  #----------------------------------------------------------------------------
  # Input checks
  conditions <- .airdas_conditions_check(conditions)
  segdata.method <- match.arg(segdata.method)
  
  if (!("dist_from_prev" %in% names(x))) 
    stop("x must contain a 'dist_from_prev' column; ", 
         "was this function called by the top-level effort function?")
  
  stopifnot(
    inherits(seg.lengths, c("numeric", "integer")), 
    inherits(section.id, c("numeric", "integer"))
  )
  
  if (!.equal(sum(seg.lengths), sum(x$dist_from_prev)))
    stop("The sum of the seg.lengths values does not equal the sum of the ", 
         "x$dist_from_prev' values; ", 
         "was this function called by the top-level effort function?")
  
  
  #----------------------------------------------------------------------------
  # Prep stuff - get the info that is consistent for the entire effort length
  # ymd determined below to be safe
  val.trans <- unique(na.omit(x$Trans))
  df.out1 <- data.frame(
    file = unique(x$file_das),
    transect = ifelse(length(val.trans) == 0, NA, val.trans), 
    event = x$Event[1], 
    stringsAsFactors = FALSE
  )
  
  if (nrow(df.out1) != 1)
    warning("Error in airdas_segdata(): ", 
            "unexpected inconsistencies in continuous effort section. ", 
            "Please report this as an issue")
  
  df.out1 <- df.out1[1, ]
  
  
  #----------------------------------------------------------------------------
  segdata.all <- .segdata_proc(
    das.df = x, conditions = conditions, segdata.method = segdata.method,
    seg.lengths = seg.lengths, section.id = section.id, df.out1 = df.out1
  )
  
  
  #----------------------------------------------------------------------------
  segdata.all %>% 
    select(.data$seg_idx, .data$section_id, .data$section_sub_id, 
           .data$event, .data$transect, 
           .data$file, .data$stlin, .data$endlin, 
           .data$lat1, .data$lon1, .data$DateTime1, 
           .data$lat2, .data$lon2, .data$DateTime2, 
           .data$mlat, .data$mlon, .data$mDateTime, 
           .data$dist, .data$year, .data$month, .data$day, .data$mtime, 
           everything())
}
