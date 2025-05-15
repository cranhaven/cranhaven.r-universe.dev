#' Chop AirDAS data - condition
#' 
#' Chop AirDAS data into a new effort segment every time a condition changes
#' 
#' @param x \code{airdas_df} object, 
#'   or a data frame that can be coerced to a \code{airdas_df} object. 
#'   This data must be filtered for 'OnEffort' events; 
#'   see the Details section below
#' @param ... ignored
#' @param conditions the conditions that trigger a new segment; 
#' see \code{\link{airdas_effort}}
#' @param seg.min.km numeric; minimum allowable segment length (in kilometers).
#'   Default is 0.1. See the Details section below for more information
#' @param distance.method character; see \code{\link{airdas_effort}}.
#'   Default is \code{NULL} since these distances should have already been
#'   calculated in \code{\link{airdas_effort}}
#' @param num.cores See \code{\link{airdas_effort}}
#'   
#' @details WARNING - do not call this function directly!
#'   It is exported for documentation purposes, but is intended for internal package use only.
#'
#'   This function is intended to only be called by \code{\link{airdas_effort}} 
#'   when the "condition" method is specified. 
#'   Thus, \code{x} must be filtered for events (rows) where either
#'   the 'OnEffort' column is \code{TRUE} or the 'Event' column is either "E" or "O"; 
#'   see \code{\link{airdas_effort}} for more details. 
#'   This function chops each continuous effort section (henceforth 'effort sections') 
#'   in \code{x} into modeling segments (henceforth 'segments') by 
#'   creating a new segment every time a condition changes. 
#'   Each effort section runs from a T/R event to its corresponding E/O event. 
#'   After chopping, \code{\link{airdas_segdata}} is called 
#'   (with \code{segdata.method = "maxdist"})
#'   to get relevant segdata information for each segment.
#'   
#'   Changes in the one of the conditions specified in the \code{conditions}
#'   argument triggers a new segment.
#'   An exception is when multiple condition changes happen at
#'   the same location, such as a 'TVPAW' series of events.
#'   When this happens, no segments of length zero are created;
#'   rather, a single segment is created that includes all of the condition changes
#'   (i.e. all of the events in the event series) that happened during
#'   the series of events (i.e. at the same location).
#'   Note that this combining of events at the same Lat/Lon happens
#'   even if \code{seg.min.km = 0}.
#'   
#'   In addition, (almost) all segments whose length is less than \code{seg.min.km}
#'   are combined with the segment immediately following them to ensure that the length
#'   of (almost) all segments is at least \code{seg.min.km}.
#'   This allows users to account for situations where multiple conditions,
#'   such as Beaufort and a viewing condition, change in rapid succession, say <0.1 km apart.
#'   When segments are combined, a message is printed, and the condition that was
#'   recorded for the maximum distance within the new segment is reported.
#'   See \code{\link{airdas_segdata}}, \code{segdata.method = "maxdist"}, for more details
#'   about how the segdata information is determined.
#'   The only exception to this rule is if the short segment ends in an "E" or an "O" event,
#'   meaning it is the last segment of the effort section.
#'   Since in this case there is no 'next' segment,
#'   this short segment is left as-is.
#'
#'   If the column \code{dist_from_prev} does not exist, the distance between
#'   subsequent events is calculated as described in \code{\link{airdas_effort}}
#'   
#' @return List of two data frames:
#' \itemize{
#'   \item \code{x}, with columns added for the corresponding unique segment code and number
#'   \item segdata: data frame with one row for each segment, and columns with
#'     relevant data (see \code{\link{airdas_effort}} for specifics)
#' }
#' 
#' @export
airdas_chop_condition <- function(x, ...) UseMethod("airdas_chop_condition")


#' @name airdas_chop_condition
#' @export
airdas_chop_condition.data.frame <- function(x, ...) {
  airdas_chop_condition(as_airdas_df(x), ...)
}


#' @name airdas_chop_condition
#' @export
airdas_chop_condition.airdas_df <- function(x, conditions, seg.min.km = 0.1, 
                                            distance.method = NULL, 
                                            num.cores = NULL, ...) {
  #----------------------------------------------------------------------------
  # Input checks
  if (!all(x$OnEffort | x$Event %in% c("O", "E"))) 
    stop("x must be filtered for on effort events; see `?airdas_chop_condition")
  
  if (missing(seg.min.km))
    stop("You must specify a 'seg.min.km' argument when using the \"condition\" ", 
         "method. See `?airdas_chop_condition` for more details")
  
  if (!inherits(seg.min.km, c("integer", "numeric")))
    stop("When using the \"condition\" method, seg.min.km must be a numeric. ",
         "See `?airdas_chop_condition` for more details")
  
  if (!.greater_equal(seg.min.km, 0))
    stop("seg.min.km must be greater than or equal to 0; ", 
         "see `?airdas_chop_condition")
  
  conditions <- .airdas_conditions_check(conditions)
  
  
  #----------------------------------------------------------------------------
  # Calculate distance between points if necessary
  if (!("dist_from_prev" %in% names(x))) {
    if (is.null(distance.method))
      stop("If the distance between consectutive points (events) ",
           "has not already been calculated, ",
           "then you must provide a valid argument for distance.method")
    
    x$dist_from_prev <- .dist_from_prev(x, distance.method)
  }
  
  # Get distance to next point
  x$dist_to_next <- c(x$dist_from_prev[-1], NA)
  
  
  #----------------------------------------------------------------------------
  # ID continuous effort sections, then for each modeling segment: 
  #   1) chop by condition change
  #   2) aggregate 0-length segments (e.g. tvpaw),
  #   3) aggregate small segments as specified by user via seg.min.km
  if (!("cont_eff_section" %in% names(x))) {
    x$cont_eff_section <- cumsum(x$Event %in% c("T", "R"))
  }
  
  eff.uniq <- unique(x$cont_eff_section)
  
  # Prep for parallel
  call.x <- x
  call.conditions <- conditions
  call.seg.min.km <- seg.min.km
  call.func1 <- airdas_segdata
  
  # Setup number of cores
  if(is.null(num.cores)) num.cores <- parallel::detectCores() - 1
  if(is.na(num.cores)) num.cores <- 1
  num.cores <- max(1, num.cores)
  num.cores <- min(parallel::detectCores() - 1, num.cores)
  
  # Use parallel to lapply through - modeled after rfPermute
  cl <- swfscMisc::setupClusters(num.cores)
  eff.chop.list <- tryCatch({
    if(is.null(cl)) { # Don't parallelize if num.cores == 1
      lapply(
        eff.uniq, swfscDAS::.chop_condition_eff, call.x = call.x,
        call.conditions = call.conditions, call.seg.min.km = call.seg.min.km,
        call.func1 = call.func1
      )
      
    } else { # Run lapply using parLapplyLB
      parallel::clusterExport(
        cl = cl,
        varlist = c("call.x", "call.conditions", "call.seg.min.km", 
                    "call.func1"),
        envir = environment()
      )
      parallel::parLapplyLB(
        cl, eff.uniq, swfscDAS::.chop_condition_eff, call.x = call.x,
        call.conditions = call.conditions, call.seg.min.km = call.seg.min.km,
        call.func1 = call.func1
      )
    }
  }, finally = if(!is.null(cl)) parallel::stopCluster(cl) else NULL)
  
  
  #----------------------------------------------------------------------------
  # Extract information from eff.chop.list, and return
  
  ### Segdata
  segdata <- data.frame(
    do.call(rbind, lapply(eff.chop.list, function(i) i[["das.df.segdata"]])), 
    stringsAsFactors = FALSE
  ) %>%
    mutate(segnum = seq_along(.data$file), 
           dist = round(.data$dist, 4)) %>%
    select(.data$segnum, .data$seg_idx, everything())
  
  ### Segment lengths
  x.len <- lapply(eff.chop.list, function(i) i[["seg.lengths"]])
  
  ### Each das data point, along with segnum
  x.eff <- data.frame(
    do.call(rbind, lapply(eff.chop.list, function(i) i[["das.df"]])), 
    stringsAsFactors = FALSE
  ) %>% 
    left_join(segdata[, c("seg_idx", "segnum")], by = "seg_idx") %>% 
    select(-.data$dist_to_next)
  
  ### Message about segments that were combined
  segs.message <- na.omit(vapply(eff.chop.list, function(i) i[["segs.combine"]], 1))
  if (length(segs.message) > 0)
    message("Since seg.min.km > 0, ",
            "segments with different conditions were combined ",
            "in the following continuous effort section(s): ",
            paste(segs.message, collapse = ", "))
  
  
  #----------------------------------------------------------------------------
  # Return; NULL is for randpicks
  list(as_airdas_df(x.eff), segdata, NULL)
}
