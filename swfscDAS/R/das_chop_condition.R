#' Chop DAS data - condition
#'
#' Chop DAS data into a new effort segment every time a specified condition changes
#'
#' @param x an object of class \code{das_df},
#'   or a data frame that can be coerced to class \code{das_df}.
#'   This data must be filtered for continuous effort sections;
#'   see the Details section below
#' @param ... ignored
#' @param conditions the conditions that trigger a new segment;
#' see \code{\link{das_effort}}
#' @param seg.min.km numeric; minimum allowable segment length (in kilometers).
#'   Default is 0.1. See the Details section below for more information
#' @param distance.method character; see \code{\link{das_effort}}.
#'   Default is \code{NULL} since these distances should have already been calculated
#' @param num.cores see \code{\link{das_effort}}
#'
#' @details WARNING - do not call this function directly!
#'   It is exported for documentation purposes, but is intended for internal package use only.
#'
#'   This function is intended to be called by \code{\link{das_effort}}
#'   when the "condition" method is specified.
#'   Thus, \code{x} must be filtered for events (rows) where either
#'   the 'OnEffort' column is \code{TRUE} or the 'Event' column is "E";
#'   see \code{\link{das_effort}} for more details.
#'   This function chops each continuous effort section (henceforth 'effort sections')
#'   in \code{x} into modeling segments (henceforth 'segments') by
#'   creating a new segment every time a specified condition changes.
#'   Each effort section runs from an "R" event to its corresponding "E" event.
#'   After chopping, \code{\link{das_segdata}} is called
#'   (with \code{segdata.method = "maxdist"})
#'   to get relevant segdata information for each segment.
#'
#'   Changes in the one of the conditions specified in the \code{conditions}
#'   argument triggers a new segment.
#'   One exception is if the event at which this condition change occurs is part of an event series,
#'   meaning one of several events in a row at the same lat/lon points (such as a PVNW event series).
#'   In this situation, the final event of the event series is considered the last event
#'   of the current effort segment, and thus also the start of the next effort segment.
#'
#'   Related, when multiple condition changes happen at the same lat/lon points,
#'   such as a "RPVNW" series of events at the beginning of the effort section.
#'   When this happens, no segments of length zero are created;
#'   rather, a single segment is created that includes all of the condition changes
#'   (i.e. all of the events in the event series) that happened during
#'   the series of events (i.e. at the same location).
#'   Note that this combining of events at the same position happens
#'   even if \code{seg.min.km = 0}.
#'
#'   In addition, (almost) all segments whose length is less than \code{seg.min.km}
#'   are combined with the segment immediately following them to ensure that the length
#'   of (almost) all segments is at least \code{seg.min.km}.
#'   This allows users to account for situations where multiple conditions,
#'   such as Beaufort and the visibility, change in rapid succession, for instance <0.1 km apart.
#'   When segments are combined, a message is printed, and the condition that was
#'   recorded for the maximum distance within the new segment is reported.
#'   See \code{\link{das_segdata}}, \code{segdata.method = "maxdist"}, for more details
#'   about how the segdata information is determined.
#'   The only exception to this rule is if the short segment ends in an "E" event,
#'   meaning it is the last segment of the effort section.
#'   Since in this case there is no 'next' segment,
#'   this short segment is left as-is.
#'
#'   If the column \code{dist_from_prev} does not exist, the distance between
#'   subsequent events is calculated as described in \code{\link{das_effort}}
#'
#' @return List of two data frames:
#' \itemize{
#'   \item \code{x}, with columns added for the corresponding unique segment code and number
#'   \item segdata: data frame with one row for each segment, and columns with
#'     relevant data (see \code{\link{das_effort}} for specifics)
#' }
#'
#' @export
das_chop_condition <- function(x, ...) UseMethod("das_chop_condition")


#' @name das_chop_condition
#' @export
das_chop_condition.data.frame <- function(x, ...) {
  das_chop_condition(as_das_df(x), ...)
}


#' @name das_chop_condition
#' @export
das_chop_condition.das_df <- function(x, conditions, seg.min.km = 0.1,
                                      distance.method = NULL, num.cores = NULL,
                                      ...) {
  #----------------------------------------------------------------------------
  # Input checks
  conditions <- .das_conditions_check(conditions, "condition")

  if (!all(x$OnEffort | x$Event == "E"))
    stop("x must be filtered for on effort events; see `?das_chop_condition")

  if (!inherits(seg.min.km, c("integer", "numeric")))
    stop("When using the \"condition\" method, seg.min.km must be a numeric. ",
         "See `?das_chop_condition` for more details")

  if (!.greater_equal(seg.min.km, 0))
    stop("seg.min.km must be greater than or equal to 0; ",
         "see `?das_chop_condition")

  #----------------------------------------------------------------------------
  # Add columns if necessary

  # Determine continuous effort sections
  if (!("cont_eff_section" %in% names(x))) {
    x$cont_eff_section <- cumsum(x$Event %in% c("R", "strataR"))
  }

  # Calculate distance between points; checks happen in .dist_from_prev()
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
  # For each modeling segment:
  #   1) chop by condition change
  #   2) aggregate 0-length segments (e.g. brpvnw),
  #   3) aggregate small segments as specified by user via seg.min.km

  eff.uniq <- unique(x$cont_eff_section)
  stopifnot(length(eff.uniq) == sum(x$Event %in% c("R", "strataR")))

  # Prep for parallel
  call.x <- x
  call.conditions <- conditions
  call.seg.min.km <- seg.min.km
  call.func1 <- das_segdata

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
        eff.uniq, .chop_condition_eff, call.x = call.x,
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
        cl, eff.uniq, .chop_condition_eff, call.x = call.x,
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
    mutate(segnum = seq_along(.data$seg_idx),
           dist = round(.data$dist, 4)) %>%
    select(.data$segnum, .data$seg_idx, everything())

  ### Segment lengths
  x.len <- lapply(eff.chop.list, function(i) i[["seg.lengths"]])

  ### Each DAS data point, along with segnum
  x.eff <- data.frame(
    do.call(rbind, lapply(eff.chop.list, function(i) i[["das.df"]])),
    stringsAsFactors = FALSE
  ) %>%
    left_join(segdata[, c("seg_idx", "segnum")], by = "seg_idx") %>%
    select(-.data$dist_to_next)

  ### Message about segments that were combined
  ###   Must be outside b/c no messages come out of parallel
  segs.message <- na.omit(vapply(eff.chop.list, function(i) i[["segs.combine"]], 1))
  if (length(segs.message) > 0)
    message("Since seg.min.km > 0, ",
            "segments with different conditions were combined ",
            "in the following continuous effort section(s): ",
            paste(segs.message, collapse = ", "))


  #----------------------------------------------------------------------------
  # Return; NULL is for randpicks
  list(as_das_df(x.eff), segdata, NULL)
}



#' @name swfscAirDAS-internals
#' @param i ignore
#' @param call.x ignore
#' @param call.conditions ignore
#' @param call.seg.min.km ignore
#' @param call.func1 ignore
#' @export
.chop_condition_eff <- function(i, call.x, call.conditions, call.seg.min.km,
                                call.func1) {
  ### Inputs; mostly same as das_chop_condition but with "call" prefix
  # i: Index of current continuous effort section
  # call.x: x argument from das_chop_condition(), with a few additional columns
  # call.conditions: conditions argument from das_chop_condition()
  # call.seg.min.km: call.seg.min.km argument from das_chop_condition()
  # call.func1: _segdata_ function - needs to be passed in since
  #   this function is used by swfscAirDAS as well

  ### Output
  # List with, for this continuous effort section:
  #   1) DAS data frame, 2) segment lengths, and 3) segdata

  #------------------------------------------------------
  # Prep
  das.df <- filter(call.x, .data$cont_eff_section == i)

  # Ignore distance from last effort
  das.df$dist_from_prev[1] <- 0
  # Ignore distance past this continuous effort section
  das.df$dist_to_next[nrow(das.df)] <- 0


  #------------------------------------------------------
  ### Determine indices of condition changes, and combine as needed
  cond.list <- lapply(call.conditions, function(j) {
    which(c(NA, head(das.df[[j]], -1) != das.df[[j]][-1]))
  })
  cond.idx.pre <- sort(unique(c(1, unlist(cond.list))))

  effort.seg.pre <- rep(FALSE, nrow(das.df))
  effort.seg.pre[cond.idx.pre] <- TRUE

  das.df$effort_seg_pre <- cumsum(effort.seg.pre)
  das.df$idx <- seq_len(nrow(das.df))

  # Get distances of current effort sections
  # Remove last row - there is no next segment to join it with,
  #   even if the last segment is < seg.min.km.
  #   Because of indexing method, the last break point will still be
  #   removed to join the final two segments if necessary
  d.pre <- das.df %>%
    group_by(.data$effort_seg_pre) %>%
    summarise(idx_start = min(.data$idx),
              idx_end = max(.data$idx),
              dist_length = sum(.data$dist_to_next)) %>%
    slice(-n())

  # == 0 check is here in case seg.min.km is 0
  seg.len0 <- d.pre$idx_end[.equal(d.pre$dist_length, 0)] + 1
  seg.len1 <- d.pre$idx_end[.less(d.pre$dist_length, call.seg.min.km)] + 1

  seg.diff <- setdiff(seg.len1, seg.len0)
  segs.combine <- if (length(seg.diff) > 0) i else NA

  idx.torm <- sort(unique(c(seg.len0, seg.len1)))


  # Remove segment breaks that create too-small segments
  cond.idx <- cond.idx.pre[!(cond.idx.pre %in% idx.torm)]
  effort.seg <- rep(FALSE, nrow(das.df))
  effort.seg[cond.idx] <- TRUE

  das.df <- das.df %>%
    select(-.data$effort_seg_pre, -.data$idx) %>%
    mutate(seg_idx = paste(i, cumsum(effort.seg), sep = "_"))


  #------------------------------------------------------
  ### Calculate lengths of effort segments
  das.df.dist.summ <- das.df %>%
    group_by(.data$seg_idx) %>%
    summarise(sum_dist = sum(.data$dist_to_next))

  seg.lengths <- das.df.dist.summ$sum_dist

  #------------------------------------------------------
  ### Get segdata and return
  # das.df.segdata <- das_segdata(as_das_df(das.df), seg.lengths, i)
  das.df.segdata <- call.func1(
    x = das.df, conditions = call.conditions, segdata.method = "maxdist",
    seg.lengths = seg.lengths, section.id = i
  )

  list(
    das.df = das.df, seg.lengths = seg.lengths,
    das.df.segdata = das.df.segdata,
    segs.combine = segs.combine
  )
}
