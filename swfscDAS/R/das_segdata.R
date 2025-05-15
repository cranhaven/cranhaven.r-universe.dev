#' Summarize DAS data for a continuous effort section
#'
#' Summarize DAS effort data by effort segment, while averaging or getting the max for each condition
#'
#' @param x an object of class \code{das_df},
#'   or a data frame that can be coerced to class \code{das_df}
#'   Must contain a single continuous effort section of DAS data;
#'   see the Details section below
#' @param conditions see \code{\link{das_effort}}, or
#'   see Details section for more information
#' @param segdata.method character; either avg" or "maxdist".
#'   See Details section for more information
#' @param seg.lengths numeric; length of the modeling segments
#'   into which \code{x} will be chopped
#' @param section.id numeric; the ID of \code{x} (the current continuous effort section)
#' @param ... ignored
#'
#' @details WARNING - do not call this function directly!
#'   It is exported for documentation purposes, but is intended for internal package use only.
#'
#'   This function was designed to be called by one of the das_chop_ functions,
#'   e.g. \code{\link{das_chop_equallength}}, and thus
#'   users should avoid calling it themselves.
#'   It loops through the events in \code{x}, chopping \code{x} into modeling segments
#'   while calculating and storing relevant information for each segment.
#'   Because \code{x} is a continuous effort section, it must begin with
#'   a "B" or "R" event and end with the corresponding "E" event.
#'
#'   For each segment, this function reports the segment number,
#'   segment ID, cruise number, the start/end/mid coordinates (lat/lon),
#'   start/end/mid date/times (DateTime), segment length,
#'   year, month, day, midpoint time, mode, effort type,
#'   effective strip width sides (number of sides searched),
#'   and average conditions (which are specified by \code{conditions}).
#'   The segment ID is designated as \code{section.id} _ index of the modeling segment.
#'   Thus, if \code{section.id} is \code{1}, then the segment ID for
#'   the second segment from \code{x} is \code{"1_2"}.
#'   The start/end coordinates and date/times are interpolated as needed,
#'   e.g. when using the 'equallength' method.
#'
#'   When \code{segdata.method} is "avg", the condition values are
#'   calculated as a weighted average by distance.
#'   The reported value for logical columns (e.g. Glare) is the percentage
#'   (in decimals) of the segment in which that condition was \code{TRUE}.
#'   For character columns, the reported value for each segment is
#'   the unique value(s) present in the segment, with \code{NA}s omitted,
#'   pasted together via \code{paste(..., collapse = "; ")}.
#'   When \code{segdata.method} is "maxdist", the reported values
#'   are, for each condition, the value recorded for the longest distance
#'   during that segment (with \code{NA}s omitted).
#'
#'   Cruise number, mode, effort type, sides searched, and file name are
#'   also included in the segdata output.
#'   These values (excluding \code{NA}s) must be consistent across the
#'   entire effort section, and thus across all segments in \code{x};
#'   a warning is printed if there are any inconsistencies
#'
#'   \code{\link[swfscMisc]{bearing}} and \code{\link[swfscMisc]{destination}}
#'   are used to calculate the segment start, mid, and end points,
#'   with \code{method = "vincenty"}.
#'
#' @return Data frame with the segdata information described in Details
#'   and in \code{\link{das_effort}}
#'
#' @export
das_segdata <- function(x, ...) UseMethod("das_segdata")


#' @name das_segdata
#' @export
das_segdata.data.frame <- function(x, ...) {
  das_segdata(as_das_df(x), ...)
}


#' @name das_segdata
#' @export
das_segdata.das_df <- function(x, conditions, segdata.method = c("avg", "maxdist"),
                               seg.lengths, section.id, ...) {
  #----------------------------------------------------------------------------
  # Input checks
  conditions <- .das_conditions_check(conditions, "condition")
  segdata.method <- match.arg(segdata.method)

  if (!("dist_from_prev" %in% names(x)))
    stop("x must contain a 'dist_from_prev' column; ",
         "was this function called by a _chop_ function?")

  stopifnot(
    inherits(seg.lengths, c("numeric", "integer")),
    inherits(section.id, c("numeric", "integer"))
  )

  if (!.equal(sum(seg.lengths), sum(x$dist_from_prev)))
    stop("The sum of the seg.lengths values does not equal the sum of the ",
         "x$dist_from_prev' values; ",
         "was this function called by a _chop_ function?")


  #----------------------------------------------------------------------------
  # Prep stuff - get the info that is consistent for the entire effort length
  # ymd determined below to be safe
  df.out1.cols <- c("file_das", "Cruise", "Mode", "EffType", "ESWsides")

  # <=1 accounts for when all
  df.out1.check <- vapply(df.out1.cols, function(i) {
    length(unique(na.omit(x[[i]]))) <= 1
  }, as.logical(1))
  if (!all(df.out1.check))
    warning("Is there an error in the data? ",
            "Not all of the following data were consistent across ",
            "continuous effort section ", section.id, ":\n",
            paste(df.out1.cols, collapse  = ", "))

  df.out1 <- x %>%
    select(!!df.out1.cols) %>%
    select(file = .data$file_das, everything()) %>%
    slice(n()) #use n() instead of 1 b/c some vars may be NA in first line


  #----------------------------------------------------------------------------
  segdata.all <- .segdata_proc(
    das.df = x, conditions = conditions, segdata.method = segdata.method,
    seg.lengths = seg.lengths, section.id = section.id, df.out1 = df.out1
  )

  #----------------------------------------------------------------------------
  segdata.all %>%
    select(.data$seg_idx, .data$section_id, .data$section_sub_id,
           .data$file, .data$stlin, .data$endlin,
           .data$lat1, .data$lon1, .data$DateTime1,
           .data$lat2, .data$lon2, .data$DateTime2,
           .data$mlat, .data$mlon, .data$mDateTime,
           .data$dist, .data$year, .data$month, .data$day, .data$mtime,
           everything())
}





#' @name swfscAirDAS-internals
#' @param das.df ignore
#' @param conditions ignore
#' @param segdata.method ignore
#' @param seg.lengths ignore
#' @param section.id ignore
#' @param df.out1 ignore
#' @export
.segdata_proc <- function(das.df, conditions, segdata.method,
                          seg.lengths, section.id, df.out1) {
  #----------------------------------------------------------------------------
  ### Prep
  # print(paste0(section.id, " | ", das.df$stlin[1]))
  # if (section.id == 6) browser()

  # Conditions list
  conditions.list.init <- lapply(seq_along(conditions), function(i) {
    data.frame(val = NA, dist = 0, stringsAsFactors = FALSE)
  })
  names(conditions.list.init) <- conditions
  conditions.names <- paste0(segdata.method, conditions)
  # switch(segdata.method, avg = "ave", maxdist = "maxdist"),


  # Calculate objects for for loop
  n.subseg <- length(seg.lengths)
  subseg.cumsum <- cumsum(seg.lengths)
  subseg.mid.cumsum <- (c(0, head(subseg.cumsum, -1)) + subseg.cumsum) / 2

  if (!("dist_from_prev_cumsum" %in% names(das.df)))
    das.df$dist_from_prev_cumsum <- cumsum(das.df$dist_from_prev)


  # 'Initialize' necessary objects
  subseg.curr <- 1
  stlin.curr <- das.df$line_num[1]
  startpt.curr <- c(das.df$Lat[1], das.df$Lon[1])
  startdt.curr <- das.df$DateTime[1]
  midpt.curr <- NULL
  segdata.all <- NULL

  conditions.list <- conditions.list.init

  if (!(nrow(das.df) >= 2))
    stop("Segdata - section ", section.id,
         " - das.df must have at least 2 rows; please report this as an issue")


  #----------------------------------------------------------------------------
  ### If length of continuous effort section is 0, do the basics
  if (isTRUE(all.equal(sum(das.df$dist_from_prev), 0))) {
    if (n.subseg == 1  & isTRUE(all.equal(subseg.cumsum, 0))) {
      conditions.list.df <- data.frame(
        lapply(names(conditions.list), function(k, das.df) {
          val.out <- unique(na.omit(das.df[[k]]))
          if (length(val.out) > 1)
            warning("The continuous effort section with section_id ",
                    section.id, " has a distance of 0, ",
                    "and multiple values for condition ", k,
                    ". Only the first element will be output",
                    immediate. = TRUE)
          if (length(val.out) == 0) NA else val.out
        }, das.df = das.df)
      )
      names(conditions.list.df) <- conditions.names

      enddt.curr <- tail(das.df$DateTime, 1)

      # ### Message printed in das_chop_equallength, outside of parallel calls
      # dt.test <- identical(startdt.curr, enddt.curr) |
      #   (abs(difftime(enddt.curr, startdt.curr, units = "sec")) < 10)
      # if (!dt.test)
      #   warning("Segdata - section ", section.id, " - this segment is a 0 length segment, ",
      #           "but spans more than 10 seconds. ",
      #           "It is strongly recommended that you review this effort section in the DAS file",
      #           immediate. = TRUE)

      segdata.all <- data.frame(
        seg_idx = paste(section.id, 1, sep = "_"),
        section_id = section.id,
        section_sub_id = 1,
        stlin = min(das.df$line_num), endlin = max(das.df$line_num),
        lat1 = startpt.curr[1], lon1 = startpt.curr[2], DateTime1 = startdt.curr,
        lat2 = startpt.curr[1], lon2 = startpt.curr[2], DateTime2 = enddt.curr,
        mlat = startpt.curr[1], mlon = startpt.curr[2], mDateTime = mean(c(startdt.curr, enddt.curr)),
        dist = 0,
        stringsAsFactors = FALSE
      ) %>%
        mutate(mtime = strftime(.data$mDateTime, format = "%H:%M:%S",
                                tz = tz(.data$mDateTime)),
               year = year(.data$mDateTime), month = month(.data$mDateTime),
               day = day(.data$mDateTime)) %>%
        bind_cols(df.out1, conditions.list.df)

    } else {
      stop("segdata inconsistency - please report this as an issue")
    }

    #--------------------------------------------------------------------------
  } else {
    ### If the distance is > 0, step through each point in effort length,
    ###   calculating segment points, avg conditions, etc. as you go
    for (j in 2:nrow(das.df)) {
      # t1 and t2: Is point j past the segment midpt or endpt, respectively,
      #   i.e. do we need to calculate the midpt or endpt?
      dist.pt.curr <- das.df$dist_from_prev_cumsum[j]
      t1 <- .greater_equal(dist.pt.curr, subseg.mid.cumsum[subseg.curr])
      t2 <- .greater_equal(dist.pt.curr, subseg.cumsum[subseg.curr])

      if (!t2) {
        # If we didn't cross a segment endpoint, get
        #   1) the percentage of the segment between j-1 and j, and
        #   2) the condition and sight info
        seg.perc <- das.df$dist_from_prev[j] / seg.lengths[subseg.curr]
        conditions.list <- .segdata_aggr(conditions.list, das.df, j-1, seg.perc)
        rm(seg.perc)
      }

      # While the current subsegment midpoint or endpoint
      #   comes before the next event (which is indexed by j)
      while((t1 & is.null(midpt.curr)) | t2) {
        ### Make objects for values used multiple times (pt2)
        # Needs to be here for when there are multiple trips through while loop
        dist.subseg.curr <- subseg.cumsum[subseg.curr]
        dist.subseg.prev <- subseg.cumsum[subseg.curr - 1]
        dist.pt.curr     <- das.df$dist_from_prev_cumsum[j]
        dist.pt.prev     <- das.df$dist_from_prev_cumsum[j-1]

        ### Get data
        # Calculate midpoint (if not already done for this segment)
        if (t1 & is.null(midpt.curr)) {
          midpt.curr <- destination(
            das.df$Lat[j-1], das.df$Lon[j-1],
            bearing(das.df$Lat[j-1], das.df$Lon[j-1], das.df$Lat[j], das.df$Lon[j])[1],
            units = "km",
            distance = subseg.mid.cumsum[subseg.curr] - dist.pt.prev
          )
        }

        # Calculate endpoint
        if (t2) {
          ### Destination calculated from das.df[j-1, ], so d calc is ok
          ###   (destination calculates the endpoint, not the seg length)
          d <- dist.subseg.curr - dist.pt.prev
          endpt.curr <- destination(
            das.df$Lat[j-1], das.df$Lon[j-1],
            bearing(das.df$Lat[j-1], das.df$Lon[j-1], das.df$Lat[j], das.df$Lon[j])[1],
            units = "km", type = "vincenty", distance = d
          )

          ### Get end datetime
          dt.rat <- if (.equal(das.df$dist_from_prev[j], 0)) 1 else d / das.df$dist_from_prev[j]
          dt.difftime <- difftime(das.df$DateTime[j], das.df$DateTime[j-1], units = "secs")
          enddt.curr <- das.df$DateTime[j-1] + (dt.rat * dt.difftime)
          rm(dt.rat, dt.difftime)


          ### Conditions and sightings
          #     d.tmp handles multiple segments between pts, aka when current
          #     segment start point is closer than [j-1]
          d.tmp <- max(dist.pt.prev, dist.subseg.prev)
          d.rat <- (dist.subseg.curr - d.tmp) / seg.lengths[subseg.curr]
          # if (is.nan(d.rat)) d.rat <- NA
          conditions.list <- .segdata_aggr(conditions.list, das.df, j-1, d.rat)

          rm(d, d.tmp, d.rat)

          ## If next point is at the same location, don't end the segment yet
          if (j < nrow(das.df)) {
            if (das.df$dist_from_prev[j+1] == 0) {
              # tmp1 is dist from current point to end of last segment
              tmp1a <- ifelse(subseg.curr > 1, subseg.cumsum[subseg.curr-1], 0)
              tmp1 <- dist.pt.curr - tmp1a
              tmp2 <- seg.lengths[subseg.curr]

              if (.less_equal(tmp1, tmp2)) {break}
              rm(tmp1a, tmp1, tmp2)
            }
          }

          ### Store data from this segment
          # Get condition information
          if (segdata.method == "avg") {
            conditions.list.df <- data.frame(
              lapply(names(conditions.list), function(k, k.list) {
                if (inherits(k.list[[k]]$val, "character")) {
                  paste(unique(na.omit(k.list[[k]]$val)), collapse = ";")

                } else {
                  #.segdata_aggr() throws an error if not character or numeric
                  tmp <- k.list[[k]] %>%
                    filter(!is.na(.data$val)) %>%
                    mutate(val_frac = .data$val * .data$dist)

                  if (nrow(tmp) == 0) NA else sum(tmp$val_frac) / sum(tmp$dist)
                  # Currently no rounding for comparison with EAB
                  #round(sum(tmp$val_frac) / sum(tmp$dist), 2)
                }
              }, k.list = conditions.list),
              stringsAsFactors = FALSE
            )

          } else if (segdata.method == "maxdist") {
            conditions.list.df <- data.frame(
              lapply(names(conditions.list), function(k, k.list) {
                tmp <- k.list[[k]] %>%
                  filter(!is.na(.data$val)) %>%
                  group_by(.data$val) %>%
                  summarise(dist_sum = sum(as.numeric(.data$dist))) %>%
                  arrange(desc(.data$dist_sum), .data$val)

                if (nrow(tmp) == 0) NA else tmp$val[1]
              }, k.list = conditions.list),
              stringsAsFactors = FALSE
            )

          } else {
            stop("Unrecognized segdata.method; please report this as an issue")
          }

          names(conditions.list.df) <- conditions.names


          # Get start line
          j.stlin.curr <- which(das.df$line_num == stlin.curr)

          # Add segdata to .all data frame
          segdata <- data.frame(
            seg_idx = paste(section.id, subseg.curr, sep = "_"),
            section_id = section.id,
            section_sub_id = subseg.curr,
            stlin = stlin.curr, endlin = das.df$line_num[j],
            lat1 = startpt.curr[1], lon1 = startpt.curr[2], DateTime1 = startdt.curr,
            lat2 = endpt.curr[1], lon2 = endpt.curr[2], DateTime2 = enddt.curr,
            mlat = midpt.curr[1], mlon = midpt.curr[2], mDateTime = mean(c(startdt.curr, enddt.curr)),
            dist = seg.lengths[subseg.curr],
            stringsAsFactors = FALSE
          ) %>%
            mutate(mtime = strftime(.data$mDateTime, format = "%H:%M:%S",
                                    tz = tz(.data$mDateTime)),
                   year = year(.data$mDateTime), month = month(.data$mDateTime),
                   day = day(.data$mDateTime)) %>%
            bind_cols(df.out1, conditions.list.df)

          segdata.all <- rbind(segdata.all, segdata)
          rm(conditions.list.df, j.stlin.curr, segdata) #obs.vals


          ### Prep for next segment
          if (j == nrow(das.df) & subseg.curr == n.subseg) {
            # If at the end of das.df and all segs have been processed, break
            break

          } else {
            # Else, prep for next segment:
            # Increment
            subseg.curr <- subseg.curr + 1

            # Reset/set points as appropriate
            startpt.curr <- endpt.curr
            startdt.curr <- enddt.curr
            midpt.curr <- NULL
            endpt.curr <- NULL
            enddt.curr <- NULL
            stlin.curr <- das.df$line_num[j]

            t1 <- .greater_equal(dist.pt.curr, subseg.mid.cumsum[subseg.curr])
            t2 <- .greater_equal(dist.pt.curr, subseg.cumsum[subseg.curr])

            # If pt j is before the next seg endpoint, get data from endpt to j
            #   Else, this info is calculated in t2 section above
            tmp1 <- das.df$dist_from_prev_cumsum[j] - subseg.cumsum[subseg.curr - 1]
            tmp2 <- seg.lengths[subseg.curr]
            conditions.list <- conditions.list.init

            if (.less(tmp1, tmp2)) {
              conditions.list <- .segdata_aggr(
                conditions.list.init, das.df, j-1, tmp1 / tmp2
              )
            }
            rm(tmp1, tmp2)
          }
        }
      }
    }
  }


  #--------------------------------------------------------------------------
  # Ensure longitudes are between -180 and 180, and return
  segdata.all %>%
    mutate(lon1 = ifelse(.greater(.data$lon1, 180), .data$lon1 - 360, .data$lon1),
           lon1 = ifelse(.less(.data$lon1, -180), .data$lon1 + 360, .data$lon1),
           lon2 = ifelse(.greater(.data$lon2, 180), .data$lon2 - 360, .data$lon2),
           lon2 = ifelse(.less(.data$lon2, -180), .data$lon2 + 360, .data$lon2),
           mlon = ifelse(.greater(.data$mlon, 180), .data$mlon - 360, .data$mlon),
           mlon = ifelse(.less(.data$mlon, -180), .data$mlon + 360, .data$mlon))
}




#' @name swfscAirDAS-internals
#' @param data.list ignore
#' @param curr.df ignore
#' @param idx ignore
#' @param dist.perc ignore
#' @export
.segdata_aggr <- function(data.list, curr.df, idx, dist.perc) {
  stopifnot(
    all(names(data.list) %in% names(curr.df)),
    idx <= nrow(curr.df)
  )

  if (is.na(dist.perc)) {
    warning("dist.perc is NA, ignoring. Report this as an issue",
            immediate. = TRUE)
    data.list    #lapply(data.list, function(i) NA)

  } else if (dist.perc == 0) {
    data.list

  } else {
    tmp <- lapply(names(data.list), function(k) {
      rbind(data.list[[k]], c(curr.df[[k]][idx], dist.perc))
    })
    names(tmp) <- names(data.list)
    tmp
  }
}
