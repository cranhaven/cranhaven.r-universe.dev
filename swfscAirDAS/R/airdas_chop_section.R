#' Chop AirDAS data - section
#' 
#' Chop AirDAS data into effort segments by continuous effort section
#' 
#' @param x \code{airdas_df} object, 
#'   or a data frame that can be coerced to a \code{airdas_df} object. 
#'   This data must be filtered for 'OnEffort' events; 
#'   see the Details section below
#' @param ... ignored
#' @param conditions see \code{\link{airdas_effort}}
#' @param distance.method character; see \code{\link{airdas_effort}}.
#'   Default is \code{NULL} since these distances should have already been
#'   calculated in \code{\link{airdas_effort}}
#' @param num.cores See \code{\link{airdas_effort}}
#' 
#' @details WARNING - do not call this function directly!
#'   It is exported for documentation purposes, but is intended for internal package use only.
#'
#'   This function is simply a wrapper for \code{\link{airdas_chop_equallength}}.
#'   It calls \code{\link{airdas_chop_equallength}}, with \code{seg.km} set to a 
#'   value larger than the longest continuous effort section in \code{x}. 
#'   Thus, the effort is 'chopped' into the continuous effort sections and then summarized.
#'   
#'   See the Examples section for an example where the two methods give the same output.
#'   Note that the longest continuous effort section in the sample data is ~32km.
#'   
#'   For an example of how to summarize data by transect, 
#'   see \code{vignette("swfscAirDAS")}.
#'   In short, if looking to group by individual transects, use 
#'   \code{segdata$transect_idx <- cumsum(segdata$event == "T")} 
#'   to create a column with a transect index. 
#'   Then you can use \code{\link[dplyr]{group_by}(transect_idx)} and 
#'   \code{\link[dplyr]{summarise}} to summarise the desired data by transect
#'   
#' @return See \code{\link{airdas_chop_equallength}}. 
#'   The randpicks values will all be \code{NA}
#'   
#' @examples 
#' y <- system.file("airdas_sample.das", package = "swfscAirDAS")
#' y.proc <- airdas_process(y)
#'
#' y.eff1 <- airdas_effort(y.proc, method = "equallength", seg.km = 35, num.cores = 1)
#' y.eff2 <- airdas_effort(y.proc, method = "section", num.cores = 1)
#'
#' all.equal(y.eff1, y.eff2)
#' 
#' @export
airdas_chop_section <- function(x, ...) UseMethod("airdas_chop_section")


#' @name airdas_chop_section
#' @export
airdas_chop_section.data.frame <- function(x, ...) {
  airdas_chop_section(as_airdas_df(x), ...)
}


#' @name airdas_chop_section
#' @export
airdas_chop_section.airdas_df <- function(x, conditions, distance.method = NULL, 
                                          num.cores = NULL, ...) {
  #----------------------------------------------------------------------------
  # Input checks
  if (!all(x$OnEffort | x$Event %in% c("O", "E"))) 
    stop("x must be filtered for on effort events; see `?airdas_chop_equallength")
  
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
  
  #----------------------------------------------------------------------------
  # ID continuous effort sections, make randpicks, and get max section length
  if (!("cont_eff_section" %in% names(x))) {
    x$cont_eff_section <- cumsum(x$Event %in% c("T", "R"))
  }
  
  randpicks.df <- data.frame(
    effort_section = sort(unique(x$cont_eff_section)), 
    randpicks = NA
  )
  
  x.summ <- x %>% 
    mutate(ces_dup = duplicated(.data$cont_eff_section), 
           dist_from_prev_sect = ifelse(.data$ces_dup, .data$dist_from_prev, NA)) %>% 
    group_by(.data$cont_eff_section) %>% 
    summarise(dist_sum = sum(.data$dist_from_prev_sect, na.rm = TRUE))
  
  # Call airdas_chop_equallength using max section length + 1
  airdas_chop_equallength(
    x %>% select(-.data$cont_eff_section), 
    conditions = conditions, 
    seg.km = max(x.summ$dist_sum) + 1, randpicks.load = randpicks.df, 
    num.cores = num.cores  
  )
}
