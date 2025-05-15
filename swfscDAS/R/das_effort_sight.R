#' Summarize DAS sightings by effort segment
#'
#' Summarize number of sightings and animals for selected species by segment
#'
#' @param x.list output of \code{\link{das_effort}}; a list of three data frames
#'   named 'segdata', 'sightinfo', and 'randpicks', respectively
#' @param sp.codes character; species code(s) to include in segdata output.
#'   These must exactly match the species codes in the data,
#'   such as including leading zeros
#' @param sp.events character; event code(s) to include in the sightinfo output.
#'   This argument supersedes the 'included' value when determining
#'   whether a sighting is included in the segment summaries.
#'   Must be one or more of: "S", "K", "M", "G", "t", "p" (case-sensitive).
#'   The default is that all of these event codes are kept
#' @param gs.columns character; the column(s) to use to get the group size
#'   values that will be summarized in the segdata output.
#'   Must be one or more of 'GsSpBest', 'GsSpLow', and 'GsSpBest' (case-sensitive).
#'   See Details section for more information
#'
#' @details This function takes the output of \code{\link{das_effort}} and
#'   adds columns for the number of sightings (nSI) and number of animals (ANI)
#'   for selected species (selected via \code{sp.codes}) for each segment
#'   to the segdata element of \code{x.list}.
#'   However, only sightings with an included value of \code{TRUE}
#'   (included is a column in sightinfo) are included in the summaries.
#'   Having this step separate from \code{\link{das_effort}} allows users to
#'   personalize the included values as desired for their analysis.
#'
#'   The ANI columns are the sum of the 'GsSp...' column(s) from
#'   \code{\link{das_sight}} specified using \code{gs.columns}.
#'   If \code{gs.columns} specifies more than one column,
#'   then the secondary columns will only be used if
#'   the values for the previous columns are \code{NA}.
#'   For instance, if \code{gs.columns = c('GsSpBest', 'GsSpLow')},
#'   then for each row in sightinfo, the value from GsSpLow
#'   will be used only if the value from GsSpBest is \code{NA}
#'
#' @return A list, identical to \code{x.list} except for
#'   1) the nSI and ANI columns added to \code{x.list$segdata},
#'   one each for each element of \code{sp.codes}, and
#'   2) the 'included' column of \code{x.list$sightinfo}, which has been set as
#'   \code{FALSE} for sightings of species not listed in \code{sp.codes}.
#'   Thus, the 'included' column in the output accurately reflects
#'   the sightings that were included in the effort segment summaries
#'
#' @examples
#' y <- system.file("das_sample.das", package = "swfscDAS")
#' y.proc <- das_process(y)
#' y.eff.cond <- das_effort(
#'   y.proc, method = "condition", conditions = "Bft", seg.min.km = 0.05,
#'   num.cores = 1
#' )
#'
#' das_effort_sight(y.eff.cond, sp.codes = c("013", "076", "DC"), sp.events = c("S", "t"))
#'
#' @export
das_effort_sight <- function(x.list, sp.codes, sp.events = c("S", "G", "K", "M", "t", "p"),
                             gs.columns = c("GsSpBest", "GsSpLow", "GsSpHigh")) {
  ### Input checks
  stopifnot(
    inherits(x.list, "list"),
    inherits(sp.codes, "character"),
    inherits(sp.events, "character"),
    identical(names(x.list), c("segdata", "sightinfo", "randpicks")),
    "included" %in% names(x.list$sightinfo)
  )

  sp.events <- match.arg(sp.events, several.ok = TRUE)
  gs.columns <- match.arg(gs.columns, several.ok = TRUE)

  ### Prep
  segdata <- x.list$segdata
  sightinfo <- x.list$sightinfo %>% filter(.data$Event %in% sp.events)
  randpicks <- x.list$randpicks

  ### Processing
  # Prep sp.codes
  sp.codes <- sort(sp.codes)
  if (!all(sp.codes %in% sightinfo$SpCode))
    message("The following species codes are not present in the provided data: ",
            paste(sp.codes[!(sp.codes %in% sightinfo$SpCode)], collapse = ", "))

  # Make data frame with nSI and ANI columns, and join it with segdata
  segdata$seg_idx <- paste(segdata$section_id, segdata$section_sub_id, sep = "_")
  sightinfo <- left_join(sightinfo, select(segdata, .data$segnum, .data$seg_idx),
                         by = "segnum")

  sightinfo$GsSegment <- apply(select(sightinfo, !!gs.columns), 1, function(i) {
    i.na <- is.na(i)
    if (all(i.na)) NA else i[!i.na][1]
  })

  segdata.col1 <- select(segdata, .data$seg_idx)
  sightinfo.forsegdata.list <- lapply(sp.codes, function(i, sightinfo, d1) {
    d0 <- sightinfo %>%
      filter(.data$included, .data$SpCode == i) %>%
      group_by(.data$seg_idx) %>%
      summarise(nSI = n(),
                ANI = sum(.data$GsSegment))

    names(d0) <- c("seg_idx", paste(names(d0)[-1], i, sep = "_"))

    z <- full_join(d1, d0, by = "seg_idx") %>% select(-.data$seg_idx)
    z[is.na(z)] <- 0

    z
  }, sightinfo = sightinfo, d1 = segdata.col1)

  sightinfo.forsegdata.df <- bind_cols(segdata.col1, sightinfo.forsegdata.list)


  ### Clean up and return
  segdata <- segdata %>%
    left_join(sightinfo.forsegdata.df, by = "seg_idx") %>%
    select(-.data$seg_idx)

  sightinfo <- sightinfo %>%
    mutate(included = ifelse(.data$SpCode %in% sp.codes, .data$included, FALSE)) %>%
    select(-.data$seg_idx, -.data$GsSegment)

  list(segdata = segdata, sightinfo = sightinfo, randpicks = randpicks)
}
