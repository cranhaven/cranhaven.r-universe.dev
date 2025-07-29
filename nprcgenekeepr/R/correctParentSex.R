#' Sets sex for animals listed as either a sire or dam.
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Part of Pedigree Curation
#'
#' @return A factor with levels: "M", "F", "H", and "U"
#' representing the sex codes for the ids provided
#'
#' @param id character vector with unique identifier for an individual
#' @param sire character vector with unique identifier for an
#' individual's father (\code{NA} if unknown).
#' @param dam character vector with unique identifier for an
#' individual's mother (\code{NA} if unknown).
#' @param sex factor with levels: "M", "F", "U". Sex specifier for an
#' individual.
#' @param reportErrors logical value if TRUE will scan the entire file and
#' make a list of all errors found. The errors will be returned in a
#' list of list where each sublist is a type of error found.
#' @param recordStatus character vector with value of \code{"added"} or
#' \code{"original"}, which indicates whether an animal was added or an
#' original animal.
#' @export
#' @examples
#' library(nprcgenekeepr)
#' pedOne <- data.frame(
#'   id = c("s1", "d1", "s2", "d2", "o1", "o2", "o3", "o4"),
#'   sire = c(NA, "s0", "s4", NA, "s1", "s1", "s2", "s2"),
#'   dam = c(NA, "d0", "d4", NA, "d1", "d2", "d2", "d2"),
#'   sex = c("F", "F", "M", "F", "F", "F", "F", "M"),
#'   recordStatus = rep("original", 8),
#'   stringsAsFactors = FALSE
#' )
#' pedTwo <- data.frame(
#'   id = c("s1", "d1", "s2", "d2", "o1", "o2", "o3", "o4"),
#'   sire = c(NA, "s0", "s4", NA, "s1", "s1", "s2", "s2"),
#'   dam = c("d0", "d0", "d4", NA, "d1", "d2", "d2", "d2"),
#'   sex = c("M", "M", "M", "F", "F", "F", "F", "M"),
#'   recordStatus = rep("original", 8),
#'   stringsAsFactors = FALSE
#' )
#' pedOneCorrected <- pedOne
#' pedOneCorrected$sex <- correctParentSex(
#'   pedOne$id, pedOne$sire, pedOne$dam,
#'   pedOne$sex, pedOne$recordStatus
#' )
#' pedOne[pedOne$sex != pedOneCorrected$sex, ]
#' pedOneCorrected[pedOne$sex != pedOneCorrected$sex, ]
#'
#' pedTwoCorrected <- pedTwo
#' pedTwoCorrected$sex <- correctParentSex(
#'   pedTwo$id, pedTwo$sire, pedTwo$dam,
#'   pedTwo$sex, pedOne$recordStatus
#' )
#' pedTwo[pedTwo$sex != pedTwoCorrected$sex, ]
#' pedTwoCorrected[pedTwo$sex != pedTwoCorrected$sex, ]
correctParentSex <- function(id, sire, dam, sex, recordStatus,
                             reportErrors = FALSE) {
  # Get all sires and dams
  sires <- unique(sire)
  sires <- sires[!is.na(sires)]
  dams <- unique(dam)
  dams <- dams[!is.na(dams)]

  # Check if any ids are listed in both the sire and dam columns (error)
  sireAndDam <- intersect(sires, dams)
  if ((length(sireAndDam) > 0L) && !reportErrors) {
    stop(sireAndDam, " : Subject(s) listed as both sire and dam")
  }
  if (reportErrors) {
    femaleSires <- id[(id %in% sires) & (!sex %in% c("H", "U", "M")) &
      recordStatus == "original"]
    maleDams <- id[(id %in% dams) & (!sex %in% c("H", "U", "F")) &
      recordStatus == "original"]
    if (length(femaleSires) == 0L) {
      femaleSires <- NULL
    }
    if (length(maleDams) == 0L) {
      maleDams <- NULL
    }
    if (length(sireAndDam) == 0L) {
      sireAndDam <- NULL
    }
    list(
      sireAndDam = sireAndDam, femaleSires = femaleSires,
      maleDams = maleDams
    )
  } else {
    # Update gender for sires and dams
    sex[((id %in% sires) & (sex != "M"))] <- "M"
    sex[((id %in% dams) & (sex != "F"))] <- "F"
    return(sex)
  }
}
