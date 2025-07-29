#' createPedSix makes the pedSix data object
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' @return A specific pedigree object used for testing and examples.
#'
#' @param savePed logical value if TRUE the pedigree is saved into the
#' packages \code{data} directory
#' @importFrom lubridate mdy dyears ymd
#' @importFrom stringi stri_c
#' @noRd
createPedSix <- function(savePed = TRUE) {
  set_seed(10L)
  someBirthDates <-
    ymd(paste0(
      sample(seq(0L, 15L, by = 3L), 8L, replace = TRUE) + 2000L,
      "-",
      sample.int(12L, 8L, replace = TRUE),
      "-",
      sample.int(28L, 8L, replace = TRUE)
    ))
  someBadBirthDates <-
    mdy(paste0(
      sample.int(12L, 8L, replace = TRUE),
      "-",
      sample.int(28L, 8L, replace = TRUE),
      "-",
      sample(seq(0L, 15L, by = 3L), 8L, replace = TRUE) + 2000L
    ))
  someDeathDates <-
    sample(someBirthDates, length(someBirthDates), replace = FALSE)
  someDepartureDates <-
    sample(someBirthDates, length(someBirthDates), replace = FALSE)
  pedOne <-
    data.frame(
      birth = someBadBirthDates,
      death = someDeathDates,
      departure = someDepartureDates,
      stringsAsFactors = FALSE
    )
  pedFive <-
    data.frame(
      id = c("s1", "d1", "s2", "d2", "o1", "o2", "o3", "o4"),
      sire = c(NA, "s0", "s4", NA, "s1", "s1", "s2", "s2"),
      dam = c(NA, "d0", "d4", NA, "d1", "d2", "d2", "d2"),
      sex = c("F", "F", "M", "F", "F", "F", "F", "M"),
      birth = mdy(paste0(
        sample.int(12L, 8L, replace = TRUE),
        "-",
        sample.int(28L, 8L, replace = TRUE),
        "-",
        sample(seq(0L, 15L, by = 3L), 8L, replace = TRUE) +
          2000L
      )),
      stringsAsFactors = FALSE
    )
  pedSix <-
    data.frame(pedFive[, names(pedFive) != "birth"], pedOne,
      stringsAsFactors = FALSE
    )
  pedSix$birth[pedSix$id %in% c("s1", "s2", "d1", "d2")] <-
    pedSix$birth[pedSix$id %in% c("s1", "s2", "d1", "d2")] - dyears(20L)
  names(pedSix) <-
    c(
      "Ego Id",
      "Sire Id",
      "Dam",
      "Sex",
      "Birth Date",
      "Departure",
      "Death"
    )
  if (savePed) {
    pedigree_dir <- tempdir()
    suppressWarnings(dir.create(pedigree_dir))
    pedigree_dir <- file.path(pedigree_dir, "data")
    suppressWarnings(dir.create(pedigree_dir))
    save(pedSix, file = file.path(pedigree_dir, "pedSix.RData"))
  }
  pedSix
}
