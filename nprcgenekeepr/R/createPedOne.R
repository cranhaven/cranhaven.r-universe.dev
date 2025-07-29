#' createPedOne makes the pedOne data object
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @return A specific pedigree object used for testing and examples.
#'
#' @param savePed logical value if TRUE the pedigree is saved into the
#' packages \code{data} directory
#' @importFrom lubridate mdy
#' @importFrom stringi stri_c
#' @noRd
createPedOne <- function(savePed = TRUE) {
  set_seed(10L)
  pedOne <- data.frame(
    ego_id = c("s1", "d1", "s2", "d2", "o1", "o2", "o3", "o4"),
    `si re` = c(NA, NA, NA, NA, "s1", "s1", "s2", "s2"),
    dam_id = c(NA, NA, NA, NA, "d1", "d2", "d2", "d2"),
    sex = c("F", "M", "M", "F", "F", "F", "F", "M"),
    birth_date = mdy(paste0(
      sample.int(12L, 8L, replace = TRUE),
      "-",
      sample.int(28L, 8L, replace = TRUE),
      "-",
      sample(seq(0L, 15L, by = 3L), 8L, replace = TRUE) +
        2000L
    )),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (savePed) {
    pedigree_dir <- tempdir()
    suppressWarnings(dir.create(pedigree_dir))
    pedigree_dir <- file.path(pedigree_dir, "data")
    message("pedigreeOne written into ", pedigree_dir, "/\n")
    suppressWarnings(dir.create(pedigree_dir))
    save(pedOne, file = stri_c(pedigree_dir, "/pedOne.RData"))
  }
  pedOne
}
