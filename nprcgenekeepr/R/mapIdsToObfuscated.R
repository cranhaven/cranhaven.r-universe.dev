#' Map IDs to Obfuscated IDs
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' This is not robust as it fails if all IDs are found not within \code{map}.
#'
#' @return A dataframe or vector with original IDs replaced by their obfuscated
#' counterparts.
#'
#' @param ids character vector with original IDs
#' @param map named character vector where the values are the obfuscated IDs
#' and the vector of names (\code{names(map)}) is the vector of original names.
#' @export
#' @examples
#' set_seed(1)
#' ped <- qcStudbook(nprcgenekeepr::pedSix)
#' obfuscated <- obfuscatePed(ped, map = TRUE)
#' someIds <- c("s1", "s2", "d1", "d1")
#' mapIdsToObfuscated(someIds, obfuscated$map)
mapIdsToObfuscated <- function(ids, map) {
  if (!all(ids %in% names(map))) {
    stop("Some IDs are not in map.")
  }
  as.character(vapply(ids, function(id) {
    map[names(map) == as.character(id)]
  }, character(1L)))
}
