#' Updates list of available animals by removing the selected animal
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @return list of available animals
#'
#' @param available list of available animals for each group
#' @param ids character vector having the selected animal Ids
#' @param numGp integer indicating the number of groups being formed.
#' @noRd
removeSelectedAnimalFromAvailableAnimals <- function(available, ids, numGp) { # nolint: object_length_linter
  # Remove the selected animal from consideration
  for (j in 1L:numGp) {
    available[[j]] <- setdiff(available[[j]], ids)
  }
  available
}
