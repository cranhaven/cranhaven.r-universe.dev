#' Determines the generation number for each id.
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' One of Pedigree Curation functions
#'
#' @return Integer vector indicating generation numbers for each id,
#' starting at 0 for individuals lacking IDs for both parents.
#'
#' @param id character vector with unique identifier for an individual
#' @param sire character vector with unique identifier for an
#' individual's father (\code{NA} if unknown).
#' @param dam character vector with unique identifier for an
#' individual's mother (\code{NA} if unknown).
#' @export
#' @examples
#' library(nprcgenekeepr)
#' library(stringi)
#' ped <- nprcgenekeepr::lacy1989Ped
#' ped$gen <- NULL
#' ped$population <- NULL
#' ped2 <- ped
#' ped2$id <- stri_c(ped$id, "2")
#' ped2$sire <- stri_c(ped$sire, "2")
#' ped2$dam <- stri_c(ped$dam, "2")
#' ped3 <- ped
#' ped3$id <- stri_c(ped$id, "3")
#' ped3$sire <- stri_c(ped$sire, "3")
#' ped3$dam <- stri_c(ped$dam, "3")
#' ped <- rbind(ped, ped2)
#' ped <- rbind(ped, ped3)
#' ped$pedigree <- findPedigreeNumber(ped$id, ped$sire, ped$dam)
#' ped$pedigree
findPedigreeNumber <- function(id, sire, dam) {
  founders <- id[is.na(sire) & is.na(dam)]
  pedNum <- rep(NA, length(id))
  n <- 1L

  while (!isEmpty(founders)) {
    population <- founders[1L]

    repeat {
      parents <- union(
        sire[id %in% population],
        dam[id %in% population]
      )
      parents <- parents[!is.na(parents)]

      offspring <- id[(sire %in% population) | (dam %in% population)]

      added <- setdiff(union(offspring, parents), population)

      if (isEmpty(added)) {
        break
      }

      population <- union(population, union(parents, offspring))
    }
    pedNum[id %in% population] <- n
    n <- n + 1L

    founders <- setdiff(founders, population)
  }
  pedNum
}
