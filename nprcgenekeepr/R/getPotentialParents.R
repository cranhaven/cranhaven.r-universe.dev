#' Get the lists of portential parents for all individuals born in the colony
#' with one or two unknown parents.
#'
#' `r lifecycle::badge('experimental')`
#'
#' @return a list of list with each internal list being made up of an animal
#' id (\code{id}), a vector of possible sires (\code{sire}) and a vector of
#' possible dams (\code{dam}). The \code{id} must be defined while the
#' vectors \code{sire} and \code{dam} can be empty.
#'
#' @param ped the pedigree information in data.frame format. Pedigree
#' (req. fields: id, sire, dam, gen, population).
#' This requires complete pedigree information.
#' @param minParentAge numeric values to set the minimum age in years for
#' an animal to have an offspring. Defaults to 2 years. The check is not
#' performed for animals with missing birth dates.
#' @param maxGestationalPeriod integer value describing the days between
#' conception and birth. This will be used to prevent the removal of sires
#' who exit the colony between date of conception and birth. Need to decide
#' where this will come from.
#' @importFrom data.table setDT
#' @importFrom stringi stri_sub
#' @export
getPotentialParents <- function(ped, minParentAge, maxGestationalPeriod) {
  birth <- exit <- fromCenter <- id <- sex <- NULL

  ## No point in looking at animals without a birth record.
  data.table::setDT(ped)
  ped <- ped[!is.na(ped$birth), ]
  ## No point in looking for potential parents without a "fromCenter" column.
  if (!any(names(ped) == "fromCenter")) {
    return(NULL)
  }
  ## Remove the records of automatically generated IDs
  ped <- removeAutoGenIds(ped)

  ## pUnknown becomes the pedigree records of animals with at least one unknown
  ## parent
  pUnknown <- ped[fromCenter &
    (is.na(ped$sire) | is.na(ped$dam)), ]
  pUnknown <- pUnknown[!is.na(pUnknown$id), ]

  dYear <- 365L # used for number of days in a year

  ## add calcs for births and pre-allocate memory

  potentialParents <- vector(mode = "list", length = nrow(pUnknown))
  if (nrow(pUnknown) > 0L) {
    j <- 0L # counter for potentialParents; used to prevent NULL entries
    for (i in seq_len(nrow(pUnknown))) {
      ## Calculating breeding age potential parents
      ba <- ped[birth <= (pUnknown$birth[i] - (dYear * minParentAge)), ]
      ba <- ba[!is.na(ba$id), ]
      if (nrow(ba) == 0L) {
        next
      }
      j <- j + 1L
      ## Selecting sires
      potentialSires <- ba[
        sex == "M" &
          (is.na(ba$exit) |
            exit >= (pUnknown$birth[i] - maxGestationalPeriod)),
        id
      ]

      ## Selecting dams
      potentialDams <- ba[sex == "F" &
        (is.na(ba$exit) |
          exit >= pUnknown$birth[i]), ]

      ## Females who had an offspring in rolling year of focal offspring birth
      births <-
        ped[birth >= pUnknown$birth[i] - (dYear / 2L) &
          birth <= pUnknown$birth[i] + (dYear / 2L), ]

      ## Females who had an offspring in the year prior or year after
      births_plus_minus_one <-
        ped[(
          birth <= pUnknown$birth[i] + (dYear * 1.5) &
            birth > pUnknown$birth[i] + (dYear / 2L)
        ) |
          (
            birth >= pUnknown$birth[i] - (dYear * 1.5) &
              birth < pUnknown$birth[i] - (dYear / 2L)
          ), ]
      births_plus_minus_one <-
        births_plus_minus_one[!duplicated(births_plus_minus_one$dam), ]


      ## Remove from consideration those dams who gave birth within 1/2 year
      ## of birth date
      ## TODO: This is a bit of a hack. We should be able to do this in a more
      ## principled way using gestational length.
      potentialDams <- potentialDams[!id %in% births$dam, ]
      ## Preferrentially accept dams that are proven breeders near the time of
      ## the birth.
      potentialDams <- potentialDams[id %in% births_plus_minus_one$dam, ]
      ## If no potential dams have been identified thus far, accept all females
      ## old enough to be the dam.
      if (nrow(potentialDams) == 0L) {
        potentialDams <-
          ba[sex == "F" & (is.na(ba$exit) | exit >= pUnknown$birth[i]), ]
      }

      potentialParents[[j]] <- list(
        id = pUnknown$id[i][1L],
        sires = potentialSires,
        dams = potentialDams$id
      )
    }
  }
  if (j > 0L) {
    potentialParents[1L:j]
  } else {
    NULL
  }
}
