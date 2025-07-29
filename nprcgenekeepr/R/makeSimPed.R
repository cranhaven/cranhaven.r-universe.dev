#' Makes a simulated pedigree using representative sires and dams
#'
#' For each \code{id} in \code{allSimParents} with one or more unknown parents
#' each unknown parent is replaced with a random sire or dam as needed from
#' the corresponding parent vector (\code{sires} or \code{dams}).
#'
#' The algorithm assigns parents randomly from the lists of possible sires and
#' dams and does not prevent a dam from being selected more than once within
#' the same breeding period. While this is probably not introducing a large
#' error, it is not ideal.
#'
#' @return simulated pedigree in data.frame format with the id, sire, and dam.
#'
#' @param ped pedigree information in data.frame format
#' @param allSimParents list made up of lists where the internal list
#'        has the offspring ID \code{id}, a vector of representative sires
#'        (\code{sires}), and a vector of representative dams (\code{dams}).
#' @param verbose logical vector of length one that indicates whether or not
#'        to print out when an animal is missing a sire or a dam.
#' @importFrom data.table setDT
#' @export
makeSimPed <- function(ped, allSimParents, verbose = FALSE) {
  nIds <- length(allSimParents)
  if (!inherits(ped, "data.table")) {
    data.table::setDT(ped)
  }

  for (i in seq_len(nIds)) {
    if (length(allSimParents[[i]]$sires) == 0L) {
      ped$sire[ped$id == allSimParents[[i]]$id] <- NA
      if (verbose) {
        message("id #", i, " is ", allSimParents[[i]]$id, " and has no sire\n")
      }
    } else {
      ped$sire[ped$id == allSimParents[[i]]$id] <-
        sample(allSimParents[[i]]$sires, size = 1L)
    }
    if (length(allSimParents[[i]]$dams) == 0L) {
      ped$dam[ped$id == allSimParents[[i]]$id] <- NA
      if (verbose) {
        message(
          "id #", i, " is ", allSimParents[[i]]$id,
          " and has no dam\n"
        )
      }
    } else {
      ped$dam[ped$id == allSimParents[[i]]$id] <-
        sample(allSimParents[[i]]$dams, size = 1L)
    }
  }
  ped
}
