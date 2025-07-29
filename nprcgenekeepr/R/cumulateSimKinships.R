#' Makes a list object containing kinship summary statistics using the list
#' object from __createSimKinships__.
#'
#' \code{cumulateSimKinships} creates a named
#' list of length 4 is generated where the first element is the mean of the
#' simulated kinships, the second element is the standard deviation of the
#' simulated kinships the third element is the minimum value of the kinships,
#' and the forth element is the maximum value of the kinships.
#'
#' @return List object containing the meanKinship, sdKinship, minKinship, and
#'         maxKinship.
#'
#' @param ped The pedigree information in data.frame format
#' @param allSimParents list made up of lists where the internal list
#'        has the offspring ID \code{id}, a vector of representative sires
#'        (\code{sires}), and a vector of representative dams(\code{dams}).
#' @param pop Character vector with animal IDs to consider as the population of
#' interest. The default is NULL.
#' @param n integer value of the number of simulated pedigrees to generate.
#' @export
#' @examples
#' ped <- nprcgenekeepr::smallPed
#' simParent_1 <- list(
#'   id = "A",
#'   sires = c("s1_1", "s1_2", "s1_3"),
#'   dams = c("d1_1", "d1_2", "d1_3", "d1_4")
#' )
#' simParent_2 <- list(
#'   id = "B",
#'   sires = c("s2_1", "s2_2", "s2_3"),
#'   dams = c("d2_1", "d2_2", "d2_3", "d2_4")
#' )
#' simParent_3 <- list(
#'   id = "E",
#'   sires = c("s3_1", "s3_2", "s3_3"),
#'   dams = c("d3_1", "d3_2", "d3_3", "d3_4")
#' )
#' allSimParents <- list(simParent_1, simParent_2, simParent_3)
#' pop <- LETTERS[1:7]
#' simKinships <- createSimKinships(ped, allSimParents, pop, n = 10)
cumulateSimKinships <- function(ped, allSimParents, pop = NULL, n = 10L) {
  ## If user has limited the population of interest by defining 'pop',
  ## that information is incorporated via the 'population' column.
  ped$population <- getGVPopulation(ped, pop)

  # Get the list of animals in the population to consider
  nIds <- nrow(ped)
  squaredKinship <- sumKinship <- matrix(data = 0L, nrow = nIds, ncol = nIds)
  first_time <- TRUE

  for (i in seq_len(n)) {
    simPed <- makeSimPed(ped, allSimParents)
    kmat <- kinship(
      simPed$id, simPed$sire,
      simPed$dam, simPed$gen
    )
    if (first_time) { # initializes minKinship correctly and adds IDs
      minKinship <- kmat
      maxKinship <- kmat
      first_time <- FALSE
    } else {
      minKinship <- pmin(minKinship, kmat)
      maxKinship <- pmax(maxKinship, kmat)
    }
    sumKinship <- sumKinship + kmat
    squaredKinship <- squaredKinship + kmat^2L
  }
  list(
    meanKinship = sumKinship / n,
    sdKinship = sqrt(
      ((n * squaredKinship) - sumKinship^2L) /
        (n * (n - 1L))
    ),
    minKinship = minKinship,
    maxKinship = maxKinship
  )
}
