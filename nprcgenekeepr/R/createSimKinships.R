#' Makes a list object of kinship matrices from simulated pedigrees of possible
#' parents for animals with unknown parents
#'
#' \code{createSimKinships} uses \code{makeSimPed} with the \code{ped} object
#' and the \code{allSimParents} object to create a set of kinship matrices to
#' be used in forming the \emph{Monte Carlo} estimates for the kinship values.
#'
#' @return A list of \code{n} lists with each internal list containing a
#'         kinship matrix from simulated pedigrees of possible
#'         parents for animals with unknown parents.
#'
#' @param ped The pedigree information in data.frame format
#' @param allSimParents list made up of lists where the internal list
#'        has the offspring ID, \code{id}, a vector of representative sires
#'        (\code{sires}), and a vector of representative dams (\code{dams}).
#' @param pop Character vector with animal IDs to consider as the population of
#'        interest. This allows you to provide a pedigree in \code{ped} that
#'        has more animals than you want to use in the simulation by defining
#'        \code{pop} with the subset of interest. The default is NULL.
#' @param n integer value of the number of simulated pedigrees to generate.
#' @param verbose logical vector of length one that indicates whether or not
#'        to print out when an animal is missing a sire or a dam.
#' @importFrom data.table setDT
#' @export
#' @examples
#' library(nprcgenekeepr)
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
createSimKinships <- function(ped, allSimParents, pop = NULL, n = 10L,
                              verbose = FALSE) {
  ## If user has limited the population of interest by defining 'pop',
  ## that information is incorporated via the 'population' column.
  setDT(ped)
  ped$population <- getGVPopulation(ped, pop)

  # Get the list of animals in the population to consider
  simKinships <- vector(mode = "list", length = n)

  for (i in seq_len(n)) {
    simPed <- makeSimPed(ped, allSimParents, verbose = verbose)
    simKinships[[i]] <- kinship(
      simPed$id, simPed$sire,
      simPed$dam, simPed$gen
    )
  }
  simKinships
}
