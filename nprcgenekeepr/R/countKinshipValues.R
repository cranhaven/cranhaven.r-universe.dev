#' Counts the number of occurrences of each kinship value seen for a pair of
#' individuals in a series of simulated pedigrees.
#'
#' @return A list of three lists named \code{kIds} (kinship IDs), \code{kValues}
#'         (kinship values), and \code{kCounts} (kinship counts).
#'
#' @param kinshipValues matrix of kinship values from simulated pedigrees where
#'  each row represents a pair of individuals in the pedigree and each column
#'  represents the vector of kinship values generated in a simulated
#'  pedigree.
#' @param accummulatedKValueCounts list object with same structure as that
#'  returned by this function.
#'
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
#'   sires = c("s1_1", "s1_2", "s1_3"),
#'   dams = c("d1_1", "d1_2", "d1_3", "d1_4")
#' )
#' simParent_3 <- list(
#'   id = "E",
#'   sires = c("A", "C", "s1_1"),
#'   dams = c("d3_1", "B")
#' )
#' simParent_4 <- list(
#'   id = "J",
#'   sires = c("A", "C", "s1_1"),
#'   dams = c("d3_1", "B")
#' )
#' simParent_5 <- list(
#'   id = "K",
#'   sires = c("A", "C", "s1_1"),
#'   dams = c("d3_1", "B")
#' )
#' simParent_6 <- list(
#'   id = "N",
#'   sires = c("A", "C", "s1_1"),
#'   dams = c("d3_1", "B")
#' )
#' allSimParents <- list(
#'   simParent_1, simParent_2, simParent_3,
#'   simParent_4, simParent_5, simParent_6
#' )
#'
#' extractKinship <- function(simKinships, id1, id2, simulation) {
#'   ids <- dimnames(simKinships[[simulation]])[[1]]
#'   simKinships[[simulation]][
#'     seq_along(ids)[ids == id1],
#'     seq_along(ids)[ids == id2]
#'   ]
#' }
#'
#' extractKValue <- function(kValue, id1, id2, simulation) {
#'   kValue[
#'     kValue$id_1 == id1 & kValue$id_2 == id2,
#'     paste0("sim_", simulation)
#'   ]
#' }
#'
#' n <- 10
#' simKinships <- createSimKinships(ped, allSimParents,
#'   pop = ped$id, n = n
#' )
#' kValues <- kinshipMatricesToKValues(simKinships)
#' extractKValue(kValues, id1 = "A", id2 = "F", simulation = 1:n)
#' counts <- countKinshipValues(kValues)
#' n <- 10
#' simKinships <- createSimKinships(ped, allSimParents, pop = ped$id, n = n)
#' kValues <- kinshipMatricesToKValues(simKinships)
#' extractKValue(kValues, id1 = "A", id2 = "F", simulation = 1:n)
#' accummulatedCounts <- countKinshipValues(kValues, counts)
countKinshipValues <- function(kinshipValues,
                               accummulatedKValueCounts = NULL) {
  idCols <- c("id_1", "id_2")
  valueCols <- names(kinshipValues)[!is.element(names(kinshipValues), idCols)]
  kIds <- kValues <- kCounts <-
    vector(mode = "list", length = nrow(kinshipValues))

  for (row in seq_len(nrow(kinshipValues))) {
    valuesTable <-
      table(as.numeric(kinshipValues[row, valueCols, with = FALSE]))
    kIds[[row]] <- as.character(kinshipValues[row, idCols, with = FALSE])
    kValues[[row]] <- as.numeric(names(valuesTable))
    kCounts[[row]] <- as.numeric(valuesTable)
  }
  kValueCounts <- list(
    kIds = kIds,
    kValues = kValues,
    kCounts = kCounts
  )
  if (is.null(accummulatedKValueCounts)) {
    accummulatedKValueCounts <- kValueCounts
  } else {
    if (!all(unique(unlist(kIds)) ==
             unique(unlist(accummulatedKValueCounts$kIds)))) {
      stop(
        "ID pairs in simulated pedigrees do not match: ",
        setdiff(
          unique(unlist(kIds)),
          unique(unlist(accummulatedKValueCounts$kIds))),
          " found in only one set of simulated pedigrees."
      )
    }
    for (index in seq_along(kCounts)) {
      valueDiffs <- setdiff(
        kValues[[index]],
        accummulatedKValueCounts$kValues[[index]]
      )
      for (value in accummulatedKValueCounts$kValues[[index]]) {
        accummulatedKValueCounts <-
          addKinshipValueCount(
            accummulatedKValueCounts,
            kValues,
            kCounts,
            index,
            value
          )
      }
      if (length(valueDiffs) > 0L) {
        accummulatedKValueCounts$kValues[[index]] <-
          c(accummulatedKValueCounts$kValues[[index]], valueDiffs)

        countDiffs <- integer(length(valueDiffs))
        for (value in valueDiffs) {
          countDiffs[index] <- kCounts[[index]][kValues[[index]] == value]
        }

        accummulatedKValueCounts$kCounts[[index]] <-
          c(accummulatedKValueCounts$kCounts[[index]], countDiffs)
      }
    }
  }
  accummulatedKValueCounts
}
