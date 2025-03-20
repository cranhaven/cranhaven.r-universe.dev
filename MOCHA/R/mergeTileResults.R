#' @title \code{mergeTileResults}
#'
#' @description \code{mergeTileResults} merges a list of tileResults that
#'   each contain unique samples into a single object encompassing all samples.
#'   Only cell populations shared among all input tileResults will be retained.
#'   This function can merge MultiAssayExperiment objects from callOpenTiles
#'   that are created with the same TxDb, OrgDb, and Genome assembly.
#'
#' @param tileResultsList List of MultiAssayExperiments objects returned by
#'   callOpenTiles containing containing peak calling results.
#' @param numCores Optional, the number of cores to use with multiprocessing.
#'   Default is 1.
#' @param verbose Set TRUE to display additional messages. Default is FALSE.
#'
#' @return tileResults a single MultiAssayExperiment containing a sample-tile
#'   intensity matrix for each sample and common cell population in the input
#'   tileResultsList.
#'
#' @examples
#' \dontrun{
#' # Depends on local MOCHA tileResults
#' MOCHA::mergeTileResults(
#'   list(tileResultsCelltypesABC, tileResultsCelltypesBCD)
#' )
#' }
#'
#' @export
mergeTileResults <- function(tileResultsList, numCores = 1, verbose = TRUE) {
  Freq <- NULL
  # Test for duplicate sample names
  sampleTest <- unlist(lapply(tileResultsList, function(x) rownames(x@colData)))
  if (any(duplicated(sampleTest))) {
    stop(
      "Sample names are duplicated in your list of tileResults.",
      " Samples must be unique between tileResults and not duplicated."
    )
  }

  # Test whether all the tileResultsList indices are MultiAssayExperiments
  classTest <- lapply(tileResultsList, function(x) class(x)[1])
  if (any(unlist(classTest) != "MultiAssayExperiment")) {
    stop(
      "At least one index of the tileResultsList is not ",
      "a MultiAssayExperiment. All objects in tileResultsList must be ",
      "MultiAssayExperiment output from callOpenTiles."
    )
  }

  # Test whether all the tileResults objects have the same Transcript,
  # organism, and genome databases involved before merging
  TxDbTest <- unique(unlist(lapply(tileResultsList, function(x) x@metadata$TxDb$pkgname)))
  if (length(TxDbTest) > 1) {
    stop(
      "Different TxDb were used to generate these tileResults. ",
      "tileResults must be created with the same TxDb to be merged."
    )
  }

  OrgDbTest <- unique(unlist(lapply(tileResultsList, function(x) x@metadata$OrgDb$pkgname)))
  if (length(OrgDbTest) > 1) {
    stop(
      "These tileResults are from different organisms and cannot not be ",
      " merged. tileResults must be created with the same OrgDb to be merged."
    )
  }

  GenTest <- unique(unlist(lapply(tileResultsList, function(x) x@metadata$Genome)))
  if (length(GenTest) > 1) {
    stop(
      "These tileResults are from different genome assemblies and cannot be ",
      " merged. tileResults must be created with the same Genome to be merged."
    )
  }

  # Find all celltypes across all tile results objects.
  allCellTypes <- as.data.frame(table(unlist(lapply(tileResultsList, names))))

  # Find the subset of cell types that are in common across all tileResults objects. Merge those.
  subCellTypes <- as.character(unlist(dplyr::filter(allCellTypes, Freq == length(tileResultsList))$Var))

  if (length(subCellTypes) == 0) {
    stop(
      "Input tileResults in tileResultsList do not share any common cell ",
      "populations."
    )
  }
  if (verbose) {
    message(
      "Cell population(s) ",
      paste0(subCellTypes, collapse = ", "),
      " are in present all tileResults objects and will be retained in the merge."
    )
  }

  # Iterate across each tileResults object, extracting the RaggedExperiments and turning them back into GRangesList.
  # These GRangesList can then be concatenated easily, and then turned back into RaggedExperiments, and joined into one final tile results object.
  cl <- parallel::makeCluster(numCores)
  allRaggedResults <- lapply(subCellTypes, function(y) {
    ragRes <- lapply(tileResultsList, function(x) x[[as.character(y)]])
    ragExp <- do.call("c", pbapply::pblapply(cl = cl, X = ragRes, combineRagged))
    RaggedExperiment::RaggedExperiment(ragExp)
  })
  names(allRaggedResults) <- subCellTypes

  parallel::stopCluster(cl)

  # Merged sample and other metadata information.
  allSampleData <- do.call(eval(parse(text = "dplyr::bind_rows")), lapply(tileResultsList, function(x) {
    as.data.frame(x@colData)
  }))

  # This is complicated, let's merge the summarizedData.
  # We'll need to assemble all metadata across tile results, and merge data that's the same together.

  allCellTypes <- unique(unlist(lapply(tileResultsList, function(XX) {
    rownames(XX@metadata$summarizedData)
  })))
  allSummarizedMetrics <- unique(unlist(lapply(tileResultsList, function(XX) {
    names(SummarizedExperiment::assays(XX@metadata$summarizedData))
  })))

  allSummarizedData <- pbapply::pblapply(cl = NULL, X = allSummarizedMetrics, function(YY) {
    allMats <- do.call("cbind", lapply(tileResultsList, function(XX) {
      tmpMat <- SummarizedExperiment::assays(XX@metadata$summarizedData)
      if (YY %in% names(tmpMat)) {
        nextMat <- tmpMat[[YY]]
      } else {
        nextMat <- matrix(NA, ncol = NCOL(tmpMat[[1]]), nrow = NROW(tmpMat[[1]]))
        rownames(nextMat) <- rownames(tmpMat[[1]])
        colnames(nextMat) <- colnames(tmpMat[[1]])
      }

      if (any(!allCellTypes %in% rownames(nextMat))) {
        newCellTypes <- allCellTypes[!allCellTypes %in% rownames(nextMat)]

        emptyMat <- matrix(NA, ncol = NCOL(nextMat), nrow = length(newCellTypes))
        rownames(emptyMat) <- newCellTypes
        colnames(emptyMat) <- colnames(nextMat)

        nextMat <- rbind(nextMat, emptyMat)
      }

      return(nextMat)
    }))

    return(allMats)
  })

  names(allSummarizedData) <- allSummarizedMetrics
  newSummarizedData <- SummarizedExperiment::SummarizedExperiment(allSummarizedData, colData = allSampleData)

  allHistory <- lapply(tileResultsList, function(XX) {
    XX@metadata$History
  })
  allHistory <- append(allHistory, paste("mergeTileResults", utils::packageVersion("MOCHA")))

  # Construct final tile results object.
  tileResults <- MultiAssayExperiment::MultiAssayExperiment(
    experiments = allRaggedResults,
    colData = allSampleData,
    metadata = list(
      "summarizedData" = newSummarizedData,
      "Genome" = GenTest,
      "TxDb" = tileResultsList[[1]]@metadata$TxDb,
      "OrgDb" = tileResultsList[[1]]@metadata$OrgDb,
      "Directory" = NULL,
      "History" = allHistory
    )
  )
  return(tileResults)
}


combineRagged <- function(x) {
  methods::as(x, "GRangesList")
}
