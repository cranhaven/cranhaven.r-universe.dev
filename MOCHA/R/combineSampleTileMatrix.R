
#' @title \code{combineSampleTileMatrix}
#'
#' @description \code{combineSampleTileMatrix} combines all celltypes in a
#'   SampleTileMatrix object into a SummarizedExperiment with one single matrix
#'   across all cell types and samples,
#'
#' @param SampleTileObj The SummarizedExperiment object output from
#'   getSampleTileMatrix containing your sample-tile matrices
#' @param NAtoZero Set NA values in the sample-tile matrix to zero
#' @param verbose Set TRUE to display additional messages. Default is FALSE.
#' @return TileCorr A data.table correlation matrix
#'
#'
#' @export
combineSampleTileMatrix <- function(SampleTileObj,
                                    NAtoZero = TRUE,
                                    verbose = FALSE) {
  CellTypes <- FragNumber <- NULL

  genome <- S4Vectors::metadata(SampleTileObj)$Genome
  genome <- BSgenome::getBSgenome(genome)

  Sample <- Freq <- . <- NULL
  # Extract all the Sample-Tile Matrices for each cell type
  assays <- SummarizedExperiment::assays(SampleTileObj)

  coldata <- SampleTileObj@colData

  # Let's generate a new assay, that will contain the
  # the intensity for a given cell, as well as the
  # median intensity per sample-tile for all other cell types (i.e. the background)

  newAssays <- list(do.call("cbind", methods::as(assays, "list")))
  newSamplesNames <- unlist(lapply(names(assays), function(x) {
    gsub(" ", "_", paste(x, colnames(SampleTileObj), sep = "__"))
  }))

  names(newAssays) <- "counts"
  colnames(newAssays[[1]]) <- newSamplesNames

  if (NAtoZero) {
    newAssays[[1]][is.na(newAssays[[1]])] <- 0
  }

  # Combine Sample and Cell type for the new columns.
  # This takes the colData and repeats it across cell types for a given sample
  # Rows are now CellType__Sample. So for example 'CD16 Mono' and 'Sample1' becomes "CD16_Mono__Sample1"
  allSampleData <- as.data.frame(do.call("rbind", lapply(names(assays), function(x) {
    tmp_meta <- coldata
    tmp_meta$Sample <- gsub(" ", "_", paste(x, tmp_meta$Sample, sep = "__"))
    tmp_meta$CellType <- rep(x, dim(tmp_meta)[1])
    rownames(tmp_meta) <- tmp_meta$Sample
    tmp_meta
  })))

  # This is where cell counts and fragments counts are pivoted into a long format and merged (left-Joined) into the new allSampleData
  cellTypeLabelList <- Var1 <- NULL

  summarizedData <- S4Vectors::metadata(SampleTileObj)$summarizedData
  cellCounts <- as.data.frame(
    SummarizedExperiment::assays(summarizedData)[["CellCounts"]]
  )
  cellTypes <- rownames(cellCounts)
  cellCounts <- tidyr::pivot_longer(
    cellCounts,
    cols = colnames(cellCounts),
    names_to = "Sample",
    values_to = "Freq"
  )
  cellCounts <- dplyr::mutate(
    cellCounts,
    Sample = rownames(allSampleData)
  )

  fragCounts <- as.data.frame(
    SummarizedExperiment::assays(summarizedData)[["FragmentCounts"]]
  )
  cellTypes <- rownames(fragCounts)
  fragCounts <- tidyr::pivot_longer(
    fragCounts,
    cols = colnames(fragCounts),
    names_to = "Sample",
    values_to = "FragNumber"
  )
  fragCounts <- dplyr::mutate(
    fragCounts,
    Sample = rownames(allSampleData)
  )

  # The sample column now is actually CellType_Sample, unlike before
  allSampleData <- dplyr::left_join(
    allSampleData, cellCounts,
    by = "Sample"
  )

  allSampleData <- dplyr::left_join(
    allSampleData, fragCounts,
    by = "Sample"
  )

  # Artificially set all the cell type columns in rowRanges to TRUE, incase of later subsetting.
  allRanges <- SummarizedExperiment::rowRanges(SampleTileObj)

  newMetadata <- S4Vectors::metadata(SampleTileObj)
  newMetadata$History <- append(newMetadata$History, paste("combineSampleTileMatrix", utils::packageVersion("MOCHA")))

  newObj <- SummarizedExperiment::SummarizedExperiment(
    assays = newAssays,
    colData = allSampleData,
    rowRanges = allRanges,
    metadata = newMetadata
  )
  return(newObj)
}
