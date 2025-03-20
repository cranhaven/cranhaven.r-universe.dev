#' @title \code{getDifferentialAccessibleTiles}
#'
#' @description \code{getDifferentialAccessibleTiles} allows you to
#'   determine whether regions of chromatin are differentially accessible
#'   between groups by conducting a test
#'
#' @param SampleTileObj The SummarizedExperiment object output from
#'  getSampleTileMatrix
#' @param cellPopulation A string denoting the cell population of interest
#' @param groupColumn The column containing sample group labels
#' @param foreground The foreground group of samples for differential comparison
#' @param background The background group of samples for differential comparison
#' @param signalThreshold Minimum median intensity required to keep tiles for
#'  differential testing to increase statistical power in small sample cohorts.
#'  Default is 12.
#' @param minZeroDiff Minimum difference in average dropout rates across groups
#'  require to keep tiles for differential testing. Default is 0.5 (50\%).
#' @param fdrToDisplay False-discovery rate used only for standard
#'  output messaging. Default is 0.2.
#' @param outputGRanges Outputs a GRanges if TRUE and a data.frame if
#'  FALSE. Default is TRUE.
#' @param numCores The number of cores to use with multiprocessing.
#'  Default is 1.
#' @param verbose Set TRUE to display additional messages. Default is FALSE.
#'
#' @return full_results The differential accessibility results as a GRanges or
#'   matrix data.frame depending on the flag `outputGRanges`.
#'
#' @examples
#' \dontrun{
#' cellPopulation <- "MAIT"
#' foreground <- "Positive"
#' background <- "Negative"
#' # Standard output will display the number of tiles found below a false-discovery rate threshold.
#' # This parameter does not filter results and only affects the aforementioned message.
#' fdrToDisplay <- 0.2
#' # Choose to output a GRanges or data.frame.
#' # Default is TRUE
#' outputGRanges <- TRUE
#' # SampleTileMatrices is the output of MOCHA::getSampleTileMatrix
#' differentials <- MOCHA::getDifferentialAccessibleTiles(
#'   SampleTileObj = SampleTileMatrices,
#'   cellPopulation = cellPopulation,
#'   groupColumn = groupColumn,
#'   foreground = foreground,
#'   background = background,
#'   fdrToDisplay = fdrToDisplay,
#'   outputGRanges = outputGRanges,
#'   numCores = numCores
#' )
#' }
#' @export

getDifferentialAccessibleTiles <- function(SampleTileObj,
                                           cellPopulation,
                                           groupColumn,
                                           foreground,
                                           background,
                                           signalThreshold = 12,
                                           minZeroDiff = 0.5,
                                           fdrToDisplay = 0.2,
                                           outputGRanges = TRUE,
                                           numCores = 1,
                                           verbose = FALSE) {
  if (!any(names(SummarizedExperiment::assays(SampleTileObj)) %in% cellPopulation)) {
    stop("cellPopulation was not found within SampleTileObj. Check available cell populations with `colData(SampleTileObj)`.")
  }

  if (signalThreshold < 1) {
    if (verbose) {
      warning("Setting the signalThreshold too low will reduce statistical power. You may inspect the distribution of intensities to set a threshold that will remove highly sparse regions.")
    }
  }

  metaFile <- SummarizedExperiment::colData(SampleTileObj)

  if (!(groupColumn %in% colnames(metaFile))) {
    stop(stringr::str_interp("Provided groupCol '{groupColumn}' not found in the provided SampleTileObj"))
  }
  if (!(foreground %in% metaFile[[groupColumn]])) {
    stop(stringr::str_interp("Provided foreground value is not present in the column {groupColumn} in the provided SampleTileObj"))
  }
  if (!(background %in% metaFile[[groupColumn]])) {
    stop(stringr::str_interp("Provided background value is not present in the column {groupColumn} in the provided SampleTileObj"))
  }


  # Get group labels
  foreground_samples <- metaFile[metaFile[, groupColumn] == foreground, "Sample"]
  background_samples <- metaFile[metaFile[, groupColumn] == background, "Sample"]

  # This will only include called tiles
  sampleTileMatrix <- MOCHA::getCellPopMatrix(SampleTileObj, cellPopulation, NAtoZero = FALSE)

  # Enforce that the samples included are in foreground and background groups -
  # this can onl be an A vs B comparison, i.e. this ignores other groups in groupCol

  sampleTileMatrix <- sampleTileMatrix[, colnames(sampleTileMatrix) %in% c(foreground_samples, background_samples), drop = FALSE]


  group <- as.numeric(colnames(sampleTileMatrix) %in% foreground_samples)

  #############################################################################
  # Prioritize high-signal tiles

  # Log2 transform the matrix
  # (input must not be log2 transformed prior to this)
  sampleTileMatrix <- log2(sampleTileMatrix + 1)

  medians_a <- matrixStats::rowMedians(sampleTileMatrix[, which(group == 1), drop = FALSE], na.rm = T)
  medians_b <- matrixStats::rowMedians(sampleTileMatrix[, which(group == 0), drop = FALSE], na.rm = T)

  # Set NAs to zero
  sampleTileMatrix[is.na(sampleTileMatrix)] <- 0

  zero_A <- rowMeans(sampleTileMatrix[, which(group == 1), drop = FALSE] == 0)
  zero_B <- rowMeans(sampleTileMatrix[, which(group == 0), drop = FALSE] == 0)

  diff0s <- abs(zero_A - zero_B)

  log2FC_filter <- signalThreshold
  idx <- which(medians_a > log2FC_filter | medians_b > log2FC_filter | diff0s >= minZeroDiff)

  ############################################################################
  # Estimate differential accessibility

  res_pvals <- parallel::mclapply(
    rownames(sampleTileMatrix),
    function(x) {
      if (which(rownames(sampleTileMatrix) == x) %in% idx) {
        cbind(Tile = x, estimate_differential_accessibility(sampleTileMatrix[x, ], group, F))
      } else {
        data.frame(
          Tile = x,
          P_value = NA,
          TestStatistic = NA,
          Log2FC_C = NA,
          MeanDiff = NA,
          Case_mu = NA,
          Case_rho = NA,
          Control_mu = NA,
          Control_rho = NA
        )
      }
    },
    mc.cores = numCores
  )

  # Combine results into single objects
  res_pvals <- do.call(rbind, res_pvals)

  #############################################################################
  # Apply FDR on filtered regions

  filtered_res <- res_pvals[idx, ]

  if (!all(is.na(filtered_res$P_value[filtered_res$P_value <= 0.95]))) {
    pi0_reduced <- qvalue::pi0est(filtered_res$P_value[filtered_res$P_value <= 0.95],
      pi0.method = "bootstrap",
      lambda = seq(0, 0.6, .05)
    )

    filtered_res$FDR <- qvalue::qvalue(filtered_res$P_value, pi0 = pi0_reduced$pi0)$qvalues
  } else {
    filtered_res$FDR <- NA # TODO Handle appropriately
  }
  #############################################################################

  # Join with original results
  full_results <- dplyr::left_join(res_pvals, filtered_res[, c("Tile", "FDR")], by = "Tile")

  # Set results to NA where FDR is NA
  na.idx <- which(is.na(full_results$FDR))
  full_results$P_value[na.idx] <- NA
  full_results$TestStatistic[na.idx] <- NA

  sampleTileMatrix[is.na(sampleTileMatrix)] <- 0
  meansA <- rowMeans(sampleTileMatrix[, group == 1, drop = FALSE], na.rm = T)
  meansB <- rowMeans(sampleTileMatrix[, group == 0, drop = FALSE])
  full_results$MeanDiff <- meansA - meansB
  full_results$MeanDiff[is.na(full_results$FDR)] <- NA
  full_results$CellPopulation <- rep(cellPopulation, length(meansA))
  full_results$Foreground <- rep(foreground, length(meansA))
  full_results$Background <- rep(background, length(meansA))


  colnames(full_results) <- c(
    "Tile", "P_value", "Test_Statistic", "Log2FC_C",
    "MeanDiff", "Avg_Intensity_Case", "Pct0_Case",
    "Avg_Intensity_Control", "Pct0_Control", "FDR",
    "CellPopulation", "Foreground", "Background"
  )

  full_results <- full_results[, c(
    "Tile", "CellPopulation", "Foreground", "Background", "P_value", "Test_Statistic", "FDR", "Log2FC_C",
    "MeanDiff", "Avg_Intensity_Case", "Pct0_Case",
    "Avg_Intensity_Control", "Pct0_Control"
  )]

  discoveries <- sum(full_results$FDR <= fdrToDisplay, na.rm = TRUE)
  if (verbose) {
    message(
      discoveries, " differential regions found at FDR ",
      fdrToDisplay
    )
  }

  if (outputGRanges) {
    full_results <- MOCHA::differentialsToGRanges(full_results)
  }

  full_results
}
