#' @title \code{PHyperLigandTF}
#'
#' @description \code{PHyperLigandTF} This analogous to Gene Set Enrichment
#'   Analysis. Instead of testing for enrichment of a geneset with a given gene
#'   set in a pathway, we are testing the enrichment of a given TF Motif set
#'   against a motif set downstream of a given ligand. If there is enrichment,
#'   it's a sign that that ligand could drive that set of motifs.
#'
#'   phyper function for testing TF interaction with a ligand:
#'   a. NicheNet — count all TFs regulated by any ligand — n-all
#'   b. NicheNet — count all TFs regulated by the ligand of interest — n-1
#'   c. scATAC — count all enriched TFs of interest — m-all
#'   d. NicheNet-scATAC — count all overlap between B & C — m-1
#'   Example:
#'      phyper(m1, n1, nall-n1, ma, lower.tail=F, log.p=F)
#'
#' @param ligandTFMatrix NicheNet Ligand-TF matrix
#' @param motifEnrichmentDF Dataframe (unfiltered), with a motifColumn with motif
#'   names, and a column with the -log10 adjusted p-values
#' @param motifColumn Column name within the motifEnrichmentDF that has motif
#'   names
#' @param specLigand Name of the specific ligand you want to test
#' @param statColumn Column name in motifEnrichmentDF containing the statistic
#'   to test
#' @param statThreshold Significance threshold used to select significant motif
#'   set
#' @param verbose Set TRUE to display additional messages, including the the
#'   n-all, n-1, m-all, and m-1 values. Default is FALSE.
#'
#' @return output of stats::phyper
#'
#' @noRd
#'
PHyperLigandTF <- function(ligandTFMatrix,
                           motifEnrichmentDF,
                           specLigand,
                           motifColumn,
                           statColumn,
                           statThreshold,
                           verbose = FALSE) {
  motifEnrichmentDF <- as.data.frame(motifEnrichmentDF)

  allMotifNames <- motifEnrichmentDF[, motifColumn]

  # Test if you need to cleanup the motif names
  if (any(grepl("_", allMotifNames))) {
    motifEnrichmentDF[, motifColumn] <- gsub("_.*", "", allMotifNames)
  }

  allMotifNames <- unlist(motifEnrichmentDF[, motifColumn])

  if (!any(rownames(ligandTFMatrix) %in% allMotifNames)) {
    stop("No TF names in ligandTFMatrix match those in motifEnrichmentDF")
  }

  if (!(specLigand %in% colnames(ligandTFMatrix))) {
    stop("Given specLigand does not appear in ligandTFMatrix.")
  }

  if (any(colnames(ligandTFMatrix) %in% colnames(motifEnrichmentDF))) {
    if (verbose) {
      warning(
        "Column names in motifEnrichmentDF and ligandTFMatrix cannot be",
        " the same. Renaming identical columns 'TranscriptionFactor'",
        " and setting motifColumn to 'TranscriptionFactor'"
      )
    }
    colnames(motifEnrichmentDF)[colnames(motifEnrichmentDF) == motifColumn] <- "TranscriptionFactor"
    motifColumn <- "TranscriptionFactor"
  }

  # Filter ligandTFMatrix down to those interactions with TFs within motifEnrichmentDF
  allTFsByAnyLigand <- ligandTFMatrix[rownames(ligandTFMatrix) %in% allMotifNames, ]
  allTFsByAnyLigand <- allTFsByAnyLigand[, colSums(allTFsByAnyLigand) > 0]

  if (!specLigand %in% colnames(allTFsByAnyLigand)) {
    stop(
      stringr::str_interp(
        "The ligand named ${specLigand} does not have any interactions with motif set"
      )
    )
  }

  TFMat <- as.data.frame(allTFsByAnyLigand)
  TFMat$TranscriptionFactor <- rownames(allTFsByAnyLigand)

  # Check for number of overlapping Transcription Factors between
  # NicheNet and motifEnrichmentDF
  NicheNet_Motif_overlap <- sum(TFMat$TranscriptionFactor %in% allMotifNames)

  otherMotifs <- "TranscriptionFactor"
  joinedDF <- dplyr::inner_join(
    motifEnrichmentDF,
    TFMat,
    by = structure(names = motifColumn, .Data = otherMotifs)
  )

  mergedDF <- tidyr::pivot_longer(joinedDF,
    cols = colnames(joinedDF)[c(
      (dim(joinedDF)[2] - dim(allTFsByAnyLigand)[2] + 1):dim(joinedDF)[2]
    )],
    names_to = "Ligand", values_to = "Score"
  )

  nall <- dim(allTFsByAnyLigand)[1]
  n1 <- sum(allTFsByAnyLigand[, specLigand] > 0)

  # Filter to just significantly enriched motifs
  mergedDF_f <- dplyr::filter(mergedDF, .data[[statColumn]] > statThreshold)

  # Number of significantly enrichment motifs
  mall <- length(unique(mergedDF_f$TranscriptionFactor))

  # Number of enriched TFs that could be downstream of a given ligand
  m1 <- sum(
    unique(mergedDF_f$TranscriptionFactor) %in%
      rownames(allTFsByAnyLigand)[allTFsByAnyLigand[, specLigand] > 0]
  )

  if (verbose) {
    message("TF Overlap: ", NicheNet_Motif_overlap, sep = "")
    message("All TF by Any Ligand: ", nall, sep = "")
    message("Significantly Enriched Motifs: ", mall, sep = "")
    message("Enriched TF Downstream of Ligand: ", m1, sep = "")
  }

  stats::phyper(m1, n1, nall - n1, mall, lower.tail = F, log.p = F)
}


#' @title \code{MotifSetEnrichmentAnalysis}
#'
#' @description This analogous to Gene Set
#'   Enrichment Analysis. Instead of testing for enrichment of a geneset with a
#'   given gene set in a pathway, we are testing the enrichment of a given TF
#'   motif set against a motif set downstream of a multiple ligands. If there is
#'   enrichment, it's a sign that that ligand could drive that set of motifs.
#'
#'
#' @param ligandTFMatrix NicheNet Ligand-TF matrix
#' @param motifEnrichmentDF Dataframe (unfiltered) from ArchR's peakAnnoEnrich
#'   step. Expected to have a column with motif names, and a column with the
#'   -log10 adjusted p-values.
#' @param motifColumn Column name within the motifEnrichmentDF that has motif
#'   names.
#' @param ligands Vector of ligands to test
#' @param statColumn Column name in motifEnrichmentDF containing the statistic
#'   to test
#' @param statThreshold Significance threshold used to select significant motif
#'   set
#' @param annotation Optional annotation value added to all rows of the output
#'   motif dataframe. Can be character vector or numeric. Default is "none".
#' @param annotationName Optional column name for the annotation. Default is
#'   "CellType".
#' @param numCores The number of cores to use with multiprocessing. Default is
#'   1.
#' @param verbose Set TRUE to display additional messages. Default is FALSE.
#'
#' @return specDF A dataframe containing enrichment analysis results
#'
#' @export
#'
MotifSetEnrichmentAnalysis <- function(ligandTFMatrix,
                                       motifEnrichmentDF,
                                       motifColumn,
                                       ligands,
                                       statColumn,
                                       statThreshold,
                                       annotationName = "CellType",
                                       annotation = "none",
                                       numCores = 1, verbose = FALSE) {
  if (
    any(!ligands %in% colnames(ligandTFMatrix)) | any(duplicated(ligands))
  ) {
    stop("Some ligands does not appear in NicheNet matrix, or are duplicated.")
  }

  # cl <- parallel::makeCluster(numCores)
  # parallel::clusterExport(
  #   cl,
  #   varlist = c(
  #     "ligandTFMatrix", "motifEnrichmentDF", "motifColumn", "verbose",
  #     "statThreshold", "PHyperLigandTF", "statColumn"
  #   ),
  #   envir = environment()
  # )

  specificLigands <- pbapply::pblapply(ligands, function(x) {
    PHyperLigandTF(ligandTFMatrix,
      motifEnrichmentDF,
      specLigand = x,
      motifColumn = motifColumn,
      statThreshold = statThreshold,
      statColumn = statColumn,
      verbose = verbose
    )
  }, cl = NULL)
  names(specificLigands) <- ligands

  # parallel::stopCluster(cl)

  # create data frame of ligand, p-val, adjusted p-value, and percentage of ligand-TFs in motif set.
  specDF <- data.frame(
    ligand = names(specificLigands),
    p_val = unlist(specificLigands)
  )

  # The PHyperLigandTF function will return 0, which is simply because it's
  # a small number than R can record. The function will set these values to
  # the e-323, which is close to the lower limit in R, so the -log10 of the
  # pvalue doesn't generate an error.
  if (any(specDF$p_val == 0, na.rm = TRUE)) {
    specDF$p_val[specDF$p_val == 0] <- 1e-323
  }
  specDF$adjp_val <- stats::p.adjust(specDF$p_val, method = "fdr")

  # Subset ligand matrix down to all TFs related to ligands
  subsetMat <- ligandTFMatrix[
    rownames(ligandTFMatrix) %in% motifEnrichmentDF[, motifColumn],
    colnames(ligandTFMatrix) %in% ligands,
    drop = FALSE
  ]

  # Identify significant motifs
  sigMotifs <- unlist(unique(
    motifEnrichmentDF[motifEnrichmentDF[[statColumn]] > statThreshold, motifColumn]
  ))

  # Subset ligand matrix down to significant motifs that are associated
  # with ligands
  sigSubsetMat <- ligandTFMatrix[
    rownames(ligandTFMatrix) %in% sigMotifs,
    colnames(ligandTFMatrix) %in% ligands,
    drop = FALSE
  ]

  # Order columns of subset matrices by ligand order
  subsetMat <- subsetMat[, specDF$ligand]
  sigSubsetMat <- sigSubsetMat[, specDF$ligand]

  # Calculate percentage of significant motifs against background of
  # all motifs for each tested ligand
  specDF$PercentSigTF <- colSums(sigSubsetMat > 0) / colSums(subsetMat > 0)
  # Calculate percentage of significant motifs that interact with a given
  # ligand against the background of all significant motifs
  specDF$PercInNicheNet <- colSums(sigSubsetMat > 0) / length(sigMotifs)

  # Label these values
  specDF[, annotationName] <- annotation
  specDF
}
