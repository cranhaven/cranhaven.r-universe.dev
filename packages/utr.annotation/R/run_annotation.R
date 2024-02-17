# Author: Yating Liu
# Description: UTR annotation pipeline interface
# Date: 02/04/2019


#' Query transcripts regions and sequences from Ensembl database
#' @description \code{initUTRAnnotation} query transcripts regions, UTRs and coding sequences from Ensembl database, which will be used by \code{runUTRAnnotation} to do UTR annotation.
#' @param variantFile a CSV file with Chr, Pos, Ref, Alt
#' @param species either human or mouse
#' @param ensemblVersion (optional) a number specifying which version of Ensembl annotation you'd like to use, by default use the latest version
#' @param getTranscript (optional) Whether to get ids of the transcripts that overlap with all the variants. If the number of variants is too large (for example > 100,000), set it to FALSE and do this in runUTRAnnotation on each partition in parallel.
#' @param format (optional) csv or vcf, the default is csv
#' @param dataDir (optional) path to the store the database information, if not specified will create a folder named as input variant file name with a "db_" prefix
#' @param verbose Whether print diagnostic messages. The default is FALSE.
#' @return A variant table with Transcript column which contains the ids of the transcripts that overlap with the variants
#' @importFrom ensembldb EnsDb
#' @export
#' @examples
#' \donttest{
#' test_variant_file <- system.file("extdata", "variants_sample.csv", package = "utr.annotation")
#' initUTRAnnotation(variantFile = test_variant_file,
#'                   species = "human",
#'                   ensemblVersion = 93,
#'                   dataDir = "test_db")
#' }
initUTRAnnotation <- function(variantFile, species, ensemblVersion, getTranscript = TRUE, format = "csv", dataDir = NULL, verbose = FALSE) {
  variants <- readVariantData(variantFile, format = tolower(format))
  if (is.null(dataDir)) {
    dataDir <- paste0("db_", basename(variantFile))
  }
  # Create a EnsDb database file from AnnotationHub
  if (verbose) cat("Create a EnsDb database file from AnnotationHub\n")
  createEnsDbFromAH(species, ensemblVersion, dataDir)
  if (getTranscript == TRUE && ! ("Transcript" %in% colnames(variants))) {
    if (verbose) cat("Get ids of the transcripts that overlap with the variants\n")
    variants$Transcript <- getTranscriptIds(variants, species, ensemblVersion, dataDir)
  }
  if ("Transcript" %in% colnames(variants)) {
    if (verbose) cat("Query the Ensembl database information for transcripts in Transcript column\n")
    queryEnsemblInfo(variants$Transcript, species, ensemblVersion, dataDir)
  } else {
    if (verbose) cat("Transcript column doesn't exist, we will query the Ensembl database information for all transcripts for this species\n")
    ensDb <- EnsDb(createEnsDbFromAH(species, ensemblVersion, dataDir))
    all_tran_ids <- names(transcripts(ensDb))
    queryEnsemblInfo(all_tran_ids, species, ensemblVersion, dataDir)
  }
  return (variants)
}

#' Get the latest Ensembl version that available by querying with both biomRt and AnnotationHub
#' @keywords internal
#' @param species human or mouse
#' @return Latest Ensembl version number
#' @import AnnotationHub
#' @import stringr
#' @importFrom biomaRt listEnsemblArchives
getLatestEnsemblVersion <- function(species) {
  # get the latest version number from Ensembl
  current <- max(as.numeric(listEnsemblArchives()$version), na.rm = T)
  # get the latest version number from AnnotationHub
  ah <- AnnotationHub()
  ## Query all available files for Ensembl release version for the species
  orgName <- getOrgName(species)
  gtffiles <- query(ah, c(orgName, "GTF"))
  gtffiles <- gtffiles[str_detect(gtffiles$title, paste0("[0-9+]", ".gtf$")) & tolower(gtffiles$species) == tolower(orgName)]
  ah_current <- max(as.numeric(lapply(str_split(gtffiles$title, "\\."), function(x) x[length(x)-1])), na.rm = T)
  ensemblVersion <- min(current, ah_current)
  return (ensemblVersion)
}

#' Run UTR annotation on a variant file
#' @description Run UTR annotation on a variant file to check whether a variant alter any of the feature element in UTRs, return a CSV file with annotated information
#' @param variantFile a CSV file with Chr, Pos, Ref, Alt
#' @param annotationResult output annotation file
#' @param species species, either human or mouse
#' @param ensemblVersion (optional) a number specifying which version of Ensembl annotation you'd like to use, by default use the latest version
#' @param dataDir (optional) path to the store / extract database information, if not specified will create a folder named as input variant file name with a "db_" prefix
#' @param conservationBwFiles (optional) path to a folder which contains one or more conservation bigWig files. If not set, will skip conservation analysis.
#' @param cores (optional) number of cores to use for parallel computing. If not specified, use a single core by default.
#' @param format (optional) csv or vcf, the default is csv
#' @param mrl_prediction (optional) Whether do MRL prediction and check if it increase or decrease. The default is TRUE.
#' @param verbose Whether print diagnostic messages. The default is FALSE.
#' @return A CSV file with annotated variants
#' @import data.table stringr
#' @importFrom utils write.csv
#' @export
#' @examples
#' \donttest{
#' test_variant_file <- system.file("extdata", "variants_sample.csv", package = "utr.annotation")
#'
#' # run UTR annotation with 1 core and skip getting conservation scores for variant positions
#' runUTRAnnotation(variantFile = test_variant_file,
#'                  annotationResult = "annotated_variants_sample.csv",
#'                  species = "human",
#'                  ensemblVersion = 93,
#'                  dataDir = "test_db")
#' unlink("test_db", recursive = TRUE)
#' unlink("annotated_variants_sample.csv")
#'}
#' \dontrun{
#' # Download BigWig format conservation track files hg38.phastCons100way.bw and hg38.phyloP100way.bw
#' # from UCSC Genome Browser to a folder (here we name it folder_store_conservation_bw_files)
#' Conservation_scores <- "folder_store_conservation_bw_files"
#' runUTRAnnotation(variantFile = test_variant_file,
#'                  annotationResult = "annotated_variants_sample.csv",
#'                  species = "human",
#'                  ensemblVersion = 93,
#'                  dataDir = "test_db",
#'                  cores = 8,
#'                  conservationBwFiles = Conservation_scores)
#' }
runUTRAnnotation <- function(variantFile, annotationResult, species, ensemblVersion = NULL, dataDir = NULL, conservationBwFiles = NULL, cores = NULL, format = "csv", mrl_prediction = TRUE, verbose = FALSE) {
  # Just something to make CRAN check happy
  Transcript <- ensembl_transcript_id <- transcript_id <- utr3_transcript_id <- utr5_transcript_id <- cds_transcript_id <- startCodon_transcript_id <- stopCodon_transcript_id <- stopCodon_positions <- startCodon_positions <- kozak_transcript_id <- kozak_positions <- utr5_start <- utr5_end <- strand <- num_uAUG <- num_uAUG_altered <- num_kozak_altered <- utr_num_uAUG_gainedOrLost <- utr_num_kozak_gainedOrLost <- num_polyA_signal <- num_polyA_signal_altered <- num_polyA_signal_gainedOrLost <- stop_codon_altered <- start_codon_altered <- kozak_altered <- lost_stop_codon <- lost_start_codon <- num_kozak <- kozak_altered_score <- tss_kozak_score_gainedOrLost <- mrl_gainedOrLost <- mrl <- mrl_altered <- utr3_start <- utr3_end <- stop_codon  <- start_codon <- kozak <- kozak_score <- NULL
  if (!file.exists(variantFile)) {
    stop("VariantFile ", variantFile, " doesn't exist!")
  }
  if (file.exists(annotationResult)) {
    stop("annotationResult ", annotationResult, " already exist!")
  }
  if (is.null(ensemblVersion)) {
    ensemblVersion <- getLatestEnsemblVersion(species)
  }
  if (is.null(dataDir)) {
    dataDir <- paste0("db_", basename(variantFile))
  }
  if (!dir.exists(dirname(annotationResult))) {
    if (verbose) cat("Create result directory:", dirname(annotationResult), "\n")
    dir.create(dirname(annotationResult), showWarnings = TRUE, recursive = TRUE, mode = "0777")
  }
  cat("Run UTR annotation on", variantFile,
          "\nFormat =", format,
          "\nOutput to", annotationResult,
          "\nspecies =", species,
          "\nUse ensembl version =", ensemblVersion,
          "\nDatabase information will be stored or extract from", dataDir, "\n")

  ## If on Mac M1, skip MRL prediction
  if (mrl_prediction == TRUE && (Sys.info()[["sysname"]] == "Darwin" && str_detect(Sys.info()[["version"]], "RELEASE_ARM"))) {
    mrl_prediction <- FALSE
    warning("Skipped MRL prediction, because tensorflow cannot run on Mac M1 currently or because tensorflow is not installed.\n")
  }

  # create parallel backend
  init_backend(cores)

  variants <- readVariantData(variantFile, format = tolower(format))
  # Get transcripts that overlap with the variants if not available
  if (!("Transcript" %in% colnames(variants))) {
    if (verbose) cat("Get transcripts that overlap with the variants.\n")
    variants$Transcript <- getTranscriptIds(variants, species, ensemblVersion, dataDir)
  }

  if (!file.exists(file.path(dataDir, species, ensemblVersion)) ||
      !file.exists(file.path(dataDir, species, ensemblVersion, "transcript_regions.rds")) ||
      !file.exists(file.path(dataDir, species, ensemblVersion, "utr5_seq.rds")) ||
      !file.exists(file.path(dataDir, species, ensemblVersion, "utr3_seq.rds")) ||
      !file.exists(file.path(dataDir, species, ensemblVersion, "coding_seq.rds"))) {
    # query the Ensembl database for annotation and sequences
    if (verbose) cat("Query the Ensembl database for annotation and sequences.\n")
    queryEnsemblInfo(variants$Transcript, species, ensemblVersion, dataDir)
  }
  dbDir <- file.path(dataDir, species, ensemblVersion)
  if (all(is.na(variants[, Transcript])) == TRUE || !file.exists(file.path(dbDir, "transcript_regions.rds"))) {
    write.csv(variants, annotationResult, row.names = FALSE)
    return(cat("Done annotation!\nNo valid transcript overlap with variants, so skip the analysis. Write the original table to", annotationResult, "\n"))
  }
  # Get transcripts id, transcript info, 5'UTR and 3'UTR coordinates for each gene in variants table.
  if (verbose) cat("Get database information from the stored data.\n")
  transcript_regions <- readRDS(file.path(dbDir, "transcript_regions.rds"))
  transcript_regions <- transcript_regions[ensembl_transcript_id %in% unique(unlist(str_split(variants$Transcript, ";")))]
  transcriptIds <- unique(transcript_regions[, ensembl_transcript_id])
  # extract sequences from DB
  if (verbose) cat("extract sequences from DB...\n")
  utr5_seq <- readRDS(file.path(dbDir, "utr5_seq.rds"))
  utr5_seq <- utr5_seq[ensembl_transcript_id %in% transcriptIds]
  utr3_seq <- readRDS(file.path(dbDir, "utr3_seq.rds"))
  utr3_seq <- utr3_seq[ensembl_transcript_id %in% transcriptIds]
  coding_seq <- readRDS(file.path(dbDir, "coding_seq.rds"))
  coding_seq <- coding_seq[ensembl_transcript_id %in% transcriptIds]

  # Get ensembl_transcript_id for transcript, CDS, 5' and 3' UTR variants genes
  if (verbose) cat("Get ensembl_transcript_id for transcript, CDS, 5' and 3' UTR variants genes...\n")
  variants[, transcript_id := getTranscriptIdsForUTRVariants(variants, transcript_regions, "transcript")]
  variants[, utr3_transcript_id := getTranscriptIdsForUTRVariants(variants, transcript_regions, "utr3")]
  variants[, utr5_transcript_id := getTranscriptIdsForUTRVariants(variants, transcript_regions, "utr5")]
  variants[, cds_transcript_id := getTranscriptIdsForUTRVariants(variants, transcript_regions, "cds")]

  # Get ensembl_transcript_id for start and stop codon region variants genes
  if (verbose) cat("Get ensembl_transcript_id start codon region variants genes...\n")
  startCodonInfo <- getTranscriptIdsForCodonVariants(variants, transcript_regions, "startCodon")

  if (verbose) cat("Get ensembl_transcript_id stop codon region variants genes...\n")
  stopCodonInfo <- getTranscriptIdsForCodonVariants(variants, transcript_regions, "stopCodon")

  variants[, startCodon_transcript_id := startCodonInfo$ensembl_transcript_id]
  variants[, startCodon_positions := startCodonInfo$codon_positions]
  variants[, stopCodon_transcript_id := stopCodonInfo$ensembl_transcript_id]
  variants[, stopCodon_positions := stopCodonInfo$codon_positions]

  # Get ensembl_transcript_id for TSS Kozak region variants genes
  if (verbose) cat("Get ensembl_transcript_id TSS Kozak region variants genes...\n")
  kozakInfo <- getTranscriptIdsForTSSKozakVariants(variants, transcript_regions)
  variants[, kozak_transcript_id := kozakInfo$ensembl_transcript_id]
  variants[, kozak_positions := kozakInfo$kozak_positions]

  # 5' UTR analysis
  ## Construct 5' UTR coordinates table
  transcript_regions_utr5 <- data.table(ensembl_transcript_id = transcript_regions[, ensembl_transcript_id],
                                        start = transcript_regions[, utr5_start],
                                        end = transcript_regions[, utr5_end],
                                        strand = transcript_regions[, strand])
  ## Count number of uAUG in 5' UTR
  if (verbose) cat("Count number of uAUG...\n")
  variants[, num_uAUG := countDNAPattern(variants, utr5_seq, "ATG", "utr5_transcript_id")]

  ## Count number of ATG in altered sequence for 5' UTR
  if (verbose) cat("Count number of ATG in altered sequence for 5' UTR...\n")
  variants[, num_uAUG_altered := countDNAPatternInAlt(variants, utr5_seq, transcript_regions_utr5, "ATG", "utr5_transcript_id")]

  ## Count number of Kozak in 5' UTR
  if (verbose) cat("Count number of Kozak in 5' UTR...\n")
  variants[, num_kozak := countDNAPattern(variants, utr5_seq, "[GA]..ATGG", "utr5_transcript_id")]

  ## Count number of Kozak in altered sequence for 5' UTR
  if (verbose) cat("Count number of Kozak in altered sequence for 5' UTR...\n")
  variants[, num_kozak_altered := countDNAPatternInAlt(variants, utr5_seq, transcript_regions_utr5, "[GA]..ATGG", "utr5_transcript_id")]

  ## Check if gained or lost uAUG and Kozak in 5' UTR
  if (verbose) cat("Check if gained or lost uAUG in 5' UTR...\n")
  variants[, utr_num_uAUG_gainedOrLost := checkIfGainOrLoseAfterAlt(variants, "num_uAUG", "num_uAUG_altered")]

  if (verbose) cat("Check if gained or lost Kozak in 5' UTR...\n")
  variants[, utr_num_kozak_gainedOrLost := checkIfGainOrLoseAfterAlt(variants, "num_kozak", "num_kozak_altered")]

  # 3' UTR analysis
  ## Construct 3' UTR coordinates table
  transcript_regions_utr3 <- data.table(ensembl_transcript_id = transcript_regions[, ensembl_transcript_id],
                                        start = transcript_regions[, utr3_start],
                                        end = transcript_regions[, utr3_end],
                                        strand = transcript_regions[, strand])
  polyASignal <- "A[AT]TAAA"
  if (verbose) cat("Count number of polyA signals in 3' UTR...\n")
  variants[, num_polyA_signal := countDNAPattern(variants, utr3_seq, polyASignal, "utr3_transcript_id")]
  if (verbose) cat("Count number of polyA signals in altered 3' UTR...\n")
  variants[, num_polyA_signal_altered := countDNAPatternInAlt(variants, utr3_seq, transcript_regions_utr3, polyASignal, "utr3_transcript_id")]

  ## Check if gained or lost pA signal
  if (verbose) cat("Check if gained or lost pA signal...\n")
  variants[, num_polyA_signal_gainedOrLost := checkIfGainOrLoseAfterAlt(variants, "num_polyA_signal", "num_polyA_signal_altered")]

  # TSS kozak, start and stop codon analysis
  if (verbose) cat("get stop codon sequence...\n")
  variants[, stop_codon := getCodon(variants, coding_seq, "stopCodon_transcript_id", "stopCodon")]
  if (verbose) cat("get altered stop codon sequence...\n")
  variants[, stop_codon_altered := getCodonInAlt(variants, transcript_regions, "stopCodon_transcript_id", "stopCodon_positions", "stop_codon")]
  if (verbose) cat("get start codon sequence...\n")
  variants[, start_codon := getCodon(variants, coding_seq, "startCodon_transcript_id", "startCodon")]
  if (verbose) cat("get altered start codon sequence...\n")
  variants[, start_codon_altered := getCodonInAlt(variants, transcript_regions, "startCodon_transcript_id", "startCodon_positions", "start_codon")]
  if (verbose) cat("get TSS Kozak sequence...\n")
  variants[, kozak := getTSSKozak(variants, utr5_seq, coding_seq, "kozak_transcript_id")]
  if (verbose) cat("get altered TSS Kozak sequence...\n")
  variants[, kozak_altered := getCodonInAlt(variants, transcript_regions, "kozak_transcript_id", "kozak_positions", "kozak")]

  ## check if lost start codon, stop codon
  if (verbose) cat("check if lost start codon after Alt...\n")
  variants[, lost_start_codon := checkIfLostCodonAfterAlt(variants, "start_codon_altered", "ATG")]
  if (verbose) cat("check if lost stop codon after Alt...\n")
  variants[, lost_stop_codon := checkIfLostCodonAfterAlt(variants, "stop_codon_altered", c("TAG", "TAA", "TGA"))]

  ## Get Kozak PWM
  kozakPWM <- getKozakPWM()

  ## Calculate Kozak scores
  if (verbose) cat("Get Kozak scores...\n")
  variants[, kozak_score := getKozakScore(variants, "kozak", kozakPWM)]
  variants[, kozak_altered_score := getKozakScore(variants, "kozak_altered", kozakPWM)]

  ## Check if gain or lost Kozak
  if (verbose) cat("check if Kozak scores gained or lost...\n")
  variants[, tss_kozak_score_gainedOrLost := checkIfGainOrLoseAfterAlt(variants, "kozak_score", "kozak_altered_score")]

  ## get conservation scores
  if (verbose) cat("get conservation scores...\n")
  if (!is.null(conservationBwFiles) && dir.exists(conservationBwFiles)) {
    for (bw in list.files(conservationBwFiles, full.names = T, pattern = "*.bw")) {
      if (!file.exists(bw)) {
        stop(message("Conservation track file ", bw, " doesn't exist!"))
      }
      consName <- basename(bw)
      variants[[consName]] <- get_conservation_scores(variants, bw)
    }
  }

  ## Predict MRL for 5' UTR variants
  if (mrl_prediction == TRUE) {
    if (verbose) cat("check if MRL increased or decreased...\n")
    variants[, mrl := predictMRL(variants, utr5_seq, "utr5_transcript_id")]
    variants[, mrl_altered := predictMRLInAlt(variants, utr5_seq, transcript_regions_utr5, "utr5_transcript_id")]
    variants[, mrl_gainedOrLost:= checkIfGainOrLoseAfterAlt(variants, "mrl", "mrl_altered")]
  }

  write_csv(variants, annotationResult)
  cat("Done annotation!\nOutput the final annotation matrix to a CSV file:", annotationResult, "\n")
}

#' Partition the variant file to run in parallel
#' @description \code{partitionVariantFile} is used to partition the variant file into small partition files by user defined size or number
#' @param variantFile variant file in CSV format
#' @param chunkSize Partition the variant file into chunk files, which has a certain number of rows
#' @param chunkNum Partition the variant file into a certain number of chunk files
#' @param chunkPath (optional) A file directory to store all partition files. By default will output to "chunks" folder
#' @param species human or mouse
#' @param ensemblVersion (optional) a number specifying which version of Ensembl annotation you'd like to use, by default use the latest version
#' @param overwrite (optional) If chunkPath already exists and not empty, whether or not to overwrite it. By default, do not overwrite and will raise error.
#' @param dataDir (optional) path to store database information, if not specified will create a folder named as input variant file name with a "db_" prefix
#' @param getTranscript (optional) Whether to get ids of the transcripts that overlap with all the variants. The default value is TRUE. If the number of variants is too large (for example > 100,000), set it to FALSE and do this in runUTRAnnotation on each partition in parallel.
#' @param format (optional) csv or vcf, the default is csv
#' @param verbose Whether print diagnostic messages. The default is FALSE.
#' @return a list of partition variant files
#' @export
#' @importFrom utils write.table
#' @examples
#' \donttest{
#' variants_sample <- system.file("extdata", "variants_sample.csv", package = "utr.annotation")
#'
#' # Partition variants_sample file equally into 3 variant files
#' # and store them in user specified chunkPath folder
#' partitionVariantFile(variantFile = variants_sample,
#'                      chunkNum = 3,
#'                      chunkPath = "chunks_3",
#'                      species = "human",
#'                      ensemblVersion = 93,
#'                      dataDir = "db_all_variants")
#'
#' # Partition variants_sample file into smaller variant files each of which contains 7 variants,
#' # and store them in user specified chunkPath folder
#' partitionVariantFile(variantFile = variants_sample,
#'                      chunkSize = 7,
#'                      chunkPath = "chunks_7_vars",
#'                      species = "human",
#'                      ensemblVersion = 93,
#'                      dataDir = "db_all_variants")
#' unlink("db_all_variants", recursive = TRUE)
#' unlink("chunks_3", recursive = TRUE)
#' unlink("chunks_7_vars", recursive = TRUE)
#' }
partitionVariantFile <- function(variantFile, chunkSize = NULL, chunkNum = NULL, chunkPath = "chunks", species, ensemblVersion = NULL, overwrite = FALSE, dataDir = NULL, getTranscript = TRUE, format = "csv", verbose = FALSE) {
  if (is.null(chunkSize) && is.null(chunkNum)) {
    stop("Please specify the chunkSize or chunkNum!")
  }
  if (!is.null(chunkSize) && !is.null(chunkNum)) {
    stop("You can partition the file by specifying either chunkNum or chunkSize, but not both!")
  }
  if (!is.null(chunkSize)) {
    chunkSize <- as.numeric(chunkSize)
    stopifnot(!is.na(chunkSize) && chunkSize > 0)
    if (verbose) cat("Partitioning", variantFile,
        "\nchunk size =", chunkSize)
  }
  if (!is.null(chunkNum)) {
    chunkNum <- as.numeric(chunkNum)
    stopifnot(!is.na(chunkNum) && chunkNum > 0)
    if (verbose) cat("Partitioning", variantFile,
        "\nchunk number =", chunkNum)
  }
  if (is.null(ensemblVersion)) {
    ensemblVersion <- getLatestEnsemblVersion(species)
  }

  ## warn or stop if chunkPath already exist
  if (dir.exists(chunkPath)) {
    if (length(list.files(chunkPath)) > 0 && overwrite == FALSE) {
      stop(paste0("chunkPath already exists and not empty, the program will delete all files in the specified chunkPath: ", chunkPath,
                  "\nIf you are sure to delete and overwrite everything in directory:  ", chunkPath, " , then set overwrite = True."))
    } else {
      warning("chunkPath already exists and not empty. Deleting the chunkPath and all files in this directory.")
      unlink(chunkPath, recursive = T)
    }
  }

  dir.create(chunkPath, showWarnings = TRUE, recursive = TRUE, mode = "0777")

  # Query Ensembl Db information and store them to dataDir
  variants <- initUTRAnnotation(variantFile, species, ensemblVersion, getTranscript = getTranscript, format = tolower(format), dataDir = dataDir)
  variants_colnames <- colnames(variants)
  index <- 1
  if (!is.null(chunkSize) && !is.na(chunkSize)) {
    chunkNum <- ceiling(nrow(variants) / chunkSize)
  } else {
    chunkSize <- ceiling(nrow(variants) / chunkNum)
  }

  while (index <= chunkNum) {
    # With fread file is opened in each iteration
    startRow <- (index - 1) * chunkSize + 1
    endRow <- startRow+chunkSize-1
    if (endRow > nrow(variants)) {
      endRow <- nrow(variants)
    }

    dataChunk <- write_csv(variants[startRow:endRow], file.path(chunkPath, paste0("variants_chunks_",index, ".csv")))
    #increment the index to read next chunk
    index = index + 1
  }

  varchunksFiles <- list.files(chunkPath, pattern = "*.csv")
  ## sort varchunksFiles by the chunk index
  varchunksFiles <- str_sort(varchunksFiles, numeric = T)
  if (verbose) cat("Write lookup file to", file.path(chunkPath, "lookup.tab"))
  write.table(varchunksFiles, file.path(chunkPath, "lookup.tab"), row.names = TRUE, col.names = FALSE, quote = FALSE, sep = "\t")
  cat("Done partition!\nOutput partition files to", chunkPath,
      "\nGenerate lookup table for variant chunks to", file.path(chunkPath, "lookup.tab"), "\n")
}


#' Concatenate annotation result files into one file
#' @description \code{concatenateAnnotationResult} Concatenate annotation result files into one file
#' @param varResultPath annotation result directory
#' @param annotationFinalResult final annotation file
#' @param cores (optional) number of cores you'd like to use, by default would use a single core.
#' @param verbose Whether print diagnostic messages. The default is FALSE.
#' @return write concatenated annotation file to annotationFinalResult
#' @import readr
#' @export
#' @examples
#' \dontrun{
#' # After we run UTR annotation on all partition variant files and
#' # store the annotations in "partition_results" for example,
#' # we can concatenate the annotation results into one file and
#' # store it in concatenated_annotation.csv
#' concatenateAnnotationResult(varResultPath = "partition_results",
#'                             annotationFinalResult = "concatenated_annotation.csv")
#' }
concatenateAnnotationResult <- function(varResultPath, annotationFinalResult, cores = NULL, verbose = FALSE) {
  i <- NULL
  if (verbose) cat("Read setting from the arguments.",
            "\npath of the annotation result of the partition files =", varResultPath,
            "\nfinal result file =", annotationFinalResult, "\n")

  if (file.exists(annotationFinalResult)) {
    stop("annotationFinalResult file: ", annotationFinalResult, " already exist!")
  }
  if (!dir.exists(dirname(annotationFinalResult))) {
    dir.create(dirname(annotationFinalResult), recursive = T)
  }
  # create parallel backend
  init_backend(cores)

  chunksMatrix <- list.files(varResultPath, pattern = ".csv")
  ## sort chunksMatrix by the chunk index
  chunksMatrix <- str_sort(chunksMatrix, numeric = T)

  result_all <- foreach(i=1:length(chunksMatrix), .combine = rbind) %dopar% {
    read_csv(file.path(varResultPath, chunksMatrix[i]),col_types = cols(.default = "c"))
  }

  write_csv(result_all, annotationFinalResult)
  cat("Done concatenation!\nThe final annotation file is at", annotationFinalResult)
}
