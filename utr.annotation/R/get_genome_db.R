# Author: Yating Liu
# Description: Define functions to get genome information from database
# Date: 02/01/2019

#' Convert species name to AnnotationHub acceptable species name
#' @keywords internal
#' @param species human or mouse
#' @return AnnotationHub acceptable species name
getOrgName <- function(species) {
  if (species == "human") {
    return ("Homo sapiens")
  } else if (species == "mouse") {
    return ("Mus musculus")
  } else {
    stop("We only support human and mouse at this point!")
  }
}


#' Create a EnsDb database file from AnnotationHub
#' @keywords internal
#' @description \code{createEnsDbFromAH} create a sqlite db for specified species and version
#' @param species human or mouse
#' @param ensemblVersion Ensembl database version
#' @param dataDir path to store database information
#' @return save Db file into dataDir/species/ensemblVersion/
#' @import AnnotationHub
#' @importFrom ensembldb ensDbFromAH ensemblVersion organism
createEnsDbFromAH <- function(species, ensemblVersion, dataDir) {
  dataDir <- file.path(dataDir, species, ensemblVersion)
  # If DB file already exists, load the Db file
  if (length(list.files(dataDir, pattern = "sqlite$")) == 1) {
    return (list.files(dataDir, pattern = "sqlite$", full.names = T))
  }
  if (!dir.exists(dataDir)) {
    dir.create(dataDir, showWarnings = TRUE, recursive = TRUE, mode = "0777")
  }
  ## Load the AnnotationHub data.
  ah <- AnnotationHub()
  ## Query all available files for Ensembl release version for the species
  orgName <- getOrgName(species)
  gtffiles <- query(ah, c(orgName, paste0("release-", ensemblVersion), "GTF"))
  ## select full annotation by default
  ahId <- names(gtffiles)[str_detect(gtffiles$title, paste0(ensemblVersion, ".gtf$")) & tolower(gtffiles$species) == tolower(orgName)]
  ## Get the resource for the gtf file with the gene/transcript definitions.
  Gtf <- ah[ahId]
  ## Create a EnsDb database file from this.
  DbFile <- ensDbFromAH(Gtf, path = dataDir)
  return (DbFile)
}

#' Get protein coding transcript ids that overlap with each variant in the variant table
#' @keywords internal
#' @description \code{getTranscriptIds} Get transcript ids that overlap with each variant in the variant table
#' @param variantsTable variant table
#' @param species human or mouse
#' @param ensemblVersion Ensembl database version
#' @param dataDir path to store database information
#' @return a list of protein coding transcript ids that overlap with each variant, if more than one transcripts overlap with a variant, they are concatenated with ";"
getTranscriptIds <- function(variantsTable, species, ensemblVersion, dataDir) {
  i <- NULL
  # Create a EnsDb database file from AnnotationHub
  createEnsDbFromAH(species, ensemblVersion, dataDir)
  transcriptIds <- foreach(i=1:nrow(variantsTable), .combine = c) %dopar% getTranscriptIdsOneVariant(variantsTable[i,], species, ensemblVersion, dataDir)
  return (transcriptIds)
}

#' Search the db to find protein coding transcript ids that overlap with a variant
#' @keywords internal
#' @description \code{getTranscriptIdsOneVariant} Get transcript ids that overlap with one variant
#' @param variant one variant
#' @param species human or mouse
#' @param ensemblVersion Ensembl database version
#' @param dataDir path to store database information
#' @return protein coding transcript ids that overlap with the variant, if more than one transcripts overlap with the variant, they are concatenated with ";", if no transcript found, return NA.
#' @importFrom ensembldb EnsDb
#' @importFrom AnnotationFilter GRangesFilter TxBiotypeFilter
#' @importFrom GenomicRanges GRanges
#' @importFrom GenomicFeatures transcripts
#' @importFrom IRanges IRanges
getTranscriptIdsOneVariant <- function(variant, species, ensemblVersion, dataDir) {
  # Connect to an EnsDb object
  ensDb <- EnsDb(createEnsDbFromAH(species, ensemblVersion, dataDir))
  ## Define the filter
  grf <- GRangesFilter(GRanges(str_remove(variant[["Chr"]], "chr"),
                               ranges = IRanges(as.numeric(variant[["Pos"]]), as.numeric(variant[["Pos"]]) + max(nchar(variant[["Ref"]]), nchar(variant[["Alt"]])) - 1)),
                       type = "any")
  biotype <- TxBiotypeFilter("protein_coding", "==")

  ## Query transcripts that overlap with the variant and must be protein coding gene
  trans <- transcripts(ensDb, filter = c(grf, biotype))
  if (length(trans) > 0) {
    return (paste0(sort(trans$tx_id), collapse = ";"))
  } else {
    return (NA)
  }
}

#' Checking whether a transcript is valid
#' @keywords internal
#' @description \code{validateTranscripts} checks 1) whether a transcript has a peptide sequence, 2) whether the peptide sequence ends with "*", 3) whether the peptide starts with "M"
#' @param transcriptIds a list of Ensembl transcript ids
#' @param species human or mouse
#' @param ensemblVersion Ensembl database version
#' @return transcript_regions with additional flags: protein_coding, valid_startcodon, valid_stopcodon
#' @import data.table
validateTranscripts <- function(transcriptIds, species, ensemblVersion, dataDir = "data") {
  pep_seq <- getSeqTable(transcriptIds, 'peptide', species, ensemblVersion, dataDir)
  pep_seq[, `:=` (protein_coding = (seq != "Sequence unavailable"),
                  valid_startcodon = !startsWith(seq, "X"),
                  valid_stopcodon = endsWith(seq, "*"),
                  startcodon_M = startsWith(seq, "M"))]
  return (pep_seq[, seq := NULL])
}

#' query transcrips regions, utr sequences, and coding sequences from Ensembl database
#' @keywords internal
#' @description \code{queryEnsemblInfo} query transcrips regions, utr sequences, and coding sequences from Ensembl database and save rds into sepecified directory
#' @param transcriptIds Ensembl transcript ids
#' @param species human or mouse
#' @param ensemblVersion Ensembl database version
#' @param dataDir path to store database information
#' @param verbose Whether print diagnostic messages. The default is FALSE.
#' @return save all Db objects into dataDir
#' @import data.table
queryEnsemblInfo <- function(transcriptIds, species, ensemblVersion, dataDir, verbose = FALSE) {
  ensembl_transcript_id <- protein_coding <- valid_stopcodon <- valid_startcodon <-  NULL
  dataDir <- file.path(dataDir, species, ensemblVersion)
  if (!dir.exists(dataDir)) {
    dir.create(dataDir, showWarnings = TRUE, recursive = TRUE, mode = "0777")
  }
  # If no Transcript that overlap with any variants, return
  if (all(is.na(transcriptIds)) == TRUE) {
    if (verbose) cat("no transcript overlaps with variants, return.")
    return (NA)
  }
  # Get transcript info, 5'UTR and 3'UTR coordinates for each transcript.
  if (verbose) cat("Get all ensembl transcript id to extract sequences from DB...\n")
  transcript_regions <- getTrasncriptsRegions(transcriptIds, species, ensemblVersion, dataDir)
  transcriptIds <- unique(transcript_regions[, ensembl_transcript_id])
  # check whether a transcript is protein coding, have valid start codon, have valid stop codon
  transcript_validation <- validateTranscripts(transcriptIds, species, ensemblVersion, dataDir)
  transcript_validation <- transcript_validation[protein_coding == TRUE & valid_stopcodon == TRUE & valid_startcodon,]
  ## remove transcripts that are not protein coding and do not have stop codon
  transcript_regions <- left_join(transcript_validation[, c("ensembl_transcript_id", "startcodon_M")], transcript_regions)
  transcript_regions <- as.data.table(transcript_regions)
  transcriptIds <- unique(transcript_validation[, ensembl_transcript_id])
  # To check if any transcripts. If no transcripts in the variants, no further analysis is needed
  if (length(transcriptIds) == 0) {
    if (verbose) cat("no valid transcript overlaps with variants, return.")
  } else {
    if (verbose) cat("Saving transcript_regions to:", file.path(dataDir, "transcript_regions.rds"))
    saveRDS(transcript_regions, file.path(dataDir, "transcript_regions.rds"))
    # extract sequences from DB
    if (verbose) cat("extract sequences from DB...\n")
    utr5_seq <- getSeqTable(transcriptIds, '5utr', species, ensemblVersion, dataDir)
    if (verbose) cat("Saving utr5_seq to:", file.path(dataDir, "utr5_seq.rds"))
    saveRDS(utr5_seq, file.path(dataDir, "utr5_seq.rds"))
    utr3_seq <- getSeqTable(transcriptIds, '3utr', species, ensemblVersion, dataDir)
    if (verbose) cat("Saving utr3_seq to:", file.path(dataDir, "utr3_seq.rds"))
    saveRDS(utr3_seq, file.path(dataDir, "utr3_seq.rds"))
    coding_seq <- getSeqTable(transcriptIds, 'coding', species, ensemblVersion, dataDir)
    if (verbose) cat("Saving coding_seq to:", file.path(dataDir, "coding_seq.rds"))
    saveRDS(coding_seq, file.path(dataDir, "coding_seq.rds"))
  }

}

#' Get Ensembl database object for the specified species
#' @keywords internal
#' @param species human or mouse
#' @param ensemblVersion Ensembl version number
#' @param dataDir Path to store database information
#' @return Save BioMart object to dataDir
#' @importFrom biomaRt useEnsembl
getEnsembl <- function(species, ensemblVersion, dataDir) {
  if (!dir.exists(dataDir)) {
    dir.create(dataDir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
  }
  if (species == "human") {
    if (!file.exists(file.path(dataDir, paste0("ensembl_human_", ensemblVersion, ".rds")))) {
      ensembl = useEnsembl(biomart="ensembl", dataset="hsapiens_gene_ensembl", version = ensemblVersion)
      saveRDS(ensembl, file.path(dataDir, paste0("ensembl_human_", ensemblVersion, ".rds")))
    } else {
      ensembl <- readRDS(file.path(dataDir, paste0("ensembl_human_", ensemblVersion, ".rds")))
    }
  } else if (species == "mouse") {
    if (!file.exists(file.path(dataDir, paste0("ensembl_mouse_", ensemblVersion, ".rds")))) {
      ensembl = useEnsembl(biomart="ensembl", dataset="mmusculus_gene_ensembl", version = ensemblVersion)
      saveRDS(ensembl, file.path(dataDir, paste0("ensembl_mouse_", ensemblVersion, ".rds")))
    } else {
      ensembl <- readRDS(file.path(dataDir, paste0("ensembl_mouse_", ensemblVersion, ".rds")))
    }
  } else {
    stop("Species ", species, " is not supported currently! Only human or mouse is supported.")
  }
  return (ensembl)
}

#' Get information on transcript regions, UTRs regions, coding regions, chromosome name, and strand from Ensembl database
#' @description \code{getTrasncriptsRegions} query "ensembl_gene_id", "ensembl_transcript_id", "transcript_start", "transcript_end", "5_utr_start", "5_utr_end", "3_utr_start", "3_utr_end", "genomic_coding_start", "genomic_coding_end", "gene_biotype", "strand" from Ensembl database
#' @keywords internal
#' @param transcriptIds Ensembl transcript ids
#' @param species human or mouse
#' @param ensemblVersion Ensembl database version
#' @param dataDir directory to store DB information
#' @return transcript structure table containing "ensembl_gene_id", "ensembl_transcript_id", "transcript_start", "transcript_end", "5_utr_start", "5_utr_end", "3_utr_start", "3_utr_end", "genomic_coding_start", "genomic_coding_end","gene_biotype", "strand"
#' @importFrom biomaRt getBM
getTrasncriptsRegions <- function(transcriptIds, species, ensemblVersion, dataDir) {
  ensembl <- getEnsembl(species, ensemblVersion, dataDir)
  tran_ids <- unique(unlist(str_split(transcriptIds, ";")))
  transcript_regions <- getBM(attributes = c("ensembl_gene_id", "ensembl_transcript_id", "transcript_start", "transcript_end", "5_utr_start", "5_utr_end", "3_utr_start", "3_utr_end", "genomic_coding_start", "genomic_coding_end", "gene_biotype", "strand"), filters = c("ensembl_transcript_id", "transcript_gencode_basic"), values = list(tran_ids, TRUE), mart = ensembl)
  transcript_regions <- as.data.table(transcript_regions)
  # change column names which start with a number, which is a not good name convension for data table
  setnames(transcript_regions, c("5_utr_start", "5_utr_end", "3_utr_start", "3_utr_end"), c("utr5_start", "utr5_end", "utr3_start", "utr3_end"))
  return (transcript_regions)
}

#' Get UTR sequences of a list of transcripts
#' @keywords internal
#' @description \code{getSeqTable} will query 5' UTR or 3' UTR sequences of a list of transcripts from Ensembl database
#' @param transcriptIds a list of Ensembl transcript ids
#' @param seqType Either '5utr' or '3utr'. 5utr' for 5' UTR sequence, '3utr' for 3' UTR sequences
#' @param species human or mouse
#' @param dataDir directory to store DB information
#' @return sequences table containing columns: ensembl_transcript_id and seq
#' @importFrom biomaRt getSequence
getSeqTable <- function(transcriptIds, seqType, species, ensemblVersion, dataDir, downstream = NULL, upstream = NULL) {
  ensembl <- getEnsembl(species, ensemblVersion, dataDir)

  if (!is.null(downstream) & !is.null(upstream)) {
    stop("Currently getSequence only allows the user to specify either an upstream of a downstream argument but not both.")
  }
  if (!is.null(downstream)) {
    seqTable <- getSequence(id=transcriptIds,
                            type='ensembl_transcript_id',
                            seqType = seqType,
                            downstream = downstream,
                            mart = ensembl)
  } else if (!is.null(upstream)) {
    seqTable <- getSequence(id=transcriptIds,
                            type='ensembl_transcript_id',
                            seqType = seqType,
                            upstream = upstream,
                            mart = ensembl)
  } else {
    seqTable <- getSequence(id=transcriptIds,
                            type='ensembl_transcript_id',
                            seqType = seqType,
                            mart = ensembl)
  }
  seqTable <- as.data.table(seqTable)
  setnames(seqTable, old=seqType, new='seq')

  return (seqTable)
}
