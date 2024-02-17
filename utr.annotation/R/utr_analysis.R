#' Create parallel backend with user specified number of CPUs
#' @keywords internal
#' @param cores number of CPUs
#' @return Register the parallel backend with user specified number of cores
#' @importFrom doParallel registerDoParallel
init_backend <- function(cores) {
  no_cores <- as.numeric(cores)
  if (length(no_cores) == 0 || no_cores < 1) {
    no_cores <- 1
  }
  # registerDoParallel use FORK cluster for UNIX like system and use PFORK cluster for windows and stop the cluster automatically
  registerDoParallel(no_cores)
}


#' Find the ensembl_transcript_id for utr variants genes, or cds
#' @keywords internal
#' @description \code{getTranscriptIdsForUTRVariants} find transcripts if a variant fall in their specified region and return all transcripts id for each variant in the variant table
#' @param variantsTable variants table
#' @param transcriptRegions transcripts structure table from Ensembl
#' @param checkRegion check if a transcript within a region: utr5 or utr3 or cds
#' @return  A list containing ensembl_transcript_id of each variant which its mutation Pos is within those transcripts checkRegion.
#' @importFrom foreach foreach %dopar%
getTranscriptIdsForUTRVariants <- function(variantsTable, transcriptRegions, checkRegion) {
  i <- NULL
  if (checkRegion == "utr5") {
    start <- "utr5_start"
    end <- "utr5_end"
  } else if (checkRegion == "utr3") {
    start <- "utr3_start"
    end <- "utr3_end"
  } else if (checkRegion == "cds") {
    start <- "genomic_coding_start"
    end <- "genomic_coding_end"
  } else if (checkRegion == "transcript") {
    start <- "transcript_start"
    end <- "transcript_end"
  } else {
    stop(paste0("Input error. checkRegion", checkRegion, "not support!"))
  }

  utrTranscriptIds <- foreach(i=1:nrow(variantsTable), .combine = c) %dopar% getTranscriptIdsForOneUTRVariant(variantsTable[i,], transcriptRegions, start, end)

  if (length(utrTranscriptIds)) {
    return (utrTranscriptIds)
  }
}


#' Find all the ensembl_transcript_id for a variant which fall in the specified region for one variant
#' @keywords internal
#' @description \code{getTranscriptIdsForOneUTRVariant} find transcripts if a variant fall in their specified region
#' @param variant one row of variants table
#' @param transcriptRegions transcripts structure table from ensembl
#' @param start column name of the start region in the transcriptRegions table
#' @param end column name of the end region in the transcriptRegions table
#' @return  A string representing a list of containing ensembl_transcript_id and collapsed with ;
#' @importFrom IRanges subsetByOverlaps IRanges
#' @importFrom dplyr select
#' @importFrom GenomicRanges GRanges
getTranscriptIdsForOneUTRVariant <- function(variant, transcriptRegions, start, end) {
  ensembl_transcript_id <- NULL
  Gene = variant[["Transcript"]]
  Pos = variant[["Pos"]]
  Ref = variant[["Ref"]]
  Alt = variant[["Alt"]]

  if (is.na(Gene)) {
    return(NA_character_)
  }
  Gene <- unique(unlist(str_split(Gene, ";")))
  transcripts <- transcriptRegions[ensembl_transcript_id %in% Gene][order(ensembl_transcript_id)]
  transcripts <- dplyr::select(transcripts, start, end, ensembl_transcript_id)
  transcripts <- transcripts[complete.cases(transcripts)]
  if (nrow(transcripts) <= 0) {
    return(NA_character_)
  }
  varRange <- GRanges(seqnames = str_remove(variant[["Chr"]], "chr"),
                      ranges = IRanges(as.numeric(Pos), as.numeric(Pos) + nchar(Ref)- 1))

  featureRange <- GRanges(seqnames = str_remove(variant[["Chr"]], "chr"),
                          ranges = IRanges(as.numeric(transcripts[[start]]), as.numeric(transcripts[[end]])),
                          ensembl_transcript_id = transcripts$ensembl_transcript_id)

  tran_ids <- subsetByOverlaps(featureRange, varRange)$ensembl_transcript_id
  tran_ids <- sort(unique(tran_ids))

  if (length(tran_ids) > 0) {
    return (str_c(tran_ids, collapse = ";"))
  } else {
    return(NA_character_)
  }

}



#' Find the ensembl_transcript_id and codon positions for variants
#' @keywords internal
#' @param variantsTable variants table
#' @param transcriptRegions transcripts structure table from ensembl
#' @param checkRegion check if a transcript within a region stopCodon or startCodon
#' @return A data table containing two columns: ensembl_transcript_id (id of transcripts for each variant which its mutation Pos is within those transcripts checkRegion.)
#'                                        codon_positions (positions of each nucleotides in the codon)
#' @details
#' For each variant, get all transcripts and extract their checkRegion's coordinates. For each transcript, check if the mutation Pos
#' is within the region, if so, add that transcript's ensembl_transcript_id. If the mutation Pos is within multiple transcripts checkRegions,
#' the transcripts ids are concatenate into one string with ";".
#' @importFrom foreach foreach %dopar%
getTranscriptIdsForCodonVariants <- function(variantsTable, transcriptRegions, checkRegion) {
  i <- NULL
  if (checkRegion == "stopCodon" | checkRegion == "startCodon") {
    start <- "genomic_coding_start"
    end <- "genomic_coding_end"
  } else {
    stop("Input error. checkRegion should be either startCodon or stopCodon")
  }
  codonTranscriptIds <- foreach(i=1:nrow(variantsTable), .combine = rbind) %dopar% getTranscriptIdsForOneCodonVariant(variantsTable[i,], transcriptRegions, checkRegion, start, end)

  return (codonTranscriptIds)
}


#' Find the ensembl_transcript_id and codon postions for one variant
#' @keywords internal
#' @param variant one row of variants table
#' @param transcriptRegions transcripts structure table from ensembl
#' @param checkRegion check if a transcript within a region stopCodon or startCodon
#' @param start column name of the start region of checkRegion in transcripts structure table
#' @param end column name of the end region of checkRegion in transcripts structure table
#' @return A data table containing two columns: ensembl_transcript_id (id of transcripts for each variant which its mutation Pos is within those transcripts checkRegion.)
#'                                        codon_positions (positions of each nucleotides in the codon)
#' @details
#' For each variant, get all transcripts and extract their checkRegion's coordinates. For each transcript, check if the mutation Pos
#' is within the region, if so, add that transcript's ensembl_transcript_id. If the mutation Pos is within multiple transcripts checkRegions,
#' the transcripts ids are concatenate into one string with ";".
#' @importFrom GenomicRanges makeGRangesListFromDataFrame resize width start end
#' @import stringr
#' @importFrom BiocGenerics strand
#' @importFrom stats complete.cases
getTranscriptIdsForOneCodonVariant <- function(variant, transcriptRegions, checkRegion, start, end) {
  ensembl_transcript_id <- strand <- NULL
  Gene = variant[["Transcript"]]
  Pos = variant[["Pos"]]
  Ref = variant[["Ref"]]
  Alt = variant[["Alt"]]

  if (is.na(Gene)) {
    return (data.frame(ensembl_transcript_id = NA_character_, codon_positions = NA_character_))
  }
  Gene <- unique(unlist(str_split(Gene, ";")))
  transcripts <- transcriptRegions[ensembl_transcript_id %in% Gene][order(ensembl_transcript_id)]
  transcripts <- dplyr::select(transcripts, start, end, strand, ensembl_transcript_id)
  transcripts <- transcripts[complete.cases(transcripts)]

  if (nrow(transcripts) <= 0) {
    return(data.frame(ensembl_transcript_id = NA_character_, codon_positions = NA_character_))
  }

  featureDf <- data.frame(chr=variant[["Chr"]], start=as.numeric(transcripts[[start]]), end=as.numeric(transcripts[[end]]),
                   strand= ifelse(transcripts$strand > 0, '+', '-'),
                   ensembl_transcript_id = transcripts$ensembl_transcript_id)

  featureRange <- makeGRangesListFromDataFrame(featureDf, split.field = "ensembl_transcript_id", names.field = "ensembl_transcript_id")

  tran_ids <- c()
  codonsPosList <- c()
  pos_start <- as.numeric(Pos)
  pos_end <- pos_start + nchar(Ref) -1
  # find overlap for each transcript
  for (id in names(featureRange)) {
    codonsPos <- c()
    reverseOrder <- FALSE
    if (all(strand(featureRange[[id]]) == '-')) {
      reverseOrder <- TRUE
    }
    featureRange[[id]] <- sort(featureRange[[id]], decreasing = reverseOrder)
    if (checkRegion == "startCodon") {
      i <- 1
      while (length(codonsPos) < 3 && i <= length(featureRange[[id]])) {
        rg <- resize(featureRange[[id]][i], min(3-length(codonsPos), width(featureRange[[id]][i])), fix = "start")
        codonsPos <- append(codonsPos, c(start(rg):end(rg)))
        i <- i+1
      }
    } else {
      i <- length(featureRange[[id]])
      while (length(codonsPos) < 3 && i > 0) {
        rg <- resize(featureRange[[id]][i], min(3-length(codonsPos), width(featureRange[[id]][i])), fix = "end")
        codonsPos <- append(codonsPos, c(start(rg):end(rg)))
        i <- i-1
      }
    }
    if (length(intersect(c(pos_start:pos_end), codonsPos)) > 0) {
      tran_ids <- append(tran_ids, id)
      codonsPosList <- append(codonsPosList, str_c(sort(codonsPos), collapse = "|"))
    }
  }

  if (length(tran_ids)) {
    return (data.frame(ensembl_transcript_id = str_c(tran_ids, collapse = ";"), codon_positions =str_c(codonsPosList, collapse = ";")))
  } else{
    return (data.frame(ensembl_transcript_id = NA_character_, codon_positions = NA_character_))
  }
}

#' Find the ensembl_transcript_id and kozak_positions for variants in TSS Kozak region (8nt) [AG]..AUGG.
#' @keywords internal
#' @description \code{getTranscriptIdsForTSSKozakVariants} get the ensembl_transcript_id and kozak_positions for variants in TSS Kozak region (8nt) [AG]..AUGG.
#' @param variantsTable variants table
#' @param transcriptRegions transcripts structure table from ensembl
#' @return A data table containing two columns: ensembl_transcript_id (id of the transcripts for each variant which its mutation Pos is within TSS Kozak regions)
#'                                        kozak_positions (positions of each nucleotide in Kozak sequence)
#' @details
#' For each variant, get all transcripts and extract their checkRegion's coordinates. For each transcript, check if the mutation Pos
#' is within the region, if so, add that transcript's ensembl_transcript_id. If the mutation Pos is within multiple transcripts checkRegions,
#' the transcripts ids are concatenate into one string with ";".
#' @importFrom foreach foreach %dopar%
getTranscriptIdsForTSSKozakVariants <- function(variantsTable, transcriptRegions) {
  i <- NULL
  TSSKozakVariantsIds <- foreach(i=1:nrow(variantsTable), .combine = rbind) %dopar% getTranscriptIdsForOneTSSKozakVariant(variantsTable[i,], transcriptRegions)

  return (TSSKozakVariantsIds)
}


#' Find the ensembl_transcript_id and kozak_positions for one variant in TSS Kozak region (8nt) [AG]..AUGG.
#' @keywords internal
#' @description \code{getTranscriptIdsForOneTSSKozakVariants} get the ensembl_transcript_id and kozak_positions for one variant in TSS Kozak region (8nt) [AG]..AUGG.
#' @param variant one variant
#' @param transcriptRegions transcripts structure table from ensembl
#' @return A data table containing two columns: ensembl_transcript_id (id of the transcripts for each variant which its mutation Pos is within TSS Kozak regions)
#'                                        kozak_positions (positions of each nucleotide in Kozak sequence)
#' @details
#' Get all transcripts and extract their checkRegion's coordinates. For each transcript, check if the mutation Pos
#' is within the region, if so, add that transcript's ensembl_transcript_id. If the mutation Pos is within multiple transcripts checkRegions,
#' the transcripts ids are concatenate into one string with ";".
getTranscriptIdsForOneTSSKozakVariant <- function(variant, transcriptRegions) {
  ensembl_transcript_id <- strand <- NULL
  Gene = variant[["Transcript"]]
  Pos = variant[["Pos"]]
  Ref = variant[["Ref"]]
  Alt = variant[["Alt"]]

  utr5Start <- "utr5_start"
  utr5End <- "utr5_end"
  codingStart <- "genomic_coding_start"
  codingEnd <- "genomic_coding_end"

  if (is.na(Gene)) {
    return(data.frame(ensembl_transcript_id = NA_character_, kozak_positions = NA_character_))
  }
  Gene <- unique(unlist(str_split(Gene, ";")))
  transcripts <- transcriptRegions[ensembl_transcript_id %in% Gene][order(ensembl_transcript_id)]
  utr5Regions <- dplyr::select(transcripts, utr5Start, utr5End, strand, ensembl_transcript_id)
  utr5Regions <- utr5Regions[complete.cases(utr5Regions)]
  codingRegions <- dplyr::select(transcripts, codingStart, codingEnd, strand, ensembl_transcript_id)
  codingRegions <- codingRegions[complete.cases(codingRegions)]
  if (nrow(utr5Regions) ==  0 || nrow(codingRegions) == 0) {
    return (data.frame(ensembl_transcript_id = NA_character_, kozak_positions = NA_character_))
  }
  # generate GRangesList for 5' UTRs
  utr5Df <- data.frame(chr=variant[["Chr"]], start=as.numeric(utr5Regions[[utr5Start]]), end=as.numeric(utr5Regions[[utr5End]]),
                          strand= ifelse(utr5Regions$strand > 0, '+', '-'),
                          ensembl_transcript_id = utr5Regions$ensembl_transcript_id)
  utr5Range <- makeGRangesListFromDataFrame(utr5Df, split.field = "ensembl_transcript_id", names.field = "ensembl_transcript_id")
  # genernate GRangesList for cds
  codingDf <- data.frame(chr=variant[["Chr"]], start=as.numeric(codingRegions[[codingStart]]), end=as.numeric(codingRegions[[codingEnd]]),
                       strand= ifelse(codingRegions$strand > 0, '+', '-'),
                       ensembl_transcript_id = codingRegions$ensembl_transcript_id)

  codingRange <- makeGRangesListFromDataFrame(codingDf, split.field = "ensembl_transcript_id", names.field = "ensembl_transcript_id")

  tran_ids <- c()
  kozakPosList <- c()

  pos_start <- as.numeric(Pos)
  pos_end <- pos_start + nchar(Ref)-1
  transcript_ids <- unique(transcripts[, ensembl_transcript_id])

  for (id in transcript_ids) {
    reverseOrder <- FALSE
    if (all(strand(utr5Range[[id]]) == '-')) {
      reverseOrder <- TRUE
    }
    utr5Range[[id]] <- sort(utr5Range[[id]], decreasing = reverseOrder)
    codingRange[[id]] <- sort(codingRange[[id]], decreasing = reverseOrder)
    kozakPos <- c()
    # pick the last three positions in 5' UTR
    i <- length(utr5Range[[id]])
    while (length(kozakPos) < 3 && i > 0) {
      rg <- resize(utr5Range[[id]][i], min(3-length(kozakPos), width(utr5Range[[id]][i])), fix = "end")
      kozakPos <- append(kozakPos, c(start(rg):end(rg)))
      i <- i-1
    }
    # pick the first four positions in coding sequence
    i <- 1
    while (length(kozakPos) < 8 && i <= length(codingRange[[id]])) {
      rg <- resize(codingRange[[id]][i], min(8-length(kozakPos), width(codingRange[[id]][i])), fix = "start")
      kozakPos <- append(kozakPos, c(start(rg):end(rg)))
      i <- i+1
    }

    if (length(intersect(c(pos_start:pos_end), kozakPos)) > 0) {
      tran_ids <- append(tran_ids, id)
      kozakPosList <- append(kozakPosList, str_c(sort(kozakPos), collapse = "|"))
    }
  }
  tran_ids <- unique(tran_ids)
  if (length(tran_ids) > 0) {
    return (data.frame(ensembl_transcript_id = str_c(tran_ids, collapse = ";"), kozak_positions = str_c(kozakPosList, collapse = ";")))
  } else {
    return (data.frame(ensembl_transcript_id = NA_character_, kozak_positions = NA_character_))
  }
}



#' Check if the nucleotides in Ref column match the actual nucleotides in the fragment
#' @keywords internal
#' @param fragment sequence within [fragment_start, fragment_end]
#' @param fragment_start start postion of the fragment
#' @param fragment_end end postion of the fragment
#' @param strand strand of the current sequence, 1 or -1
#' @param ref nucleotides in Ref column
#' @param alt nucleotides in Alt column
#' @param pos variant postion in Pos column
#' @return ragment sequence after mutation or -1 if the nucleotides in Ref column do not match the actual nucleotides in the sequence
#' @importFrom Biostrings reverseComplement DNAString
checkIfRefMatchAndAltFragment <- function(fragment, fragment_start, fragment_end, strand, ref, pos, alt, refAlwaysFromPlusStrand = TRUE) {
  varLen <- nchar(ref)
  fragLen <- nchar(fragment)
  ## make sure fragment_start, fragment_end, pos are numeric
  fragment_start <- as.numeric(fragment_start)
  fragment_end <- as.numeric(fragment_end)
  pos <- as.numeric(pos)
  # if pos is not inside the fragment region, pick the overlapped positions
  if (pos < fragment_start) {
    # if the whole varLen is outside, return the original fragment seuqnece
    if (pos + varLen -1 < fragment_start) {
      return (fragment)
    }
    ref <- substr(ref, fragment_start-pos+1, nchar(ref))
    alt <- substr(alt, fragment_start-pos+1, nchar(alt))
    varLen <- nchar(ref)
    pos <- fragment_start
  }
  if (strand == 1) {
    actualSeq <- substr(fragment, pos - fragment_start + 1, pos - fragment_start + 1 + varLen - 1)
    ## in some cases, the end position of Ref exceeds end coordinate of 3' or 5', so we only compare the sequences within 3' or 5'
    if (nchar(actualSeq) < varLen) {
      ref <- substr(ref, 1, nchar(actualSeq))
    }
    if (actualSeq != ref) {
      warning(paste("Ref = ", ref))
      warning(paste("Actual = ", substr(fragment, pos - fragment_start + 1, pos - fragment_start + 1 + varLen - 1)))
      return (-1)
    }
    fragment <- str_c(substr(fragment, 1, pos - fragment_start), alt, substr(fragment, pos - fragment_start + 1 + varLen, fragLen))
  } else if (strand == -1) {
    actualSeq <- substr(fragment, fragment_end - pos + 1 - varLen + 1, fragment_end - pos + 1)
    if (nchar(actualSeq) < varLen) {
      ref <- substr(ref, 1, nchar(actualSeq))
    }
    ## In the input data, if the Ref sequences of a transcript are always sequences on plus strand, then need to make them reverse complement
    ## when a transcript is on minus strand; If the Ref sequences are the actual sequences, then do not need to make them reverse complement.
    if (refAlwaysFromPlusStrand == "TRUE") {
      ref <- toString(reverseComplement(DNAString(ref)))
    }
    if (actualSeq != ref) {
      warning(paste("Ref = ", ref))
      warning(paste("Actual = ", substr(fragment, fragment_end - pos + 1 - varLen + 1, fragment_end - pos + 1)))
      return (-1)
    }
    ## should also convert alt to reverse complement sequence for minus strand
    if (refAlwaysFromPlusStrand == "TRUE") {
      alt <- toString(reverseComplement(DNAString(alt)))
    }
    fragment <- str_c(substr(fragment, 1, fragment_end - pos + 1 - varLen), alt, substr(fragment, fragment_end - pos + 2, fragLen))
  } else {
    stop(paste("Invalid strand :", strand))
  }
  return (fragment)
}


#' Check if Ref matches the actual sequence, if so then return altered sequence after replacing Ref with Alt. The sequence can have multiple fragments, only check the fragment which pos >= fragment_start & pos <= fragment_end
#' @keywords internal
#' @param seqTable sequences table containing columns: ensembl_transcript_id and seq. seq is the sequence of a seqType region: e.g. 5' UTR sequence or 3' UTR sequence. The seqTable is output from function getSeqTable.
#' @param trascriptsTable transcripts structure table, must have 4 columns: ensembl_transcript_id, start, end, strand.
#' start: start coordinate column in transcriptsTable for target region, for example "utr5_start", "utr3_start", or "genomic_coding_start"
#' end: end coordinate columns in transcriptsTable for target region, for example "utr5_end", "utr3_end", or "genomic_coding_end"
#' strand is the strand of the transcript.
#' @param transcriptId ensembl transcript id
#' @param ref nucleotides in Ref column
#' @param alt nucleotides in Alt column
#' @return Altered sequence which combines all its fragments. If Ref doesn't match the actual sequence, raise error
#' @importFrom stats complete.cases
getAltSequence <- function(seqTable, transcriptsTable, transcriptId, pos, ref, alt) {
  ensembl_transcript_id <- strand <- NULL
  # find all fragments coordiantes
  regions <- transcriptsTable[ensembl_transcript_id == transcriptId, c("start", "end", "strand")]
  # remove rows with NAs
  regions <- regions[complete.cases(regions)]
  # if a transcript is in minus strand, order the fragments in descending order
  if (regions[, strand][1] == 1) {
    regions <- regions[order(start, end)]
  } else {
    regions <- regions[order(start, end, decreasing = T)]
  }
  sequence <- seqTable[ensembl_transcript_id == transcriptId, seq]
  if (is.na(sequence) || length(sequence) == 0 || sequence == "Sequence unavailable" || sequence == "NA") {
    return (NA_character_)
  }
  # the total length of the fragments should equal to the length of the sequence
  if (sum(regions[, end] - regions[, start] + 1) != nchar(sequence)) {
    end <- regions[, end]
    start <- regions[, start]
    num <- as.character(nchar(sequence))
    warning(transcriptId, ": sum(", end, " - ", start, " + 1) != nchar(sequence): ", num)
  }
  #stopifnot(sum(regions[, end] - regions[, start] + 1) == nchar(sequence))

  # get altered sequence
  sequence_altered <- c()
  offset <- 1
  for (i in 1:nrow(regions)) {
    fragment_start <- regions[i, start]
    fragment_end <- regions[i, end]
    fragLen <- fragment_end - fragment_start + 1
    fragment <- substr(sequence, offset, offset + fragLen - 1)
    # found the fragment that contain the variant position
    pos <- as.numeric(pos)
    if (!(pos+nchar(ref)-1 < fragment_start || pos > fragment_end)) {
      varLen <- nchar(ref)
      # if the ref charactor doesn't match inside the transcript, skip this transcript, altered_num_kozak <- NA
      fragment <- checkIfRefMatchAndAltFragment(fragment, fragment_start, fragment_end, regions[i, strand], ref, pos, alt)
      if (fragment == -1) {
        warning(paste0("Ref ", ref, " at postion ", pos, " doesn't match the transcript ", transcriptId))
        return (NA_character_)
      }
    }
    sequence_altered <- append(sequence_altered, fragment)
    ## the length of the fragment may change after alt, so use the original fragLen, instead of nchar(fragment)
    offset <- offset + fragLen
  }
  # concatenate fragments into the altered sequence
  sequence_altered <- str_c(sequence_altered, collapse = "")
  return (sequence_altered)
}


#' Count the number of DNA pattern for each transcript, and concatenate the numbers with ";".
#' @keywords internal
#' @description \code{countDNAPattern} count the number of DNA pattern for each transcript, and concatenate the numbers with ";"
#' @param variantsTable variants table
#' @param seqTable sequences table containing two columns: ensembl_transcript_id and seq. seq is the sequence of the variantType region: e.g. 5' UTR sequence or 3' UTR sequence. The seqTable is output from function getSeqTable.
#' @param dnaPattern the string pattern that you want to count, for example "ATG"
#' @param transcriptIdCol the ensemble transcript id column
#' @return a list containing values for number of DNA pattern. Can be used to add new column to the variants table.
countDNAPattern <- function(variantsTable, seqTable, dnaPattern, transcriptIdCol) {
  i <- NULL
  if (!transcriptIdCol %in% colnames(variantsTable)) {
    return (NA)
  }
  num_dna_pattern <- foreach(i=1:nrow(variantsTable), .combine = c) %dopar% countDNAPatternOneVariant(variantsTable[i,], seqTable, dnaPattern, transcriptIdCol)
  return (num_dna_pattern)
}


#' Count the number of DNA pattern for one variant
#' @description \code{countDNAPattern} count the number of DNA pattern for one variant
#' @keywords internal
#' @param variant variant info
#' @param seqTable sequences table containing two columns: ensembl_transcript_id and seq. seq is the sequence of the variantType region: e.g. 5' UTR sequence or 3' UTR sequence. The seqTable is output from function getSeqTable.
#' @param dnaPattern the string pattern that you want to count, for example "ATG"
#' @param transcriptIdCol the ensemble transcript id column
#' @return number of DNA pattern of all transcripts for that variant.
countDNAPatternOneVariant <- function(variant, seqTable, dnaPattern, transcriptIdCol) {
  ensembl_transcript_id = variant[[transcriptIdCol]]

  # skip genes that are not protein coding (those genes ensembl_transcript_id == NA)
  if (is.na(ensembl_transcript_id)) {
    return (NA)
  }
  transcrip_ids <- str_split(ensembl_transcript_id, ";")[[1]]
  pattern_num <- c()

  for (id in transcrip_ids) {
    sequence <- seqTable[ensembl_transcript_id == id, seq]
    pattern_num <- append(pattern_num, str_count(sequence, dnaPattern))
  }
  ## use ";" to separate instead of ",", because CSV will add "," automatically for numbers bigger than 1000, which messed up the values
  if (length(pattern_num) > 0) {
    return(str_c(pattern_num, collapse = ";"))
  } else {
    return (NA)
  }

}


#' Count the number of DNA pattern in the altered sequence of each transcript.
#' @keywords internal
#' @description \code{countDNAPatternInAlt} Count the number of DNA pattern in the altered sequence of each transcript, and concatenate the numbers with ";". The alter sequence is generated by find the fragment where Pos is in, replace the Ref with Alt in the fragments, and then concatenating all fragments together.
#' @param variantsTable variants table
#' @param seqTable sequences table containing columns: ensembl_transcript_id and seq. seq is the sequence of a seqType region: e.g. 5' UTR sequence or 3' UTR sequence.
#'             The seqTable is output from function getSeqTable.
#' @param trascriptsTable transcripts structure table from ensembl with start and end columns for start and end coordinates of the target regions
#' @param dnaPattern the string pattern that you want to count, for example "ATG"
#' @param transcriptIdCol the ensemble transcript id column
#' @return a list containing values for number of DNA pattern. Can be used to add new column to the variants table.
countDNAPatternInAlt <- function(variantsTable, seqTable, transcriptsTable, dnaPattern, transcriptIdCol) {
  i <- NULL
  if (!transcriptIdCol %in% colnames(variantsTable) || all(is.na(variantsTable[[transcriptIdCol]]))) {
    return (NA)
  }
  num_dna_pattern_altered <- foreach(i=1:nrow(variantsTable), .combine = c) %dopar% countDNAPatternInAltOneVariant(variantsTable[i,], seqTable, transcriptsTable, dnaPattern, transcriptIdCol)
  return (num_dna_pattern_altered)

}


#' Count the number of DNA pattern in the altered sequence of one transcript.
#' @keywords internal
#' @description \code{countDNAPatternInAlt} Count the number of DNA pattern in the altered sequence of one transcript. The alter sequence is generated by find the fragment where Pos is in, replace the Ref with Alt in the fragments, and then concatenating all fragments together.
#' @param variant variant info
#' @param seqTable sequences table containing columns: ensembl_transcript_id and seq. seq is the sequence of a seqType region: e.g. 5' UTR sequence or 3' UTR sequence.
#'             The seqTable is output from function getSeqTable.
#' @param trascriptsTable transcripts structure table from ensembl with start and end columns for start and end coordinates of the target regions
#' @param dnaPattern the string pattern that you want to count, for example "ATG"
#' @param transcriptIdCol the ensemble transcript id column
#' @return number of DNA pattern
countDNAPatternInAltOneVariant <- function(variant, seqTable, transcriptsTable, dnaPattern, transcriptIdCol) {
  ensembl_transcript_id = variant[[transcriptIdCol]]
  pos = variant[["Pos"]]
  ref = variant[["Ref"]]
  alt = variant[["Alt"]]

  # skip genes that are not protein coding (those genes ensembl_transcript_id == NA)
  if (is.na(ensembl_transcript_id)) {
    return (NA)
  }

  transcrip_ids <- str_split(ensembl_transcript_id, ";")[[1]]
  alteredNum <- c()
  for (id in transcrip_ids) {
    sequence_altered <- getAltSequence(seqTable, transcriptsTable, id, pos, ref, alt)
    if (is.na(sequence_altered)) {
      warning("For variant ", paste0(variant, collapse = "_"), " Ref = ", ref, " at Pos ", pos, " doesn't match the transcript ", id)
    }
    alteredNum <- append(alteredNum, str_count(sequence_altered, dnaPattern))
  }
  ## use ";" to separate instead of ",", because CSV will add "," automatically for numbers bigger than 1000, which messed up the values
  if (length(alteredNum) && !all(is.na(alteredNum))) {
    return(str_c(str_replace_na(alteredNum), collapse = ";"))
  } else {
    return (NA)
  }
}



#' Check if the number of uAUG, ployA signal, or Kozak in UTR changes after alt
#' @keywords internal
#' @description \code{checkIfGainOrLoseAfterAlt} check if the number of uAUG, ployA signal, or Kozak in UTR changes after alt. This function only works to compare two "numeric" columns
#' @param variantsTable variantsTable: variants table
#' @param colNameBeforeAlt the name of the column before alter, type of a string
#' @param colNameAfterAlt the name of the column after alter, type of a string
#' @return A list containing whether the number gained, lost, or equal after alter. If colNameBeforeAlt or colNameAfterAlt does not exist in the variantsTable, return NA
checkIfGainOrLoseAfterAlt <- function(variantsTable, colNameBeforeAlt, colNameAfterAlt) {
  i <- NULL
  if (!colNameBeforeAlt %in% colnames(variantsTable) || all(is.na(variantsTable[[colNameBeforeAlt]])) || !colNameAfterAlt %in% colnames(variantsTable) || all(is.na(variantsTable[[colNameAfterAlt]]))) {
    return (NA)
  }
  gainOrLose <- foreach(i=1:nrow(variantsTable), .combine = c) %dopar% checkIfGainOrLoseAfterAltOneVariant(variantsTable[i,], colNameBeforeAlt, colNameAfterAlt)
  return (gainOrLose)
}


#' Check if the number of uAUG, ployA signal, or Kozak in UTR changes after alt for one variant
#' @keywords internal
#' @description \code{checkIfGainOrLoseAfterAlt} check if the number of uAUG, ployA signal, or Kozak in UTR changes after alt. This function only works to compare two "numeric" columns
#' @param variant variant info
#' @param colNameBeforeAlt the name of the column before alter, type of a string
#' @param colNameAfterAlt the name of the column after alter, type of a string
#' @return whether the number gained, lost, or equal after alter. If colNameBeforeAlt or colNameAfterAlt does not exist in the variantsTable, return NA
checkIfGainOrLoseAfterAltOneVariant <- function(variant, colNameBeforeAlt, colNameAfterAlt) {
  lt <- c()
  if (is.na(variant[[colNameBeforeAlt]]) | is.na(variant[[colNameAfterAlt]])) {
    return (NA)
  }
  beforeAlt <- str_split(variant[[colNameBeforeAlt]], ";")[[1]]
  afterAlt <- str_split(variant[[colNameAfterAlt]], ";")[[1]]
  for (i in 1:length(beforeAlt)) {
    if (as.double(beforeAlt[i]) < as.double(afterAlt[i])) {
      lt <- append(lt, "gained")
    } else if (as.double(beforeAlt[i]) > as.double(afterAlt[i])) {
      lt <- append(lt, "lost")
    } else {
      lt <- append(lt, "equal")
    }
  }
  lt_uniq <- unique(lt)
  if (length(lt_uniq) == 1) {
    return(lt_uniq[1])
  } else {
    return(str_c(lt, collapse = ";"))
  }

}


#' Check if the codon lost after alt.
#' @keywords internal
#' @param variantsTable variants table
#' @param colNameAfterAlt the name of the codon column after alter, type of a string. For example, name of column for the stop codon sequence after alt
#' @param patterns codon patterns, "ATG" for start codon, c("TAG", "TAA", "TGA") for stop codon
#' @return A list containing TURE or FALSE or NA (if the codon sequence is NA). If colNameAfterAlt does not exist in the variantsTable, return NA
checkIfLostCodonAfterAlt <- function(variantsTable, colNameAfterAlt, patterns) {
  i <- NULL
  if (!colNameAfterAlt %in% colnames(variantsTable)) {
    return (NA)
  }
  ifLose <- foreach(i=1:nrow(variantsTable), .combine = c) %dopar% checkIfLostCodonAfterAltOneVariant(variantsTable[i,], colNameAfterAlt, patterns)
  return (ifLose)
}



#' Check if the codon lost after alt for one variant.
#' @keywords internal
#' @param variant variant info
#' @param colNameAfterAlt the name of the codon column after alter, type of a string. For example, name of column for the stop codon sequence after alt
#' @param patterns codon patterns, "ATG" for start codon, c("TAG", "TAA", "TGA") for stop codon
#' @return A string concatenated TURE or FALSE or NA of all transcripts for that variant. If colNameAfterAlt is NA, return NA
checkIfLostCodonAfterAltOneVariant <- function(variant, colNameAfterAlt, patterns) {
  if (is.na(variant[[colNameAfterAlt]])) {
    return (NA)
  }
  codons <- str_split(variant[[colNameAfterAlt]], ";")[[1]]
  lt <- c()
  for (codon in codons) {
    if (!codon %in% patterns) {
      lt <- append(lt, TRUE)
    } else {
      lt <- append(lt, FALSE)
    }
  }
  # if all transcripts of this gene have the same result, either all TRUE or all FALSE, only keep one
  if (length(unique(lt)) == 1) {
    lt <- unique(lt)
  } else {
    lt <- str_c(lt, collapse = ";")
  }

  return (lt)
}



#' Get the codon sequence for transcripts in transcriptIdColumn.
#' @keywords internal
#' @description \code{getCodon} Get the codon sequence for transcripts in transcriptIdColumn.
#' @param variantsTable variants table
#' @param seqTable cooresponding sequence table from Ensembl DB (either 5' UTR sequence or 3' UTR sequence). Have two columns: ensembl_transcript_id and seq
#' @param transcriptIdColumn the name of the transcript id column for the codon region
#' @param codonRegion "startCodon" or "stopCodon"
#' @return A list containing codon sequences (NA if the transcript id in that row is NA). If transcriptIdColumn does not exist in the variantsTable, return NA
getCodon <- function(variantsTable, seqTable, transcriptIdColumn, codonRegion) {
  i <- NULL
  if (!transcriptIdColumn %in% colnames(variantsTable)) {
    return (NA_character_)
  }

  if (codonRegion != "startCodon" & codonRegion != "stopCodon") {
    stop("The codonRegion should be startCodon or stopCodon")
  }
  codon_seqs <- foreach(i=1:nrow(variantsTable), .combine = c) %dopar% getCodonOneVariant(variantsTable[i,], seqTable, transcriptIdColumn, codonRegion)
  return (codon_seqs)
}


#' Get the codon sequence of transcripts in transcriptIdColumn for one variant.
#' @keywords internal
#' @description \code{getCodonOneVariant} Get the codon sequence of transcripts in transcriptIdColumn for one variant.
#' @param variant variant info
#' @param seqTable cooresponding sequence table from Ensembl DB (either 5' UTR sequence or 3' UTR sequence). Have two columns: ensembl_transcript_id and seq
#' @param transcriptIdColumn the name of the transcript id column for the codon region
#' @param codonRegion "startCodon" or "stopCodon"
#' @return A string concatenated codon sequences (NA if the transcript id in that row is NA) for one variant.
getCodonOneVariant <- function(variant, seqTable, transcriptIdColumn, codonRegion) {
  ensembl_transcript_id <- NULL
  # skip rows that are not protein coding (those genes ensembl_transcript_id == NA)
  if (is.na(variant[[transcriptIdColumn]])) {
    return (NA_character_)
  }
  transcrip_ids <- str_split(variant[[transcriptIdColumn]], ";")[[1]]
  codons <- c()
  for (id in transcrip_ids) {
    sequence <- seqTable[ensembl_transcript_id == id, seq]
    if (codonRegion == "stopCodon") {
      codons <- append(codons, substr(sequence, nchar(sequence)-2, nchar(sequence)))
    } else {
      codons <- append(codons, substr(sequence, 1, 3))
    }
  }
  if (length(codons)) {
    return (str_c(codons, collapse = ";"))
  } else {
    return (NA_character_)
  }

}



#' Get the altered codon sequence for transcripts in transcriptIdColumn.
#' @keywords internal
#' @description Get the altered codon sequence for transcripts in transcriptIdColumn.
#' @param variantsTable variants table
#' @param trascriptsTable transcripts structure table from ensembl with start and end columns for start and end coordinates of the target regions
#' @param transcriptIdColumn the name of the transcript id column for the codon region
#' @param codonPositionsColumnn the codon coordinates column output from getTranscriptIdsForCodonVariants
#' @param codonSeqColumn the codon sequence column output from getCodon
#' @return A list containing codon sequences after alt (NA if the transcript id in that row is NA). If transcriptIdColumn, codonPositionsColumn or codonSeqColumn does not exist in the variantsTable, return NA
getCodonInAlt <- function(variantsTable, transcriptsTable, transcriptIdColumn, codonPositionsColumn, codonSeqColumn) {
  i <- NULL
  if (!transcriptIdColumn %in% colnames(variantsTable) || all(is.na(variantsTable[[transcriptIdColumn]])) ||
      !codonPositionsColumn %in% colnames(variantsTable) || all(is.na(variantsTable[[codonPositionsColumn]])) ||
      !codonSeqColumn %in% colnames(variantsTable) || all(is.na(variantsTable[[codonSeqColumn]]))) {
    return (NA_character_)
  }
  codon_seqs_altered <- foreach(i=1:nrow(variantsTable), .combine = c) %dopar% getCodonInAltOneVariant(variantsTable[i,], transcriptsTable, transcriptIdColumn, codonPositionsColumn, codonSeqColumn)
  return (codon_seqs_altered)
}



#' Get the altered codon sequence for transcripts in transcriptIdColumn for one variant.
#' @keywords internal
#' @description Get the altered codon sequence for transcripts in transcriptIdColumn for one variant.
#' @param variant variant info
#' @param trascriptsTable transcripts structure table from ensembl with start and end columns for start and end coordinates of the target regions
#' @param transcriptIdColumn the name of the transcript id column for the codon region
#' @param codonPositionsColumnn the codon coordinates column output from getTranscriptIdsForCodonVariants
#' @param codonSeqColumn the codon sequence column output from getCodon
#' @return A string concatenated codon sequences after alt (NA if the transcript id in that row is NA).
getCodonInAltOneVariant <- function(variant, transcriptsTable, transcriptIdColumn, codonPositionsColumn, codonSeqColumn) {
  ensembl_transcript_id = variant[[transcriptIdColumn]]
  codon_positions = variant[[codonPositionsColumn]]
  codon_seqs = variant[[codonSeqColumn]]
  pos = variant[["Pos"]]
  ref = variant[["Ref"]]
  alt = variant[["Alt"]]

  # skip rows that mutation doesn't change the codon sequence or Kozak sequence is NA
  if (is.na(ensembl_transcript_id) || is.na(codon_seqs)) {
    return (NA_character_)
  }
  transcriptIds <- str_split(ensembl_transcript_id, ";")[[1]]
  codonSeqs <- str_split(codon_seqs, ";")[[1]]
  codonPositions <- str_split(codon_positions, ";")[[1]]
  seqTable <- data.table(ensembl_transcript_id = transcriptIds, seq = codonSeqs)
  condonSeqsInAlt <- c()
  for (idx in 1:length(transcriptIds)) {
    id <- transcriptIds[idx]
    codonPos <- str_split(codonPositions[idx], "\\|")[[1]]
    ## sort positions in asc order, since in checkIfRefMatchAndAltFragment fragment_start is always less than fragment_end
    strand <- transcriptsTable[ensembl_transcript_id == id, strand][1]

    codonTable <- getRegionsForDiscretePos(codonPos)
    codonTable$ensembl_transcript_id <- id
    codonTable$strand <- strand
    codonSeqInAlt <- getAltSequence(seqTable, codonTable, id, pos, ref, alt)
    if (is.na(codonSeqInAlt)) {
      warning("For variant ", paste0(variant, collapse = "_"), " Ref = ", ref, " at Pos ", pos, " doesn't match the transcript ", id)
    }
    condonSeqsInAlt <- append(condonSeqsInAlt, codonSeqInAlt)
  }
  if (length(condonSeqsInAlt)) {
    return (str_c(str_replace_na(condonSeqsInAlt), collapse = ";"))
  } else {
    return (NA_character_)
  }

}

#' Get codon region table used for getAltSequence
#' @keywords internal
#' @param positions A vector of positions
#' @return A data table of regions with start and end columns showing the input positions
#' @importFrom dplyr bind_rows
getRegionsForDiscretePos <- function(positions) {
  positions <- sort(as.numeric(positions))
  regions <- data.table()
  start <- positions[1]
  end <- positions[1]
  for (i in 2:length(positions)) {
    # discrete
    if (positions[i] != positions[i-1]+1) {
      regions <- bind_rows(regions, data.frame(start = start, end = end))
      start = positions[i]
      end = positions[i]
    } else {
      end = positions[i]
    }
  }
  regions <- bind_rows(regions, data.frame(start = start, end = end))
  return (regions)
}




#' Get the Kozak sequence in TSS region for transcripts in transcriptIdColumn.
#' @keywords internal
#' @description Get the Kozak sequence in TSS region for transcripts in transcriptIdColumn.
#' @param variantsTable variants table
#' @param utr5SeqTable 5' UTR sequence table from Ensembl DB. Have two columns: ensembl_transcript_id and seq
#' @param codingSeqTable coding sequence table from Ensembl DB. Have two columns: ensembl_transcript_id and seq
#' @param transcriptIdColumn the name of the transcript id column for the codon region
#' @return A list containing TSS Kozak sequences (NA if the transcript id in that row is NA). If transcriptIdColumn does not exist in the variantsTable, return NA
getTSSKozak <- function(variantsTable, utr5SeqTable, codingSeqTable, transcriptIdColumn) {
  i <- NULL
  if (!transcriptIdColumn %in% colnames(variantsTable)) {
    return (NA_character_)
  }
  tss_kozak_seqs <- foreach(i=1:nrow(variantsTable), .combine = c) %dopar% getTSSKozakOneVariant(variantsTable[i,], utr5SeqTable, codingSeqTable, transcriptIdColumn)
  return (tss_kozak_seqs)

}



#' Get the Kozak sequence in TSS region for transcripts in transcriptIdColumn for one variant.
#' @keywords internal
#' @description Get the Kozak sequence in TSS region for transcripts in transcriptIdColumn for one variant.
#' @param variantsTable variants table
#' @param utr5SeqTable 5' UTR sequence table from Ensembl DB. Have two columns: ensembl_transcript_id and seq
#' @param codingSeqTable coding sequence table from Ensembl DB. Have two columns: ensembl_transcript_id and seq
#' @param transcriptIdColumn the name of the transcript id column for the codon region
#' @return A string concatenated TSS Kozak sequences (NA if the transcript id in that row is NA)
getTSSKozakOneVariant <- function(variant, utr5SeqTable, codingSeqTable, transcriptIdColumn) {
  ensembl_transcript_id <- NULL
  ensembl_transcript_ids = variant[[transcriptIdColumn]]
  # skip rows that are not protein coding (those genes ensembl_transcript_id == NA)
  if (is.na(ensembl_transcript_ids)) {
    return (NA_character_)
  }
  transcrip_ids <- str_split(ensembl_transcript_ids, ";")[[1]]
  kozakSeqList <- c()
  for (id in transcrip_ids) {
    utr5Seq <- utr5SeqTable[ensembl_transcript_id == id, seq]
    if (utr5Seq == "Sequence unavailable") {
      kozakSeqList <- append(kozakSeqList, NA_character_)
      next
    }
    codingSeq <- codingSeqTable[ensembl_transcript_id == id, seq]
    kozakSeq <- str_c(substr(utr5Seq, nchar(utr5Seq)-2, nchar(utr5Seq)), substr(codingSeq, 1, 5))
    kozakSeqList <- append(kozakSeqList, kozakSeq)
  }
  if (length(kozakSeqList) && !all(is.na(kozakSeqList))) {
    return (str_c(str_replace_na(kozakSeqList), collapse = ";"))
  } else {
    return (NA_character_)
  }

}


#' Get Kozak PWM
#' @keywords internal
#' @param kozakStrenFile Customed Kozak strength file
#' @return Kozak PWM
#' @importFrom Biostrings PWM consensusMatrix
getKozakPWM <- function(kozakStrenFile=NULL) {
  out <- NULL
  ## Get Kozak PWM
  if (!is.null(kozakStrenFile)) {
    ## if user specify an alternative strength file, use it to get Kozak sequences instead
    uAUG_repressive_strength_external <- fread(kozakStrenFile, header = T, sep = ",", data.table = T)
    kozakSeqs <- uAUG_repressive_strength_external[order(out)]
  } else {
    ## if user doesn't specify kozak strength file, use default uAUG_repressive_strength in sysdata
    kozakSeqs <- uAUG_repressive_strength[order(out)]
  }
  kozakSeqsTop20 <- kozakSeqs[1:20, gsub('U', 'T', seq)]
  pfm <- consensusMatrix(kozakSeqsTop20)
  kozakPWM <- PWM(pfm, type = "log2probratio")
  return (kozakPWM)
}



#' Calculate the Kozak score for each Kozak sequence, and concatenate the numbers with ",".
#' @keywords internal
#' @description Calculate the Kozak score for each Kozak sequence, and concatenate the numbers with ",".
#' @param variantsTable variants table
#' @param kozakSeqs the column name of the kozak sequences
#' @param kozakPWM Kozak PWM output from consensusMatrix() and PWM()
#' @return a list containing scores for the kozak sequences. Can be used to add new column to the variants table.
getKozakScore <- function(variantsTable, kozakSeqs, kozakPWM) {
  i <- NULL
  if (!kozakSeqs %in% colnames(variantsTable) || all(is.na(variantsTable[[kozakSeqs]]))) {
    return (NA)
  }
  kozak_scores <- foreach(i=1:nrow(variantsTable), .combine = c) %dopar% getKozakScoreOneVariant(variantsTable[i,], kozakSeqs, kozakPWM)
  return (kozak_scores)
}


#' Calculate the Kozak score for each Kozak sequence, and concatenate the numbers with ";" for one variant
#' @keywords internal
#' @description Calculate the Kozak score for each Kozak sequence, and concatenate the numbers with "," for one variant
#' @param variant variant info
#' @param kozakSeqs the column name of the kozak sequences
#' @param kozakPWM Kozak PWM output from consensusMatrix() and PWM()
#' @return a string concatenated scores for the kozak sequences.
#' @importFrom Biostrings PWMscoreStartingAt
getKozakScoreOneVariant <- function(variant, kozakSeqs, kozakPWM) {
  kozak_seqs = as.character(variant[[kozakSeqs]])

  # skip rows where Kozak sequence is NA
  if (is.na(kozak_seqs)) {
    return (NA)
  }
  kozak <- str_split(kozak_seqs, ";")[[1]]
  scores <- c()

  for (s in kozak) {
    # if the kozak sequence is less than 8 nt, set its score to 0
    if (nchar(s) < 8) {
      score <- 0
    } else {
      score <- PWMscoreStartingAt(kozakPWM, s)
    }
    scores <- append(scores, score)
  }
  ## use ";" to separate instead of ",", because CSV will add "," automatically for numbers bigger than 1000, which messed up the values
  if (length(scores)) {
    return (str_c(scores, collapse = ";"))
  } else {
    return (NA)
  }
}

