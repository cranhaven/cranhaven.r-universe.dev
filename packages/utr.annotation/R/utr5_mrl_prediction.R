# Run CNN model with Keras in R
# ref: https://blog.rstudio.com/2017/09/05/keras-for-r/

#' Get the 100nt 5' UTR sequence upstream of start codon for transcripts in transcriptIdColumn and predict their MRL
#' @keywords internal
#' @description \code{predictMRL} Get the 100nt 5' UTR sequence upstream of start codon for transcripts in transcriptIdColumn and predict their MRL
#' @param variantsTable variants table
#' @param seqTable corresponding 5' UTR sequence table from Ensembl DB. Have two columns: ensembl_transcript_id and seq
#' @param transcriptIdColumn the name of the transcript id column
#' @return A list predicted MRL (NA if the transcript id in that row is NA). If transcriptIdColumn does not exist in the variantsTable, return NA
#' @importFrom keras load_model_hdf5
predictMRL <- function(variantsTable, seqTable, transcriptIdColumn) {
  if (!transcriptIdColumn %in% colnames(variantsTable) || all(is.na(variantsTable[[transcriptIdColumn]]))) {
    return (NA)
  }
  # clean up the session before loading the model
  ##k_clear_session()
  model <- load_model_hdf5(system.file("extdata", "Varying_length_25to100_model.hdf5", package = "MRL.dl.model", mustWork = TRUE))
  # utr5_mrl <- foreach(i=1:nrow(variantsTable), .combine = c) %dopar% predictMRLOneVariant(variantsTable[i,], seqTable, transcriptIdColumn, model)
  utr5_mrl <- c()
  for (i in 1:nrow(variantsTable)) {
    utr5_mrl <- append(utr5_mrl, predictMRLOneVariant(variantsTable[i, ], seqTable, transcriptIdColumn, model))
  }
  return (utr5_mrl)
}

#' Predict MRL for one variant
#' @keywords internal
#' @param variant A variant
#' @param seqTable corresponding 5' UTR sequence table from Ensembl DB. Have two columns: ensembl_transcript_id and seq
#' @param transcriptIdColumn the name of the transcript id column
#' @param model MRL prediction model
#' @return MRL prediction of reference 5' UTR for the input variant
#' @import stringr
#' @importFrom stats predict
predictMRLOneVariant <- function(variant, seqTable, transcriptIdColumn, model) {
  ensembl_transcript_id <- NULL
  if (is.na(variant[[transcriptIdColumn]])) {
    return (NA)
  }
  transcrip_ids <- str_split(variant[[transcriptIdColumn]], ";")[[1]]
  utr5_mrl <- c()
  for (id in transcrip_ids) {
    sequence <- seqTable[ensembl_transcript_id == id, seq]
    if (length(sequence) == 0 || sequence == "Sequence unavailable") {
      utr5_mrl <- append(utr5_mrl, NA)
      next
    }
    # if 5' UTR is less than 100nt, pad it with N
    if (nchar(sequence) < 100) {
      sequence <- str_pad(sequence, width = 100, side = "left", pad = "N")
    }
    # pick the upstream 100nt
    sequence <- str_sub(sequence, -100)
    # encode the sequence
    sequence_encoded <- one_hot_encode(sequence)
    # predict MRL
    mrl <- inverse_transform(predict(model, sequence_encoded))
    utr5_mrl <- append(utr5_mrl, mrl)
  }
  if (!all(is.na(utr5_mrl))) {
    return (str_c(str_replace_na(utr5_mrl), collapse = ";"))
  } else {
    return (NA)
  }

}

#' Get the mutated 100nt 5' UTR sequence upstream of start codon for transcripts in transcriptIdColumn and predict their MRL
#' @keywords internal
#' @description \code{predictMRL} Get the 100nt 5' UTR sequence upstream of start codon for transcripts in transcriptIdColumn and predict their MRL
#' @param variantsTable variants table
#' @param seqTable corresponding 5' UTR sequence table from Ensembl DB. Have two columns: ensembl_transcript_id and seq
#' @param transcriptsTable transcripts structure table from ensembl with start and end columns for start and end coordinates of the 5' UTR
#' @param transcriptIdColumn the name of the transcript id column
#' @return A list predicted MRL (NA if the transcript id in that row is NA). If transcriptIdColumn does not exist in the variantsTable, return NA
#' @importFrom keras load_model_hdf5
predictMRLInAlt <- function(variantsTable, seqTable, transcriptsTable, transcriptIdColumn) {
  if (!transcriptIdColumn %in% colnames(variantsTable) || all(is.na(variantsTable[[transcriptIdColumn]]))) {
    return (NA)
  }
  ##k_clear_session()
  # clean up the session before loading the model
  model <- load_model_hdf5(system.file("extdata", "Varying_length_25to100_model.hdf5", package = "MRL.dl.model", mustWork = TRUE))
  # seqs_altered_mrl <- foreach(i=1:nrow(variantsTable), .combine = c) %dopar% predictMRLInAltOneVariant(variantsTable[i,], seqTable, transcriptsTable, transcriptIdColumn, model)
  seqs_altered_mrl <- c()
  for (i in 1:nrow(variantsTable)) {
    seqs_altered_mrl <- append(seqs_altered_mrl, predictMRLInAltOneVariant(variantsTable[i, ], seqTable, transcriptsTable, transcriptIdColumn, model))
  }
  return (seqs_altered_mrl)
}

#' Predict altered MRL for one variant
#' @keywords internal
#' @param variant A variant
#' @param seqTable corresponding 5' UTR sequence table from Ensembl DB. Have two columns: ensembl_transcript_id and seq
#' @param transcriptsTable transcripts structure table from ensembl with start and end columns for start and end coordinates of the 5' UTR
#' @param transcriptIdColumn the name of the transcript id column
#' @param model MRL prediction model
#' @return MRL prediction of altered 5' UTR for the input variant
#' @import stringr
#' @importFrom stats predict
predictMRLInAltOneVariant <- function(variant, seqTable, transcriptsTable, transcriptIdColumn, model) {
  ensembl_transcript_id = variant[[transcriptIdColumn]]
  pos = variant[["Pos"]]
  ref = variant[["Ref"]]
  alt = variant[["Alt"]]

  # skip rows that mutation doesn't change the codon sequence
  if (is.na(ensembl_transcript_id)) {
    return (NA)
  }
  transcrip_ids <- str_split(ensembl_transcript_id, ";")[[1]]
  utr5_mrl <- c()
  for (id in transcrip_ids) {
    sequence <- seqTable[ensembl_transcript_id == id, seq]
    if (length(sequence) == 0 || sequence == "Sequence unavailable") {
      utr5_mrl <- append(utr5_mrl, NA)
      next
    }
    sequence_altered <- getAltSequence(seqTable, transcriptsTable, id, pos, ref, alt)
    if (is.na(sequence_altered)) {
      utr5_mrl <- append(utr5_mrl, NA)
      next
    }
    if (!is.na(sequence_altered) && nchar(sequence_altered) < 100) {
      sequence_altered <- str_pad(sequence_altered, width = 100, side = "left", pad = "N")
    }
    # pick the upstream 100nt
    sequence_altered <- str_sub(sequence_altered, -100)
    # encode the sequence
    sequence_encoded <- one_hot_encode(sequence_altered)
    # predict MRL
    mrl <- inverse_transform(predict(model, sequence_encoded))
    utr5_mrl <- append(utr5_mrl, mrl)
  }

  if (!all(is.na(utr5_mrl))) {
    return (str_c(str_replace_na(utr5_mrl), collapse = ";"))
  } else {
    return (NA)
  }

}


#' Apply one hot encode to the 100nt 5' UTR sequence
#' @keywords internal
#' @description \code{one_hot_encode} will encode 5' UTR sequence of 100nt
#' @param utr5_seq 100nt DNA sequence
#' @return an array with dimension as c(1, 100, 4)
#' @import stringr
one_hot_encode <- function(utr5_seq) {
  nuc_d <- list('a'=c(1,0,0,0),'c'=c(0,1,0,0),'g'=c(0,0,1,0),'t'=c(0,0,0,1), 'n'=c(0,0,0,0))
  if (is.na(utr5_seq) || nchar(utr5_seq) != 100) {
    return (NA)
  }
  utr5_seq <- tolower(utr5_seq)
  utr5_seq_arr <- str_split(utr5_seq, "")[[1]]
  encoded <- matrix(numeric(), ncol = 4)
  for (c in utr5_seq_arr) {
    encoded <- rbind(encoded, nuc_d[[c]])
  }

  return (array(encoded, dim = c(1, 100, 4)))
}


#' Reverse the scaling of the predicted MRL
#' @keywords internal
#' @param mrl MRL prediction
#' @return inverse transformed MRL prediction
inverse_transform <- function(mrl) {
  return(mrl*stdVal + meanVal)
}

