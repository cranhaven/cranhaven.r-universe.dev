## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
suppressPackageStartupMessages(library(Biostrings))
library(cubar)

## -----------------------------------------------------------------------------
human_mt

ctab <- get_codon_table(gcid = '2')
head(ctab)

## -----------------------------------------------------------------------------
human_mt_qc <- check_cds(
    human_mt,
    codon_table = ctab,
    check_stop = FALSE,
    rm_stop = FALSE,
    check_len = FALSE,
    start_codons = c('ATG', 'ATA', 'ATT'))

human_mt_qc

## -----------------------------------------------------------------------------
len_trim <- width(human_mt_qc) %% 3
len_trim <- ifelse(len_trim == 0, 3, len_trim)
human_mt_qc <- subseq(human_mt_qc, start = 1, end = width(human_mt_qc) - len_trim)

human_mt_qc

## -----------------------------------------------------------------------------
# calculate codon frequency
mt_cf <- count_codons(human_mt_qc)

# calculate ENC
get_enc(mt_cf, codon_table = ctab)

