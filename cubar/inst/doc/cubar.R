## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
suppressPackageStartupMessages(library(Biostrings))
library(cubar)
library(ggplot2)

## -----------------------------------------------------------------------------
# example data
yeast_cds

# qc
yeast_cds_qc <- check_cds(yeast_cds)
yeast_cds_qc

## -----------------------------------------------------------------------------
# convert a CDS to codon sequence
seq_to_codons(yeast_cds_qc[['YDR320W-B']])

# convert a CDS to amino acid sequence
Biostrings::translate(yeast_cds_qc[['YDR320W-B']])

## -----------------------------------------------------------------------------
# get codon frequency
yeast_cf <- count_codons(yeast_cds_qc)

## -----------------------------------------------------------------------------
# get codon table for the standard genetic code
ctab <- get_codon_table(gcid = '1')

# plot possible codon and anticodon pairings
plot_ca_pairing(ctab)

## -----------------------------------------------------------------------------
# get enc
enc <- get_enc(yeast_cf)
head(enc)

plot_dist <- function(x, xlab = 'values'){
    x <- stack(x)
    ggplot(x, aes(x = values)) +
        geom_histogram() +
        labs(x = xlab, y = 'Number of genes')
}

plot_dist(enc, 'ENC')

## -----------------------------------------------------------------------------
# get fop
fop <- get_fop(yeast_cds)
plot_dist(fop, 'Fop')

## -----------------------------------------------------------------------------
optimal_codons <- est_optimal_codons(yeast_cds_qc, codon_table = ctab)
head(optimal_codons[optimal_codons$coef < 0 & optimal_codons$qvalue < 0.01, ])

## -----------------------------------------------------------------------------
# estimate RSCU of highly expressed genes
yeast_heg <- head(yeast_exp[order(-yeast_exp$fpkm), ], n = 500)
yeast_heg <- yeast_heg[yeast_heg$gene_id %in% rownames(yeast_cf), ]
rscu_heg <- est_rscu(yeast_cf[yeast_heg$gene_id, ], codon_table = ctab)

# calculate CAI of all genes
# note: CAI values are usually calculated based RSCU of highly expressed genes.
cai <- get_cai(yeast_cf, rscu = rscu_heg)
plot_dist(cai, xlab = 'CAI')

## -----------------------------------------------------------------------------
# get tRNA gene copy number from GtRNADB
path_gtrnadb <- 'http://gtrnadb.ucsc.edu/genomes/eukaryota/Scere3/sacCer3-mature-tRNAs.fa'
yeast_trna <- Biostrings::readRNAStringSet(path_gtrnadb)
trna_gcn <- table(data.table::tstrsplit(sub(' .*', '', names(yeast_trna)), '-')[[3]])
trna_gcn <- trna_gcn[names(trna_gcn) != 'NNN'] # copy of each anticodon

# calculate tRNA weight for each codon
trna_w <- est_trna_weight(trna_level = trna_gcn, codon_table = ctab)

# get tAI
tai <- get_tai(yeast_cf, trna_w = trna_w)
plot_dist(tai, 'tAI')

