## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----eval = FALSE-------------------------------------------------------------
#  # Install from GitHub
#  install.packages("devtools")
#  devtools::install_github("tacazares/SeedMatchR")

## ----include = FALSE----------------------------------------------------------
#  # Import library
#  library(SeedMatchR)
#  library(msa)
#  library(GenomicFeatures)

## -----------------------------------------------------------------------------
#  # siRNA sequence of interest targeting a 23 bp region of the Ttr gene
#  guide.seq = "UUAUAGAGCAAGAACACUGUUUU"

## ----echo = T, results = 'hide', message=FALSE, warning=FALSE, error=FALSE----
#  # Load the species specific annotation database object
#  anno.db <- load_species_anno_db("rat")

## -----------------------------------------------------------------------------
#  features = get_feature_seqs(anno.db$tx.db, anno.db$dna, feature.type = "3UTR")

## ----echo = T, results = 'hide', message=FALSE, warning=FALSE, error=FALSE----
#  get_example_data("sirna")

## -----------------------------------------------------------------------------
#  sirna.data = load_example_data("sirna")

## -----------------------------------------------------------------------------
#  res <- sirna.data$Schlegel_2022_Ttr_D1_30mkg

## -----------------------------------------------------------------------------
#  # Dimensions before filtering
#  
#  dim(res) # [1] 32883    6
#  
#  # Filter DESeq2 results for SeedMatchR
#  res = filter_deseq(res, fdr.cutoff=1, fc.cutoff=0, rm.na.log2fc = TRUE)
#  
#  # Dimensions after filtering
#  dim(res) # [1] 13582     8

## ----fig.height=5, fig.width=10, out.retina=1---------------------------------
#  # Plot the seed sequence options for the siRNA of interest
#  avail.seed.plot = plot_seeds(guide.seq)
#  
#  avail.seed.plot

## -----------------------------------------------------------------------------
#  # Get the seed sequence information for the seed of interest
#  seed = get_seed(guide.seq, "mer7m8")
#  
#  seed

## -----------------------------------------------------------------------------
#  res = SeedMatchR(res,
#                   anno.db$gtf,
#                   features$seqs,
#                   guide.seq)
#  
#  head(res)

## -----------------------------------------------------------------------------
#  for (seed in c("mer8", "mer6", "mer7A1")){
#  res <- SeedMatchR(res,
#                    anno.db$gtf,
#                    features$seqs,
#                    guide.seq,
#                    seed.name = seed)
#  }
#  
#  head(res)

## -----------------------------------------------------------------------------
#  for (indel.bool in c(TRUE, FALSE)){
#    for (mm in c(0,1,2)){
#      for (seed in c("mer7m8", "mer8", "mer6", "mer7A1")){
#        res <- SeedMatchR(res,
#                          anno.db$gtf,
#                          features$seqs,
#                          guide.seq,
#                          seed.name = seed,
#                          col.name = paste0(seed, ".", "mm", mm, "_indel", indel.bool),
#                          mismatches = mm,
#                          indels = indel.bool)
#      }
#    }
#  }
#  
#  head(res)

## ----fig.height=5, fig.width=10, out.retina=1---------------------------------
#  # Gene set 1
#  mer7m8.list = res$gene_id[res$mer7m8.mm0_indelFALSE >= 1 & res$mer8.mm0_indelFALSE ==0]
#  
#  # Gene set 2
#  mer8.list = res$gene_id[res$mer8.mm0_indelFALSE >= 1]
#  
#  background.list = res$gene_id[res$mer7m8.mm0_indelFALSE == 0 & res$mer8.mm0_indelFALSE == 0]
#  
#  ecdf.results = deseq_fc_ecdf(res,
#                               list("Background" = background.list, "mer8" = mer8.list, "mer7m8" = mer7m8.list),
#                               stats.test = "KS",
#                               factor.order = c("Background", "mer8", "mer7m8"),
#                               null.name = "Background",
#                               target.name = "mer8",
#                               alternative = "greater")
#  
#  ecdf.results$plot

## ----fig.height=5, fig.width=10, out.retina=1---------------------------------
#  # Group transcripts by gene
#  sequences <- transcriptsBy(anno.db$tx.db, by="gene")
#  
#  # Extract promoter sequences from tx.db object
#  prom.seq = getPromoterSeq(sequences,
#                 anno.db$dna,
#                   upstream=2000,
#                   downstream=100)
#  
#  # perform a seed search of the promoter sequences. Set tx.id.col to F to use gene annotations
#  res = SeedMatchR(res, anno.db$gtf, prom.seq@unlistData, guide.seq, tx.id.col = FALSE, col.name = "promoter.mer7m8")
#  
#  # Find the genes with matches
#  promoterWseed = res$gene_id[res$promoter.mer7m8 >= 1]
#  
#  # Generate the background list of genes
#  background.list = res$gene_id[!(res$gene_id %in% promoterWseed)]
#  
#  # Plot ecdf results for promoter matches with stats testing
#  ecdf.results = deseq_fc_ecdf(res,
#                               title = "Ttr D1 30mkg",
#                               list("Background" = background.list,
#                                    "Promoter w/ mer7m8" = promoterWseed),
#                               stats.test = "KS",
#                               factor.order = c("Background",
#                                                "Promoter w/ mer7m8"),
#                               null.name = "Background",
#                               target.name = "Promoter w/ mer7m8",
#                               alternative = "less",
#                               palette = c("black", "#d35400"))
#  
#  ecdf.results$plot
#  

## -----------------------------------------------------------------------------
#  sessionInfo()

