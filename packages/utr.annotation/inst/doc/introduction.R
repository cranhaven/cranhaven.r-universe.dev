## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  tidy.opts = list(width.cutoff = 60),
  tidy = TRUE,
  message = FALSE,
  warning = FALSE,
  fig.width = 6
)

## ----install_dependency, eval=FALSE-------------------------------------------
#  cran_pkgs <- c("parallel", "doParallel", "data.table", "readr", "stringr", "vcfR", "dplyr", "tidyr", "keras", "devtools", "reticulate")
#  bioc_pkgs <- c("biomaRt", "Biostrings", "AnnotationHub", "ensembldb")
#  
#  for (pkg in cran_pkgs) {
#    if (!(pkg %in% installed.packages())) {
#      install.packages(pkg)
#    }
#  }
#  
#  if (!requireNamespace("BiocManager", quietly = TRUE)) {
#        install.packages("BiocManager")
#  }
#  
#  for (pkg in bioc_pkgs) {
#    if (!(pkg %in% installed.packages())) {
#      BiocManager::install(pkg)
#    }
#  }

## ---- eval=F------------------------------------------------------------------
#  library(keras)
#  # Install tensorflow backend
#  reticulate::install_miniconda()
#  keras::install_keras(version = "2.2.4", tensorflow = "1.14.0", method = "conda")
#  # install deep learning model data package
#  devtools::install_bitbucket("jdlabteam/mrl.dl.model")

## ----install, message=FALSE, eval=FALSE---------------------------------------
#  # Install the release version from CRAN
#  install.packages("utr.annotation")
#  # Or install the latest version from Bitbucket
#  devtools::install_bitbucket("jdlabteam/utr.annotation")

## ----setup--------------------------------------------------------------------
library(utr.annotation)

## ----example------------------------------------------------------------------
# load sample variants
require(readr)
variants <- read_csv(system.file("extdata", "variants_sample.csv", package = "utr.annotation"))
# There're 13 variants in total
print(nrow(variants))
# The file has four required columns
print(variants)

## ----run_annotation, eval = FALSE---------------------------------------------
#  variants_sample <- system.file("extdata", "variants_sample.csv", package = "utr.annotation")
#  # sample conservation bigWig files only include conservation scores at the regions of variants in variants_sample.csv
#  Conservation_scores <- system.file("extdata", "Conservation_scores", package = "utr.annotation")
#  
#  runUTRAnnotation(variantFile = variants_sample, annotationResult = "annotated_variants_sample.csv", species = "human", ensemblVersion = 93, dataDir = "test_db", conservationBwFiles = Conservation_scores, mrl_prediction = TRUE)
#  

## -----------------------------------------------------------------------------
results <- read_csv(system.file("extdata", "annotated_variants_sample.csv", package = "utr.annotation"))
print(colnames(results))

## -----------------------------------------------------------------------------
print(results[, c("Chr", "Pos", "Ref", "Alt", "utr5_transcript_id")])

## -----------------------------------------------------------------------------
print(results[, c("Chr", "Pos", "Ref", "Alt", "lost_start_codon")])

## -----------------------------------------------------------------------------
print(results[, c("Chr", "Pos", "Ref", "Alt", "startCodon_transcript_id")])

## -----------------------------------------------------------------------------
print(results[, c("Chr", "Pos", "Ref", "Alt", "start_codon", "start_codon_altered")])

## -----------------------------------------------------------------------------
print(results[, c("Chr", "Pos", "Ref", "Alt", "utr_num_uAUG_gainedOrLost")])

## -----------------------------------------------------------------------------
print(results[, c("Chr", "Pos", "Ref", "Alt", "num_uAUG", "num_uAUG_altered")])

## -----------------------------------------------------------------------------
print(results[, c("Chr", "Pos", "Ref", "Alt", "utr_num_kozak_gainedOrLost")])

## -----------------------------------------------------------------------------
print(results[, c("Chr", "Pos", "Ref", "Alt", "num_kozak", "num_kozak_altered")])

## -----------------------------------------------------------------------------
print(results[, c("Chr", "Pos", "Ref", "Alt", "mrl_gainedOrLost")])

## -----------------------------------------------------------------------------
print(results[, c("Chr", "Pos", "Ref", "Alt", "mrl", "mrl_altered")])

## -----------------------------------------------------------------------------
print(results[, c("Chr", "Pos", "Ref", "Alt", "utr3_transcript_id")])

## -----------------------------------------------------------------------------
print(results[, c("Chr", "Pos", "Ref", "Alt", "stopCodon_transcript_id")])

## -----------------------------------------------------------------------------
print(results[, c("Chr", "Pos", "Ref", "Alt", "lost_stop_codon")])

## -----------------------------------------------------------------------------
print(results[, c("Chr", "Pos", "Ref", "Alt", "stop_codon", "stop_codon_altered")])

## -----------------------------------------------------------------------------
print(results[, c("Chr", "Pos", "Ref", "Alt", "num_polyA_signal_gainedOrLost")])

## -----------------------------------------------------------------------------
print(results[, c("Chr", "Pos", "Ref", "Alt", "num_polyA_signal", "num_polyA_signal_altered")])

## -----------------------------------------------------------------------------
print(results[, c("Chr", "Pos", "Ref", "Alt", "kozak_transcript_id")])

## -----------------------------------------------------------------------------
print(results[, c("Chr", "Pos", "Ref", "Alt", "kozak", "kozak_altered")])

## -----------------------------------------------------------------------------
print(results[, c("Chr", "Pos", "Ref", "Alt", "tss_kozak_score_gainedOrLost")])

## -----------------------------------------------------------------------------
print(results[, c("Chr", "Pos", "Ref", "Alt", "kozak_score", "kozak_altered_score")])

## -----------------------------------------------------------------------------
print(results[, c("Chr", "Pos", "Ref", "Alt", "hg38.phastCons100way.bw", "hg38.phyloP100way.bw")])

## ---- eval=FALSE--------------------------------------------------------------
#  initUTRAnnotation(variantFile = "../benckmark/data/asd_10000.csv", species = "human", ensemblVersion = 93, dataDir = "../benckmark/db_1000_vars")
#  
#  var_num <- c(10, 100, 250, 500, 1000, 2500, 5000, 7500, 10000)
#  cores <- c(1, 2, 4, 8, 10)
#  replicates <- c(1, 2, 3)
#  runtimes <- data.frame()
#  for (i in replicates) {
#    for (core in cores) {
#      for (num in var_num) {
#        message("varfile = ", num, " core = ", core, " rep = ", i)
#        res <- tempfile()
#        varfile <- file.path("benchmark_htcf/data", paste0("asd_", num, ".csv"))
#        t <- system.time(runUTRAnnotation(variantFile = varfile, annotationResult = res, species = "human", ensemblVersion = 100, dataDir = "data/db_GTEX_eOutlier_rare_variants", conservationBwFiles = "/tmp/conservation_tracks", cores = core))
#        runtimes <- rbind(runtimes, data.frame(num_vars = num, replicate = i, core = core, runtime = t[["elapsed"]]))
#      }
#    }
#  }
#  saveRDS(runtimes, "benchmark_htcf/results/runtimes_upto_10000_vars.rds")
#  

## -----------------------------------------------------------------------------
require(ggplot2)
diff_num_vars <- readRDS(system.file("extdata", "runtimes_upto_10000_vars.rds", package = "utr.annotation"))
ggplot(diff_num_vars, aes(x = num_vars, y = runtime/60, group = core, color = factor(core))) +
  scale_color_manual(breaks = c("1", "2", "4", "8", "10"), values = c("grey70", "grey60", "grey50", "grey40", "black")) +
  geom_smooth() +
  labs(y = "Runtime (min)", color = "Number of CPUs", x = "Number of variants") +
  theme(legend.position = "bottom") 

## ----eval=FALSE---------------------------------------------------------------
#  partitionVariantFile(variantFile = system.file("extdata", "variants_sample.csv", package = "utr.annotation"), chunkNum = 3, chunkPath = "chunks_3", species = "human", ensemblVersion = 93, dataDir = "db_all_variants")
#  # print(list.files("chunks_3", pattern = "*.csv"))

## ----eval=FALSE---------------------------------------------------------------
#  partitionVariantFile(variantFile = system.file("extdata", "variants_sample.csv", package = "utr.annotation"), chunkSize = 7, chunkPath = "chunks_7_vars", species = "human", ensemblVersion = 93, dataDir = "db_all_variants")

## ----eval=FALSE---------------------------------------------------------------
#  partitionVariantFile(variantFile = system.file("extdata", "variants_sample.csv", package = "utr.annotation"), chunkSize = 7, chunkPath = "chunks_7_vars_v2", species = "human", ensemblVersion = 93, dataDir = "db_all_variants", getTranscript = FALSE)

## ----message=FALSE, warning=FALSE, results='hide'-----------------------------
partitions <- list.files("chunks_3", pattern = "*.csv")
print(partitions)
for (f in partitions) {
  runUTRAnnotation(variantFile = file.path("chunks_3", f), annotationResult = file.path("partition_results", f), species = "human", ensemblVersion = 93, dataDir = "db_all_variants", mrl_prediction = F)
}

## ---- eval=FALSE--------------------------------------------------------------
#  library(utr.annotation)
#  args = commandArgs(trailingOnly=TRUE)
#  
#  if (length(args) >= 5) {
#    input <- args[1]
#    output <- args[2]
#    species <- args[3]
#    version <- args[4]
#    dbDir <- args[5]
#    if (length(args) >= 6) {
#      conservationBwTracks <- args[6]
#      numCores <- args[7]
#      message("Run UTR annotation on ", input, "\nspecies = ", species, "\nensembl version = ", version, " with conservation files ", paste(conservationBwTracks, collapse = ","))
#      runUTRAnnotation(input, output, species = species, ensemblVersion = version, dataDir = dbDir, conservationBwFiles = conservationBwTracks, cores = numCores)
#  
#    } else {
#      message("Run UTR annotation on ", input, "\nspecies = ", species, "\nensembl version = ", version, " without conservation files")
#      runUTRAnnotation(input, output, species = species, ensemblVersion = version, dataDir = dbDir)
#    }
#  } else {
#    stop("Please pass at least 5 arguments, CSV input file, output CSV file, species, ensembl version, and dataDir")
#  }
#  

## ----eval=FALSE---------------------------------------------------------------
#  concatenateAnnotationResult(varResultPath = "partition_results", annotationFinalResult = "results/concatenated_annotation.csv")
#  # results <- read_csv("results/concatenated_annotation.csv")
#  # print(nrow(results))
#  # print(colnames(results))

