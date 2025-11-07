## ----eval = FALSE-------------------------------------------------------------
# install.packages("RapidoPGS")

## ----eval=FALSE---------------------------------------------------------------
# if (!requireNamespace("remotes", quietly = TRUE))
#     install.packages("remotes")
# remotes::install_github("GRealesM/RapidoPGS")

## ----message=FALSE, warning = FALSE-------------------------------------------
library(RapidoPGS)

## ----eval =FALSE--------------------------------------------------------------
# ds <- gwascat.download(29059683)
# 
# # Select the harmonised hg38 file
# # This is equivalent to:
# # ds <- fread("ftp://ftp.ebi.ac.uk/pub/databases/gwas/summary_statistics/GCST004001-GCST005000/GCST004988/harmonised/29059683-GCST004988-EFO_0000305.h.tsv.gz")
# 
# # Then apply some reformatting
# setnames(ds, old = c("hm_rsid","hm_chrom","hm_pos", "hm_other_allele", "hm_effect_allele", "hm_effect_allele_frequency", "hm_beta", "standard_error", "p_value"), new = c("SNPID","CHR", "BP", "REF","ALT","ALT_FREQ", "BETA", "SE", "P"))
# ds <- ds[,.(SNPID, CHR, BP, REF, ALT, ALT_FREQ, BETA, SE, P)]
# ds <- ds[CHR !="X"]
# ds$CHR <- as.numeric(ds$CHR)
# ds <- ds[order(CHR, BP)]
# ds <- na.omit(ds, cols = c("BETA", "ALT_FREQ"))
# 

## -----------------------------------------------------------------------------
ds <- michailidou38

## -----------------------------------------------------------------------------
summary(ds)

## -----------------------------------------------------------------------------
full_PGS <- rapidopgs_single(ds, trait = "cc", build = "hg38")

## -----------------------------------------------------------------------------
head(full_PGS)

## -----------------------------------------------------------------------------
PGS_1e4 <- rapidopgs_single(ds, trait ="cc", build = "hg38", filt_threshold = 1e-4)
head(PGS_1e4)

## -----------------------------------------------------------------------------
PGS_1e4_norecalc <- rapidopgs_single(ds, trait ="cc", build = "hg38", filt_threshold = 1e-4, recalc = FALSE)
head(PGS_1e4_norecalc)

## -----------------------------------------------------------------------------
PGS_top10 <- rapidopgs_single(ds, trait ="cc", build = "hg38", filt_threshold = 10)
head(PGS_top10)

