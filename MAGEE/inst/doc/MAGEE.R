### R code from vignette source 'MAGEE.Rnw'

###################################################
### code chunk number 1: installation (eval = FALSE)
###################################################
## ## try http:// if https:// URLs are not supported
## ## remove "doMC" below if you are running Windows
## install.packages(c("devtools", "RcppArmadillo", "CompQuadForm", "doMC", 
##         "foreach", "Matrix", "GMMAT", "BiocManager", "testthat", "data.table"), 
## 	repos = "https://cran.r-project.org/")
## BiocManager::install(c("SeqArray", "SeqVarTools"))
## devtools::install_github("https://github.com/large-scale-gxe-methods/MAGEE")


###################################################
### code chunk number 2: convert2GDS (eval = FALSE)
###################################################
## SeqArray::seqVCF2GDS("VCF_file_name", "GDS_file_name")
## SeqArray::seqBED2GDS("BED_file_name", "FAM_file_name", "BIM_file_name", 
##         "GDS_file_name")


###################################################
### code chunk number 3: loading (eval = FALSE)
###################################################
## library(MAGEE)


###################################################
### code chunk number 4: help (eval = FALSE)
###################################################
## ?MAGEE


###################################################
### code chunk number 5: MAGEEglmmkingds (eval = FALSE)
###################################################
## library(GMMAT)
## GRM.file <- system.file("extdata", "GRM.txt.bz2", package = "MAGEE")
## GRM <- as.matrix(read.table(GRM.file, check.names = FALSE))
## model0 <- glmmkin(disease ~ age + sex, data = pheno, kins = GRM,
##                   id = "id", family = binomial(link = "logit"))


###################################################
### code chunk number 6: MAGEEgeigds (eval = FALSE)
###################################################
## infile <- system.file("extdata", "geno.gds", package = "MAGEE")
## gds_outfile <- tempfile()
## glmm.gei(model0, interaction='sex', geno.file = infile, 
##          outfile = gds_outfile)


###################################################
### code chunk number 7: MAGEEgeibgen (eval = FALSE)
###################################################
## infile <- system.file("extdata", "geno.bgen", package = "MAGEE")
## gds_outfile <- tempfile()
## glmm.gei(model0, interaction='sex', geno.file = infile, 
##          outfile = gds_outfile)


###################################################
### code chunk number 8: MAGEEgeigds (eval = FALSE)
###################################################
## infile1 <- system.file("extdata", "meta1.txt", package = "MAGEE")
## infile2 <- system.file("extdata", "meta2.txt", package = "MAGEE")
## infile3 <- system.file("extdata", "meta3.txt", package = "MAGEE")
## infile4 <- system.file("extdata", "meta4.txt", package = "MAGEE")
## infile5 <- system.file("extdata", "meta5.txt", package = "MAGEE")
## outfile <- tempfile()
## glmm.gei.meta(files = c(infile1, infile2, infile3, infile4, infile5),
##                interaction="sex", outfile = outfile)


###################################################
### code chunk number 9: MAGEEmageegds (eval = FALSE)
###################################################
## geno.file <- system.file("extdata", "geno.gds", package = "MAGEE")
## group.file <- system.file("extdata", "SetID.withweights.txt", 
##                           package = "MAGEE")
## MAGEE(model0, interaction='sex', geno.file, group.file, 
##              group.file.sep = "\t", tests=c("JV", "JF", "JD"))


###################################################
### code chunk number 10: MAGEEmageegds (eval = FALSE)
###################################################
## geno.file <- system.file("extdata", "geno.gds", package = "MAGEE")
## group.file <- system.file("extdata", "SetID.withweights.txt", 
##                           package = "MAGEE")
## meta.files.prefix <- tempfile()
## MAGEE.meta(meta.files.prefix = meta.files.prefix,  
##                    group.file=group.file,
##                     tests=c("JV", "JF", "JD"))


###################################################
### code chunk number 11: MKL (eval = FALSE)
###################################################
## Sys.setenv(MKL_NUM_THREADS = 1)


###################################################
### code chunk number 12: RhpcBLASctlL (eval = FALSE)
###################################################
## #install.packages("RhpcBLASctl")
## library(RhpcBLASctl)
## blas_set_num_threads(1)


