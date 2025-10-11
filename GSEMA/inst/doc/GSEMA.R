## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
library(GSEMA)
data("simulatedData")

## -----------------------------------------------------------------------------
#List of expression matrices
listMatrices <- list(study1Ex, study2Ex)

## -----------------------------------------------------------------------------
listPhenodata <- list(study1Pheno, study2Pheno)

## -----------------------------------------------------------------------------
study1Pheno$Condition

## -----------------------------------------------------------------------------
head(names(GeneSets))
GeneSets[[1]]

## -----------------------------------------------------------------------------
listMatrices <- list(study1Ex, study2Ex)
listPhenodata <- list(study1Pheno, study2Pheno)
phenoGroups <- c("Condition","Condition", "Condition")
phenoCases <- list("Case", "Case", "Case")
phenoControls <- list("Healthy", "Healthy", "Healthy")
objectMApathSim <- createObjectMApath(
    listEX = listMatrices,
    listPheno = listPhenodata, namePheno = phenoGroups,
    expGroups = phenoCases, refGroups = phenoControls,
    geneSets = GeneSets,
    pathMethod = "Zscore",
    n.cores = 1,
    internal.n.cores = 1, minSize = 7)

## -----------------------------------------------------------------------------
objectMApathSim <- filteringPaths(objectMApathSim, threshold = 0.65)

## -----------------------------------------------------------------------------
results <- metaAnalysisESpath(objectMApath = objectMApathSim,
    measure = "limma", typeMethod = "REM", missAllow = 0.3, 
    numData = length(objectMApathSim))

## -----------------------------------------------------------------------------
results[1,]
results[nrow(results),]

## -----------------------------------------------------------------------------
res <- heatmapPaths(objectMA=objectMApathSim, results,
            scaling = "zscor", regulation = "all",breaks=c(-2,2),
            fdrSig = 0.05, comES_Sig = 1, numSig=20, fontsize = 5)

## -----------------------------------------------------------------------------
#Effects <- calculateESpath(objectMApath = objectMApathSim, measure = "limma")
#Effects[[1]]["Simulated_Pathway",]
#Effects[[2]]["Simulated_Pathway",]

## -----------------------------------------------------------------------------
sessionInfo()

