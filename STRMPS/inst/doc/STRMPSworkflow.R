## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>"
)

## ----message = FALSE----------------------------------------------------------
library("Biostrings")
library("pwalign")
library("ShortRead")

readPath <- system.file('extdata', 'sampleSequences.fastq', package = 'STRMPS')
sequences <- readFastq(readPath)

## -----------------------------------------------------------------------------
sequences@sread

## -----------------------------------------------------------------------------
library("STRMPS")
data("flankingRegions")

## -----------------------------------------------------------------------------
head(flankingRegions, 5)

## -----------------------------------------------------------------------------
identifiedSTRs <- identifySTRRegions(
    reads = sequences,
    flankingRegions = flankingRegions,
    numberOfMutation = 1,
    control = identifySTRRegions.control(
        numberOfThreads = 1,
        includeReverseComplement = FALSE
    )
)

## -----------------------------------------------------------------------------
names(identifiedSTRs$identifiedMarkersSequencesUniquelyAssigned$CSF1PO)

## -----------------------------------------------------------------------------
sortedIncludedMarkers <- sapply(
    names(identifiedSTRs$identifiedMarkersSequencesUniquelyAssigned),
    function(m) which(m == flankingRegions$Marker)
)

## -----------------------------------------------------------------------------
stringCoverageList <- stringCoverage(
    extractedReadsListObject = identifiedSTRs,
    flankingRegions = flankingRegions,
    control = stringCoverage.control(
        numberOfThreads = 1,
        trace = FALSE,
        simpleReturn = TRUE
    )
)

## -----------------------------------------------------------------------------
stringCoverageList$CSF1PO

## -----------------------------------------------------------------------------
genotypeList <- getGenotype(stringCoverageList)

## -----------------------------------------------------------------------------
genotypeList$CSF1PO

## -----------------------------------------------------------------------------
noiseList <- identifyNoise(stringCoverageList, thresholdSignal = 0.03)

## -----------------------------------------------------------------------------
noiseList$CSF1PO

## -----------------------------------------------------------------------------
stringCoverageGenotypeList <- mergeGenotypeStringCoverage(stringCoverageList, genotypeList)

stutterList <- findStutter(stringCoverageGenotypeList)
stutterTibble <- subset(do.call("rbind", stutterList), !is.na(Genotype))
head(stutterTibble, 5)

## ----eval = FALSE-------------------------------------------------------------
# STRMPSWorkflow(
#     read_path,
#     control = workflow.control(
#         restrictType = "Autosomal",
#         numberOfThreads = 1
#     )
# )

