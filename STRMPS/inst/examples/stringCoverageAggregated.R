# Regions identified using 'identifySTRs()'
data("identifiedSTRs")

# Aggregate the strings
stringCoverage(
    extractedReadsListObject = identifiedSTRs,
    flankingRegions = flankingRegions,
    control = stringCoverage.control(
        numberOfThreads = 1,
        trace = FALSE,
        simpleReturn = TRUE
    )
)
