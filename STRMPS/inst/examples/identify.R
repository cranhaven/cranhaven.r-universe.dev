library("Biostrings")
library("ShortRead")

# Path to file
readPath <- system.file('extdata', "sampleSequences.fastq", package = 'STRMPS')

# Flanking regions
data("flankingRegions")

# Read the file into memory
readFile <- readFastq(readPath)
sread(readFile)
quality(readFile)
\donttest{
# Identify the STR's of the file, both readPath and readFile can be used.
identifySTRRegions(
    reads = readFile,
    flankingRegions = flankingRegions,
    numberOfMutation = 1,
    control = identifySTRRegions.control(
        numberOfThreads = 1,
        includeReverseComplement = FALSE)
)
}
