#' Workflow default options
#'
#' Control object for workflow function returning a list of default parameter options.
#'
#' @param variantDatabase A \link[tibble]{tibble} of 'trusted' STR regions containing columns for 'Marker', 'Type', and 'Region'.
#' @param numberOfMutations The maximum number of mutations (base-calling errors) allowed during flanking region identification.
#' @param numberOfThreads The number of threads used by the \link[parallel]{mclapply}-function (stuck at '2' on windows).
#' @param createdThresholdSignal Noise threshold.
#' @param thresholdHomozygote Homozygote threshold for genotype identification.
#' @param internalTrace Show trace.
#' @param simpleReturn TRUE/FALSE: Should the regions be aggregated without including flanking regions?
#' @param identifyNoise TRUE/FALSE: Should noise be identified.
#' @param identifyStutter TRUE/FALSE: Should stutters be identified.
#' @param flankingRegions The flanking regions used to identify the STR regions. If 'NULL' a default set is loaded and used.
#' @param trimRegions TRUE/FALSE: Should the identified regions be further trimmed.
#' @param restrictType A character vector specifying the marker 'Types' to be identified.
#' @param reduceSize TRUE/FALSE: Should the size of the data-set be reduced using the quality and the variant database?
#' @param additionalFlags TRUE/FALSE: Should the 'flankingRegions'-object be used to extract additional information from the flanking regions of each string?
#' @param trace TRUE/FALSE: Should a trace be shown?
#'
#' @return List of default of options.
workflow.control <- function(numberOfMutations = 1, numberOfThreads = 4, createdThresholdSignal = 0.05, thresholdHomozygote = 0.4,
                             internalTrace = FALSE, simpleReturn = TRUE, identifyNoise = FALSE, identifyStutter = FALSE,
                             flankingRegions = NULL, trimRegions = TRUE, restrictType = NULL, trace = TRUE,
                             variantDatabase = NULL, reduceSize = FALSE, additionalFlags = TRUE) {
    res <- list(numberOfMutations = numberOfMutations, numberOfThreads = numberOfThreads, createdThresholdSignal = createdThresholdSignal,
                thresholdHomozygote = thresholdHomozygote, internalTrace = internalTrace, simpleReturn = simpleReturn,
                identifyNoise = identifyNoise, identifyStutter = identifyStutter, flankingRegions = flankingRegions,
                trimRegions = trimRegions, restrictType = restrictType, trace = trace,
                variantDatabase = variantDatabase, reduceSize = reduceSize, additionalFlags = additionalFlags)
    return(res)
}

#' @title Workflow function
#'
#' @description The function takes an input file and performs all preliminary analyses.
#' The function creates a series of objects which can be further analysed.
#' An output folder can be provided to store the objects as \code{.RData}-files.
#'
#' @param input A path to a \code{.fastq}-file.
#' @param output A directory where output-files are stored.
#' @param continueCheckpoint Choose a checkpoint to continue from in the workflow. If NULL the function will run the entire workflow.
#' @param control Function controlling non-crucial parameters and other control functions.
#' @return If 'output' not provided the function simply returns the stringCoverageList-object.
#' If an output is provided the function will store ALL created objects at the output-path, i.e. nothing is returned.
#' @example inst/examples/workFlow.R
STRMPSWorkflow <- function(input, output = NULL, continueCheckpoint = NULL, control = workflow.control()) {
    if (!is.null(output)) {
        dirExists <- dir.exists(output)
        if (!dirExists) {
            dir.create(output)
        }

        output <- paste(output, sapply(strsplit(output, "/"), function(ss) ss[length(ss)]), sep = "/")
        saveCheckpoint = TRUE
    }
    else {
        saveCheckpoint = FALSE
    }

    if (is.null(continueCheckpoint))
        continueCheckpoint <- FALSE

    if (is.null(control$flankingRegions)) {
        flankingRegions <- .loadRData(system.file('extdata', 'flankingRegionsForenSeqSTRsShifted.RData', package = "STRMPS"))
    } else if (is.character(control$flankingRegions)) {
        if (tolower(control$flankingRegions) == "forenseq") {
            flankingRegions <- .loadRData(system.file('extdata', 'flankingRegionsForenSeqSTRsShifted.RData', package = "STRMPS"))
        }
        else if (tolower(control$flankingRegions) == "10plex") {
            flankingRegions <- .loadRData(system.file('extdata', 'flankingRegions10plexSTRsShifted.RData', package = "STRMPS"))
        }
        else if (tolower(control$flankingRegions) == "s5") {
            flankingRegions <- .loadRData(system.file('extdata', 'flankingRegionsS5STRsShifted.RData', package = "STRMPS"))
        }
        else {
            stop("The provided 'flankingRegion' type is not supported choose one of '10plex', 'forenseq', or 's5'.")
        }
    } else {
        flankingRegions <- control$flankingRegions
    }

    if (!is.null(control$restrictType)) {
        flankingRegions <- flankingRegions %>%
            filter(tolower(Type) == tolower(control$restrictType))
    }

    readPath <- input

    fileExists <- file.exists(paste(output, "_", "fastqfileloaded", ".RData", sep = ""))
    if (continueCheckpoint & fileExists) {
        load(paste(output, "_", "fastqfileloaded", ".RData", sep = ""))
    }
    else {
        readfile <- readFastq(readPath)
        if (saveCheckpoint)
            save(readfile, file = paste(output, "_", "fastqfileloaded", ".RData", sep = ""))
    }

    # Identified sequences list
    fileExists <- file.exists(paste(output, "_", "identifiedSTRRegions", ".RData", sep = ""))
    if (continueCheckpoint & fileExists) {
        load(paste(output, "_", "identifiedSTRRegions", ".RData", sep = ""))
    }
    else {
        identifiedSTRs <- identifySTRRegions(
            reads = readfile,
            flankingRegions = flankingRegions,
            numberOfMutation = control$numberOfMutations,
            control = identifySTRRegions.control(
                numberOfThreads = control$numberOfThreads
            )
        )

        if (saveCheckpoint)
            save(identifiedSTRs, file = paste(output, "_", "identifiedSTRRegions", ".RData", sep = ""))
    }

    # String coverage list
    fileExists <- file.exists(paste(output, "_", "stringCoverageList", ".RData", sep = ""))
    if (continueCheckpoint & fileExists) {
        stringCoverageList <- .loadRData(paste(output, "_", "stringCoverageList", ".RData", sep = ""))
    }
    else {
        sortedIncludedMarkers <- sapply(names(identifiedSTRs$identifiedMarkersSequencesUniquelyAssigned), function(m) which(m == flankingRegions$Marker))
        stringCoverageList <- stringCoverage(
            extractedReadsListObject = identifiedSTRs,
            flankingRegions = flankingRegions,
            control = stringCoverage.control(
                numberOfThreads = control$numberOfThreads,
                trace = control$internalTrace,
                additionalFlags = control$additionalFlags,
                simpleReturn = control$simpleReturn
            )
        )

        if (saveCheckpoint)
            save(stringCoverageList, file = paste(output, "_", "stringCoverageList", ".RData", sep = ""))

    }

    if (control$trimRegions) {
        fileExists <- file.exists(paste(output, "_", "stringCoverageListTrimmed", ".RData", sep = ""))
        if (continueCheckpoint & fileExists) {
            stringCoverageList = .loadRData(paste(output, "_", "stringCoverageListTrimmed", ".RData", sep = ""))
        }
        else {
            stringCoverageListTrimmed <- lapply(seq_along(stringCoverageList), function(ss) {
                stringCoverageList_ss <- stringCoverageList[[ss]]

                if (!is.null(stringCoverageList_ss)) {
                    flankingRegions_m <- flankingRegions %>%
                        filter(Marker == unique(stringCoverageList_ss$Marker))

                    if (length(flankingRegions_m$Marker) == 0) {
                        res <- stringCoverageList_ss
                    }
                    else {
                        if ((flankingRegions_m$ForwardShift == 0) & (flankingRegions_m$ReverseShift == 0)) {
                            res <- stringCoverageList_ss
                        }
                        else {
                            res <- stringCoverageList_ss %>%
                                mutate(ExpandedRegion = Region) %>%
                                mutate(
                                    BasePairs = nchar(ExpandedRegion),
                                    AdjustedBasePairs = BasePairs - flankingRegions_m$Offset,
                                    Region = str_sub(ExpandedRegion, start = flankingRegions_m$ForwardShift + 1, end = - flankingRegions_m$ReverseShift - 1)
                                ) %>%
                                select(-ExpandedRegion, BasePairs, AdjustedBasePairs) %>%
                                group_by(Marker, Type, Region, MotifLength) %>%
                                summarise(
                                    Allele = max(AdjustedBasePairs) / unique(MotifLength), Coverage = sum(Coverage),
                                    Quality = list(str_sub(unlist(Quality), start = flankingRegions_m$ForwardShift + 1, end = - flankingRegions_m$ReverseShift - 1)),
                                    AggregateQuality = .aggregateQuality(unlist(Quality))
                                ) %>%
                                ungroup() %>%
                                select(Marker, Type, Allele, MotifLength, Region, Coverage, AggregateQuality, Quality) %>%
                                arrange(Allele, Region)
                        }
                    }

                    return(res)
                }
                else {
                    return(NULL)
                }
            })

            names(stringCoverageListTrimmed) <- names(stringCoverageList)
            class(stringCoverageListTrimmed) <- "stringCoverageList"
            if (saveCheckpoint)
                save(stringCoverageListTrimmed, file = paste(output, "_", "stringCoverageListTrimmed", ".RData", sep = ""))

            stringCoverageList <- stringCoverageListTrimmed
        }

        if (control$reduceSize) {
            fileExists <- file.exists(paste(output, "_", "stringCoverageListTrimmedReduced", ".RData", sep = ""))
            if (continueCheckpoint & fileExists) {
                stringCoverageList = .loadRData(paste(output, "_", "stringCoverageListTrimmedReduced", ".RData", sep = ""))
            }
            else {
                if (!is.null(control$variantDatabase)) {
                    stringCoverageListTrimmedReduced <- .sampleQualityCleaning(stringCoverageList, control$variantDatabase) %>%
                        split(., f = .$Marker)

                    class(stringCoverageListTrimmedReduced) <- "stringCoverageList"

                    if (saveCheckpoint)
                        save(stringCoverageListTrimmedReduced, file = paste(output, "_", "stringCoverageListTrimmedReduced", ".RData", sep = ""))

                    stringCoverageList <- stringCoverageListTrimmedReduced
                }
            }
        }
    }

    # Noise list
    if (control$identifyNoise) {
        fileExists <- file.exists(paste(output, "_", "noiseStringCoverageList", ".RData", sep = ""))
        if (!(continueCheckpoint & fileExists)) {
            identifiedNoise <- identifyNoise(stringCoverageList, "Coverage", thresholdSignal = control$createdThresholdSignal)
            noiseStringCoverageList <- mergeNoiseStringCoverage(stringCoverageList, identifiedNoise)

            if (saveCheckpoint)
                save(noiseStringCoverageList, file = paste(output, "_", "noiseStringCoverageList", ".RData", sep = ""))
        }
    }

    # Stutters
    if (control$identifyStutter) {
        fileExists <- file.exists(paste(output, "_", "genotypeList", ".RData", sep = ""))
        if (continueCheckpoint & fileExists) {
            genotypeList = .loadRData(paste(output, "_", "genotypeList", ".RData", sep = ""))
        }
        else {
            namesStringCoverageList <- names(stringCoverageList)
            if (is.null(namesStringCoverageList)) {
                namesStringCoverageList <- sapply(stringCoverageList, function(xx) unique(xx$Marker))
            }

            sortedIncludedMarkers <- sapply(namesStringCoverageList, function(m) which(m == flankingRegions$Marker))
            t_H <- control$thresholdHomozygote + (1 - control$thresholdHomozygote - 0.01) * as.numeric(flankingRegions$Type[sortedIncludedMarkers] == "Y")

            genotypeList <- getGenotype(stringCoverageList, thresholdHeterozygosity = t_H, thresholdSignal = 0.01, thresholdAbsoluteLowerLimit = 1)

            if (saveCheckpoint) {
                save(genotypeList, file = paste(output, "_", "genotypeList", ".RData", sep = ""))
            }
        }

        fileExists <- file.exists(paste(output, "_", "stutterTibble", ".RData", sep = ""))
        if (!(continueCheckpoint & fileExists)) {
            stringCoverageGenotypeList <- mergeGenotypeStringCoverage(stringCoverageList, genotypeList)

            stutterList <- findStutter(stringCoverageGenotypeList)
            stutterTibble <- do.call(rbind, stutterList) %>% filter(!is.na(NeighbourAllele))

            if (saveCheckpoint) {
                save(stutterTibble, file = paste(output, "_", "stutterTibble", ".RData", sep = ""))
            }
        }
    }

    if (is.null(output) & !saveCheckpoint) {
        return(stringCoverageList)
    }
    else {
        return(invisible(NULL))
    }
}

#' @title Batch wrapper for the workflow function
#'
#' @description The function takes an input directory and performs the entire analysis workflow described in (ADD REF).
#' The function creates a series of objects needed for further analyses and stores them at the output location.
#'
#' @param input A directory where fastq input-files are stored.
#' @param output A directory where output-files are stored.
#' @param ignorePattern A pattern parsed to grepl used to filter input strings.
#' @param continueCheckpoint Choose a checkpoint to continue from in the workflow. If NULL the function will run the entire workflow.
#' @param control Function controlling non-crucial parameters and other control functions.
#' @return If 'output' not provided the function simply returns the stringCoverageList-object.
#' If an output is provided the function will store ALL created objects at the output-path, i.e. nothing is returned.
STRMPSWorkflowBatch <- function(input, output, ignorePattern = NULL, continueCheckpoint = NULL, control = workflow.control()) {
    files <- list.files(input, full.names = T, recursive = TRUE)
    files <- files[nchar(gsub("fastq", "", files)) < nchar(files)]

    if (!is.null(ignorePattern)) {
        ignoredFiles <- grepl(ignorePattern, files)
    }
    else {
        ignoredFiles <- rep(FALSE, length(files))
    }

    run_names <- str_split(list.files(input, recursive = TRUE), "/")[!ignoredFiles]
    dir_names <- lapply(run_names, function(ss) gsub("[- ]", "_", ss[-length(ss)]))
    file_names <- gsub("[- \\.]", "_", sapply(str_split(sapply(run_names, function(ss) ss[length(ss)]), ".fastq"), function(ss) ss[1]))

    dir_exists <- dir.exists(output)
    if (!dir_exists) {
        dir.create(output)
    }

    for (i in seq_along(files[!ignoredFiles])) {
        if (control$trace) {
            cat("File:", i, "/", length(files[!ignoredFiles]), "\n")
        }

        dir_names_i <- dir_names[[i]]

        if (length(dir_names_i) > 0) {
            for (j in seq_along(dir_names_i)) {
                dir_exists <- dir.exists(paste(output, dir_names_i[1:j], collapse = "/", sep = "/"))
                if (!dir_exists) {
                    dir.create(paste(output, dir_names_i[1:j], collapse = "/", sep = "/"))
                }
            }

            output_i <- paste(output, dir_names_i, file_names[i], collapse = "/", sep = "/")
        }
        else {
            output_i <- paste(output, file_names[i], collapse = "/", sep = "/")
        }

        STRMPSWorkflow(input = files[!ignoredFiles][i], output = output_i,
                       continueCheckpoint = continueCheckpoint, control = control)
    }
}

#' @title Collect stutters files
#'
#' @description Collects all stutter files created by the batch version of the \link{STRMPSWorkflow} function.
#'
#' @param stutterDirectory The out most directory containing all stutter files to be collected.
#' @param storeCollection TRUE/FALSE: Should the collected tibble be stored? If 'FALSE' the tibble is returned.
#'
#' @return If 'storeCollection' is TRUE nothing is returned, else the stutter collection is returned.
STRMPSWorkflowCollectStutters <- function(stutterDirectory, storeCollection = TRUE) {
    files <- list.files(stutterDirectory, pattern = "*_stutterTibble.RData", full.names = TRUE, recursive = TRUE)

    collectedStutter <- vector("list", length(files))
    for (i in seq_along(files)) {
        files_i <- files[i]
        collectedStutter[[i]] <- .loadRData(files_i) %>% mutate(Sample = i)
    }

    collectedStutter <- bind_rows(collectedStutter)
    if (storeCollection) {
        save(collectedStutter, file = paste(stutterDirectory, "collectedStutterTibble.RData", sep = "_"))
        return(invisible(NULL))
    }
    else {
        return(collectedStutter)
    }
}

