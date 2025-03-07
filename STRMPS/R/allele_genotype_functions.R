.lapplymatchPatternBoth <- function(flanks, seqs, numberOfMutation) {
    gapOpeningPenalty <- -2
    gapExtensionPenalty <- -3

    substitutionMatrix <- nucleotideSubstitutionMatrix(
        match = 1, mismatch = -1, baseOnly = FALSE
    )

    res <- vector("list", length(flanks))
    for (j in seq_along(flanks)) {
        seqs_j <- seqs[[j]]

        patterns <- vector("list", length(seqs_j))
        subjects <- vector("list", length(seqs_j))
        subjects_position <- vector("list", length(seqs_j))
        mismatches <- vector("list", length(seqs_j))
        indels <- vector("list", length(seqs_j))
        for (i in seq_along(seqs_j)) {
            s1 <- DNAString(seqs_j[i])
            s2 <- DNAString(flanks[j])

            ss <- pairwiseAlignment(pattern = s1,
                                    subject = s2,
                                    substitutionMatrix = substitutionMatrix,
                                    type = "global",
                                    gapOpening = gapOpeningPenalty,
                                    gapExtension = gapExtensionPenalty)

            if (aligned(subject(ss))@ranges@width > (nchar(flanks[j]) + numberOfMutation)) {
                if (j == 1) {
                    s1 <- DNAString(str_sub(s1, start = 2))
                }
                else {
                    s1 <- DNAString(str_sub(s1, end = -2))
                }

                ss <- pairwiseAlignment(pattern = s1,
                                        subject = s2,
                                        substitutionMatrix = substitutionMatrix,
                                        type = "global",
                                        gapOpening = gapOpeningPenalty,
                                        gapExtension = gapExtensionPenalty)
            }

            patterns[[i]] <- aligned(subject(ss))
            subjects[[i]] <-  aligned(pattern(ss))
            subjects_position[[i]] <- ss@pattern@range
            mismatches[[i]] <- mismatchTable(ss)
            indels[[i]] <- nindel(ss)
        }

        res[[j]] <- list(Patterns = patterns,
                         Subjects = subjects,
                         Position = subjects_position,
                         Mismatches = mismatches,
                         Indels = indels)
    }

    res <- structure(res, .Names = c("Forward", "Reverse"))
    return(res)
}

.getSingleIndel <- function(x, y) {
    id <- rep("-", length(x))
    id[which(x == "-")] <- y[which(x == "-")]
    id <- paste(id, collapse = "")
    id
}

.getIndel <- function(s, p, m) {
    insertions <- rep(NA, length(s))
    deletions <- rep(NA, length(s))
    mismatches <- rep(NA, length(s))
    for (i in seq_along(s)) {
        s_i <- str_split(s[i], "")[[1]]
        p_i <- str_split(p[i], "")[[1]]

        any_insertions <- "-" %in% p_i
        if (any_insertions) {
            insertions[i] <- .getSingleIndel(p_i, s_i)
        }

        any_deletions <- "-" %in% s_i
        if (any_deletions) {
            deletions[i] <- .getSingleIndel(s_i, p_i)
        }

        any_mismatches <- dim(m[[i]])[1] > 0
        if (any_mismatches) {
            xx_i <- rep("-", length(s_i))
            xx_i[m[[i]]$SubjectStart] <- as.character(m[[i]]$PatternSubstring)
            mismatches[i] <- paste(xx_i, collapse = "")
        }
    }

    list(Insertions = insertions, Deletions = deletions, Mismatches = mismatches)
}

.flankingAdditionalInformation <- function(sequences,
                                           forwardFlank, reverseFlank,
                                           forwardShift, reverseShift,
                                           numberOfMutation) {
    ##
    seqs <- list(sequences$ForwardFlank, sequences$ReverseFlank)
    flanks <- c(forwardFlank, reverseFlank)

    ##
    identifiedSequences <- .lapplymatchPatternBoth(flanks = flanks, seqs = seqs, numberOfMutation = numberOfMutation)
    forwardIndels <- .getIndel(s = sapply(identifiedSequences$Forward$Subjects, as.character),
                                        p = sapply(identifiedSequences$Forward$Patterns, as.character),
                                        m = identifiedSequences$Forward$Mismatches)
    reverseIndels <- .getIndel(s = sapply(identifiedSequences$Reverse$Subjects, as.character),
                                        p = sapply(identifiedSequences$Reverse$Patterns, as.character),
                                        m = identifiedSequences$Reverse$Mismatches)

    res <- sequences %>%
        mutate(ForwardMismatches = forwardIndels$Mismatches,
               ForwardInsertions = forwardIndels$Insertions,
               ForwardDeletions = forwardIndels$Deletions,
               ReverseMismatches = reverseIndels$Mismatches,
               ReverseInsertions = reverseIndels$Insertions,
               ReverseDeletions = reverseIndels$Deletions,
               NumberForwardMismatches = sapply(identifiedSequences$Forward$Mismatches, function(xx) dim(xx)[1]),
               NumberForwardInsertions = sapply(identifiedSequences$Forward$Indels, function(xx) if (xx@insertion[1, 1] != 0) sum(xx@insertion[, 2]) else 0),
               NumberForwardDeletions = sapply(identifiedSequences$Forward$Indels, function(xx) if (xx@deletion[1, 1] != 0) sum(xx@deletion[, 2]) else 0),
               NumberReverseMismatches = sapply(identifiedSequences$Reverse$Mismatches, function(xx) dim(xx)[1]),
               NumberReverseInsertions = sapply(identifiedSequences$Reverse$Indels, function(xx) if (xx@insertion[1, 1] != 0) sum(xx@insertion[, 2]) else 0),
               NumberReverseDeletions = sapply(identifiedSequences$Reverse$Indels, function(xx) if (xx@deletion[1, 1] != 0) sum(xx@deletion[, 2]) else 0))

    return(res)
}


#' @title A string coverage list
#'
#' @description A list of tibbles, one for every marker, used to contain the sequencing information of STR MPS data.
#' The tibbles should include columns with the following names: "Marker", "BasePairs", "Allele", "Type", "MotifLength", "ForwardFlank", "Region", "ReverseFlank", "Coverage", "AggregateQuality", and "Quality".
setClass("stringCoverageList")

#' String coverage coontrol object
#'
#' @details Control function for the 'stringCoverage' function. Sets default values for the parameters.
#'
#' @param numberOfThreads The number of cores used for parallelisation.
#' @param simpleReturn TRUE/FALSE: Should the returned object be simplified?
#' @param uniquelyAssigned TRUE/FALSE: Should regions which are not uniquely assigned be removed?
#' @param additionalFlags TRUE/FALSE: Create additional flags for assessing sequence reliability?
#' @param numberOfMutations the maximum number of mutations (base-calling errors) allowed during flanking region identification.
#' @param includeAverageBaseQuality Should the average base quality of the region be included?
#' @param meanFunction The function used to average the base qualities.
#' @param trace TRUE/FALSE: Show trace?
#'
#' @return List of parameters used for the 'stringCoverage' function.
stringCoverage.control <- function(numberOfThreads = 4L,
                                   simpleReturn = TRUE,
                                   uniquelyAssigned = TRUE,
                                   additionalFlags = FALSE,
                                   numberOfMutations = 1,
                                   includeAverageBaseQuality = FALSE,
                                   meanFunction = mean,
                                   trace = FALSE) {
    list(numberOfThreads = numberOfThreads,
         simpleReturn = simpleReturn,
         uniquelyAssigned = uniquelyAssigned,
         additionalFlags = additionalFlags,
         numberOfMutations = numberOfMutations,
         includeAverageBaseQuality = includeAverageBaseQuality,
         meanFunction = meanFunction,
         trace = trace)
}

.extractedReadsList.stringCoverage <- function(extractedReadsListObject,
                                               flankingRegions,
                                               control = stringCoverage.control()) {

    if (control$uniquelyAssigned) {
        sortedIncludedMarkers <- sapply(names(extractedReadsListObject$identifiedMarkersSequencesUniquelyAssigned), function(m) which(flankingRegions$Marker == m))
        flankingRegions <- flankingRegions[sortedIncludedMarkers, ]

        extractedReads <- extractedReadsListObject$identifiedMarkersSequencesUniquelyAssigned
    } else {
        warning("Only uniquely assigned sequences extract list should be used.")

        sortedIncludedMarkers <- sapply(names(extractedReadsListObject$identifiedMarkers), function(m) which(flankingRegions$Marker == m))
        flankingRegions <- flankingRegions[sortedIncludedMarkers, ]

        extractedReads <- extractedReadsListObject$identifiedMarkers
    }

    if (control$additionalFlags) {
        if (sum(str_detect(names(flankingRegions), "[Forward|Reverse]Shift")) != 2) {
            warning("'flankingRegions'-object should contain columns 'ForwardShift' and 'ReverseShift'. These are added with value 0.")
            flankingRegions$ForwardShift <- 0
            flankingRegions$ReverseShift <- 0
        }
    }

    alleles <- mclapply(seq_along(extractedReads), function(i) {
        if ((is.null(extractedReads[[i]]$trimmed))) {
            return(NULL)
        } else if ((dim(extractedReads[[i]]$trimmed)[1] == 0)) {
            return(NULL)
        }

        matchedFlanks <- extractedReads[[i]]
        marker <- matchedFlanks$name

        if (control$trace)
            cat(i, "/", length(extractedReads), ":: Marker:", marker, "\n")

        motifLength_i <- flankingRegions$MotifLength[i]
        type_i <- flankingRegions$Type[i]

        if (control$additionalFlags) {
            sequences_i <- matchedFlanks$trimmed
            quality_i <- matchedFlanks$trimmedQuality$Region
            stringCoverageQuality <- cbind(sequences_i, Quality = quality_i) %>%
                group_by(ForwardFlank, Region, ReverseFlank) %>%
                summarise(Coverage = n(),
                          AggregateQuality = .aggregateQuality(Quality),
                          Quality = list(as.character(Quality))) %>%
                ungroup()

            flankingRegions_i <- flankingRegions %>% filter(Marker == marker)
            additionalInformation <- .flankingAdditionalInformation(
                sequences = stringCoverageQuality,
                forwardFlank = flankingRegions_i$ForwardFlank,
                reverseFlank = flankingRegions_i$ReverseFlank,
                forwardShift = flankingRegions_i$ForwardShift,
                reverseShift = flankingRegions_i$ReverseShift,
                numberOfMutation = control$numberOfMutation
            )

            additionalInformation <- additionalInformation %>%
                rowwise() %>%
                mutate(maxIndels = max(NumberForwardInsertions, NumberForwardDeletions,
                                       NumberReverseInsertions, NumberReverseDeletions)) %>%
                ungroup()

            stringCoverageQuality <- additionalInformation %>%
                group_by(ForwardFlank, Region, ReverseFlank) %>%
                summarise(Coverage = sum(Coverage),
                          AggregateQuality = .aggregateQuality(AggregateQuality),
                          Quality = list(unlist(Quality)),
                          ForwardMismatches = unique(ForwardMismatches),
                          ForwardInsertions = unique(ForwardInsertions),
                          ForwardDeletions = unique(ForwardDeletions),
                          ReverseMismatches = unique(ReverseMismatches),
                          ReverseInsertions = unique(ReverseInsertions),
                          ReverseDeletions = unique(ReverseDeletions),
                          NumberForwardMismatches = unique(NumberForwardMismatches),
                          NumberForwardInsertions = unique(NumberForwardInsertions),
                          NumberForwardDeletions = unique(NumberForwardDeletions),
                          NumberReverseMismatches = unique(NumberReverseMismatches),
                          NumberReverseInsertions = unique(NumberReverseInsertions),
                          NumberReverseDeletions = unique(NumberReverseDeletions)) %>%
                ungroup() %>%
                mutate(Marker = marker,
                       MotifLength = motifLength_i,
                       Type = type_i,
                       BasePairs = nchar(Region),
                       Allele = BasePairs / MotifLength) %>%
                select(Marker,
                       BasePairs,
                       Allele,
                       Type,
                       MotifLength,
                       ForwardFlank,
                       Region,
                       ReverseFlank,
                       Coverage,
                       AggregateQuality,
                       Quality,
                       ForwardMismatches,
                       ForwardInsertions,
                       ForwardDeletions,
                       ReverseMismatches,
                       ReverseInsertions,
                       ReverseDeletions,
                       NumberForwardMismatches,
                       NumberForwardInsertions,
                       NumberForwardDeletions,
                       NumberReverseMismatches,
                       NumberReverseInsertions,
                       NumberReverseDeletions) %>%
                arrange(Allele, -Coverage)
        }
        else {
            stringCoverageQuality <-
                cbind(matchedFlanks$trimmed,
                      Quality = matchedFlanks$trimmedQuality$Region) %>%
                group_by(ForwardFlank, Region, ReverseFlank) %>%
                summarise(Coverage = n(),
                          AggregateQuality = .aggregateQuality(Quality),
                          Quality = list(as.character(Quality))) %>%
                ungroup() %>%
                mutate(Marker = marker,
                       MotifLength = motifLength_i,
                       Type = type_i,
                       BasePairs = nchar(Region),
                       Allele = BasePairs / MotifLength) %>%
                select(Marker,
                       BasePairs,
                       Allele,
                       Type,
                       MotifLength,
                       ForwardFlank,
                       Region,
                       ReverseFlank,
                       Coverage,
                       AggregateQuality,
                       Quality) %>%
                arrange(Allele, -Coverage)
        }

        if (control$simpleReturn) {
            if (!control$additionalFlags) {
                stringCoverageQuality <- stringCoverageQuality %>%
                    group_by(Marker, BasePairs, Allele, Type, MotifLength, Region) %>%
                    summarise(Coverage = sum(Coverage),
                              AggregateQuality = .aggregateQuality(AggregateQuality),
                              Quality = list(unlist(Quality))) %>%
                    ungroup()
            }
            else {
                stringCoverageQuality <- stringCoverageQuality %>%
                    mutate(C = Coverage) %>%
                    group_by(Marker, BasePairs, Allele, Type, MotifLength, Region) %>%
                    summarise(ForwardFlankList = list(unique(ForwardFlank)),
                              ReverseFlankList = list(unique(ReverseFlank)),
                              Coverage = sum(Coverage),
                              AggregateQuality = .aggregateQuality(AggregateQuality),
                              Quality = list(unlist(Quality)),
                              ForwardMismatches = list(ForwardMismatches),
                              ForwardInsertions = list(ForwardInsertions),
                              ForwardDeletions = list(ForwardDeletions),
                              ReverseMismatches = list(ReverseMismatches),
                              ReverseInsertions = list(ReverseInsertions),
                              ReverseDeletions = list(ReverseDeletions),
                              NumberForwardMismatches = list(NumberForwardMismatches),
                              NumberForwardInsertions = list(NumberForwardInsertions),
                              NumberForwardDeletions = list(NumberForwardDeletions),
                              NumberReverseMismatches = list(NumberReverseMismatches),
                              NumberReverseInsertions = list(NumberReverseInsertions),
                              NumberReverseDeletions = list(NumberReverseDeletions),
                              CoverageFraction = list(C / Coverage)) %>%
                    ungroup() %>%
                    left_join(flankingRegions %>% select(Marker, ForwardFlank, ReverseFlank), by = "Marker")
            }
        }

        return(stringCoverageQuality)
    }, mc.cores = control$numberOfThreads)

    names(alleles) <- names(extractedReads)
    class(alleles) <- "stringCoverageList"
    return(alleles)
}

#' Get string coverage STR identified objects.
#'
#' \code{stringCoverage} takes an extractedReadsList-object and finds the coverage of every unique string for every marker in the provided list.
#'
#' @param extractedReadsListObject An extractedReadsList-object, created using the \link{identifySTRRegions}-function.
#' @param flankingRegions containing marker ID/name, the directly adjacent forward and reverse flanking regions, used for identification.
#' @param control an \link{stringCoverage.control}-object.
#' @return Returns a list, with an element for every marker in extractedReadsList-object, each element contains the string coverage of all unique strings of a given marker.
#' @example inst/examples/stringCoverageAggregated.R
setGeneric("stringCoverage", signature = "extractedReadsListObject",
           function(extractedReadsListObject, flankingRegions, control = stringCoverage.control())
               standardGeneric("stringCoverage")
)

#' Get string coverage STR identified objects.
#'
#' \code{stringCoverage} takes an extractedReadsList-object and finds the coverage of every unique string for every marker in the provided list.
#'
#' @param extractedReadsListObject an extractedReadsList-object, created using the \link{identifySTRRegions}-function.
#' @param flankingRegions containing marker ID/name, the directly adjacent forward and reverse flanking regions, used for identification.
#' @param control an \link{stringCoverage.control}-object.
#' @return Returns a list, with an element for every marker in extractedReadsList-object, each element contains the string coverage of all unique strings of a given marker.
#' @example inst/examples/stringCoverageAggregated.R
setMethod("stringCoverage", "extractedReadsList",
          function(extractedReadsListObject, flankingRegions, control = stringCoverage.control())
              .extractedReadsList.stringCoverage(extractedReadsListObject, flankingRegions, control)
)

#' Get string coverage STR identified objects.
#'
#' \code{stringCoverage} takes an extractedReadsList-object and finds the coverage of every unique string for every marker in the provided list.
#'
#' @param extractedReadsListObject an extractedReadsList-object, created using the \link{identifySTRRegions}-function.
#' @param flankingRegions containing marker ID/name, the directly adjacent forward and reverse flanking regions, used for identification.
#' @param control an \link{stringCoverage.control}-object.
#' @return Returns a list, with an element for every marker in extractedReadsList-object, each element contains the string coverage of all unique strings of a given marker.
#' @example inst/examples/stringCoverageAggregated.R
setMethod("stringCoverage", "extractedReadsListReverseComplement",
          function(extractedReadsListObject, flankingRegions, control = stringCoverage.control())
              .extractedReadsList.stringCoverage(extractedReadsListObject, flankingRegions, control)
)

#' Get string coverage STR identified objects.
#'
#' \code{stringCoverage} takes an extractedReadsList-object and finds the coverage of every unique string for every marker in the provided list.
#'
#' @param extractedReadsListObject an extractedReadsList-object, created using the \link{identifySTRRegions}-function.
#' @param flankingRegions containing marker ID/name, the directly adjacent forward and reverse flanking regions, used for identification.
#' @param control an \link{stringCoverage.control}-object.
#' @return Returns a list, with an element for every marker in extractedReadsList-object, each element contains the string coverage of all unique strings of a given marker.
#' @example inst/examples/stringCoverageAggregated.R
setMethod("stringCoverage", "extractedReadsListCombined",
          function(extractedReadsListObject, flankingRegions, control = stringCoverage.control())
              .extractedReadsList.stringCoverage(extractedReadsListObject, flankingRegions, control)
)

#' Get string coverage STR identified objects.
#'
#' \code{stringCoverage} takes an extractedReadsList-object and finds the coverage of every unique string for every marker in the provided list.
#'
#' @param extractedReadsListObject an extractedReadsList-object, created using the \link{identifySTRRegions}-function.
#' @param flankingRegions containing marker ID/name, the directly adjacent forward and reverse flanking regions, used for identification.
#' @param control an \link{stringCoverage.control}-object.
#' @return Returns a list, with an element for every marker in extractedReadsList-object, each element contains the string coverage of all unique strings of a given marker.
#' @example inst/examples/stringCoverageAggregated.R
setMethod("stringCoverage", "extractedReadsListNonCombined",
          function(extractedReadsListObject, flankingRegions, control = stringCoverage.control())
              stop("'stringCoverage' not implemented for 'extractedReadsListNReveseComplementList'. Use lapply on the two elements on the list.")
)

#' Genotype list
#'
#' A reduced stringCoverageList restricted to the identified genotypes.
setClass("genotypeIdentifiedList")

#' Noise list
#'
#' Creates a flag to the sequences in a stringCoverageList which cloud be classified as noise.
setClass("noiseIdentifiedList")


.stringCoverageList.NoiseGenotype <- function(stringCoverageListObject, colBelief = "Coverage",
                                              thresholdSignal = 0, thresholdHeterozygosity = 0,
                                              thresholdAbsoluteLowerLimit = 1,
                                              trueGenotype = NULL, identified = "genotype") {
    if (length(thresholdSignal) == 1L) {
        if(thresholdSignal < 1 & thresholdSignal > 0) {
            thresholdSignal <- unlist(lapply(stringCoverageListObject, function(s) thresholdSignal * max(s[, colBelief])))
            thresholdSignal <- sapply(seq_along(thresholdSignal), function(s) max(thresholdSignal[s], thresholdAbsoluteLowerLimit))
        }
        else {
            thresholdSignal <- rep(max(thresholdSignal, thresholdAbsoluteLowerLimit), length(stringCoverageListObject))
        }
    }

    if (length(thresholdHeterozygosity) == 1L) {
        thresholdHeterozygosity <- rep(thresholdHeterozygosity, length(stringCoverageListObject))
    }

    if (length(thresholdSignal) != length(stringCoverageListObject)) {
        stop("'stringCoverageListObject' and 'thresholdSignal' must have the same length.")
    }

    if (length(thresholdHeterozygosity) != length(stringCoverageListObject)) {
        stop("'stringCoverageListObject' and 'thresholdHeterozygosity' must have the same length.")
    }

    colsize_all <- sapply(stringCoverageListObject, function(xx) ifelse(is.null(xx), NA, dim(xx)[2]))
    colsize <- unique(colsize_all[!is.na(colsize_all)])

    res <- vector("list", length(stringCoverageListObject))
    for (i in seq_along(stringCoverageListObject)) {
        stringCoverage_i <- stringCoverageListObject[[i]]
        if (is.null(trueGenotype)) {
            belief <- unname(unlist(stringCoverage_i[, colBelief]))

            if (length(belief) > 0) {
                beliefOrder <- order(belief, decreasing = TRUE)[seq_len(min(2, length(belief)))]
                beliefMax <- max(belief)
                beliefKeepers <- beliefOrder[(belief[beliefOrder] > thresholdSignal[i]) & (belief[beliefOrder] > thresholdHeterozygosity[i] * beliefMax)]
            }
            else {
                beliefKeepers <- NULL
            }
        }
        else {
            beliefKeepers <- which(stringCoverage_i$Region %in% trueGenotype[[i]])
        }

        if (length(beliefKeepers) > 0) {
            res_i <- stringCoverage_i[beliefKeepers, ] %>% mutate(Indices = beliefKeepers)
        }
        else {
            res_i <- as_tibble(data.frame(matrix(ncol = colsize + 1, nrow = 0)))
            colnames(res_i) <- c("Marker", "BasePairs", "Allele", "Type", "MotifLength",
                                 "Region", "Coverage", "AggregateQuality", "Quality", "Indices")
        }

        res[[i]] <- res_i
    }

    names(res) <- names(stringCoverageListObject)
    class(res) <- if(tolower(identified) == "genotype") "genotypeIdentifiedList" else if(tolower(identified) == "noise") "noiseIdentifiedList"
    return(res)
}

#' Assigns genotype.
#'
#' \code{getGenotype} takes an stringCoverageList-object, assumes the sample is a reference file and assings a genotype, based on a heterozygote threshold, for every marker in the provided list.
#'
#' @param stringCoverageListObject an stringCoverageList-object, created using the \link{stringCoverage}-function.
#' @param colBelief the name of the coloumn used for identification.
#' @param thresholdSignal threshold applied to the signal (generally the coverage) of every string.
#' @param thresholdHeterozygosity threshold used to determine whether a marker is hetero- or homozygous.
#' @param thresholdAbsoluteLowerLimit a lower limit on the coverage for it to be called as an allele.
#'
#' @return Returns a list, with an element for every marker in stringCoverageList-object, each element contains the genotype for a given marker.
#' @example inst/examples/getGenotype.R
setGeneric("getGenotype", signature = "stringCoverageListObject",
           function(stringCoverageListObject, colBelief = "Coverage", thresholdSignal = 0, thresholdHeterozygosity = 0.35, thresholdAbsoluteLowerLimit = 1)
               standardGeneric("getGenotype")
)

#' Assigns genotype.
#'
#' \code{getGenotype} takes an stringCoverageList-object, assumes the sample is a reference file and assings a genotype, based on a heterozygote threshold, for every marker in the provided list.
#'
#' @param stringCoverageListObject an stringCoverageList-object, created using the \link{stringCoverage}-function.
#' @param colBelief the name of the coloumn used for identification.
#' @param thresholdSignal threshold applied to the signal (generally the coverage) of every string.
#' @param thresholdHeterozygosity threshold used to determine whether a marker is hetero- or homozygous.
#' @param thresholdAbsoluteLowerLimit a lower limit on the coverage for it to be called as an allele.
#'
#' @return Returns a list, with an element for every marker in stringCoverageList-object, each element contains the genotype for a given marker.
#' @example inst/examples/getGenotype.R
setMethod("getGenotype", "stringCoverageList",
          function(stringCoverageListObject, colBelief = "Coverage", thresholdSignal = 0, thresholdHeterozygosity = 0.35, thresholdAbsoluteLowerLimit = 1)
              .stringCoverageList.NoiseGenotype(stringCoverageListObject, colBelief, thresholdSignal, thresholdHeterozygosity,
                                                thresholdAbsoluteLowerLimit, NULL, "genotype")
)

#' Idenfities the noise.
#'
#' \code{identifyNoise} takes an stringCoverageList-object and identifies the noise based on a signal threshold for every marker in the provided list.
#'
#' @param stringCoverageListObject an stringCoverageList-object, created using the \link{stringCoverage}-function.
#' @param colBelief the name of the coloumn used for identification.
#' @param thresholdSignal threshold applied to the signal (generally the coverage) of every string.
#'
#' @return Returns a list, with an element for every marker in stringCoverageList-object, each element contains the genotype for a given marker.
#' @example inst/examples/getNoise.R
setGeneric("identifyNoise", signature = "stringCoverageListObject",
           function(stringCoverageListObject, colBelief = "Coverage", thresholdSignal = 0.01)
               standardGeneric("identifyNoise")
)

#' Idenfities the noise.
#'
#' \code{identifyNoise} takes an stringCoverageList-object and identifies the noise based on a signal threshold for every marker in the provided list.
#'
#' @param stringCoverageListObject an stringCoverageList-object, created using the \link{stringCoverage}-function.
#' @param colBelief the name of the coloumn used for identification.
#' @param thresholdSignal threshold applied to the signal (generally the coverage) of every string.
#'
#' @return Returns a list, with an element for every marker in stringCoverageList-object, each element contains the genotype for a given marker.
#' @example inst/examples/getNoise.R
setMethod("identifyNoise", "stringCoverageList",
          function(stringCoverageListObject, colBelief = "Coverage", thresholdSignal = 0.01)
              .stringCoverageList.NoiseGenotype(stringCoverageListObject, colBelief, thresholdSignal, 0, 0, NULL, "noise")
)


.noiseGenotypeIdentified.stringCoverageList.merge <- function(stringCoverageListObject, noiseGenotypeIdentifiedListObject, identified = "genotype") {
    stringCoverageListObjectMerged <- vector("list", length(stringCoverageListObject))
    indValue <- if(tolower(identified) == "genotype") TRUE else if(tolower(identified) == "noise") FALSE
    indCol <- if(tolower(identified) == "genotype") "AlleleCalled" else if(tolower(identified) == "noise") "Noise"

    for(i in seq_along(stringCoverageListObject)) {
        stringCoverageListObject_i <- stringCoverageListObject[[i]]

        if (is.null(stringCoverageListObject_i)) {
            next
        }

        stringCoverageListObjectMerged[[i]] <- stringCoverageListObject_i %>% mutate(tempName = !indValue, FLAGMoreThanTwoAlleles = FALSE)

        if (!is.null(noiseGenotypeIdentifiedListObject[[i]]) && nrow(noiseGenotypeIdentifiedListObject[[i]]) > 0L) {
            stringCoverageListObjectMerged[[i]]$tempName[noiseGenotypeIdentifiedListObject[[i]]$Indices] <- indValue
        }

        if (nrow(noiseGenotypeIdentifiedListObject[[i]]) > 2L) {
            stringCoverageListObjectMerged[[i]]$FLAGMoreThanTwoAlleles <- TRUE
        }

        names(stringCoverageListObjectMerged[[i]]) <- gsub("tempName", indCol, names(stringCoverageListObjectMerged[[i]]))
    }

    names(stringCoverageListObjectMerged) <- names(stringCoverageListObject)
    class(stringCoverageListObjectMerged) <- if(tolower(identified) == "genotype") "stringCoverageGenotypeList" else if(tolower(identified) == "noise") "stringCoverageNoiseList"
    return(stringCoverageListObjectMerged)
}

#' Merge genotypeIdentifiedList and stringCoverageList.
#'
#' \code{mergeGenotypeStringCoverage} merges genotypeIdentifiedList-objects and stringCoverageList-objects.
#'
#' @param stringCoverageListObject a stringCoverageList-object, created using the \link{stringCoverage}-function.
#' @param noiseGenotypeIdentifiedListObject a noiseGenotypeIdentifiedList-object, created using the \link{getGenotype}-function.
#'
#' @return Returns a list, with an element for every marker in extractedReadsList-object, each element contains the string coverage of all unique strings of a given marker.
#' @example inst/examples/mergeLists.R
setGeneric("mergeGenotypeStringCoverage", signature = "noiseGenotypeIdentifiedListObject",
           function(stringCoverageListObject, noiseGenotypeIdentifiedListObject)
               standardGeneric("mergeGenotypeStringCoverage")
)

#' Merge genotypeIdentifiedList and stringCoverageList.
#'
#' \code{mergeGenotypeStringCoverage} merges genotypeIdentifiedList-objects and stringCoverageList-objects.
#'
#' @param stringCoverageListObject a stringCoverageList-object, created using the \link{stringCoverage}-function.
#' @param noiseGenotypeIdentifiedListObject a noiseGenotypeIdentifiedList-object, created using the \link{getGenotype}-function.
#'
#' @return Returns a list, with an element for every marker in extractedReadsList-object, each element contains the string coverage of all unique strings of a given marker.
#' @example inst/examples/mergeLists.R
setMethod("mergeGenotypeStringCoverage", "genotypeIdentifiedList",
          function(stringCoverageListObject, noiseGenotypeIdentifiedListObject)
              .noiseGenotypeIdentified.stringCoverageList.merge(stringCoverageListObject, noiseGenotypeIdentifiedListObject, identified = "genotype")
)

#' Merge noiseIdentifiedList and stringCoverageList.
#'
#' \code{mergeNoiseStringCoverage} merges noiseIdentifiedList-objects and stringCoverageList-objects.
#'
#' @param stringCoverageListObject a stringCoverageList-object, created using the \link{stringCoverage}-function.
#' @param noiseGenotypeIdentifiedListObject a noiseGenotypeIdentifiedList-object, created using the \link{identifyNoise}-function.
#'
#' @return Returns a list, with an element for every marker in extractedReadsList-object, each element contains the string coverage of all unique strings of a given marker.
#' @example inst/examples/mergeLists.R
setGeneric("mergeNoiseStringCoverage", signature = "noiseGenotypeIdentifiedListObject",
           function(stringCoverageListObject, noiseGenotypeIdentifiedListObject)
               standardGeneric("mergeNoiseStringCoverage")
)

#' Merge noiseIdentifiedList and stringCoverageList.
#'
#' \code{mergeNoiseStringCoverage} merges noiseIdentifiedList-objects and stringCoverageList-objects.
#'
#' @param stringCoverageListObject a stringCoverageList-object, created using the \link{stringCoverage}-function.
#' @param noiseGenotypeIdentifiedListObject a noiseGenotypeIdentifiedList-object, created using the \link{identifyNoise}-function.
#'
#' @return Returns a list, with an element for every marker in extractedReadsList-object, each element contains the string coverage of all unique strings of a given marker.
#' @example inst/examples/mergeLists.R
setMethod("mergeNoiseStringCoverage", "noiseIdentifiedList",
          function(stringCoverageListObject, noiseGenotypeIdentifiedListObject)
              .noiseGenotypeIdentified.stringCoverageList.merge(stringCoverageListObject, noiseGenotypeIdentifiedListObject, identified = "noise")
)

#' Combined stringCoverage- and genotypeIdentifiedList
#'
#' Merges a stringCoverageList with a genotypeIdentifiedList.
setClass("stringCoverageGenotypeList")

#' Combined stringCoverage- and noiseIdentifiedList
#'
#' Merges a stringCoverageList with a noiseIdentifiedList
setClass("stringCoverageNoiseList")
