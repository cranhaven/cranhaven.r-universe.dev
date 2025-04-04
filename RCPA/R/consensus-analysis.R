#' @title Consensus Analysis Using Wighted Mean
#' @description This function performs consensus analysis based on computing weighted average of z-values.
#' This function is used internally by runConsensusAnalysis.
#' @param resultsDFs A list of at least length two from enrichment analysis results.
#' @param weightsLst A vector of integer values.
#' Each element shows the corresponding input result weight.
#' When selected method is weightedAvg this parameter needs to be specified.
#' If it is null all the weights are considered as equal.
#' @param useFDR A logical parameter, indicating if adjusted p-values should be used.
#' @return A dataframe of consensus analysis results
#' @details This function is used internally by runConsensusAnalysis.
#' @importFrom dplyr %>%
#' @importFrom tidyr drop_na
#' @importFrom stats qnorm pnorm weighted.mean
#' @noRd
.runWeightedMean <- function(resultsDFs, weightsLst, useFDR){
    if(length(resultsDFs) != length(weightsLst)){
        stop("The length of input list of results must be equal to the length of weights vector!")
    }

    study.names <- resultsDFs %>% names()

    resultsDFs <- 1:length(resultsDFs) %>% lapply(function(i){
        resultsDFs[[i]]$weight <- rep(weightsLst[i], nrow(resultsDFs[[i]]))
        resultsDFs[[i]]
    }) %>% `names<-`(study.names)

    allResults <- resultsDFs %>% do.call(what = rbind)

    consensusResults <- allResults %>% group_by(.data$ID) %>% group_split() %>% lapply(function (data){
        if(useFDR == TRUE){
            data$zscore <- data$pFDR %>% qnorm()
        }else{
            data$zscore <- data$p.value %>% qnorm()
        }

        consensusPval <- weighted.mean(data$zscore, data$weight) %>% pnorm()
        data.frame(
            ID = data$ID[1],
            p.value = consensusPval,
            name = data$name[1],
            pathwaySize = data$pathwaySize[1],
            stringsAsFactors = FALSE
        )
    }) %>% do.call(what = rbind)

    consensusResults
}

#' @title Generating Rank List of Enrichment Results
#' @description This function generates a list of lists.
#' Each list includes pathways IDs in order of their importance.
#' This function is used internally by runConsensusAnalysis.
#' @param resultsDFs A list of at least length two from enrichment analysis results.
#' @param rankParam A character parameter which specifies how the input results should be ranked.
#' @return A list of lists each containing ordered pathways IDs.
#' @details This function is used internally by runConsensusAnalysis.
#' @importFrom dplyr %>%
#' @importFrom tidyr drop_na
#' @noRd
.runRankPathways <- function (resultsDFs, rankParam){
    rankedList <- NULL

    if(rankParam == "normalizedScore"){
        rankedList <- resultsDFs %>% lapply(function (data){
            data[order(abs(data$normalizedScore), decreasing = TRUE),] %>% `[[`("ID")
        })
    }else if(rankParam == "pFDR"){
        rankedList <- resultsDFs %>% lapply(function (data){
            data[order(data$pFDR, decreasing = FALSE),] %>% `[[`("ID")
        })
    }else{
        rankedList <- resultsDFs %>% lapply(function (data){
            data[order(abs(data$normalizedScore), data$pFDR, decreasing = c(TRUE, FALSE)),] %>% `[[`("ID")
        })
    }

    rankedList
}

#' @title Perform Consensus Analysis
#' @description This function performs consensus analysis using two methods.
#' These methods are weighted.mean and RRA.
#' @param PAResults A list of at least length two from enrichment analysis results.
#' @param method The consensus analsyis method. This can be either weighted.mean or RRA.
#' @param weightsList A vector of integer values.
#' Each element shows the corresponding input result weight.
#' When selected method is weighted.mean this parameter needs to be specified.
#' If it is null all the weights are considered as equal.
#' @param useFDR A logical parameter, indicating if adjusted p-values should be used.
#' @param rank.by An string parameter which specifies how the input results should be ranked.
#' This parameter is used when the selected method is RRA.
#' @param backgroundSpace A list of lists with the same length as PAResults.
#' Each list contains underlying space (set of pathways) for each input dataframe in PAResults.
#' This parameter is optional.
#' NULL means all input dataframes share a common space.
#' So the union pathways of all input dataframes is taken into account.
#' @return A dataframe of consensus analysis result, which contains the following columns:
#' \itemize{
#' \item{ID: The ID of pathway}
#' \item{p.value: The p-value of pathway}
#' \item{pFDR: The adjusted p-value using Benjamini-Hochberg method}
#' \item{name: The name of pathway}
#' \item{pathwaySize: The size of pathway}
#' }
#' @examples
#' \donttest{
#'
#' library(RCPA)
#'
#' affyFgseaResult <- loadData("affyFgseaResult")
#' agilFgseaResult <- loadData("agilFgseaResult")
#' RNASeqFgseaResult <- loadData("RNASeqFgseaResult")
#'
#' consensusPAResult <- RCPA::runConsensusAnalysis(
#'     list(affyFgseaResult, agilFgseaResult, RNASeqFgseaResult),
#'     method = "weightedZMean"
#' )
#' 
#' print(head(consensusPAResult))
#'
#' }
#' @importFrom dplyr %>%
#' @importFrom RobustRankAggreg rankMatrix aggregateRanks
#' @export
runConsensusAnalysis <- function(PAResults,
                               method = c("weightedZMean", "RRA"),
                               weightsList = NULL,
                               useFDR = TRUE,
                               rank.by = c("normalizedScore", "pFDR", "both"),
                               backgroundSpace = NULL
) {
    method <- match.arg(method)

    if(is.null(PAResults)){
        stop("There is no study to be integrated!")
    }

    if(length(PAResults) < 2){
        stop("Number of studies to perform consensus analysis should be at least two!")
    }

    commonPathways <- Reduce(intersect, lapply(PAResults, function (data) data$ID))

    if(length(commonPathways) == 0){
        stop("There is no common pathways among input data!")
    }

    commonResults <- lapply(PAResults, function(data) data[data$ID %in% commonPathways,])

    commonResults <- commonResults[!is.na(commonResults)]
    commonResults <- commonResults[!is.null(commonResults)]

    if(length(commonResults) < 2){
        stop("After intersecting the results, there is less than two results to be analyzed!")
    }

    result <- NULL

    if(method == "weightedZMean"){
        if(is.null(weightsList)){
            weightsList <- rep(1, length(commonResults))
        }

        result <- .runWeightedMean(commonResults, weightsList, useFDR)

        #result$pFDR <- ifelse(useFDR == TRUE, result$p.value, p.adjust(result$p.value, method = "fdr"))

        if(useFDR == TRUE){
            result$pFDR <- result$p.value
        }else{
            result$pFDR <- p.adjust(result$p.value, method = "fdr")
        }

    }else{
        if(is.null(rank.by)){
            stop("For RRA method, please specify the type of ranking.")
        }

        rank.by <- match.arg(rank.by)
        spaceSet <- NULL

        rankedPathwaysList <- .runRankPathways(PAResults, rank.by)

        if(!is.null(backgroundSpace)){
            backgroundSpace <- backgroundSpace[!is.na(backgroundSpace)]

            if(length(backgroundSpace) != length(PAResults)){
                stop("In the case of specifying backgroundSpace, the length of backgroundSpace and PAResults must be equal!")
            }
            spaceSet <- backgroundSpace %>% unlist() %>% unique()

        }else{
            spaceSet <- rankedPathwaysList %>% unlist() %>% unique()
        }

        r = RobustRankAggreg::rankMatrix(rankedPathwaysList, N = length(spaceSet))

        result = RobustRankAggreg::aggregateRanks(rmat = r, full = TRUE, method = "RRA")
        colnames(result) <- c("ID", "p.value")

        allData <- PAResults %>% do.call(what = rbind)

        result$pFDR <- p.adjust(result$p.value, method = "fdr")
        result$name <- allData[match(result$ID, allData$ID), c("name")]
        result$pathwaySize <- allData[match(result$ID, allData$ID), c("pathwaySize")]
    }

    if(is.null(result)){
        stop("There is an error in performing consensus analysis!")
    }
    result <- result[order(result$p.value),]
    rownames(result) <- NULL
    return(result)
}