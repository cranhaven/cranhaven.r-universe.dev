#' @title Combine p-values using Fisher's
#' @description This function combines p-values based on Fisher's method.
#' This function is used internally by .combinePvalues.
#' @param pvals The vector of P-Values to be combined.
#' @return A combined P-Value
#' @details This function is used internally by .combinePvalues.
#' @importFrom stats qnorm pchisq
#' @noRd
.runFisher <- function(pvals) {
    pvals[pvals == 0] <- .Machine$double.eps
    p.value <- pchisq(-2 * sum(log(pvals)), df = 2 * length(pvals), lower.tail = FALSE)
    return(p.value)
}

#' @title Combine p-values using Stouffer's
#' @description This function combines p-values based on Stouffer's method.
#' This function is used internally by .combinePvalues.
#' @param pvals The vector of p-values to be combined.
#' @return A combined P-Value
#' @details This function is used internally by .combinePvalues.
#' @importFrom stats pnorm qnorm
#' @noRd
.runStouffer <- function(pvals) {
    pvals[pvals == 0] <- .Machine$double.eps
    p.value <- pnorm(sum(qnorm(pvals)) / sqrt(length(pvals)))
    return(p.value)
}

#' @title Combine p-values using addCLT
#' @description This function combines p-values based on addCLT method.
#' This function is used internally by .combinePvalues.
#' @param pvals The vector of P-Values to be combined.
#' @return A combined P-Value
#' @details This function is used internally by .combinePvalues.
#' @importFrom stats pnorm
#' @noRd
.runAddCLT <- function(pvals) {
    pvals <- pvals[!is.na(pvals)]
    pvals[pvals == 0] <- .Machine$double.eps
    n <- length(pvals)
    p.value <- 1
    if (n <= 20) {
        x <- sum(pvals)
        p.value <- 1 / factorial(n) * sum(sapply(0:floor(x), function(k) (-1)^k * choose(n, k) * (x - k)^(n)))
    }else {
        p.value <- pnorm(sum(pvals), n / 2, sqrt(n / 12), lower.tail = TRUE)
    }
    return(p.value)
}

#' @title Combine p-values using Geometric Mean
#' @description This function combines P-Values by computing their geometric mean.
#' This function is used internally by .combinePvalues.
#' @param pvals The vector of P-Values to be combined.
#' @return A combined P-Value
#' @details This function is used internally by .combinePvalues.
#' @noRd
.runGeoMean <- function(pvals) {
    pvals[pvals == 0] <- .Machine$double.eps
    p.value <- exp(mean(log(pvals)))
    return(p.value)
}

#' @title Perform Meta Analysis
#' @description This function performs meta analysis on multiple pathway analysis results.
#' @param PAResults A list of at least size two of data frames obtained from pathway analysis
#' @param method A method used to combine pathway analysis results, which can be "stouffer", "fisher", "addCLT", "geoMean", "minP", or "REML"
#' @return A dataframe of meta analysis results including the following columns:
#' \itemize{
#' \item{ID: The ID of pathway}
#' \item{name: The name of pathway}
#' \item{p.value: The meta p-value of pathway}
#' \item{pFDR: The adjusted meta p-value of pathway using Benjamini-Hochberg method}
#' \item{score: The combined score of pathway}
#' \item{normalizedScore: The combined normalized score of pathway}
#' \item{pathwaySize: The size of pathway}
#' }
#' @examples
#' \donttest{
#'
#' library(RCPA)
#' affyFgseaResult <- loadData("affyFgseaResult")
#' agilFgseaResult <- loadData("agilFgseaResult")
#' RNASeqFgseaResult <- loadData("RNASeqFgseaResult")
#'
#' metaPAResult <- RCPA::runPathwayMetaAnalysis(
#'     list(affyFgseaResult, agilFgseaResult, RNASeqFgseaResult),
#'     method = "stouffer"
#' )
#' }
#' @details This function performs meta-analysis on multiple pathway analysis results.
#' @importFrom dplyr %>% bind_rows mutate group_by summarise filter group_split select inner_join
#' @importFrom tidyr drop_na
#' @export
runPathwayMetaAnalysis <- function(PAResults, method = c("stouffer", "fisher", "addCLT", "geoMean", "minP", "REML")) {

    method <- match.arg(method)

    if (!.requirePackage("meta")){
        return(NULL)
    }

    if (length(PAResults) == 1) {
        stop("Meta analysis is valid for two or more studies.")
    }

    for (PAResult in PAResults) {
        if (is.null(PAResult)) {
            stop("There is null object in the input list.")
        }

        if (!all(c("ID", "p.value", "normalizedScore", "sampleSize") %in% colnames(PAResult))) {
            stop("All the dataframes in the input list must have ID, p.value, normalizedScore and sampleSize columns.")
        }
    }

    if (method != "REML"){
        combinePFunc <- switch(method,
                           fisher = .runFisher,
                           stouffer = .runStouffer,
                           minP = min,
                           addCLT = .runAddCLT,
                           geoMean = .runGeoMean)

        pvalRes <- PAResults %>%
            lapply(function(df) {
                df[, c("ID", "p.value", "normalizedScore")] %>% as.data.frame()
            }) %>%
            bind_rows() %>%
            drop_na() %>%
            mutate(
                left.p = ifelse(.$normalizedScore < 0, .$p.value, 1 - .$p.value),
                right.p = ifelse(.$normalizedScore > 0, .$p.value, 1 - .$p.value)
            ) %>%
            group_by(.data$ID) %>%
            summarise(
                left.p = combinePFunc(.data$left.p),
                right.p = combinePFunc(.data$right.p),
                n = length(.data$ID)
            ) %>%
            filter(.data$n == length(PAResults))
    }

    metagenRes <- PAResults %>%
        lapply(function(df) {
            df[, c("ID", "normalizedScore", "p.value", "sampleSize")] %>% as.data.frame()
        }) %>%
        bind_rows() %>%
        group_by(.data$ID) %>%
        group_split() %>%
        lapply(function(dat) {
            if (nrow(dat) < length(PAResults)) {
                return(NULL)
            }

            res <- try({
                meta::metagen(data = dat,
                        studlab = dat$ID,
                        TE = dat$normalizedScore,
                        # seTE = logFCSE,
                        pval = dat$p.value,
                        sm = "SMD",
                        method.tau = "REML",
                        hakn = TRUE,
                        n.e = dat$sampleSize
                ) }, silent = TRUE)
            if (inherits(res, "try-error")) {
                res <- NULL
            }

            return(res)
        }) %>%
        do.call(what = rbind)


    metaResult <- metagenRes[, c("studlab", "TE.fixed", "seTE.fixed", "pval.fixed")] %>%
        as.data.frame() %>%
        mutate(
            ID = .data$studlab %>% sapply(`[`, 1),
            p.value = unlist(.data$pval.fixed),
            score = unlist(.data$TE.fixed),
            normalizedScore = .data$score
        ) %>%
        mutate(
            pFDR = p.adjust(.data$p.value, method = "fdr")
        )

    if (method != "REML"){
        metaResult <- metaResult %>%
            select("ID", "score", "normalizedScore") %>%
            inner_join(pvalRes, by = "ID") %>%
            mutate(
                p.value = ifelse(.$normalizedScore < 0, .$left.p, .$right.p),
                pFDR = p.adjust(.data$p.value, method = "fdr")
            )
    }

    metaResult$name <- PAResults[[1]][match(metaResult$ID, PAResults[[1]]$ID), "name"]
    metaResult$pathwaySize <- PAResults[[1]][match(metaResult$ID, PAResults[[1]]$ID), "pathwaySize"]
    
    metaResult <- metaResult[order(metaResult$p.value),]
    
    rownames(metaResult) <- NULL
    
    metaResult[, c("ID", "name", "p.value", "pFDR", "score", "normalizedScore", "pathwaySize")]
    
    
}


#' @title Combine DE analysis results
#' @description This function performs mata analysis on multiple DE analysis results.
#' @param DEResults A list of dataframes containing DE analysis results.
#' Each dataframe must have ID, p.value, logFC and logFCSE columns.
#' @param method The method to combine p-values. It can be one of "fisher", "stouffer", "geoMean", "addCLT", "minP", or "REML".
#' @return A dataframe containing combined DE analysis results.
#' The dataframe has ID, p.value, pDFR, logFC, and logFCSE columns.
#' @examples
#' \donttest{
#' library(RCPA)
#' library(SummarizedExperiment)
#' affyDEExperiment <- loadData("affyDEExperiment")
#' agilDEExperiment <- loadData("agilDEExperiment")
#' RNASeqDEExperiment <- loadData("RNASeqDEExperiment")
#'
#' metaDEResult <- RCPA::runDEMetaAnalysis(list(
#'     rowData(affyDEExperiment)[1:1000,],
#'     rowData(agilDEExperiment)[1:1000,],
#'     rowData(RNASeqDEExperiment)[1:1000,]
#' ), method = "stouffer")
#' 
#' }
#' @importFrom dplyr %>% bind_rows group_by summarize mutate
#' @importFrom stats p.adjust
#' @importFrom tidyr drop_na
#' @importFrom SummarizedExperiment rowData
#' @export
runDEMetaAnalysis <- function(DEResults, method = c("stouffer", "fisher", "addCLT", "geoMean", "minP", "REML")) {

    method <- match.arg(method)

    if (!.requirePackage("meta")){
        return(NULL)
    }

    if (length(DEResults) == 1) {
        stop("Meta analysis is valid for two or more studies.")
    }

    for (DEResult in DEResults) {
        if (is.null(DEResult)) {
            stop("There is null object in the input list.")
        }

        if (!all(c("ID", "p.value", "logFC", "logFCSE", "sampleSize") %in% colnames(DEResult))) {
            stop("All the dataframes in the input list must have p.value, logFC, logFCSE, and sampleSize columns.")
        }
    }

    if (method != "REML"){
        combinePFunc <- switch(method,
                           fisher = .runFisher,
                           stouffer = .runStouffer,
                           minP = min,
                           addCLT = .runAddCLT,
                           geoMean = .runGeoMean
    )

    pvalRes <- DEResults %>%
        lapply(function(df) {
            df[, c("ID", "p.value", "logFC")] %>% as.data.frame()
        }) %>%
        bind_rows() %>%
        drop_na() %>%
        mutate(
            left.p = ifelse(.$logFC < 0, .$p.value, 1 - .$p.value),
            right.p = ifelse(.$logFC > 0, .$p.value, 1 - .$p.value)
        ) %>%
        group_by(.data$ID) %>%
        summarise(
            left.p = combinePFunc(.data$left.p),
            right.p = combinePFunc(.data$right.p),
            n = length(.data$ID)
        ) %>%
        filter(.data$n == length(DEResults))
    }

    metagenRes <- DEResults %>%
        lapply(function(df) {
            df[, c("ID", "logFC", "logFCSE", "p.value", "sampleSize")] %>% as.data.frame()
        }) %>%
        bind_rows() %>%
        group_by(.data$ID) %>%
        group_split() %>%
        lapply(function(dat) {
            if (nrow(dat) < length(DEResults)) {
                return(NULL)
            }

            res <- try({
                meta::metagen(data = dat,
                        studlab = dat$ID,
                        TE = dat$logFC,
                        # seTE = logFCSE,
                        pval = dat$p.value,
                        sm = "SMD",
                        method.tau = "REML",
                        hakn = TRUE,
                        n.e = dat$sampleSize
                ) }, silent = TRUE)
            if (inherits(res, "try-error")) {
                res <- NULL
            }

            return(res)
        }) %>%
        do.call(what = rbind)


    metaResult <- metagenRes[, c("studlab", "TE.fixed", "seTE.fixed", "pval.fixed")] %>%
        as.data.frame() %>%
        mutate(
            ID = .data$studlab %>% sapply(`[`, 1),
            p.value = unlist(.data$pval.fixed),
            logFC = unlist(.data$TE.fixed),
            logFCSE = unlist(.data$seTE.fixed)
        ) %>%
        mutate(
            pFDR = p.adjust(.data$p.value, method = "fdr")
        ) %>%
        select("ID", "p.value", "pFDR", "logFC", "logFCSE") %>%
        drop_na() %>%
        as.data.frame()

    if (method != "REML"){
        metaResult <- metaResult %>%
            select("ID", "logFC", "logFCSE") %>%
            inner_join(pvalRes, by = "ID") %>%
            mutate(
                p.value = ifelse(.$logFC < 0, .$left.p, .$right.p),
                pFDR = p.adjust(.data$p.value, method = "fdr")
            ) %>%
            select("ID", "p.value", "pFDR", "logFC", "logFCSE")
    }
    
    metaResult <- metaResult[order(metaResult$p.value),]
    rownames(metaResult) <- NULL
    return(metaResult)
}