#' @title Plot Venn diagram from multiple DE Analysis results
#' @description Plot a Venn diagram from multiple DE Analysis results.
#' @param DEResults A list of data frames with the results of DE analysis.
#' @param pThreshold The p-value threshold to determine if a gene is differentially expressed.
#' @param useFDR Use the FDR adjusted p-value instead of the raw p-value.
#' @param stat The additional statistics column to use for filtering differentially expressed genes.
#' @param statThreshold The absolute value of the statistic threshold to use for filtering differentially expressed genes.
#' Default is 0, which means no filtering.
#' @param topToList The number of common DE genes that are used to annotate the plot
#' @return A ggplot2 object.
#' @examples
#' \donttest{
#' library(RCPA)
#' library(SummarizedExperiment)
#'
#' affyDEExperiment <- loadData("affyDEExperiment")
#' agilDEExperiment <- loadData("agilDEExperiment")
#' RNASeqDEExperiment <- loadData("RNASeqDEExperiment")
#'
#' DEResults <- list(
#'     "Affymetrix - GSE5281" = rowData(affyDEExperiment),
#'     "Agilent - GSE61196"   = rowData(agilDEExperiment),
#'     "RNASeq - GSE153873"   = rowData(RNASeqDEExperiment)
#' )
#' 
#' DEResultUps <- lapply(DEResults, function(df) df[!is.na(df$logFC) & df$logFC > 0, ])
#' 
#' DEResultDowns <- lapply(DEResults, function(df) df[!is.na(df$logFC) & df$logFC < 0, ])
#' 
#' if (require("ggvenn", quietly = TRUE)){
#' p1 <- RCPA::plotVennDE(DEResults) + 
#'         ggplot2::ggtitle("All DE Genes")
#' p2 <- RCPA::plotVennDE(DEResultUps) +
#'         ggplot2::ggtitle("Up-regulated DE Genes")
#' p3 <- RCPA::plotVennDE(DEResultDowns) + 
#'         ggplot2::ggtitle("Down-regulated DE Genes")
#' }
#'
#' }
#' @importFrom ggplot2 scale_fill_gradient theme layer_scales
#' @importFrom dplyr %>% filter
#' @importFrom scales trans_new
#' @export
plotVennDE <- function(DEResults, pThreshold = 0.05, useFDR = TRUE, stat = "logFC", statThreshold = 0, topToList = 10) {

    if (length(DEResults) < 2) {
        stop("The number of DE results must be at least 2.")
    }

    for (DERes in DEResults) {
        if (useFDR && !("pFDR" %in% colnames(DERes))) {
            stop("The FDR adjusted p-value column is not in the results data frame.")
        } else {
            if (!("p.value" %in% colnames(DERes))) {
                stop("The p.value column is not in the results data frame.")
            }
        }

        if (!stat %in% colnames(DERes)) {
            stop("The statistic column is not in the results data frame.")
        }
    }

    plotDat <- lapply(DEResults, function(DERes) {
        data.frame(DERes) %>%
            filter(
                abs(.data[[stat]]) > statThreshold & (
                    if (useFDR) {
                        .data$pFDR < pThreshold
                    } else {
                        .data$p.value < pThreshold
                    }
                )
            ) %>%
            `[[`("ID")
    })
    
    commonGenes <- Reduce(f = intersect, plotDat)
    commonGenes <- commonGenes[1:min(topToList, length(commonGenes))]
    
    commonGenesSym <- getEntrezAnnotation(commonGenes)$Symbol
    
    labelsToList <- sapply(1:topToList, function(i) paste0(commonGenes[i], " - ", commonGenesSym[i]))

    if (is.null(names(plotDat))) {
        names(plotDat) <- paste0("Dataset ", seq_along(plotDat))
    }

    if (!.requirePackage("ggvenn")){
        return(NULL)
    }

    pR <- ggvenn::ggvenn(plotDat,
           fill_color = c(
               "#316b9d",
               # "#fce397",
               # "#99cc83",
               "#f77a65",
               "#a6a1d0",
               "#fea9c4",
               "#74e7bc",
               "#febb73",
               "#1db4db",
               "#ffc5a6",
               "#b6c9fa",
               "#ee5437"),
           stroke_size = 0.5,
           set_name_size = 4,
           fill_alpha = 0.75
    ) 
    
    xrange <- layer_scales(pR)$x$range$range
    yrange <- layer_scales(pR)$y$range$range
    
    pR <- pR + ggplot2::theme(plot.margin = margin(0, 0, 0, 0, "pt")) +
      ggplot2::annotate(geom="text", x=xrange[2] + 0.1, y=yrange[2] + 0.1, label="Top Common\nSignificant Genes\n(Entrez ID - Symbol)",
                        color="black", hjust = 0, vjust = 1, fontface = 2) +
      ggplot2::annotate(geom="text", x=xrange[2] + 0.1, y=yrange[2] + 0.1, label= paste0("\n\n\n\n", paste0(labelsToList, collapse = "\n"), "\n...\t..."),
                        color="black", hjust = 0, vjust = 1) +
      scale_x_continuous(limits = c(xrange[1], xrange[2] + 1.5))
    return(pR)
    
    # pR + ggplot2::theme(plot.margin = margin(0, 0, 0, 0, "pt")) +
    #   
    #   ggplot2::annotate(geom="text", x=xrange[2] + 0.1, y=yrange[2] + 0.05, label="Top Common\nSignificant Genes",
    #                     color="red", hjust = 0, fontface = 2) + 
    #   
    #   ggplot2::annotate(geom="text", x=xrange[2] + 0.1, y=yrange[2] - 1.5, label= paste0(labelsToList, collapse = "\n"),
    #                     color="red", hjust = 0) +
    #   
    #   scale_x_continuous(limits = c(xrange[1], xrange[2] + 1))
    # 
}

#' @title Plot Venn diagram from multiple pathway analysis results
#' @description Plot a Venn diagram from multiple pathway analysis results.
#' @param PAResults A list of data frames with the results of pathway analysis.
#' @param pThreshold The p-value threshold to determine if a pathway is enriched.
#' @param useFDR Use the FDR adjusted p-value instead of the raw p-value.
#' @param topToList The number of common signifcant pathways that are used to annotate the plot.
#' @return A ggplot2 object.
#' @examples
#' \donttest{
#' library(RCPA)
#'
#'
#' affyFgseaResult <- loadData("affyFgseaResult")
#' agilFgseaResult <- loadData("agilFgseaResult")
#' RNASeqFgseaResult <- loadData("RNASeqFgseaResult")
#' metaPAResult <- loadData("metaPAResult")
#'
#' PAResults <- list(
#'     "Affymetrix - GSE5281" = affyFgseaResult,
#'     "Agilent - GSE61196" = agilFgseaResult,
#'     "RNASeq - GSE153873" = RNASeqFgseaResult,
#'     "Meta-analysis" = metaPAResult
#' )
#'
#' PAREsultUps <- lapply(PAResults, function(df) df[df$normalizedScore > 0,])
#' 
#' PAREsultDowns <- lapply(PAResults, function(df) df[df$normalizedScore < 0,])
#'
#' if (require("ggvenn", quietly = TRUE)){
#' p1 <- RCPA::plotVennPathway(PAResults, pThreshold = 0.05) +
#'     ggplot2::ggtitle("All Significant Pathways")
#' p2 <- RCPA::plotVennPathway(PAREsultUps, pThreshold = 0.05) +
#'     ggplot2::ggtitle("Significantly Up-regulated Pathways")
#' p3 <- RCPA::plotVennPathway(PAREsultDowns, pThreshold = 0.05) +
#'     ggplot2::ggtitle("Significantly Down-regulated Pathways")
#'}
#' }
#' @importFrom ggplot2 scale_fill_gradient theme layer_scales
#' @importFrom dplyr %>% filter
#' @importFrom scales trans_new
#' @export
plotVennPathway <- function(PAResults, pThreshold = 0.05, useFDR = TRUE, topToList = 10) {

    if (length(PAResults) < 2) {
        stop("The number of results must be at least 2.")
    }

    for (PARes in PAResults) {
        if (useFDR && !("pFDR" %in% colnames(PARes))) {
            stop("The FDR adjusted p-value column is not in the results data frame.")
        } else {
            if (!("p.value" %in% colnames(PARes))) {
                stop("The p.value column is not in the results data frame.")
            }
        }
    }

    plotDat <- lapply(PAResults, function(DERes) {
        data.frame(DERes) %>%
            filter(
                (
                    if (useFDR) {
                        .data$pFDR < pThreshold
                    } else {
                        .data$p.value < pThreshold
                    }
                )
            ) %>%
            `[[`("ID")
    })
    
    commonPathways <- Reduce(f = intersect, plotDat)
    
    commonPathways <- commonPathways[1:min(topToList, length(commonPathways))]
    
    allPathNames <- PAResults[[1]]$name
    names(allPathNames) <- PAResults[[1]]$ID
    labelsToList <- allPathNames[commonPathways]
    labelsToList <- sapply(1:length(labelsToList), function(i) paste0(names(labelsToList)[i], " - ", labelsToList[i]))
    labelsToList <- stringr::str_sub(labelsToList, 0, 40)
    labelsToList <- ifelse(stringr::str_length(labelsToList) == 40, paste0(labelsToList, "..."), labelsToList)

    if (is.null(names(plotDat))) {
        names(plotDat) <- paste0("Dataset ", seq_along(plotDat))
    }

    if (!.requirePackage("ggvenn")){
        return(NULL)
    }

    pR <- ggvenn::ggvenn(plotDat,
           fill_color = c(
               "#316b9d",
               # "#fce397",
               # "#99cc83",
               "#f77a65",
               "#a6a1d0",
               "#fea9c4",
               "#74e7bc",
               "#febb73",
               "#1db4db",
               "#ffc5a6",
               "#b6c9fa",
               "#ee5437"),
           stroke_size = 0.5,
           set_name_size = 4,
           fill_alpha = 0.75
    ) 
    
    xrange <- layer_scales(pR)$x$range$range
    yrange <- layer_scales(pR)$y$range$range
    
    pR <- pR + ggplot2::theme(plot.margin = margin(0, 0, 0, 0, "pt")) +
      ggplot2::annotate(geom="text", x=xrange[2] + 0.1, y=yrange[2] + 0.1, label="Top Common\nSignificant Pathways\n(ID - Name)",
                        color="black", hjust = 0, vjust = 1, fontface = 2) +
      ggplot2::annotate(geom="text", x=xrange[2] + 0.1, y=yrange[2] + 0.1, label= paste0("\n\n\n\n", paste0(labelsToList, collapse = "\n"), "\n... ..."),
                        color="black", hjust = 0, vjust = 1) +
      scale_x_continuous(limits = c(xrange[1], xrange[2] + 3))
   return(pR)
}

