#' @title Plot gene heatmap from a SummarizedExperiment object with DE analysis results
#' @description Plot gene heatmap from a SummarizedExperiment object with DE analysis results.
#' The heatmap contains p-values and log fold changes from the DE analysis.
#' @param DEResults A named list of data frame of DE analysis results.
#' @param genes A vector of gene id (e.g. Entrez IDs) to plot.
#' The genes must be in the ID column of the data frame in DEResults.
#' @param labels A vector of labels for the genes. If not provided, the gene IDs will be used as labels.
#' @param useFDR If TRUE, use FDR adjusted p-values. Otherwise, use raw p-values.
#' @param logFCLims A vector of length 2 specifying the minimum and maximum log fold change to plot.
#' @param negLog10pValueLims A vector of length 2 specifying the minimum and maximum -log10(p-value) to plot.
#' @return A heatmap of the genes from ggplot2.
#' @examples
#' \donttest{
#' library(RCPA)
#' library(SummarizedExperiment)
#'
#' affyDEExperiment <- loadData("affyDEExperiment")
#' agilDEExperiment <- loadData("agilDEExperiment")
#' RNASeqDEExperiment <- loadData("RNASeqDEExperiment")
#' metaDEResult <- loadData("metaDEResult")
#' genesets <- loadData("genesets")
#'
#' DEResults <- list(
#'     "Affymetrix - GSE5281" = rowData(affyDEExperiment),
#'     "Agilent - GSE61196" = rowData(agilDEExperiment),
#'     "RNASeq - GSE153873" = rowData(RNASeqDEExperiment)
#' )
#' 
#'  metaDEResult <- metaDEResult[order(metaDEResult$pFDR),]
#'
#'  alzheimerGenes <- genesets$genesets[["path:hsa05010"]]
#'  genesToPlot <- head(metaDEResult[metaDEResult$ID %in% alzheimerGenes, ], 50)$ID
#'
#'  genesAnnotation <- RCPA::getEntrezAnnotation(genesToPlot)
#'  labels <- genesAnnotation[genesToPlot, "Description"]
#'
#'  genesOrderByFC <- order(metaDEResult[match(genesToPlot, metaDEResult$ID), "logFC"])
#'  resultsToPlot <- c(DEResults, list(metaDEResult))
#'  names(resultsToPlot) <- c(names(DEResults), "Meta-analysis")
#'
#'  plotObj <- RCPA::plotDEGeneHeatmap(
#'      resultsToPlot,
#'      genesToPlot[genesOrderByFC],
#'      labels = labels[genesOrderByFC],
#'      negLog10pValueLims = c(0, 5), logFCLims = c(-1, 1)
#'  )
#'
#'
#' }
#' @importFrom SummarizedExperiment rowData
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_gradient2 theme_minimal theme geom_tile coord_flip scale_x_discrete scale_y_discrete guide_legend element_blank expansion sec_axis
#' @importFrom dplyr %>% select mutate
#' @importFrom tidyr gather
#' @importFrom ggnewscale new_scale_fill
#' @export
plotDEGeneHeatmap <- function(DEResults, genes, useFDR = TRUE, labels = NULL, logFCLims = c(-5, 5), negLog10pValueLims = c(0, 5)) {
    commonGenes <- lapply(DEResults, function(x) intersect(genes, x$ID)) %>%
        Reduce(intersect, .)

    if (any(commonGenes == 0)) {
        stop("No common genes found between the input genes and the genes in the DE results")
    }

    commonGenes <- genes[genes %in% commonGenes]

    if (length(commonGenes) < length(genes)) {
        warning("Some input genes are not found in the DE results")
    }

    DEdfs <- lapply(DEResults, function(x) x[match(commonGenes, x$ID),])

    if (is.null(labels)) {
        labels <- commonGenes
    } else {
        names(labels) <- genes
        labels <- labels[commonGenes]
    }

    scaleMinMax <- function(x, minx, maxx) {
        x[x < minx] <- minx
        x[x > maxx] <- maxx
        x
    }

    if (is.null(names(DEdfs))) {
        names(DEdfs) <- paste0("Dataset ", seq_along(DEdfs))
    }

    plotData <- names(DEdfs) %>%
        lapply(function(n) {
            DEdf <- as.data.frame(DEdfs[[n]])
            DEdf %>%
                mutate(
                    p.value = ifelse(rep(useFDR, nrow(DEdf)), .$pFDR, .$p.value) %>%
                        log10() %>%
                        abs() %>%
                        scaleMinMax(negLog10pValueLims[1], negLog10pValueLims[2]),
                    logFC = scaleMinMax(.$logFC, logFCLims[1], logFCLims[2]),
                    label = factor(labels, levels = labels),
                    dataset = n
                ) %>%
                dplyr::select("label", "logFC", "p.value", "dataset")
        }) %>%
        do.call(what = rbind) %>%
        gather("type", "value", -"label", -"dataset") %>%
        mutate(
            label = factor(.data$label, levels = labels),
            dataset = factor(.data$dataset, levels = names(DEdfs)),
            type = factor(.data$type, levels = c("p.value", "logFC")),
            # colOrder = as.numeric(.data$dataset)*2 + as.numeric(.data$type) + as.numeric(.data$dataset)*0.1 + as.numeric(.data$type)*0.01
            colOrder = as.numeric(.data$dataset) + as.numeric(.data$type)*length(DEdfs) + as.numeric(.data$dataset)*0.01 + as.numeric(.data$type)*0.1
        )

    uniqueY <- sort(unique(plotData$colOrder))

    x <- ggplot() +
        geom_tile(data = plotData[plotData$type == "logFC",], aes(x = .data$label, y = .data$colOrder, fill = .data$value, width = 1, height = 1)) +
        scale_fill_gradient2(
            high = "#B80F0A",
            low = "#004F98",
            mid = "white",
            na.value = "white",
            limits = logFCLims,
            guide = guide_colorbar(
                title = "log2 FC"
            )
        ) +
        new_scale_fill() +
        geom_tile(data = plotData[plotData$type == "p.value",], aes(x = .data$label, y = .data$colOrder, fill = .data$value, width = 1, height = 1)) +
        scale_fill_gradient(
            low = "white",
            high = "#B80F0A",
            na.value = "white",
            limits = negLog10pValueLims,
            guide = guide_colorbar(title = paste0("-log10", ifelse(useFDR, " pFDR", " p-value")))
        ) +
        theme_minimal() +
        coord_flip() +
        theme(
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x.bottom = element_text(angle = 45, vjust = 1, hjust = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
        ) +
        scale_x_discrete(
            labels = labels
        ) +
        scale_y_continuous(
            breaks = uniqueY,
            # labels = rep(c(
            #     paste0("-log10", ifelse(useFDR, " pFDR", " p-value")),
            #     "log2 FC"
            # ), length(DEdfs)),
            labels = rep(names(DEdfs), 2),
            expand = c(0, 0),
            # sec.axis = sec_axis(~., breaks = sapply(seq_along(DEdfs), function(i) mean(uniqueY[(i-1)*2 + 1:2])), labels = names(DEdfs))
            # sec.axis = sec_axis(~., breaks = sapply(seq_along(c(
            #       paste0("-log10", ifelse(useFDR, " pFDR", " p-value")),
            #       "log2 FC"
            #   )), function(i) mean(uniqueY[(i-1)*4 + 1:4])), labels = c(
            #     paste0("-log10", ifelse(useFDR, " pFDR", " p-value")),
            #     "log2 FC"
            #   ))
            sec.axis = sec_axis(~., breaks = sapply(seq_along(c(
              paste0("-log10", ifelse(useFDR, " pFDR", " p-value")),
              "log2 FC"
            )), function(i) mean(uniqueY[(i-1)*length(DEdfs) + 1:length(DEdfs)])), labels = c(
              paste0("-log10", ifelse(useFDR, " pFDR", " p-value")),
              "log2 FC"
            ))
        )
}