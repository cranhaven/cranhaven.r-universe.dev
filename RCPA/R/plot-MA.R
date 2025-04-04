#' @title Plot MA plot from DE analysis results
#' @description Plot MA plot from DE analysis results
#' @param DEResult A data frame with DE analysis results.
#' The columns are ID, p.value, pFDR, logFC, and aveExpr.
#' @param pThreshold The p-value threshold to color significant points.
#' @param useFDR Use FDR instead of p-value for significance.
#' @param logFCThreshold The log2 fold change threshold to color significant points.
#' @param labels named vector of labels to use for points, e.g., c("gene1" = "Gene 1", "gene2" = "Gene 2")
#' @param fitMethod The method to use for fitting the loess line.
#' If NULL then no line is drawn.
#' @return A ggplot object.
#' @examples
#' \donttest{
#' library(RCPA)
#' library(SummarizedExperiment)
#'
#' RNASeqDEExperiment <- loadData("RNASeqDEExperiment")
#' 
#' plotObj <- RCPA::plotMA(rowData(RNASeqDEExperiment), logFCThreshold = 0.5) + 
#' ggtitle("RNASeq - GSE153873")
#'
#' }
#' @importFrom ggplot2 ggplot aes geom_point geom_hline theme_minimal theme theme_bw geom_vline scale_x_continuous scale_color_manual labs geom_smooth
#' @importFrom ggrepel geom_label_repel
#' @export
plotMA <- function(DEResult, pThreshold = 0.05, useFDR = TRUE, logFCThreshold = 1, labels = NULL, fitMethod = "loess") {

    plotDat <- data.frame(
        x = DEResult$avgExpr,
        y = DEResult$logFC,
        isSig = (
            if (useFDR) {
                DEResult$pFDR < pThreshold
            } else {
                DEResult$p.value < pThreshold
            }
        )
    )

    plotDat$color <- factor(plotDat$isSig * (abs(plotDat$y) > logFCThreshold) * sign(plotDat$y), levels = c(1, -1, 0))

    if (!is.null(labels)) {
        plotDat$label <- labels[DEResult$ID]
    }

    pl <- ggplot(plotDat, aes(x = .data$x, y = .data$y, color = .data$color)) +
        geom_point() +
        theme_bw() +
        theme_minimal() +
        theme(
            axis.line.x = element_blank(),
            axis.line.y = element_blank(),
        ) +
        scale_color_manual(
            values = c(
                "1" = "#B80F0A",
                "-1" = "#004F98",
                "0" = "darkgray"
            ),
            labels = c(
                "1" = paste0("Upregulated (", sum(plotDat$color == 1, na.rm = TRUE), ")"),
                "-1" = paste0("Downregulated (", sum(plotDat$color == -1, na.rm = TRUE), ")"),
                "0" = paste0("Not significant (", sum(plotDat$color == 0, na.rm = TRUE), ")")
            ),
            guide = guide_legend(override.aes = list(size = 3), title = "Significance")
        ) +
        theme(
            legend.position = "bottom"
        ) +
        labs(
            x = "Average expression",
            y = "Log2 fold change"
        ) +
        geom_hline(yintercept = -logFCThreshold, linetype = "dashed") +
        geom_hline(yintercept = logFCThreshold, linetype = "dashed")

    if (!is.null(labels)) {
        labelDat <- filter(plotDat, !is.na(.data$label))

        pl <- pl + geom_label_repel(
            labelDat,
            mapping = aes(x = .data$x, y = .data$y, label = .data$label),
            size = 3,
            segment.size = 0.75,
            segment.color = "#888888",
            color = "black",
            box.padding = 1,
            force = 3,
            point.size = NA
        )
    }

    if (!is.null(fitMethod)) {
        pl <- pl + geom_smooth(
            method = fitMethod,
            se = FALSE,
            color = "#228b22"
        ) +
            geom_hline(yintercept = 0, color = "#888888")
    }

    pl

}