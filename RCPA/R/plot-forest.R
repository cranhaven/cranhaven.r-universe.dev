#' @title Plot pathway forest plot from pathway/geneset/meta analysis results
#' @description pathways heatmap plot from pathway/geneset/meta analysis results.
#' @param resultsList A named list of dataframes from pathway analysis, geneset analysis, and/or meta analysis results.
#' The columns are ID, name, description, p.value, pFDR, size, nDE, score and normalizedScore.
#' @param yAxis The column to use for the y-axis.
#' @param statLims A numeric vector of length 2 specifying the limits for score to use in the x-axis.
#' @param useFDR Logical to indicate whether to use FDR or p-value.
#' @param selectedPathways A vector of pathways ID, which is in the same format as ID column in the pathway analysis result, to be included in the plot.
#' If it is NULL, all pathways will be included.
#' @return A ggplot2 object for presenting the heatmap of the pathways.
#' @examples
#' \donttest{
#' library(RCPA)
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
#' selectedPathways <- c("path:hsa05010", "path:hsa05012", "path:hsa05014", "path:hsa05016",
#'                       "path:hsa05017", "path:hsa05020", "path:hsa05022", "path:hsa04724",
#'                       "path:hsa04727", "path:hsa04725", "path:hsa04728", "path:hsa04726",
#'                       "path:hsa04720", "path:hsa04730", "path:hsa04723", "path:hsa04721",
#'                       "path:hsa04722")
#' resultsToPlot <- lapply(PAResults, function(df) df[df$ID %in% selectedPathways,])
#'
#' plotObj <- RCPA::plotForest(resultsToPlot, yAxis = "name", statLims = c(-3.5, 3.5))
#'
#' }
#' @importFrom ggplot2 ggplot aes geom_point geom_hline theme_minimal theme theme_bw geom_vline scale_color_gradient scale_size_continuous labs scale_fill_continuous scale_size geom_tile geom_errorbarh
#' @importFrom ggrepel geom_label_repel
#' @importFrom dplyr %>%
#' @export
plotForest <- function(resultsList, yAxis = c("ID", "name"), statLims = c(-2.5, 2.5), useFDR = TRUE, selectedPathways = NULL) {
    if (!is.null(selectedPathways)) {
      resultsList <- lapply(resultsList, function(df) df[df$ID %in% selectedPathways,])
    }
    yAxis <- match.arg(yAxis)

    for (result in resultsList) {
        if (is.null(result$ID) |
            is.null(result$name) |
            is.null(result$normalizedScore)) {
            stop("All dataframes in the input list must have 'ID', 'name', 'normalizedScore' columns.")
        }

        if (useFDR & is.null(result$pFDR)) {
            stop("All dataframes in the input list must have 'pFDR' column.")
        }

        if (!useFDR & is.null(result$p.value)) {
            stop("All dataframes in the input list must have 'p.value' column.")
        }
    }

    commonIds <- Reduce(intersect, lapply(resultsList, function(x) x$ID))
    if (length(commonIds) == 0) {
        stop("All dataframes in the input list must have at least one common ID.")
    }

    if (!is.numeric(statLims[1]) | !is.numeric(statLims[2])) {
        stop("statLims should be a numeric vector of length two.")
    }

    if (statLims[1] >= statLims[2]) {
        stop("statLims parameter must be a vector of two values with minimum as the first value and maximum as the second value. ")
    }

    if (is.null(names(resultsList))) {
        names(resultsList) <- paste0("Dataset ", seq_along(resultsList))
    }

    plotData <- lapply(names(resultsList), function(n) {
        data <- resultsList[[n]]
        rownames(data) <- data$ID
        data[commonIds, unique(c("ID", yAxis, "normalizedScore", "p.value", "pFDR"))] %>% mutate(dataset = n)
    }) %>% do.call(what = rbind)

    pathwayOrder <- plotData %>%
            mutate(
                logP = if (useFDR) -log10(.data$pFDR) else -log10(.data$p.value)
            ) %>%
            group_by(.data$ID) %>%
            dplyr::summarize(
                avgLogP = mean(.data$logP, na.rm = TRUE)
            ) %>%
            arrange(.data$avgLogP) %>%
            pull(.data$ID)

    plotData$dataset <- factor(plotData$dataset, levels = names(resultsList))

    pvalues <- if (useFDR) plotData$pFDR else plotData$p.value

    plotData$sd <- abs(plotData$normalizedScore / qnorm(pvalues))
    plotData$sd[plotData$sd > 1.5] <- 1.5
    plotData$min <- plotData$normalizedScore - plotData$sd * 1.96
    plotData$max <- plotData$normalizedScore + plotData$sd * 1.96
    plotData$min[plotData$min < statLims[1]] <- statLims[1]
    plotData$min[plotData$min > statLims[2]] <- statLims[2]
    plotData$max[plotData$max < statLims[1]] <- statLims[1]
    plotData$max[plotData$max > statLims[2]] <- statLims[2]

    plotData$ID <- factor(plotData$ID, levels = pathwayOrder)
    yLabels <- plotData %>% select("ID", sym(yAxis)) %>% unique() %>% arrange(as.numeric(.data$ID)) %>% pull(sym(yAxis))

    statRange <- statLims[2] - statLims[1]
    statMid <- (statLims[2] + statLims[1]) / 2
    gap <- 1

    plotData$normalizedScoreShifted <- plotData$normalizedScore + (as.numeric(plotData$dataset) - 1) * statRange + (as.numeric(plotData$dataset) - 1) * gap
    plotData$minShifted <- plotData$min + (as.numeric(plotData$dataset) - 1) * statRange + (as.numeric(plotData$dataset) - 1) * gap
    plotData$maxShifted <- plotData$max + (as.numeric(plotData$dataset) - 1) * statRange + (as.numeric(plotData$dataset) - 1) * gap

    zeros <- (seq_along(resultsList) - 1) * statRange + (seq_along(resultsList) - 1) * gap
    breaks <- lapply(seq_along(zeros), function(i) zeros[i] + ceiling(statLims[1]):floor(statLims[2])) %>% unlist()
    labels <- rep(ceiling(statLims[1]):floor(statLims[2]), length(zeros))

    ggplot() +
        geom_rect(
            plotData,
            mapping = aes(
                xmin = -Inf,
                xmax = Inf,
                ymin = as.numeric(.data$ID) - 0.5,
                ymax = as.numeric(.data$ID) + 0.5,
                fill = as.character(as.numeric(.data$ID) %% 2)
            )
        ) +
        scale_fill_manual(
            values = c("white", "#eeeeee"),
            guide = "none"
        ) +
        geom_rect(
            mapping = aes(
                ymin = -Inf,
                ymax = Inf,
                xmin = (seq_along(resultsList) - 1) * statRange - statRange / 2 + (seq_along(resultsList) - 1) * gap + statMid,
                xmax = (seq_along(resultsList) - 1) * statRange + statRange / 2 + (seq_along(resultsList) - 1) * gap + statMid
            ),
            fill = "transparent",
            color = "black"
        ) +
        geom_vline(
            mapping = aes(
                xintercept = breaks
            ),
            color = "#cccccc",
            size = 0.5,
            linetype = "dashed"
        ) +
        geom_vline(
            mapping = aes(
                xintercept = zeros
            ),
            color = "#D00000",
            size = 0.5,
            linetype = "dashed"
        ) +
        geom_errorbarh(data = plotData,
                       aes(
                           y = as.numeric(.data$ID),
                           xmin = .data$minShifted,
                           xmax = .data$maxShifted),
                       height = 0.2) +
        geom_point(data = plotData,
                   aes(
                       x = .data$normalizedScoreShifted,
                       y = as.numeric(.data$ID)),
                   color = "red") +
        scale_y_discrete(
            limits = unique(plotData$ID),
            name = "",
            labels = yLabels
        ) +
        scale_x_continuous(breaks = breaks,
                           labels = labels,
                           name = "Normalized Score",
                           sec.axis = sec_axis(~. / 1, breaks = zeros + statMid, labels = names(resultsList))) +
        theme_bw() +
        theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
        )
}