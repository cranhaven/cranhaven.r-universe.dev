#' @title Plot pathways heatmap plot from pathway/geneset/meta analysis results
#' @description pathways heatmap plot from pathway/geneset/meta analysis results.
#' @param resultsList A named list of dataframes from pathway analysis, geneset analysis, and/or meta analysis results.
#' The columns are ID, name, description, p.value, pFDR, size, nDE, score and normalizedScore.
#' @param yAxis The column to use for the y-axis.
#' @param negLog10pValueLims A vector of length 2 specifying the minimum and maximum -log10(p-value) to plot.
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
#' selectedPathways <- c("path:hsa05010", "path:hsa05012", "path:hsa05014", 
#'                       "path:hsa05016", "path:hsa05017", "path:hsa05020", 
#'                       "path:hsa05022", "path:hsa04724", "path:hsa04727", 
#'                       "path:hsa04725", "path:hsa04728", "path:hsa04726", 
#'                       "path:hsa04720", "path:hsa04730", "path:hsa04723", 
#'                       "path:hsa04721", "path:hsa04722")
#'                       
#' resultsToPlot <- lapply(PAResults, function(df) df[df$ID %in% selectedPathways,])
#'
#' plotObj <- RCPA::plotPathwayHeatmap(resultsToPlot, yAxis = "name")
#'
#' }
#' @importFrom ggplot2 ggplot aes geom_point geom_hline theme_minimal theme theme_bw geom_vline scale_color_gradient scale_size_continuous labs scale_fill_continuous scale_size geom_tile
#' @importFrom ggrepel geom_label_repel
#' @importFrom dplyr %>%
#' @export
plotPathwayHeatmap <- function(resultsList, yAxis = c("ID", "name"), negLog10pValueLims = c(0, 5), useFDR = TRUE, selectedPathways = NULL) {
    
    if (!is.null(selectedPathways)) {
      resultsList <- lapply(resultsList, function(df) df[df$ID %in% selectedPathways,])
    }
  
    yAxis <- match.arg(yAxis)

    studyIDs <- names(resultsList)

    if (any(sapply(studyIDs, is.null))) {
        stop("The names of the input list should be specified.")
    }
    
    checkNS <- lapply(resultsList, function(data) c("normalizedScore") %in% colnames(data))
    
    resultsList <- lapply(seq_along(checkNS), function(i) {
      if (!checkNS[[i]]) {
        df <- resultsList[[i]]
        df$normalizedScore <- rep(.Machine$double.eps, nrow(df))
        resultsList[[i]] <- df
      } else {
        resultsList[[i]]
      }
    })

    cols_list <- lapply(resultsList, function(data) colnames(data))
    
    

    if (!all(sapply(cols_list, function(x) c("ID", "name", "normalizedScore", "p.value") %in% x))) {
        stop("All dataframes in the input list must have 'ID', 'name', 'normalizedScore', and 'p.value' columns.")
    }

    rows_list <- lapply(resultsList, function(data) data$ID %>% unlist() %>% as.vector())

    if (!all(lengths(rows_list) == length(rows_list[[1]]))) {
        stop("All dataframes in the input list must have the same number of rows.")
    }

    initial_names <- resultsList[[1]] %>%
        .$ID %>%
        unlist() %>%
        as.vector() %>%
        sort()

    if (!all(sapply(rows_list, function(x) all.equal(sort(x), initial_names)))) {
        stop("All dataframes in the input list must have the same set of pathways.")
    }

    plotData <- lapply(1:length(resultsList), function(i) {

        data <- resultsList[[i]][, unique(c("ID", yAxis, "normalizedScore", "p.value", "pFDR"))]
        data$dataset <- studyIDs[i]
        data$Direction <- ifelse(data$normalizedScore <= 0, "Negative", "Positive")
        data$abs.normalizedScore <- data$normalizedScore %>% abs()
        as.data.frame(data)
    }) %>% do.call(what = rbind)
    
    plotData$Direction <- factor(plotData$Direction, levels = c("Positive", "Negative"))

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

    plotData$dataset <- factor(plotData$dataset, levels = studyIDs)
    plotData$ID <- factor(plotData$ID, levels = pathwayOrder)
    yLabels <- plotData %>% select("ID", sym(yAxis)) %>% unique() %>% arrange(as.numeric(.data$ID)) %>% pull(sym(yAxis))

    scaleMinMax <- function(x, minx, maxx) {
        x[x < minx] <- minx
        x[x > maxx] <- maxx
        x
    }

    if (yAxis == "ID") {
        plotData$yLabel <- plotData$ID
    }else {
        plotData$yLabel <- plotData$name
    }

    plotData$p.value.scaled <- (if (useFDR) plotData$pFDR else plotData$p.value) %>%
        log10() %>%
        abs() %>%
        scaleMinMax(negLog10pValueLims[1], negLog10pValueLims[2])


    ggplot(plotData, aes(y = .data$ID, x = factor(.data$dataset))) +
        geom_tile(
            aes(fill = .data$p.value.scaled)
        ) +
        scale_fill_continuous(
            low = "white",
            high = "#CD5C5C",
            limits = c(negLog10pValueLims[1], negLog10pValueLims[2]),
            breaks = c(negLog10pValueLims[1], (negLog10pValueLims[1] + negLog10pValueLims[2]) / 2, negLog10pValueLims[2]),
            guide = guide_colorbar(
                title = paste0("-log10", ifelse(useFDR, " pFDR", " p-value"))
            )
        ) +
        new_scale_fill() +
        geom_point(
            aes(
                fill = .data$Direction,
                size = .data$abs.normalizedScore
            ),
            shape = 21,
            color = "white",
            stroke = 0.5
        ) +
        scale_y_discrete(
            labels = yLabels
        ) +
        scale_size_continuous(
            guide = guide_legend(override.aes = list(shape = 21, fill = "gray50"))
        ) +
        scale_fill_manual(
            values = c("Positive" = "#FFAA1D", "Negative" = "#72A0C1"),
            guide = guide_legend(override.aes = list(shape = 21, size = 8), title = element_text("Sign"))
        ) +
        labs(
            size = "Normalized score",
            x = "",
            y = "") +

        theme_bw() + 
      
      ggplot2::theme(plot.title = element_text(hjust = 0.5))
}