#' @title Plot a bar chart of the pathway analysis results
#' @description This function plots a bar chart of the pathway analysis results.
#' @param results A named list of data frame with pathway analysis results.
#' The columns are ID, name, p.value, pFDR, size, nDE, score and normalizedScore.
#' @param limit The maximum number of pathways to plot.
#' The pathway will be sorted by the average absolute value of the -log10(p-value) or -log10(pFDR) if the 'by' parameter is 'p.value' or 'pFDR'.
#' Otherwise, the pathway will be sorted by the average value of the 'by' parameter.
#' @param label The column to use for the labels.
#' @param by The column to use for the bar heights.
#' @param maxNegLog10PValue The maximum -log10(p-value) to plot.
#' @param pThreshold The p-value threshold to use for significance.
#' @param useFDR If TRUE, use FDR adjusted p-values for the significance threshold. Otherwise, use raw p-values.
#' This parameter is used to mark the color of the bars and is independent of the 'by' parameter.
#' @param selectedPathways A vector of pathways ID, which is in the same format as ID column in the pathway analysis result, to be included in the plot.
#' If it is NULL, all pathways will be included.
#' @return A ggplot2 object.
#' @examples
#' \donttest{
#' library(RCPA)
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
#' selectedPathways <- c("path:hsa05010", "path:hsa05012", "path:hsa05014", 
#'                        "path:hsa05016", "path:hsa05017", "path:hsa05020", 
#'                        "path:hsa05022", "path:hsa04724", "path:hsa04727", 
#'                        "path:hsa04725", "path:hsa04728", "path:hsa04726",
#'                       "path:hsa04720", "path:hsa04730", "path:hsa04723", 
#'                       "path:hsa04721", "path:hsa04722")
#' resultsToPlot <- lapply(PAResults, 
#'                     function(df) df[df$ID %in% selectedPathways,])
#'
#' plotObj <- RCPA::plotBarChart(resultsToPlot) + 
#'             ggplot2::ggtitle("FGSEA Analysis Results")
#'
#' }
#' @export
#' @importFrom ggplot2 ggplot aes geom_bar theme_minimal theme geom_text coord_flip scale_x_discrete scale_colour_discrete scale_fill_discrete scale_color_manual
#' @importFrom ggplot2 scale_y_continuous guide_legend element_blank scale_fill_manual element_line labs geom_hline
#' @importFrom dplyr %>% select mutate arrange desc
#' @importFrom utils head
#' @importFrom rlang sym
#' @importFrom ggpattern geom_bar_pattern scale_pattern_manual
plotBarChart <- function(results, limit = Inf, label = "name", by = c("normalizedScore", "score", "pFDR", "p.value"), maxNegLog10PValue = 5, pThreshold = 0.05, useFDR = TRUE, selectedPathways = NULL) {
  
  if (!is.null(selectedPathways)) {
    results <- lapply(results, function(df) df[df$ID %in% selectedPathways,])
  }
  
  by <- match.arg(by)
  
  if (!"list" %in% class(results)) {
    results <- list(results)
  }
  
  for (result in results) {
    
    if (!"ID" %in% colnames(result)) {
      stop("The column 'ID' does not exist in the results.")
    }
    
    if (!by %in% colnames(result)) {
      stop(paste0("The column '", by, "' does not exist in the results."))
    }
    
    if (useFDR && !("pFDR" %in% colnames(result))) {
      stop("The column 'pFDR' does not exist in the results.")
    }
    if (!label %in% colnames(result)) {
      stop(paste0("The column '", label, "' does not exist in the results."))
    }
  }
  
  if (is.null(names(results))) {
    names(results) <- paste0("Dataset ", seq_along(results))
  }
  
  commonColNames <- Reduce(intersect, lapply(results, colnames))
  
  # if (by %in% c("p.value", "pFDR")) {
  pathwayOrder <- lapply(results, function(r) {
    r[, c("ID", "p.value", "pFDR")]
  }) %>%
    do.call(rbind, .) %>%
    mutate(
      logP = if (useFDR) -log10(.data$pFDR) else -log10(.data$p.value)
    ) %>%
    group_by(.data$ID) %>%
    dplyr::summarize(
      avgLogP = mean(.data$logP, na.rm = TRUE)
    ) %>%
    arrange(desc(.data$avgLogP)) %>%
    head(limit) %>%
    pull(.data$ID)
  # } else {
  #     pathwayOrder <- lapply(results, function(r) {
  #         r[, c("ID", by)] %>% `colnames<-`(c("ID", "value"))
  #     }) %>%
  #         do.call(rbind, .) %>%
  #         group_by(.data$ID) %>%
  #         dplyr::summarize(
  #             avgValue = mean(.data$value, na.rm = TRUE)
  #         ) %>%
  #         arrange(desc(abs(.data$avgValue))) %>%
  #         head(limit) %>%
  #         pull(.data$ID)
  # }
  
  
  plotData <- names(results) %>%
    lapply(function(n) {
      results[[n]][, commonColNames] %>%
        mutate(
          dataset = n
        )
    }) %>%
    do.call(rbind, .) %>%
    filter(.data$ID %in% pathwayOrder) %>%
    mutate(
      isSignificant = factor(ifelse((if (useFDR) .data$pFDR else .data$p.value) <= pThreshold, "Yes", "No"), levels = c("Yes", "No")),
    ) %>%
    mutate(
      p.value = pmin(-log10(.data$p.value), maxNegLog10PValue),
      pFDR = pmin(-log10(.data$pFDR), maxNegLog10PValue),
      ID = factor(.data$ID, levels = rev(pathwayOrder)),
      dataset = factor(.data$dataset, levels = names(results)),
      stat = .data[[by]],
      label = factor(.data[[label]], levels = results[[1]][match(rev(pathwayOrder), results[[1]]$ID), label]),
    )
  
  if (length(results) == 1) {
    pl <- ggplot(plotData, aes(x = .data$ID, y = .data$stat, fill = .data$isSignificant)) +
      geom_bar(
        stat = "identity"
      ) +
      scale_fill_manual(
        values = c("Yes" = "#316b9d", "No" = "gray"),
        guide = guide_legend(title = "Significant")
      )
  } else {
    pl <- ggplot(plotData) +
      geom_rect(
        aes(
          xmin = as.numeric( .data$ID) - 0.5,
          xmax = as.numeric( .data$ID) + 0.5,
          ymin = -Inf,
          ymax = Inf,
          fill = as.character(as.numeric( .data$ID) %% 2)
        ),
        color = "transparent"
      ) +
      scale_fill_manual(
        values = c("0" = "#f2f2f2", "1" = "#ffffff"),
        guide = "none"
      ) +
      new_scale_fill() +
      geom_bar_pattern(
        aes(x = .data$ID, y = .data$stat, fill = .data$dataset, pattern = .data$isSignificant),
        stat = "identity",
        position = if (length(results) > 1) "dodge" else "fill",
        width = ifelse(length(results) > 1, 0.8, 1),
        pattern_size = 0,
        pattern_alpha = 0.5,
        pattern_fill = "white",
        pattern_spacing = 0.02,
        color = "#444444",
        linewidth = 0.25
      ) +
      scale_fill_discrete(
        guide = guide_legend(
          title = "Dataset",
          override.aes = list(pattern = "none", color = "white"), order = 2
        )
      ) +
      scale_pattern_manual(
        values = c("Yes" = "stripe", "No" = "none"),
        guide = guide_legend(
          title = "Significant",
          override.aes = list(fill = "gray", color = "white")
        )
      ) 
      # guides(colour = guide_legend(order = 1), 
      #        shape = guide_legend(order = 2))
  }
  
  pl <- pl +
    coord_flip() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line.x = element_line(color = "darkgray"),
      axis.line.y = element_line(color = "darkgray"),
      plot.title = element_text(hjust = 0.5)
    ) +
    scale_x_discrete(labels = levels(plotData$label), expand = expansion(add = c(0.75, 0))) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(x = element_blank())
  
  if (by == "p.value" | by == "pFDR") {
    pl <- pl +
      geom_hline(yintercept = -log10(pThreshold), linetype = "dashed", color = "red") +
      labs(y = paste0("-log10 ", by))
  }
  
  if (by == "score") {
    pl <- pl + labs(y = "Score")
  }
  
  if (by == "normalizedScore") {
    pl <- pl + labs(y = "Normalized score")
  }
  
  pl 
}