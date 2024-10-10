#' @title Plot KEGG map with DE genes
#' @description This function plots KEGG map with DE genes.
#' @param DEResults A named list of data frame of DE analysis results.
#' The columns of each data frame should be at least ID, logFC, p.value and pFDR.
#' @param KEGGPathwayID The KEGG pathway ID.
#' @param statistic The column name of the statistic used to plot the DE genes.
#' If statistic is p.value or pFDR, all genes are colored.
#' Otherwise, only DE genes are colored.
#' @param useFDR If TRUE, DE genes are selected based on pFDR, otherwise p.value.
#' @param pThreshold The p-value threshold to select DE genes.
#' Only used when statistic is not p.value or pFDR.
#' @param statLimit The absolute value of the statistic to color the DE genes.
#' If statistic is p.value or pFDR, this parameter is the limit of -log10(p-value).
#' Otherwise, this parameter is the limit of the absolute value of the statistic.
#' @return A list with the following elements:
#' \itemize{
#' \item plot: A ggplot object of the KEGG map.
#' \item width: The width of the KEGG map.
#' \item height: The height of the KEGG map.
#' }
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
#'     "Agilent - GSE61196" = rowData(agilDEExperiment),
#'     "RNASeq - GSE153873" = rowData(RNASeqDEExperiment)
#' )
#'
#' plotObj <- RCPA::plotKEGGMap(DEResults, "hsa05010", stat = "logFC", pThreshold = 1, statLimit = 1)
#'
#' }
#' @importFrom stringr str_starts str_remove
#' @importFrom dplyr %>% mutate inner_join
#' @importFrom ggplot2 ggplot annotation_custom theme_void scale_x_continuous annotation_raster geom_rect scale_fill_gradient guide_colorbar annotate element_text margin
#' @export
plotKEGGMap <- function(DEResults, KEGGPathwayID, statistic = "logFC", useFDR = TRUE, pThreshold = 0.05, statLimit = 3) {

    for (DERes in DEResults) {
        if (!statistic %in% colnames(DERes)) {
            stop("Column '", statistic, "' not found in one of DEResults.")
        }
    }

    if (!.requirePackage("png")){
        return(NULL)
    }
    if (!.requirePackage("RCurl")){
        return(NULL)
    }
    if (!.requirePackage("XML")){
        return(NULL)
    }

    if (str_starts(KEGGPathwayID, "path:")) {
        KEGGPathwayID <- str_remove(KEGGPathwayID, "path:")
    }

    organismCode <- str_remove(KEGGPathwayID, "[0-9]+")

    scale <- 2
    pngContent <- try({ RCurl::getURLContent(paste0("https://www.kegg.jp/kegg/pathway/", organismCode, "/", KEGGPathwayID, "@2x.png")) })
    if (inherits(pngContent, "try-error")) {
        scale <- 1
        .checkURLAvailable(paste0("https://www.kegg.jp/kegg/pathway/", organismCode, "/", KEGGPathwayID, ".png"))
        pngContent <- RCurl::getURLContent(paste0("https://www.kegg.jp/kegg/pathway/", organismCode, "/", KEGGPathwayID, ".png"))
    }

    img <- png::readPNG(pngContent)
    .checkURLAvailable(paste0("https://rest.kegg.jp/get/", KEGGPathwayID, "/kgml"))
    xml <- RCurl::getURLContent(paste0("https://rest.kegg.jp/get/", KEGGPathwayID, "/kgml")) %>%
        XML::xmlParse()

    entries <- XML::xpathApply(xml, '//entry', XML::xmlToList) %>%
        lapply(function(x) {
            type <- try({ x$.attrs['type'] })
            if (inherits(type, "try-error")) {
                return(NULL)
            }
            if (type != "gene") {
                return(NULL)
            }
            data.frame(
                gene = x$.attrs['name'] %>% str_split(" ") %>% unlist(),
                name = x$graphics['name'] %>%
                    as.character() %>%
                    str_split(",") %>%
                    unlist() %>%
                    `[`(1),
                x = as.numeric(x$graphics['x']) * scale,
                y = nrow(img) - as.numeric(x$graphics['y']) * scale,
                width = as.numeric(x$graphics['width']) * scale,
                height = as.numeric(x$graphics['height']) * scale,
                stringsAsFactors = FALSE
            )
        }) %>%
        do.call(what = rbind) %>%
        mutate(
            gene = .$gene %>%
                str_split(":") %>%
                sapply(function(x) x[2]) %>%
                as.character(),
        )

    DEResS <- lapply(DEResults, function(DERes) {
        df <- DERes %>%
            as.data.frame() %>%
            inner_join(
                entries,
                by = c("ID" = "gene")
            )

        df$statValues <- {
            if (statistic == "p.value" | statistic == "pFDR") {
                values <- -log10(df[[statistic]])
                values[values > statLimit] <- statLimit
                values
            } else {
                values <- df[[statistic]]
                values[values > statLimit] <- statLimit
                values[values < -statLimit] <- -statLimit
                values
            }
        }

        if (!statistic %in% c("p.value", "pFDR")) {
            if (useFDR) {
                df$statValues[df$pFDR > pThreshold] <- NA
            } else {
                df$statValues[df$p.value > pThreshold] <- NA
            }
        }

        df
    })

    plotDat <- lapply(seq_len(length(DEResS)), function(i) {
        DEResS[[i]] %>% mutate(
            xmin = .data$x - .data$width / 2 - 1 + .data$width * (i - 1) / length(DEResS),
            xmax = .data$x - .data$width / 2 + 1 + .data$width * i / length(DEResS),
            ymin = .data$y - .data$height / 2 - 1,
            ymax = .data$y - .data$height / 2 - 1 + 5 * scale
        )
    }) %>%
        do.call(what = rbind)

    p <- ggplot() +
        theme_void() +
        scale_x_continuous(expand = c(0, 0), limits = c(0, ncol(img))) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, nrow(img))) +
        annotation_raster(img, 0, ncol(img), 0, nrow(img)) +
        geom_rect(data = entries, aes(xmin = .data$x - .data$width / 2 - 1, xmax = .data$x + .data$width / 2 + 1, ymin = .data$y - .data$height / 2 - 1, ymax = .data$y + .data$height / 2 + 1), fill = "#bfffbf", color = "black", linewidth = 0.075 * scale) +
        geom_rect(data = entries, aes(xmin = .data$x - .data$width / 2 - 1, xmax = .data$x + .data$width / 2 + 1, ymin = .data$y - .data$height / 2 - 1, ymax = .data$y - .data$height / 2 - 1 + 5 * scale), fill = "gray") +
        geom_text(data = entries, aes(x = .data$x, y = .data$y + 2 * scale, label = .data$name), size = scale) +
        geom_rect(data = plotDat, aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$ymax, fill = .data$statValues), color = "black", linewidth = 0.05 * scale)

    if (statistic == "p.value" | statistic == "pFDR") {
        p <- p +
            scale_fill_gradient(
                low = "white",
                high = "red",
                na.value = "gray",
                limits = c(0, statLimit),
                guide = guide_colorbar(
                    title = ifelse(statistic == "p.value", "-log10 p-value", "-log10 pFDR"),
                    barheight = 0.25 * scale,
                    barwidth = 3 * scale,
                    title.position = "left",
                    direction = "horizontal",
                    title.hjust = -2,
                    title.vjust = 1
                )
            )
    } else {
        p <- p +
            scale_fill_gradient2(
                low = "blue",
                mid = "white",
                high = "red",
                midpoint = 0,
                na.value = "gray",
                limits = c(-statLimit, statLimit),
                guide = guide_colorbar(
                    title = statistic,
                    barheight = 0.25 * scale,
                    barwidth = 3 * scale,
                    title.position = "left",
                    direction = "horizontal",
                    title.hjust = -2,
                    title.vjust = 1
                )
            )
    }

    if (!is.null(names(DEResults))) {
        p <- p +
            annotate(
                "label",
                x = 210 * scale,
                y = 30 * scale,
                label = paste0(names(DEResults), collapse = " | "),
                fill = "#bfffbf",
                hjust = 0,
                size = 1.5 * scale
            ) +
            annotate(
                "text",
                x = 175 * scale,
                y = 30 * scale,
                label = "Order:",
                size = 1.5 * scale
            )
    }


    p <- p + theme(
        legend.text = element_text(size = 4 * scale),
        legend.title = element_text(size = 4 * scale),
        legend.position = c(1, 0),
        legend.margin = margin(0, 70 * scale, 10 * scale, 0, "pt")
    )

    list(
        plot = p,
        width = ncol(img),
        height = nrow(img)
    )
}