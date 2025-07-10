

#' Function of showing SNP density at chromosome level
#'
#' @param densityData the raw density data generated from vcftools
#' @param binSize the bin size set while generating density data
#' @param densityColorBar vector Specific the color bar for plotting density plot (generally four colors)
#' @param chromSet vector Filtered chrom set which you want to plot (it must be matched with the CHROM column in densityData)
#' @param withchr logical If the chromsome labels of density plot is prefixed with "chr".
#' Note: it cannot work when the filtered chrom set contain other uncommon chrom symbols (e.g. NC0*, etc)
#'
#' @return A ggplot2 object for SNP density plot
#' @export
#'
#' @import ggplot2
#' @importFrom stats reorder quantile
#' @importFrom rlang .data
#'
#' @examples
#' library(handyFunctions)
#' data(SNV_1MB_density_data)
#' ShowSNPDensityPlot(SNV_1MB_density_data, binSize = 1e6, chromSet = c(38:1))
#'
ShowSNPDensityPlot <- function(densityData, binSize, densityColorBar = c("grey", "darkgreen", "yellow", "red"),
                               chromSet = c(1:22), withchr = FALSE) {
  "
  densityData should be like this from vcftools:
    CHROM BIN_START SNP_COUNT VARIANTS.KB
  1     1    240000         1         0.1
  2     1    250000         0         0.0
  3     1    260000         0         0.0
  ...
  "
  # densityData<-SNV_density_data
  message("## Filtering the density data with specific chrom set...")
  index <- matchIndex(densityData[, "CHROM"], chromSet, T)
  tempSNPDensityData <- densityData[index, ]
  rawChrSet <- unique(tempSNPDensityData[, "CHROM"])
  message("## Judging if it should be added the chr and factoring...")
  isChr <- sum(!is.na(stringr::str_match(rawChrSet, "[C,c][H,h][R,r]")))
  isNum <- sum(!is.na(stringr::str_match(rawChrSet, "\\d+")))
  if (withchr == T) {
    if (isChr == length(rawChrSet)) {
      message("## Raw chr set is chr set, so it won't be changed!")
      tempSNPDensityData[, "CHROM"] <- factor(tempSNPDensityData[, "CHROM"], levels = chromSet)
    } else if (isChr == 0 & isNum == length(rawChrSet)) {
      tempSNPDensityData[, "CHROM"] <- paste0("chr", tempSNPDensityData[, "CHROM"])
      tempSNPDensityData[, "CHROM"] <- factor(tempSNPDensityData[, "CHROM"], levels = paste0("chr", chromSet))
    } else {
      warning("There are other chromosome symbols, so it won't be changed, or you can remove or replace that!!")
    }
  } else {
    if (isChr == length(rawChrSet)) {
      tempSNPDensityData[, "CHROM"] <- stringr::str_replace_all(tempSNPDensityData[, "CHROM"], "[C,c][H,h][R,r]", "")

      tempSNPDensityData[, "CHROM"] <- factor(tempSNPDensityData[, "CHROM"], levels = paste0(
        "chr",
        stringr::str_replace_all(chromSet, "[C,c][H,h][R,r]", "")
      ))
    } else if (isChr == 0 & isNum == length(rawChrSet)) {
      message("## Raw chr set is numbered set, so it won't be changed!")
      tempSNPDensityData[, "CHROM"] <- factor(tempSNPDensityData[, "CHROM"], levels = chromSet)
    } else {
      warning("There are other chromosome symbols, so it won't be changed, or you can remove or replace that!")
    }
  }


  ## reformat data
  message("## Reformatting the raw density data...")
  tempSNPDensityData[, "LEN"] <- binSize
  chrSet <- unique(tempSNPDensityData[, "CHROM"])
  i <- 1
  for (chrom in chrSet) {
    formerStart <- min(tempSNPDensityData$BIN_START[tempSNPDensityData[, "CHROM"] == chrom])
    temp <- data.frame("CHROM" = chrom, "BIN_START" = 0, "SNP_COUNT" = 0, "VARIANTS.KB" = 0, "LEN" = formerStart)
    if (i == 1) {
      formerStartData <- temp
    } else {
      formerStartData <- rbind(formerStartData, temp)
    }
    i <- i + 1
  }
  tempSNPDensityData <- rbind(tempSNPDensityData, formerStartData)
  minInColorBar <- 0
  # firstInColorBar<-quantile(tempSNPDensityData$SNP_COUNT,0.01)
  # secondInColorBar<-quantile(tempSNPDensityData$SNP_COUNT,0.5)
  thirdInColorBar <- quantile(tempSNPDensityData[, "SNP_COUNT"], 0.99)
  # maxInColorBar<-max(tempSNPDensityData$SNP_COUNT)

  p <- ggplot(tempSNPDensityData) +
    geom_bar(aes(x = .data$LEN / 1e6, y = reorder(.data$CHROM, .data$CHROM), fill = .data$SNP_COUNT), stat = "identity", position = "stack") +
    xlab("Position (Mb)") +
    scale_fill_gradientn(
      limits = c(minInColorBar, thirdInColorBar),
      # breaks = c(minInColorBar, firstInColorBar, secondInColorBar, thirdInColorBar),
      colours = densityColorBar
    ) +
    theme_classic() +
    theme(
      axis.title.x = element_text(size = 25),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 16),
      axis.line.y = element_blank()
    )
  return(p)
}
