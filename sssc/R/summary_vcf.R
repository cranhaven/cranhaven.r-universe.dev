#' @title VCF Data Summary
#' @description Summarizes allele frequency information in scatter and density plots
#'
#' @param vcf VCF object from read_vcf function
#' @param ZG zygosity: (1) null, for both het and hom, default; (2) het; (3) hom
#' @param CHR chromosome number: (1) null, all chromosome, default; (2) any specific number
#'
#' @return A list containing (1) scatter: allele frequency scatter plot; (2) density: allele frequency density plot
#'
#' @export
#' @import stats
#' @import ggplot2
#'
#' @examples
#' data("vcf_example")
#' tmp <- summary_vcf(vcf = vcf_example, ZG = 'het', CHR = c(1,2))
#' plot(tmp$scatter)
#' plot(tmp$density)
summary_vcf <- function(vcf, ZG = NULL, CHR = NULL) {
  AF <- group <- location <- NULL
  table <- vcf$VCF
  detail <- NULL
  if (!is.null(ZG)) {
    table <- table[table$ZG == ZG ,]
  }
  if (!is.null(CHR)) {
    table <- table[table$CHROM %in% CHR ,]
  }
  density_plot <- density(table$AF)
  table$location <- as.numeric(rownames(table))
  s <- split(unique(table$CHROM), rep(1:2, length = length(unique(table$CHROM))))
  table$group <- table$CHROM
  table$group[table$CHROM%in%s[[1]]] <- 'Odd'
  if (length(CHR)!=1) {
    table$group[table$CHROM%in%s[[2]]] <- 'Even'
  }
  scatter_plot <- ggplot(table, aes(x=location, y=AF, color=group)) +
    geom_point(size = 1) +
    scale_color_manual(values=rep(c('red','blue')), guide=FALSE) +
    ylim(c(0, 1)) +
    labs(y="Allele Frequency",
         x="Position",
         title=paste("Scatter Plot of Allele Frequency", detail, ZG, sep = " ")) +
    theme_classic()
  res_list <- list("scatter" = scatter_plot, "density" = density_plot)
  return(res_list)
}
