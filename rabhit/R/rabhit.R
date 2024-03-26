# rabhit package documentation and import directives

#' The RAbHIT package
#'
#' The \code{rabhit} package provides a robust novel method for determining
#' antibody heavy and light chain haplotypes by adapting a Bayesian framework.
#' The key functions in \code{rabhit}, broken down by topic, are
#' described below.
#'
#'
#' @section  Haplotype and deletions inference:
#' \code{rabhit} provides tools to infer haplotypes based on given anchor genes,
#' deletion detection based on relative gene usage, pooling v genes, and a single anchor gene.
#'
#' \itemize{
#'   \item  \link{createFullHaplotype}:      Haplotypes inference and single chromosome deletions based on an anchor gene.
#'   \item  \link{deletionsByVpooled}:       Single chromosomal deletion detection by pooling V genes.
#'   \item  \link{deletionsByBinom}:         Double chromosomal deletion detection by relative gene usage.
#'   \item  \link{geneUsage}:                Relative gene usage.
#'   \item  \link{nonReliableVGenes}:        Non reliable gene assignment detection.
#' }
#'
#' @section  Haplotype and deletions visualization:
#' Functions for visualization of the inferred haplotypes and deletions
#'
#' \itemize{
#'   \item  \link{plotHaplotype}:            Haplotype inference map.
#'   \item  \link{deletionHeatmap}:          Single chromosome deletions heatmap.
#'   \item  \link{hapHeatmap}:               Chromosome comparison of multiple samples.
#'   \item  \link{hapDendo}:                 Hierarchical clustering of multiple haplotypes based on Jaccard distance.
#'   \item  \link{plotDeletionsByVpooled}:   V pooled based single chromosome deletions heatmap.
#'   \item  \link{plotDeletionsByBinom}:     Double chromosome deletions heatmap.
#' }
#'
#' @name     rabhit
#' @docType  package
#' @references
#' \enumerate{
#'   \item  Gidoni, M., Snir, O., Peres, A., Polak, P., Lindeman, I., Mikocziova, I., . . . Yaari, G. (2019).
#'   Mosaic deletion patterns of the human antibody heavy chain gene locus shown by Bayesian haplotyping.
#'   Nature Communications, 10(1). doi:10.1038/s41467-019-08489-3
#'  }
#'
#' @import   ggplot2
#' @import   methods
#' @import   utils
#' @import   dendextend
#' @importFrom  plotly           ggplotly subplot
#' @importFrom  graphics         grid image axis points text par plot
#' @importFrom  cowplot          get_legend plot_grid ggdraw draw_label background_grid
#' @importFrom  gridExtra        arrangeGrob
#' @importFrom  dplyr            do n desc funs %>% distinct
#'                               as_data_frame data_frame
#'                               bind_cols bind_rows combine rowwise slice
#'                               filter select arrange
#'                               group_by ungroup
#'                               mutate summarize
#'                               mutate_at summarize_at count
#'                               rename transmute pull ungroup row_number
#' @importFrom  data.table       := rbindlist data.table .N setDT CJ setorderv setkey .SD
#' @importFrom  reshape2         melt
#' @importFrom  gtools           ddirichlet
#' @importFrom  stats            hclust as.dendrogram as.dist binom.test p.adjust setNames weighted.mean
#' @importFrom  ggdendro         dendro_data segment
#' @importFrom  htmlwidgets      saveWidget
#' @importFrom  gtable           gtable_filter
#' @importFrom  grDevices        dev.off pdf recordPlot dev.control
#' @importFrom  alakazam         getGene
#' @importFrom  rlang            .data
#' @importFrom  tigger           sortAlleles
#' @importFrom  RColorBrewer     brewer.pal
#' @importFrom  tidyr            separate_rows
#' @importFrom  stringi          stri_detect_regex stri_detect_fixed
#' @importFrom  grid             gpar textGrob
#' @importFrom  splitstackshape  cSplit
#' @importFrom  fastmatch        %fin%
#' @importFrom  plyr             rbind.fill
#' @importFrom  readr            read_tsv cols
NULL

