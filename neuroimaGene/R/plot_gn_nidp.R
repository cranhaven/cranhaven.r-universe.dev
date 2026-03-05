#' Gene by NIDP summary plot of neuroimaGene object
#'
#' Generate overview plot of the neuroimagene object according to gene/NIDP pair
#' @param ng_obj NeuroimaGene object
#' @param maxGns maximum number of genes to visualize. default=15
#' @param maxNidps maximum number of NIDPs to visualize. default=20
#' @param title optional title tag for the plot
#' @param shortnames optional boolean tag for simplified names. Default to TRUE
#' @param verbose print runtime messages to R console. Default to FALSE
#' @keywords neuroimaging
#' @export
#' @import data.table ggplot2
#' @returns a ggplot class heatmap showing tissue models per NIDP/Gene pair
#' @examples
#' gene_list <- c('TRIM35', 'PROSER3', 'EXOSC6', 'PICK1', 'UPK1A', 'ESPNL', 'ZIC4')
#' ng <- neuroimaGene(gene_list, atlas = NA, mtc = 'BH', vignette = TRUE)
#' plot_gnNIDP(ng)
#'
plot_gnNIDP <- function(ng_obj, maxNidps = 20, maxGns = 15, title = NA, shortnames = TRUE, verbose = FALSE) {
  # initialize column names as null variables
  zscore <- maxZ <- gwas_phenotype <- gene_name <- NIDP <- training_model <- tm_ct <- NULL

  if(is.na(title)){
    tag <- ''
  } else {
    tag <- paste(' ',as.character(title))
  }
  if(!(is.integer(maxNidps) || is.double(maxNidps))) {
    stop('maxNidps must be of data.type: integer or double', call. = F)
  }
  if(!(is.integer(maxGns) || is.double(maxGns))) {
    stop('maxNidps must be of data.type: integer or double', call. = F)
  }
  if (length(unique(ng_obj$gwas_phenotype)) > maxNidps ) {
    if(verbose){message(paste('WARNING: Greater than', maxNidps, 'NIDPs detected in input data. Plot will only show the top', maxNidps, 'NIDPs ranked by effect size magnitude'))}
    nidps <- ng_obj[, list(maxZ = max(zscore)), by = 'gwas_phenotype'][order(-abs(maxZ))][1:maxNidps,]$gwas_phenotype
    ng_obj <- ng_obj[gwas_phenotype %in% nidps,]
  }

  if (length(unique(ng_obj$gene)) > maxGns ) {
    if(verbose){message(paste('WARNING: Greater than', maxGns, 'genes remaining in input data after nidp analysis. Plot will only show the top', maxGns, 'genes ranked by effect size magnitude'))}
    genes <- ng_obj[, list(maxZ = max(zscore)), by = 'gene_name'][order(-abs(maxZ))][1:maxGns,]$gene_name
    ng_obj <- ng_obj[gene_name %in% genes,]
  }

  ng <- data.table::setDT(merge(ng_obj, anno, by = 'gwas_phenotype'))
  gn_nidp <- ng[, list(tm_ct = length(unique(training_model))),
                by = c('gene_name', 'gwas_phenotype', 'NIDP')]
  
  if(shortnames == FALSE) {
    gn_nidp <- gn_nidp[, -c('NIDP')]
    setnames(gn_nidp, 'gwas_phenotype', 'NIDP')
  }
  
  gn_plot <- ggplot2::ggplot(gn_nidp, aes(x = gene_name, y = NIDP, fill = tm_ct)) +
    geom_tile(color = 'white') +
    theme_light()+
    ggtitle(paste0('Neuroimaging correlates of GReX', tag)) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 11),
          axis.text.y = element_text(size = 11)) +
    xlab('gene') +
    ylab('NIDPs') +
    labs(fill='JTI model\ncount')

  return(gn_plot)
}
