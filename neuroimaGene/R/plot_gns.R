#' Gene summary plot of neuroimaGene object
#'
#' Generates an overview plot of the neuroimaGene object according to each gene input.
#' @param ng_obj NeuroimaGene Object
#' @param maxGns maximum number of genes to visualize. default=15
#' @param title optional title tag for the plot
#' @param verbose print runtime messages to R console. Default to FALSE
#' @keywords neuroimaging
#' @export
#' @import data.table ggplot2
#' @returns a ggplot class plot detailing NIDPs per gene, colored by brain measure type
#' @examples
#' gene_list <- c('TRIM35', 'PROSER3', 'EXOSC6', 'PICK1', 'UPK1A', 'ESPNL', 'ZIC4')
#' ng <- neuroimaGene(gene_list, atlas = NA, mtc = 'BH', vignette = TRUE)
#' plot_gns(ng)
#'
plot_gns <- function(ng_obj, maxGns = 15, title = NA, verbose = FALSE) {
  # initialize column names as null variables
  zscore <- maxZ <- gwas_phenotype <- gene_name <- training_model <- gp_ct <- primary <- NULL

  if(is.na(title)){
    tag <- ''
  } else {
    tag <- paste(' ',as.character(title))
  }
  if(!(is.integer(maxGns) || is.double(maxGns))) {
    stop('maxGns must be of data.type: integer or double', call. = F)
  }

  if (length(unique(ng_obj$gene)) > maxGns ) {
    if(verbose){message(paste('WARNING: Greater than', maxGns, 'genes in input data. Plot will only show the top', maxGns, 'genes ranked by effect size magnitude'))}
    genes <- ng_obj[, list(maxZ = max(zscore)), by = 'gene_name'][order(-abs(maxZ))][1:maxGns,]$gene_name
    ng_obj <- ng_obj[gene_name %in% genes,]
  }

  ng <- data.table::setDT(merge(ng_obj, anno, by = 'gwas_phenotype'))
  ng_summ <- ng[, list(tm_ct = length(unique(training_model)),
                        gp_ct = length(unique(gwas_phenotype))),
                    by = c('gene_name', 'primary')]
  gn_plot <- ggplot2::ggplot(ng_summ, aes(x = gene_name, y = gp_ct, fill = primary, group = primary)) +
    geom_bar(position = position_dodge(preserve = "single"), stat='identity') +
    ggtitle(paste0('GReX measures with neuroimaging correlates', tag)) +
    theme_light()+
    xlab('gene') +
    ylab('NIDPs')

  return(gn_plot)
}
