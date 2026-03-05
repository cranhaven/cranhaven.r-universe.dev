#' 2D visualization plot of a neuroimaGene object
#'
#' Generates a 2D visualization plot of the neuroimaGene object. Neuroimaging
#' regions are defined by the atlas parameter and colored according to the
#' magnitude and direction of the aggregate effect from each gene in the
#' NeuroimaGene object. Colors can be defined by the user.
#'
#' @param ng_obj NeuroimaGene object produced by neuroimaGene() function
#' @param atlas desired atlas for visualization. Desikan (default), Subcortex, DKT, Destrieux.
#' @param lowcol color for low end of Zscore spectrum. Default is dark red
#' @param midcol color for middle of Zscore spectrum. Default is white
#' @param highcol color for top end of Zscore spectrum. Default is blue4
#' @param title optional title tag for the plot
#' @keywords neuroimaging
#' @export
#' @import data.table ggplot2 stringr ggseg
#' @importFrom utils write.table
#' @importFrom stats na.omit
#' @returns class: ggplot object depicting 2D visualization of the NIDPs from the neuroimaGene object portrayed on the brain and shaded by mean effect size.
#' @examples
#' gene_list <- c('TRIM35', 'PROSER3', 'EXOSC6', 'PICK1', 'UPK1A', 'ESPNL', 'ZIC4')
#' ng <- neuroimaGene(gene_list, atlas = NA, mtc = 'BH', vignette = TRUE)
#' neuro_vis(ng, atlas = 'DKT')
#'
#'
neuro_vis <- function(ng_obj, atlas = 'Desikan', lowcol = 'red2', midcol = 'white', highcol = 'royalblue2', title = NA) {
  # initialize column names as null variables
  zscore <- atlasnm <- atl <- gwas_phenotype <- meanZ <- measurement <- NULL

  # load required local data from package
  dkt_atl <- readRDS(system.file("extdata","dkt_atlas.rda", package = "neuroimaGene"))
  dest_atl <- readRDS(system.file("extdata","dest_atlas.rda", package = "neuroimaGene"))

  if(is.na(title)){
    tag <- ''
  } else {
    tag <- paste(' ',as.character(title))
  }
  ng_summ <- ng_obj[, list(meanZ = mean(zscore), sign = sign(mean(zscore))),
                    by = c('gwas_phenotype')]
  ng <- data.table::setDT(merge(ng_summ, anno[, c('gwas_phenotype', 'measurement')], by = 'gwas_phenotype'))
  ng <- data.table::setDT(merge(ng, fs_anno, by = 'gwas_phenotype'))


  atldir <- data.table::data.table(atlasnm = c('Desikan', 'DKT', 'Destrieux', 'Subcortex',
                                 'desikan', 'dkt', 'destrieux', 'subcortex'),
                       realname =c('Desikan', 'DKT', 'Destrieux', 'Subcortex',
                              'Desikan', 'DKT', 'Destrieux', 'Subcortex'),
                       fsatl = c('ggseg::dk', 'dkt_atl',
                                 'dest_atl','ggseg::aseg',
                                 'ggseg::dk', 'dkt_atl',
                                 'dest_atl','ggseg::aseg'),
                       fsnm = c('dk', 'dkt', 'destrieux', 'aseg',
                                'dk', 'dkt', 'destrieux', 'aseg'))
  atlname = atldir[atlasnm == atlas,]$realname
  fs = atldir[atlasnm == atlas,]$fsatl
  fs2 = atldir[atlasnm == atlas,]$fsnm
  stat_vis <- stats::na.omit(ng[atl == atlname,])

  if (dim(stat_vis)[1] == 0) {
    stop(paste0('No nidps from the',atlas,'atlas detected.'))
  }

  if (atlas == 'Subcortex') {
    # plot aseg volumes
    aseg_vol <- stat_vis[gwas_phenotype %like% 'volume' & atl == 'Subcortex',]
    aseg_vol1 <- aseg_vol
    aseg_vol2 <- aseg_vol
    aseg_vol1$side = 'coronal'
    aseg_vol2$side = 'sagittal'
    aseg_vol3 <- rbind(aseg_vol1, aseg_vol2)
    plot <- ggseg::ggseg(aseg_vol3, atlas = ggseg::aseg,
                  colour = "black",
                  size = .1,
                  position = "dispersed",
                  mapping = aes(fill = meanZ))+
      scale_fill_gradient2(low = lowcol, mid = midcol, high = highcol, na.value = "lightgrey") +
      theme_minimal() +
      ggtitle(paste0('Subcortical NIDPs (aseg atlas)', tag))+
      theme(text = element_text(size = 14),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.title.x = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 14))

  } else {

    measures = unique(stat_vis$measurement[!is.na(stat_vis$measurement)])
    stat_vis_final = data.table::data.table()
    for(msr in measures) {
      temp <- data.table::as.data.table(merge(stat_vis[measurement == msr,],
                                  data.table::as.data.table(eval(parse(text = fs))$data)[,c('label', 'roi')],
                                  by = c('label'),
                                  all.y = TRUE))
      temp$measurement <- msr
      stat_vis_final = rbind(stat_vis_final, temp)
    }
    stat_vis_final$atlas <- fs2
    #plot <- ggseg(stat_vis, atlas = fs,
    plot <- ggseg::ggseg(stat_vis_final, atlas = eval(parse(text = fs)),
                colour = "black",
                size = .1,
                position = "stacked",
                mapping = aes(fill = meanZ))+
    facet_wrap(~measurement) +
    scale_fill_gradient2(low = lowcol, mid = midcol, high = highcol, na.value = "lightgrey") +
    theme_minimal() +
    ggtitle(paste0(atlas, ' atlas NIDPs', tag))+
    theme(text = element_text(size = 14),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 14))
  }
  return(plot)
}



