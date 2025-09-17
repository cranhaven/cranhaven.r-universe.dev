#' Plot BEAM Feature
#'
#' plot_feat_clin produces a matrix of feature level clinical plots for a specific feature.
#'
#'
#' @param feat.id A character specifying the name of a feature. Must be in beam.result$beam.data$set.data
#' @param beam.result A beam.stats object from compute_beam_stats
#' @param beam.specs A data.frame. Default NULL, in which case beam.result$beam.specs is used. Otherwise can input other beam.specs  data.frame that must contain name, mtx, mdl, plot columns.
#' @param beam.set.pvals A list containing BEAMR set p-values from compute_set_pvalues.
#' @param beam.feat.pvals A list containing feature-level p-values from compute_feature_pvalues.
#' @param n.row A numeric. Specify the number of rows for the plot layout; default NULL will automatically define the number of rows after number of columns specified.
#' @param n.col A numeric. Specify the number of columns for the plot layout; default NULL will use the number of omics types.
#'
#' @returns A figure (ggarrange object)
#' @importFrom ggpubr ggarrange
#' @importFrom ggpubr annotate_figure
#' @importFrom ggpubr text_grob
#' @export
#'
#' @examples
#' data(beam_stats)
#' test.pvals <- compute_set_pvalues(beam.stats=beam_stats)
#' test.feat.pvals <- compute_feature_pvalues(beam.stats=beam_stats)
#' plot.specs <- prep_beam_plot(beam.data=beam_stats$beam.data, beam.specs=beam_stats$beam.specs)
#' test.plot <- plot_feat_clin(beam.result=beam_stats, beam.specs=plot.specs,
#'                             beam.set.pvals=test.pvals, beam.feat.pvals=test.feat.pvals,
#'                             feat.id="ENSG00000227443_loss",
#'                             n.col=2, n.row=NULL)
plot_feat_clin <- function(feat.id, # feature name
                           beam.result, # result of compute_beam_stats
                           beam.specs=NULL, # if NULL use beam.specs in beam.result, otherwise specify a beam.specs matrix. Must include "plot" column.
                           beam.set.pvals, # set level p-values
                           beam.feat.pvals, # feature level p-values
                           n.row=NULL, # number of rows for plot output
                           n.col=NULL) # number of columns for plot output
{
  # Check that feat.id is in the data
  if(!(feat.id %in% beam.result$beam.data$set.data$row.id))
    stop(paste0(feat.id, " not found in feature data!"))
  if(is.null(beam.specs)){
    beam.specs <- beam.result$beam.specs
  }
  beam.specs.ord <- beam.specs[order(beam.specs$mtx),]
  beam.data <- beam.result$beam.data
  # Check the number of sets that feat.id maps to
  set.dat <- beam.result$beam.data$set.data
  set.dat.filt <- set.dat[which(set.dat$row.id==feat.id),]
  n.set <- length(unique(set.dat.filt$set.id))
  # Generate list of plots for each set
  beam.plots <- gen_beam_plot_list(beam.result=beam.result, beam.specs=beam.specs.ord,
                                   beam.feat.pvals=beam.feat.pvals, number.pairs=1,
                                   set.id=NULL, feat.id=feat.id)

  # Arrange plot list
  n.tot <- length(beam.plots)
  if(is.null(n.col)){
    n.col <- length(unique(set.dat.filt$mtx.id))
  }
  if(is.null(n.row)){
    plots <- ggpubr::ggarrange(plotlist=beam.plots, ncol = n.col, nrow=ceiling(n.tot/n.col))
  }
  else{
    plots <- ggpubr::ggarrange(plotlist=beam.plots, ncol=n.col, nrow=n.row)
  }
  print(ggpubr::annotate_figure(plots, top=ggpubr::text_grob(paste0(feat.id, " Clinical Plots"), size=28),bottom=ggpubr::text_grob(paste0("Maps to set(s): ", unique(set.dat.filt$set.id)), size=14)))


} # end of function
