#' Plot BEAM Sets
#'
#' plot_beam_clin produces a matrix of feature level clinical plots for a set. Users can specify which omic/endpoint pairs they want to see as well as the number of features from the set. Default is all omic/endpoint pairs and the top feature (smallest feature-level p-value).
#'
#' @param beam.result A beam.stats object from compute_beam_stats
#' @param beam.specs A data.frame. Default NULL, in which case beam.result$beam.specs is used. Otherwise can input other beam.specs data.frame that must contain name, mtx, mdl, plot columns.
#' @param beam.set.pvals A list containing BEAMR set p-values from compute_set_pvalues.
#' @param beam.feat.pvals A list containing feature-level p-values from compute_feature_pvalues.
#' @param set.id A character specifying the name of a set. Must be in beam.result$beam.data$set.data
#' @param gene.name A character specifying a Gene Name/Symbol for the set. Default is NULL
#' @param pair.type A character vector. Default NULL, in which case clinical plots for all omic/endpoint pairs are produced. Otherwise specify pairs from beam.stats$beam.specs$name
#' @param number.pairs A numeric. Default 1, in which case only feature with best simple test for each pair is plotted. If >1, show top n simple plots ordered by feature-level p-value
#' @param pair.order One of c("both", "omic", "endpoint"). Default is "both." Specify how to choose feature-endpoint plots to include. If "both", find the best (based on q, p, effect size) feature-omic pair for each type of omic and each endpoint separately. If "omic", within each omic, find the best feature-endpoint pair and then plot this feature with all endpoints. If "endpoint", need to specify endpt.order as the name of chosen endpoint. Then, within each omic, find the feature with best association with the selected endpoint, and plot this feature for all endpoints.
#' @param endpt.order Default NULL. If pair.order="endpoint", specify character with endpoint name (from beam.specs$name, after the period).
#' @param n.col A numeric. Specify the number of columns for the plot layout; default NULL will use the number of omics types.
#' @param n.row A numeric. Specify the number of rows for the plot layout; default NULL will automatically define the number of rows after number of columns specified.
#' @param title.size A numeric. Specify the size of individual plot titles. Default is 10.
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
#' plot.specs <- prep_beam_plot(beam.data=beam_stats$beam.data,
#'                              beam.specs=beam_stats$beam.specs)
#' test.plot <- plot_beam_clin(beam.result=beam_stats, beam.specs=plot.specs,
#'                             beam.set.pvals=test.pvals,
#'                             beam.feat.pvals=test.feat.pvals,
#'                             set.id="ENSG00000099810", gene.name="MTAP",
#'                             pair.type=NULL, number.pairs=1, n.col=4,
#'                             n.row=NULL, title.size=11,
#'                             pair.order="omic", endpt.order=NULL)
plot_beam_clin <- function(beam.result, beam.specs=NULL, beam.set.pvals, beam.feat.pvals,
                           set.id, gene.name=NULL, pair.type=NULL, number.pairs=1,
                           pair.order="both", endpt.order=NULL, n.col=NULL, n.row=NULL,
                           title.size=10)
{
  # Check data
  if(!inherits(beam.result, "beam.stats"))
    stop("beam.results must be the result of compute.beam.stats.")

  #print(beam.result$beam.specs)
  if(is.null(beam.specs)){
    beam.specs <- beam.result$beam.specs
  }
  beam.specs.ord <- beam.specs[order(beam.specs$mtx),]
  beam.data <- beam.result$beam.data

  # check the specs
  mtx.names=names(beam.data$mtx.data)
  main.clms=colnames(beam.data$main.data)
  spec.check=check_beam_specs(beam.specs,
                              mtx.names)

  n.spec=nrow(beam.specs)
  boot.index=beam.data$boot.index
  #print(class(boot.index))
  main.data=beam.data$main.data



  # default settings
  if(is.null(pair.type)){
    #n.tot <- n.spec * number.pairs
    #n.col <- max(3, floor(sqrt(n.tot))) # at most 3 columns in the plot layout
    # Loop through beam.feat.pvals to find the best simple test to plot
    beam.plots <- gen_beam_plot_list(beam.result=beam.result, beam.specs=beam.specs.ord,
                                     beam.feat.pvals = beam.feat.pvals, number.pairs = number.pairs,
                                     set.id=set.id, feat.id=NULL, title.size=title.size,
                                     pair.order=pair.order, endpt.order=endpt.order)
    p.overall <- signif(beam.set.pvals$set.pvals[which(beam.set.pvals$set.pvals$set.id==set.id),c("p.set")], digits=4)
    q.overall <- signif(beam.set.pvals$set.pvals[which(beam.set.pvals$set.pvals$set.id==set.id),c("q.set")], digits=4)
    n.tot <- length(beam.plots)
    if(is.null(n.col)){
      n.col <- length(unique(beam.specs$mtx))
    }
    if(is.null(n.row)){
      plots <- ggpubr::ggarrange(plotlist=beam.plots, ncol = n.col, nrow=ceiling(n.tot/n.col))
    }
    else{
      plots <- ggpubr::ggarrange(plotlist=beam.plots, ncol=n.col, nrow=n.row)
    }
    if(is.null(gene.name)){
      print(ggpubr::annotate_figure(plots, top=ggpubr::text_grob(paste0(set.id, " Clinical Plots"), size=28),bottom=ggpubr::text_grob(paste0("BEAMR P-value = ", p.overall, "; BEAMR q-value = ", q.overall), size=14)))
    }
    else{
      print(ggpubr::annotate_figure(plots, top=ggpubr::text_grob(paste0(set.id, " (", gene.name,") Clinical Plots"), size=28),bottom=ggpubr::text_grob(paste0("BEAMR P-value = ", p.overall, "; BEAMR q-value = ", q.overall), size=14)))
    }
  }
  else{
    # Check that pair.type is in beam.spec$name
    if(!(all(pair.type %in% beam.specs$name)))
      stop(paste0(pair.type[!which(pair.type %in% beam.specs$name)], " not found in beam.specs."))
    # Filter beam.specs.ord to only include pair.type of interest
    beam.specs.ord.filt <- beam.specs.ord[which(beam.specs.ord$name %in% pair.type),]
    #n.spec.f <- nrow(beam.specs.ord.filt)
    beam.plots <- gen_beam_plot_list(beam.result=beam.result, beam.specs=beam.specs.ord.filt,
                                     beam.feat.pvals = beam.feat.pvals, number.pairs = number.pairs,
                                     set.id=set.id, feat.id=NULL,
                                     pair.order=pair.order, endpt.order=endpt.order)


    p.overall <- signif(beam.set.pvals$set.pvals[which(beam.set.pvals$set.pvals$set.id==set.id),c("p.set")], digits=4)
    q.overall <- signif(beam.set.pvals$set.pvals[which(beam.set.pvals$set.pvals$set.id==set.id),c("q.set")], digits=4)
    n.tot <- length(beam.plots)
    if(is.null(n.col)){
      n.col <- length(unique(beam.specs.ord.filt$mtx))
    }
    if(is.null(n.row)){
      plots <- ggpubr::ggarrange(plotlist=beam.plots, ncol = n.col, nrow=ceiling(n.tot/n.col))
    }
    else{
      plots <- ggpubr::ggarrange(plotlist=beam.plots, ncol=n.col, nrow=n.row)
    }
    if(is.null(gene.name)){
      print(ggpubr::annotate_figure(plots, top=ggpubr::text_grob(paste0(set.id, " Clinical Plots"), size=28),bottom=ggpubr::text_grob(paste0("BEAMR P-value = ", p.overall, "; BEAMR q-value = ", q.overall), size=14)))
    }
    else{
      print(ggpubr::annotate_figure(plots, top=ggpubr::text_grob(paste0(set.id, " (", gene.name,") Clinical Plots"), size=28),bottom=ggpubr::text_grob(paste0("BEAMR P-value = ", p.overall, "; BEAMR q-value = ", q.overall), size=14)))
    }
  }

}
