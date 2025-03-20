#' @title \code{plotRegion}
#'
#' @description \code{plotRegion} Plots the region that you've summarized across
#'   all cell groupings (groups=initial getPopFrags() split) with optional motif
#'   overlay, chromosome position ideogram, and additional GRanges tracks. If
#'   plotting motif overlay, ensure that motif annotations have been added to
#'   your counts SummarizedExperiment. A basic plot can be rendered with just a
#'   counts SummarizedExperiment, but additional formatting arguments allow for
#'   further customization. Note that to show specific genes with the option
#'   'whichGenes' the \pkg{RMariaDB} package must be installed.
#'
#' @param countSE A SummarizedExperiment from MOCHA::getCoverage
#' @param plotType Options include 'overlaid','area', 'line', or 'RidgePlot'.
#'   default is 'area', which will plot a separate track for each group with the
#'   area filled in under the curve. Setting plotType to 'overlaid' will overlay
#'   count plot histograms across samples, instead of faceting out separately.
#'   Setting plotType to 'RidgePlot' will generate a RidgePlot across all
#'   groups.
#' @param base_size Numeric, default 12. Global plot base text size parameter
#' @param counts_color Optional color palette. A named vector of color values
#'   where names are unique values in the `color_var` column
#' @param range_label_size Numeric value, default 4. Text size for the y-axis
#'   range label
#' @param legend.position Any acceptable `legend.position` argument to theme().
#'   Default NULL will place legend for overlaid plots at (0.8,0.8), or to the
#'   "right" for faceted plots.
#' @param legendRatio Ratio of width or height of the main plot to the legend. Useful if the legend is to large. If only used when legend.position is set to top, bottom, left, or right. 
#' @param facet_label_side Direction character value, default "top". Can also be
#'   "right", "left", or "bottom". Position of facet label.
#' @param counts_group_colors Optional named color vector. Values as colors,
#'   names are levels of `counts_color_var`. If provided, will color the plots
#'   specifically using `scale_color_manual()`
#' @param counts_color_var Character value, default "Groups". Column name from
#'   countdf to use to color counts plots. Only used if counts_group_colors
#'   provided
#' @param counts_theme_ls A list of named theme arguments passed to theme(). For
#'   example, `list(axis.ticks = element_blank())`. Default NULL will use
#'   `.counts_plot_default_theme`.
#' @param motifSetName The name of the motif set in ArchRProj to use for
#'   annotation. Example: 'JasparMotifs'
#' @param motif_y_space_factor A factor for vertical spacing between motif
#'   labels. Default 4. Increase to make labels farther apart, decrease to make
#'   labels closer.
#' @param motif_stagger_labels_y = FALSE Logical value, default FALSE. If TRUE,
#'   will  stagger motif labels in adjacent columns in the vertical direction
#' @param motif_weights Optional numeric vector, default NULL. If provided will
#'   be used to color motif labels by the weighted values
#' @param motif_weight_name Character value, default "Motif Weight". Used to
#'   label the legend for motif colors
#' @param motif_weight_colors Named numeric vector. Names should be color values
#'   and breaks should be the corresponding values of motif_weights. Values
#'   outside the highest and lowest value will appear as max or min defined
#'   color value.
#' @param motif_lab_size Numeric value, default 1. Size of motif labels.
#' @param motif_lab_alpha Numeric value, default 0.25. Alpha for motif labels.
#' @param motif_line_size Numeric value, default 1. Size of motif lines.
#' @param motif_line_alpha Numeric value, default 0.25. Alpha for motif lines.
#' @param showGene Logical value, default TRUE. Whether or not the gene track
#'   should be plotted.
#' @param whichGenes Name of gene for plotting this specific gene in region.
#' @param monotoneGenes Boolean. Determines whether to color-code genes by gene name, or to set them all to dark gray. 
#' @param db_id_col Character value. Column in `OrgDb` containing the output id
#'   for `whichGenes` plotting. Default "REFSEQ".
#' @param collapseGenes Options include 'collapseAll', 'longestTx', or 'None'
#'   Default 'None' will plot the expanded view of the reference genes,
#'   'collapseAll' if you want collapse the gene tracks into one, and
#'   'longestTx' will only plot the longest transcript of each gene.
#' @param gene_theme_ls Named list of parameters passed to `theme()` for the
#'   gene plot. Default NULL will use `.gene_plot_theme`
#' @param additionalGRangesTrack A GRanges object containing additional track
#'   plot data
#' @param linkdf A dataframe with co-accessible links to display as an
#'   additional track
#' @param showIdeogram Logical value, default TRUE. If TRUE plots the chromosome
#'   ideogram at the top of the multi-track plot
#' @param ideogram_genome Character value, a genome name for the ideogram plot.
#'   Default 'hg19'.
#' @param relativeHeights Named numeric vector of relative heights for each of
#'   the 4 track plots to enable clean visualization when there are many tracks.
#'   Unused tracks will be ignored. Default value = c(`Chr` = 0.9, `Normalized
#'   Counts` = 7, `Genes`= 2, `AdditionalGRanges` = 4.5)
#' @param verbose Set TRUE to display additional messages. Default is FALSE.
#'
#' @return The input ggplot object with motif labels overlaid
#'
#' @examples
#' \dontrun{
#' # my_count_SE is a counts data frame generated by extractRegion()
#'
#' # Simple counts + ideogram + all genes:
#' plotRegion(countSE = my_count_SE)
#'
#' # Motif overlay for a project my_proj containing "JasparMotifs" annotations:
#' plotRegion(
#'   countSE = my_count_SE, motifSetName = "JasparMotifs",
#'   motif_lab_alpha = 1, motif_line_alpha = 1
#' )
#'
#' # Motif overlay w/ weights:
#' plotRegion(
#'   countSE = my_count_SE, motifSetName = "JasparMotifs", motif_lab_alpha = 1,
#'   motif_line_alpha = 1, motif_weights = my_enrichment_weights
#' )
#' }
#'
#' @export

plotRegion <- function(countSE,
                       # base count plot args
                       plotType = "area",
                       base_size = 12,
                       counts_color = NULL,
                       range_label_size = 2,
                       legend.position = NULL,
                       legendRatio = 0.25,
                       facet_label_side = "top",
                       counts_color_var = "Groups",
                       counts_group_colors = NULL,
                       counts_theme_ls = NULL,
                       # For plotting motifs
                       motifSetName = NULL,
                       motif_y_space_factor = 4,
                       motif_stagger_labels_y = FALSE,
                       motif_weights = NULL,
                       motif_weight_name = "Motif Weight",
                       motif_weight_colors = c(darkblue = -10, gray = 0, darkred = 10),
                       motif_lab_size = 1,
                       motif_lab_alpha = 0.25,
                       motif_line_alpha = 0.25,
                       motif_line_size = 0.75,
                       # Genes plot args
                       showGene = TRUE,
                       whichGenes = NULL,
                       monotoneGenes = FALSE,
                       db_id_col = "REFSEQ",
                       collapseGenes = FALSE,
                       gene_theme_ls = NULL,
                       # single.strand.genes.only = TRUE,
                       # Additional Tracks
                       additionalGRangesTrack = NULL,
                       linkdf = NULL,
                       # Ideogram
                       showIdeogram = TRUE,
                       ideogram_genome = "hg19",
                       # Combined Tracks
                       relativeHeights = c(`Chr` = 0.9, `Normalized Counts` = 7, `Links` = 1.5, `Genes` = 2, `AdditionalGRanges` = 4.5),
                       verbose = FALSE) {
  if(!requireNamespace("cowplot", quietly = TRUE)){
    stop("Package `cowplot` not found. Please install `cowplot` to use MOCHA::plotRegion.")
  }
  # Validate input
  supported_tracks <- c("Chr", "Normalized Counts", "Genes", "Links", "AdditionalGRanges")
  if (length(setdiff(names(relativeHeights), supported_tracks)) > 0) {
    if (verbose) { warning(sprintf(
      "1 or more values of relative heights not in supported tracks: %s.\n Supported track names: %s",
      paste(setdiff(names(relativeHeights), supported_tracks), collapse = ", "),
      paste(supported_tracks, collapse = ", ")
    )) }
  }

  if(showIdeogram){
      if (!requireNamespace("ggbio", quietly = TRUE)) {
        stop(
        "Package 'ggbio' is required for generating ideograms. ",
        "Please install 'ggbio' to proceed or set showIdeogram = FALSE."
        )
    } 
  }
    
  # function wrapper based on verbosity to hide messages
  verbf <- function(x) {
    if (verbose) {
      x
    } else {
      suppressMessages(x)
    }
  }

  if (!is.null(motif_weights) & plotType != "area") {
    stop(
      "Motif weights can only be used with area plots due to ggplot ",
      "settings. Please remove motif weights or change plotType to 'area'."
    )
  }

  countdf <- do.call("rbind", as.list(SummarizedExperiment::assays(countSE)))

  # Extract region from region string as granges
  regionGRanges <- countdf_to_region(countdf = countdf)

  # Variables
  chrom <- toString(unique(countdf$chr))
  .relativeHeights_default <- c(`Chr` = 0.9, `Normalized Counts` = 7, `Genes` = 2, `AdditionalGRanges` = 4.5, `Links` = 1.5) # retain in case any missing

  TxDb <- getAnnotationDbFromInstalledPkgname(countSE@metadata$TxDb$pkgname, "TxDb")
  OrgDb <- getAnnotationDbFromInstalledPkgname(countSE@metadata$OrgDb$pkgname, "OrgDb")
  # Base Plot of Sample Counts

  if(is.null(legend.position) & plotType == 'overlaid'){
      
      legend.position = 'right'
      legendRatio = 0.25
  }else if(is.null(legend.position)){
  
      legend.position = 'none'
      
  }

  p1 <- verbf(
    counts_plot_samples(countdf,
      plotType = plotType,
      base_size = base_size,
      counts_color_var = counts_color_var,
      counts_color = counts_color,
      range_label_size = range_label_size,
      legend.position = legend.position,
      facet_label_side = facet_label_side,
      counts_group_colors = counts_group_colors,
      theme_ls = counts_theme_ls
    )
  )
    
  if(!is.numeric(legend.position)){

    p1 <- p1 + ggplot2::theme(legend.position = 'none')
      
  }


  # Add Motifs to Base Plot if Requested
  if (!is.null(motifSetName)) {
    assertthat::assert_that(motifSetName %in% names(countSE@metadata),
      msg = sprintf("%s not found in Summarized Experiment", motifSetName)
    )
    p1 <- verbf(
      counts_plot_motif_overlay(
        p1 = p1,
        countdf = countdf,
        motifsList = countSE@metadata[[motifSetName]],
        motif_y_space_factor = motif_y_space_factor,
        motif_stagger_labels_y = motif_stagger_labels_y,
        motif_weights = motif_weights,
        motif_weight_name = motif_weight_name,
        motif_weight_colors = motif_weight_colors,
        motif_lab_size = motif_lab_size,
        motif_lab_alpha = motif_lab_alpha,
        motif_line_alpha = motif_line_alpha,
        motif_line_size = motif_line_size
      )
    )
  } else {
    p1 <- verbf(
      p1 + ggplot2::xlim(min(countdf$Locus), max(countdf$Locus))
    )
  }

  # Build P2, Ref Genes track
  if (showGene) {

    p2 <- get_gene_plot(
          regionGRanges = regionGRanges,
          TxDb = TxDb,
          OrgDb = OrgDb,
          whichGenes = whichGenes,
          monotoneGenes = monotoneGenes,
          collapseGenes= collapseGenes,
          theme_ls = gene_theme_ls, 
          db_id_col = 'REFSEQ'
        )
  } else {
    # If user wishes to hide genes
    p2 <- NULL
    relativeHeights["Genes"] <- 0
  }

 
  if (!is.null(additionalGRangesTrack)) {

    # Check for name metadata column
    if ("name" %in% colnames(GenomicRanges::mcols(additionalGRangesTrack))) {
      # Only plot the overlap of this region and the additional GRanges Track
      overlapGRanges <- verbf(plyranges::join_overlap_intersect(additionalGRangesTrack, regionGRanges))

     
      if (length(overlapGRanges) > 0) {
        # Use the subset within our region as the track we want to plot
        p4 <-additionalGRangesTrack <- .plot_GRanges(overlapGRanges, gene_theme_ls, empty = FALSE, type = 'Tracks')

      } else {
        p4 <-additionalGRangesTrack <- .plot_GRanges(overlapGRanges, gene_theme_ls, empty = TRUE, type = 'Tracks')
      }
    } else {
      p4 <-additionalGRangesTrack <- .plot_GRanges(overlapGRanges, gene_theme_ls, empty = TRUE, type = 'Tracks')
    }
  }

  ## Generate Link track
  if (!is.null(linkdf) &
    any(linkdf$start + 250 > GenomicRanges::start(regionGRanges) &
      linkdf$end - 250 < GenomicRanges::end(regionGRanges))) {
    p5 <- get_link_plot(
      regionGRanges, legend.position,
      relativeHeights, linkdf
    )
  }

  # Combine plots P1...P4
  # Base tracks are:
  # 1: Chromosome Ideogram
  # 2: a) Normalized Counts
  # 2: b) Optional: Link track
  # 3: Ref Genes
  # Additional GRanges track is user-defined and labeled according to its
  # 'names' metadata column
  # 4: AdditionalGRanges

  # Construct Named plot list top to Bottom by appending to track list if present
  track_list <- list()

  if (showIdeogram) {
    # Show Ideogram
    p3 <- verbf(ggbio::Ideogram(genome = ideogram_genome, subchr = chrom))
    p3 <- p3@ggplot
    track_list <- c(track_list, list("Chr" = p3))
  }
    
  # Genes
  if (!is.null(p2)) {
    p1 <- p1 + 
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                    plot.margin = grid::unit(c(0, 0, 0, 0), "cm")) + 
      ggplot2::xlab(NULL)
    # Counts
    track_list <- c(track_list, list("Normalized Counts" = p1))

    track_list <- c(track_list, list("Genes" = p2))
      
  }else{
      
    # Counts
    track_list <- c(track_list, list("Normalized Counts" = p1))
    
 }

  # Links
  if (!is.null(linkdf)) {
    track_list <- c(track_list, list("Links" = p5))
  }

 


  # Additional Ranges
  if (!is.null(additionalGRangesTrack)) {
    track_list <- c(track_list, list("AdditionalGRanges" = p4))
  }

  # height params
  if (!all(names(track_list) %in% names(relativeHeights))) {
    missing_heights <- setdiff(names(track_list), names(relativeHeights))
    append_heights <- .relativeHeights_default[missing_heights]
    relativeHeights <- c(relativeHeights, append_heights)
    if (verbose) { warning(sprintf(
      paste(missing_heights, collapse = ", "),
      paste(append_heights, collapse = ", ")
    )) }
  }
  trackHeights <- relativeHeights[names(track_list)] # ensure intended order
    
  #First, combine everything but the Ideogram
  g_tracks <- cowplot::plot_grid(plotlist = track_list[names(track_list) != 'Chr'], 
                                 ncol=1, align = 'v',
                                rel_heights = trackHeights[-1])
  #Now add the ideogram
  g_tracks <- cowplot::plot_grid(track_list$Chr, g_tracks, 
                ncol=1, 
     rel_heights = c(trackHeights[1], sum(trackHeights[-1])))
    
  ## Extract the legend for the data plot, if necessary (i.e. if the legend position is set to 'none', or overlaps with graph, this is unnecessary). 
  if(!all(is.numeric(legend.position)) & 
     all(legend.position != 'none')){

    #extract all legend parameters, if any, within the counts_theme
    if(any(grepl('legend', names(counts_theme_ls)))){
        legend_theme = counts_theme_ls[grep('legend', names(counts_theme_ls))]
        
        p1 <- p1 + ggplot2::theme(legend.box.margin = ggplot2::margin(1,1,1,1)) + do.call('ggplot2::theme', legend_theme)
        
    }else{
        
    }
      
    legend1 <- cowplot::get_legend(
       # create some space to the left of the legend
       p1 +  ggplot2::theme(legend.position = legend.position)
    )
    
    ## generate legend for linkDF
    if (!is.null(linkdf)) {
        track_list <- c(track_list, list("Links" = p5))
        
        legend2 <- cowplot::get_legend(
           # create some space to the left of the legend
           p5 + ggplot2::theme(legend.position = legend.position)
        )
        legend1 <- .setUpLegend(legend1, legend2, 
                                legend.position = legend.position, legendMerge = TRUE,
                                relativeRatio = 0.5)
    }
    
    #Add legend and g_tracks together
    g_tracks <- .setUpLegend(g_tracks, legend1, 
            legend.position = legend.position, 
            legendMerge = FALSE, relativeRatio = legendRatio)
    

  }


  return(g_tracks)
}

