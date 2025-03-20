######################### Plotting Coverge and Insertion data for specific regions ----
# library(dplyr)
# library(tidyverse)
# library(plyranges)
# library(ggrepel)
# library(ggbio)
# library(OrganismDbi)
# library(doParallel)



##### supporting functions for plotRegion
# Modified Track Plots

#' Get gene promoter range string
#'
#' For a given gene, return the gene position from an ArchR project
#' @param proj An ArchR Project
#' @param gene The name of a gene
#' @param upstream The number of basepairs to extend the range upstream of the gene promoter
#' @param downstream The number of basepairs to extend the range downstream of the gene promoter
#' @param output_string Logical value, default TRUE. If TRUE outputs a range string (ie 'chr1: 1700000-1710000') else
#' outputs a granges object
#' @return A range string or granges object, depending on 'output_string' parameter.
#'
#' @noRd
get_promoter_range <- function(proj, gene, upstream = 50000, downstream = 50000, output_string = TRUE) {
  gene_pos <- ArchR::getGenes(proj, gene)
  pro_pos_gr <- GenomicRanges::promoters(gene_pos, upstream = upstream, downstream = downstream)
  if (output_string) {
    pro_pos_str <- sprintf(
      "%s: %s-%s",
      GenomicRanges::seqnames(pro_pos_gr),
      GenomicRanges::start(GenomicRanges::ranges(pro_pos_gr)),
      GenomicRanges::end(GenomicRanges::ranges(pro_pos_gr))
    )
    return(pro_pos_str)
  } else {
    return(pro_pos_gr)
  }
}

#' CountDF to Region GRanges
#'
#' Gets the range covered by counts in a count df and converts to granges
#' Used in `plotRegion()`
#'
#' @param countdf  A dataframe that comes from `getbpCounts()` or `getbpInserts()`. Expected columns "chr" and "Locus"
#' @return A granges object for the region defined in countdf
#'
#' @noRd
countdf_to_region <- function(countdf) {
  assertthat::assert_that("chr" %in% names(countdf))
  assertthat::assert_that("Locus" %in% names(countdf))

  chrom <- toString(unique(countdf$chr))
  startSite <- min(countdf$Locus)
  endSite <- max(countdf$Locus)
  regionGRanges <- GenomicRanges::GRanges(
    seqnames = chrom,
    ranges = IRanges::IRanges(start = startSite, end = endSite), strand = "*"
  )

  return(regionGRanges)
}


#' Get a spacing between overlapping genes or other genomic ranges. 
#'
#' Used in `plotRegion()`
#; 
#' @param GRangesObj A GRanges with potentially overlapping features. 
#' @param overlapFeat A string. Name of metadata column that identifies unique features that should not be overlapped.
#' @param type A string, describing what the track should visualize. Default is genes
#'
#' @noRd

.getSpacing <- function(GRangesObj, overlapFeat = tx_name){

  ## Identified overlaps
  Features <- tx_name <- NULL
  reduceRanges1 <- plyranges::reduce_ranges(GRangesObj, Features = unique({{ overlapFeat }}))
  geneSpacing = do.call('rbind', lapply(reduceRanges1$Features, function(XX) {

          tx_list_tmp = unlist(XX)
          data.frame(tx_name = tx_list_tmp,
                      Spacing = seq(0, length(tx_list_tmp)-1, by =1))
          }))
  newGR <- GenomicRanges::makeGRangesFromDataFrame(dplyr::full_join(as.data.frame(GRangesObj), geneSpacing, by = 'tx_name'),
              keep.extra.columns = TRUE)

  return(newGR)

}

#' Get a data.frame for labeling tracks. 
#'
#' Used in `plotRegion()`
#; 
#' @param TrackDF A data.frame converted from a GRanges object. Must contain a column start, end, Spacing, as well as whateveris provided for nameFeat and groupVar.
#' @param groupVar A variable name. Name of metadata column that identifies the features to be labeled together (i.e. all the exons from the same transcript, tx_name)
#' @param nameFeat A variable name. Name of metadata column that identifies the features to be named.
#'
#' @noRd

.getTrackLabels <- function(TrackDF, groupVar, nameFeat = GeneName){
    
  start <- end <- Spacing <- minStart <- maxEnd <- GeneName <- NULL
  ## Identified overlaps
  trackLabels = dplyr::reframe(dplyr::group_by(TrackDF, {{groupVar}}), 
                              minStart = min(start),
                              maxEnd = max(end),
                              name = unique({{nameFeat}}), 
                              Spacing = unique(Spacing))
  colnames(trackLabels)[colnames(trackLabels) %in% 'name'] =  deparse(substitute(nameFeat))

  trackLabels = dplyr::mutate(trackLabels, label.x = round((minStart+maxEnd)/2))

  return(trackLabels)

}

#' Set legend correctly so that it doesn't mess with x-axis alignment. 
#'
#' Used in `plotRegion()`
#; 
#' @param existingPlot combined plots, already generated via cowplot
#' @param newLegend The new legend to add
#' @param legend.position The position of the new legend. 
#' @param relativeRatio Ratio of plot to legend width or height. 
#' @param legendMergee Boolean. If TRUE, then it's meant to merge legends.
#'             For top or bottom, legends are added next to each other.
#'             For left or right, legends are added on top of each other. 
#'
#' @noRd

.setUpLegend <- function(existingPlot, newLegend, legend.position, legendMerge, relativeRatio = 0.1){
    
    positionDict = data.frame(row.names = c("left", "right", "bottom", "top"), 
                              variable = c("rel_widths", "rel_widths", "rel_heights", "rel_heights"),
                              dim_var = c(2, 2, 1, 1))
    
    ncol = ifelse((grepl('left|right', legend.position) & !legendMerge) | 
                   (!grepl('left|right', legend.position) & legendMerge), 
                2, 1)
    
    #if the legend is on the left or the top, the relativeRatio comes in first. 
    #if the legend is on the right or bottom, then we need to put it second. 
    ratioDirection = ifelse((grepl('left|top', legend.position) & !legendMerge), 
                            list(c(relativeRatio*1,1)),
                            list(c(1, relativeRatio*1)))
    #Create the optionsList to iterate over. 
    optionsList = list(ncol, unlist(ratioDirection))
    
    #if the legend is on the right or left (and we are not merging legends), then we need two columns
    #if the legend is on the top or botttom and we are merging legends, then we need two columns
    #if the legend is on the top or bottom and we are not merging legends, we need one column
    #if the legend is on the left or right and we are merging legends, then we need one column.
    if((grepl('left|right', legend.position) & !legendMerge) | 
       (!grepl('left|right', legend.position) & legendMerge)){

        names(optionsList) <- c('ncol', 'rel_widths')
    }else{
        names(optionsList) <- c('ncol', 'rel_heights')
    }
    
    #Now set-up order of the merge
    ## if left and top, then put the newLegend first (as long as we aren't merging two legends).
    ## otherwise, put the existing plot first. 
    if(grepl('left|top', legend.position) & !legendMerge){

            g1 <- do.call(cowplot::plot_grid, 
                          append(list(plotlist= list(newLegend,existingPlot)), optionsList))
        

    }else{
        g1 <- do.call(cowplot::plot_grid, append(list(plotlist= list(existingPlot, newLegend)), optionsList))

    }

  
    return(g1)   
    
}


#' Get a ggplot object with tracks to visualize
#'
#' Used in `plotRegion()`
#; 
#' @param theme_ls Named list of `ggplot2::theme()` parameters.
#' @param regionGRanges regionGRanges A region Granges object to retrieve gene bodies for. For example, the output of countdf_to_region.
#' @param empty A boolean. If the visualized regions should be set to empty. 
#' @param type A string, describing what the track should visualize. Default is genes
#'
#' @noRd

.plot_GRanges <- function(regionGRanges, theme_ls = .gene_plot_theme, empty = TRUE, type = 'Genes'){

    start <- end <- Spacing <- label.x <- NULL
   if(empty){

      p <- ggplot2::ggplot(as.data.frame(regionGRanges)) + 
          ggplot2::geom_segment(
            mapping = ggplot2::aes(
              x = start,
              y = 0,
              xend = end,
              yend = 0,
              color = "white"
            ),
            show.legend = FALSE)
    }else{

      regionGRanges <- .getSpacing(regionGRanges, overlapFeat = name)
      trackDF <- as.data.frame(regionGRanges)
      name <- NULL
      trackLabels <- .getTrackLabels(trackDF, groupVar = name, nameFeat = name)

      p <- ggplot2::ggplot() + 
          ggplot2::geom_segment(
            data = trackDF,
            mapping = ggplot2::aes(
              x = start,
              y = Spacing,
              xend = end, 
              yend = Spacing,
              color = name
            ),
            show.legend = FALSE) +
          #gene names
          ggrepel::geom_text_repel(
            data = trackLabels,
            mapping = ggplot2::aes(x = label.x, y = Spacing, label = name),
            size = 3, vjust=-1, min.segment.length = 1000
          ) 

    }
            
    p <- p +
         ggplot2::theme_minimal() +
          ggplot2::ylab(type) + 
          ggplot2::xlim(c(GenomicRanges::start(regionGRanges),GenomicRanges::end(regionGRanges))) +
          ggplot2::xlab(label = paste0(unique(GenomicRanges::seqnames(regionGRanges)), " Loci (bp)"))  +
          do.call(ggplot2::theme, theme_ls)

    return(p)
}

#' Get a ggplot object with genes visualized
#'
#' Used in `plotRegion()`
#; 
#' @param whichGenes The name of the gene to plot. 
#' @param TxDb A TxDb database
#' @param OrgDb A OrgDb database
#' @param collapseGenes A boolean. Determines whether transcripts should be plotted individually or merged into one representative gene body for gene.
#' @param regionGRanges regionGRanges A region Granges object to retrieve gene bodies for. For example, the output of countdf_to_region.
#' @param theme_ls Named list of `ggplot2::theme()` parameters.
#' @param db_id_col Character value. Column in `orgdb` containing the output id. Default "REFSEQ".
#'
#' @noRd
get_gene_plot <- function(regionGRanges,  TxDb, OrgDb,
                whichGenes = NULL, collapseGenes = TRUE, 
                theme_ls = .gene_plot_theme, 
                db_id_col = 'REFSEQ', 
                monotoneGenes = FALSE) { 

    exonic_part <- strand <-start <- end <- intronic_part <- Spacing <- NULL
    TinyIntron <- newStart <- newEnd <- unit <- label.x <- NULL
    
  # Fill in theme any unspecified theme options with defaults
  default_theme <- .gene_plot_theme
  unspec_param <- setdiff(names(default_theme), names(theme_ls))
  if (length(unspec_param) > 0) {
    theme_ls <- c(theme_ls, default_theme[unspec_param])
  }
  
  totalLength = GenomicRanges::width(regionGRanges)
    
  allGenes <- base::sort(c(GenomicFeatures::intronicParts(TxDb, linked.to.single.gene.only = TRUE),
                  GenomicFeatures::exonicParts(TxDb, linked.to.single.gene.only = TRUE)))
    
  geneTrackDF <- as.data.frame(
      plyranges::join_overlap_intersect(allGenes, regionGRanges))

  if(dim(geneTrackDF)[1] == 0){

        warning("Could not find any transcripts in this region. Setting gene plot to empty. ")
        p <- .plot_GRanges(regionGRanges, theme_ls, 
                           empty = TRUE, type = 'Genes')
        return(p)
    }
                                  
  if(!collapseGenes){
      geneTrackDF$tx_name = unlist(lapply(geneTrackDF$tx_name, function(ZZ) paste0(ZZ, collapse = ', ')))
      geneTrackDF <- tidyr::separate_longer_delim(geneTrackDF, 
                          cols = c('tx_name'), delim = ", ")
      geneTrackDF$GeneName <- AnnotationDbi::mapIds(
                    OrgDb,
                    keys = unlist(geneTrackDF$tx_name),
                    column = "SYMBOL",
                    keytype = db_id_col)

  }else{
      #Just choose the first one. You only need one to get the gene symbol. 

      geneTrackDF$tx_name = unlist(lapply(geneTrackDF$tx_name, function(ZZ) ZZ[[1]]))
      geneTrackDF$GeneName <- AnnotationDbi::mapIds(
                    OrgDb,
                    keys = unlist(geneTrackDF$tx_name),
                    column = "SYMBOL",
                    keytype = db_id_col)
      geneTrackDF$tx_name = geneTrackDF$GeneName

  }
                                          
  if(!is.null(whichGenes)){
      
      geneTrackDF <- dplyr::filter(geneTrackDF, GeneName %in% whichGenes)
      if(dim(geneTrackDF)[1] == 0){

        stop(sprintf(
          "Could not find 'whichGenes' argument [%s] within the region provided. Please check region and organism database.",
          paste(whichGenes, collapse = ",")
        ))
    }
  }
                                      
  ##reduced ranges to identify overlapping genes
  geneTrackGRanges <- GenomicRanges::makeGRangesFromDataFrame(geneTrackDF, keep.extra.columns = TRUE)  

  #Figure out spacing between genes
  tx_name <- NULL
  geneTrackGRanges <- .getSpacing(geneTrackGRanges, overlapFeat = tx_name)    
  ## Identify introns that are too small for arrows, as measured by 1/100 of the region size
  geneTrackGRanges <- plyranges::mutate(geneTrackGRanges,
                                    TinyIntron = is.na(exonic_part) & 
                                    GenomicRanges::width(geneTrackGRanges) < totalLength/100)
 
  ## Generate additional arrows for long introns. 
  dupIntrons <- plyranges::filter(geneTrackGRanges,  is.na(exonic_part))
  dupIntrons <- plyranges::join_overlap_intersect(
      plyranges::tile_ranges(dupIntrons, totalLength/20),dupIntrons)

  # Generate data.frame
  geneTrackDF <- as.data.frame(c(geneTrackGRanges, dupIntrons))
  geneTrackDF$tx_name = as.character(geneTrackDF$tx_name) 
                                      
  #Set reverse the direction of start and end if it's a gene on the reverse strand.
  geneTrackDF <- dplyr::mutate(geneTrackDF, 
                                newStart = ifelse(strand == '+', end, start), 
                                newEnd = ifelse(strand == '+', start, end))                                        
  geneTrackDF$Spacing = as.character(geneTrackDF$Spacing)

  #Generate track labels
  GeneName <- NULL
  trackLabels <- .getTrackLabels(geneTrackDF, groupVar = tx_name, nameFeat = GeneName)
    
  if(monotoneGenes){
      geneTrackDF$GeneName = 'Same'
  }
                                          
  ## Plot the genes
  p <- ggplot2::ggplot() +
    # Exons by themselves
    ggplot2::geom_segment(
      data = dplyr::filter(geneTrackDF, is.na(intronic_part)),
      mapping = ggplot2::aes(
        x = start,
        y = Spacing,
        xend = end,
        yend = Spacing,
        color = GeneName
      ),
      show.legend = FALSE,
      linewidth = 5
    ) + #plot all introns, without arrows. 
     ggplot2::geom_segment(
      data = dplyr::filter(geneTrackDF, is.na(exonic_part) ),
      mapping = ggplot2::aes(
        x = start,
        y = Spacing,
        xend = end,
        yend = Spacing,
        color = GeneName
      ),
      alpha = 0.8,
      show.legend = FALSE,
      size = 1/2
    ) +
    # Introns with arrows for direction of gene
    ggplot2::geom_segment(
      data = dplyr::filter(geneTrackDF, is.na(exonic_part) & !TinyIntron),
      mapping = ggplot2::aes(
        x = newStart,
        y = Spacing,
        xend = newEnd,
        yend = Spacing,
        color = GeneName
      ),
      alpha = 0.8,
      show.legend = FALSE,
        arrow = ggplot2::arrow(
        ends = "first",
        type = "open",
        angle = 45,
        length = unit(x = 0.05, units = "inches")
      ),
      size = 1/2
    ) + #gene names
  ggrepel::geom_text_repel(
    data = trackLabels,
    mapping = ggplot2::aes(x = label.x, y = Spacing, label = GeneName),
    size = 3, vjust=-1, max.overlaps = Inf, min.segment.length = 2
  ) + # Set theme
  ggplot2::theme_minimal() +
  ggplot2::ylab("Genes") +  ggplot2::xlim(c(GenomicRanges::start(regionGRanges),GenomicRanges::end(regionGRanges))) +
  ggplot2::xlab(label = paste0(unique(GenomicRanges::seqnames(regionGRanges)), " Loci (bp)"))  +
      do.call(ggplot2::theme, theme_ls)
                                          
 if(monotoneGenes){
  
     p <- p + 
     ggplot2::scale_color_manual(
         values = c('Same' = '#646464'))
     
  }

  return(p)
                                            
}
                                    
                                        
#' Default ggplot theme for counts plot
.counts_plot_default_theme <- list(
  panel.grid.major.y = ggplot2::element_blank(),
  panel.grid.minor.y = ggplot2::element_blank(),
  legend.title = ggplot2::element_text(size = 12),
  legend.text = ggplot2::element_text(size = 10),
  axis.text.y = ggplot2::element_blank(),
  axis.ticks.y = ggplot2::element_blank(),
  panel.border = ggplot2::element_blank(),
  strip.background = ggplot2::element_blank(),
  strip.text.x = ggplot2::element_text(size = 10, angle = 0),
  strip.text.y = ggplot2::element_text(size = 10, angle = 0)
)


#' Plot normalized counts for each sample
#'
#' Used in `plotRegion()` for the counts tracks
#'
#' @param countdf  A dataframe that comes from `getbpCounts()` or `getbpInserts()`
#' @param plotType Options include 'overlaid','area', 'line', or 'RidgePlot'. default is 'area', which will plot a separate track for each group.
#' Setting plotType to 'overlaid' will overlay count plot histograms across samples, instead of faceting out separately.
#' Setting plotType to 'RidgePlot' will generate a RidgePlot across all groups.
#' @param base_size Numeric, default 12. Global plot base text size parameter
#' @param counts_color Optional color palette. A named vector of color values where names are unique values in the `color_var` column
#' @param range_label_size Numeric value, default 4. Text size for the y-axis range label
#' @param legend.position Any acceptable `legend.position` argument to theme(). Default NULL will place legend for overlaid plots at (0.8,0.8),
#' or to the "right" for faceted plots.
#' @param facet_label_side Direction character value, default "top". Can also be "right", "left", or "bottom". Position of facet label.
#' @param counts_group_colors Optional named color vector. Values as colors, names are levels of `counts_color_var`. If provided, will color the plots specifically
#' using `scale_color_manual()`
#' @param counts_color_var Character value, default "Groups". Column name from countdf to use to color counts plots.
#' Only used if counts_group_colors provided
#' @param theme_ls A list of named theme arguments passed to theme(), defaults to `.counts_plot_default_theme`. For example, `list(axis.ticks = ggplot2::element_blank())`
#' @return A ggplot object of count histograms by sample.
#'
#' @noRd

counts_plot_samples <- function(countdf,
                                plotType = "area",
                                base_size = 12,
                                counts_color = NULL,
                                range_label_size = 4,
                                legend.position = NULL,
                                facet_label_side = "top",
                                counts_group_colors = NULL,
                                counts_color_var = "Groups",
                                theme_ls = .counts_plot_default_theme) {
  assertthat::assert_that("Counts" %in% names(countdf))
  assertthat::assert_that("Locus" %in% names(countdf))
  assertthat::assert_that("Groups" %in% names(countdf))
  assertthat::assert_that(counts_color_var %in% names(countdf))

  Locus <- Counts <- Groups <- Groups2 <- NULL
    
  # Fill in theme any unspecified theme options with defaults
  default_theme <- .counts_plot_default_theme
  unspec_param <- setdiff(names(default_theme), names(theme_ls))
  if (length(unspec_param) > 0) {
    theme_ls <- c(theme_ls, default_theme[unspec_param])
  }

  # Range df for plotting
  df_range <- data.frame(
    x = max(countdf$Locus),
    y = max(countdf$Counts),
    label = paste0("Range:", 0, "-", round(max(countdf$Counts), digits = 2))
  )

  # Intialize plot
  p1 <- ggplot2::ggplot(data = countdf, ggplot2::aes(x = Locus, y = Counts)) +
    ggplot2::theme_bw(base_size = base_size)


  # Plots
  x <- y <- label <- theme <- NULL

  if (tolower(plotType) == "overlaid") {
    # Conditional Theme
    if (is.null(legend.position)) {
      legend.position <- c(0.8, 0.8)
    }
    theme_ls <- c(
      theme_ls,
      list(
        legend.position = legend.position
      )
    )

    # Base Plot
    p1 <- p1 +
      ggplot2::geom_line(ggplot2::aes(color = !!as.name(counts_color_var)), alpha = 0.75, size = 1.5) +
      ggplot2::ylab('Normalized Coverage') +
      ggplot2::labs(Groups = "Groups") +
      #ggplot2::coord_cartesian(clip = "off") +
      ggplot2::geom_text(
        data = df_range,
        ggplot2::aes(x = x, y = y, label = label),
        size = range_label_size,
        hjust = 1,
        vjust = 1
      ) +
      do.call(ggplot2::theme, theme_ls)

    # Conditional Plot Elements
    if (!is.null(counts_group_colors)) {
      # assertthat::assert_that(all(unique(countdf[[counts_color_var]]) %in% names(counts_group_colors)),
      #                        msg = "Must supply colors for all levels of color variable")
      p1 <- p1 + ggplot2::scale_color_manual(values = counts_group_colors, breaks = names(counts_group_colors))
    }
  } else if (tolower(plotType) == "area") {
    # Conditional Theme
    if (is.null(legend.position)) {
      legend.position <- "right"
    }
    theme_ls <- c(
      theme_ls,
      list(
        legend.position = legend.position
      )
    )

    # Base Plot, conditional
    if (is.null(counts_color)) {
      p1 <- p1 +
        ggplot2::geom_area(ggplot2::aes(fill = !!as.name(counts_color_var)), position = "identity")
    } else {
      p1 <- p1 +
        ggplot2::geom_area(fill = counts_color, position = "identity")
    }

    # Base Plot, common elements
    p1 <- p1 +
      ggplot2::ylab('Normalized Coverage') +
      ggplot2::facet_wrap(dplyr::vars(Groups), ncol = 1, strip.position = facet_label_side) +
      ggplot2::geom_text(
        data = df_range,
        ggplot2::aes(x = x, y = y, label = label),
        size = range_label_size,
        hjust = 1,
        vjust = 1
      ) +
      do.call(ggplot2::theme, theme_ls)

    # Conditional Plot Elements
    if (!is.null(counts_group_colors)) {
      # commenting this section out so we can supply specific colors to highlight just a subset if we want--other samples will just be gray
      # assertthat::assert_that(all(unique(countdf[[counts_color_var]]) %in% names(counts_group_colors)),
      #                        msg = "Must supply colors for all levels of color variable")
      p1 <- p1 +
        ggplot2::scale_fill_manual(values = counts_group_colors, breaks = names(counts_group_colors))
    }
  } else if (tolower(plotType) == "line") {
    # Conditional Theme
    if (is.null(legend.position)) {
      legend.position <- "right"
    }
    theme_ls <- c(
      theme_ls,
      list(
        legend.position = legend.position
      )
    )

    # Base Plot, conditional
    if (is.null(counts_color)) {
      p1 <- p1 +
        ggplot2::geom_line(ggplot2::aes(color = !!as.name(counts_color_var)), position = "identity")
    } else {
      p1 <- p1 +
        ggplot2::geom_line(color = counts_color, position = "identity")
    }

    # Base Plot, common elements
    p1 <- p1 +
      ggplot2::ylab('Normalized Coverage') +
      ggplot2::facet_wrap(dplyr::vars(Groups), ncol = 1, strip.position = facet_label_side) +
      ggplot2::geom_text(
        data = df_range,
        ggplot2::aes(x = x, y = y, label = label),
        size = range_label_size,
        hjust = 1,
        vjust = 1
      ) +
      do.call(ggplot2::theme, theme_ls)

    # Conditional Plot Elements
    if (!is.null(counts_group_colors)) {
      # commenting this section out so we can supply specific colors to highlight just a subset if we want--other samples will just be gray
      # assertthat::assert_that(all(unique(countdf[[counts_color_var]]) %in% names(counts_group_colors)),
      #                        msg = "Must supply colors for all levels of color variable")
      p1 <- p1 +
        ggplot2::scale_fill_manual(values = counts_group_colors, breaks = names(counts_group_colors))
    }
  } else if (tolower(plotType) == "ridgeplot") {


    ## Conditional elements needed to make RidgePlots work.
    countdf_tmp <- as.data.frame(table(countdf$Groups))
    countdf$Groups2 <- match(countdf$Groups, countdf_tmp$Var1)


    # Base plot, conditional
    p1 <- p1 +
      ggridges::geom_ridgeline(
        data = as.data.frame(countdf),
        ggplot2::aes(
          x = Locus, y = Groups2, height = Counts,
          fill = Groups
        ),
        alpha = 0.25
      ) +
      ggplot2::ylab('Normalized Coverage') +
      ggplot2::scale_y_continuous(
        breaks = c(1:length(countdf_tmp$Var1)),
        label = countdf_tmp$Var1
      ) +
      ggplot2::geom_text(
        data = data.frame(
          x = Inf, y = Inf,
          label = paste("Range:", 0, "-", round(max(countdf$Counts), digits = 2), sep = "")
        ),
        ggplot2::aes(x = x, y = y, label = label),
        size = 2, hjust = 0.9, vjust = 1.4
      ) +
      ggplot2::theme(legend.position = "none")
  } else {
    stop("Error: Plot type not recognized. Please check input for variable 'plotType'")
  }

  return(p1)
}

#' Get scaled values for custom breaks
#'
#' Get scaled values for custom breaks in a given value vector for use
#' defining breaks in scale_gradientn(), for example
#' @param breaks vector of breaks
#' @param x vector of weights
#'
#' @noRd
breaks_to_scaledbreaks <- function(breaks, x) {
  rescaled_weights <- scales::rescale(x)
  rescaled_breaks <- stats::quantile(rescaled_weights, probs = stats::ecdf(x)(breaks))
  return(rescaled_breaks)
}

cleanup_breaks <- function(breaks, x) {
  breaks <- base::sort(breaks)
  n_lower <- sum(breaks < min(x))
  while (n_lower > 1) {
    breaks <- breaks[-1]
    n_lower <- sum(breaks < min(x))
  }
  n_higher <- sum(breaks > max(x))
  while (n_higher > 1) {
    breaks <- breaks[-length(breaks)]
    n_higher <- sum(breaks > max(x))
  }
  return(breaks)
}


#' Get GRanges of motif annotations within a region
#'
#' Helper to return the motif annotations within a specfic region outside of
#' the plotting function. Can supply either a counts df or a region string.
#'
#' @param motifsList List of motifs
#' @param countdf A counts data frame object from getPop
#'
#' @noRd
get_motifs_in_region <- function(motifsList, countdf) {
  . <- name <- index <- NULL
  chrom <- toString(unique(countdf$chr))
  startSite <- min(countdf$Locus)
  endSite <- max(countdf$Locus)
  regionGRanges <- GenomicRanges::GRanges(
    seqnames = chrom,
    ranges = IRanges::IRanges(start = startSite, end = endSite),
    strand = "*"
  )

  specMotifs <- unlist(motifsList) %>%
    plyranges::mutate(name = gsub("_.*", "", names(.))) %>%
    plyranges::join_overlap_intersect(regionGRanges)

  if (length(specMotifs) > 0) {
    specMotifs <- specMotifs %>%
      plyranges::mutate(type = "exon") %>%
      plyranges::mutate(index = seq(1, length(.), by = 1)) %>%
      plyranges::mutate(labels = paste(name, index, sep = "_"))
  } else {
    specMotifs <- NULL
  }

  return(specMotifs)
}

#' Plot Motifs on Counts Plot
#'
#' Add motif annotation labels on an existing counts plot. If providing weights for motif
#' text colors, ensure the input counts plot is formatted accordingly (ie counts_color = "gray90")
#' for best visibility.
#'
#' @param p1 The output of `counts_plot_samples()`
#' @param countdf  A dataframe that comes from `getbpCounts()` or `getbpInserts()`
#' @param motifsList List of motifs
#' @param motif_y_space_factor A factor for vertical spacing between motif labels. Default 4. Increase to make labels farther apart, decrease to make labels closer.
#' @param motif_stagger_labels_y = FALSE Logical value, default FALSE. If TRUE, will  stagger motif labels in adjacent columns in the vertical direction
#' @param motif_weights Optional numeric vector, default NULL. If provided will be used to color motif labels by the weighted values. Values must be uniquely named
#' with motif names, for example c(`KLF5`= 3.2, `STAT1 = 0.2`, `EOMES` = -1.4`). Weights can be anything relevant, for example if the peak/region is associated with
#' a specific group/sample then global motif enrichment results for that group: `-log10(FDR)*sign(change)`
#' @param motif_weight_name Character value, default "Motif Weight". Used to label the color legend.
#' @param motif_lab_size Numeric value, default 1. Size of motif labels.
#' @param motif_lab_alpha Numeric value, default 0.25. Alpha for motif labels.
#' @param motif_line_size Numeric value, default 1. Size of motif lines.
#' @param motif_line_alpha Numeric value, default 0.25. Alpha for motif lines.
#' @return The input ggplot object with motif labels overlaid
#'
#' @importFrom magrittr %>%
#' @noRd
counts_plot_motif_overlay <- function(p1,
                                      countdf,
                                      motifsList,
                                      motif_y_space_factor = 4,
                                      motif_stagger_labels_y = FALSE,
                                      motif_weights = NULL,
                                      motif_weight_name = "Motif Weight",
                                      motif_weight_colors = c(darkblue = -10, gray = 0, darkred = 10),
                                      motif_lab_size = 1,
                                      motif_lab_alpha = 0.25,
                                      motif_line_size = 0.75,
                                      motif_line_alpha = 0.25) {
  mweight <- name <- NULL

  # Retrieve annotations in region and format
  specMotifs <- get_motifs_in_region(
    countdf = countdf,
    motifsList = motifsList
  )

  if (is.null(specMotifs)) {
    warning("No motifs found for this region")
    return(p1)
  }

  reduceMotifs <- plyranges::reduce_ranges(specMotifs, count = plyranges::n(), names = paste(labels, collapse = ","))
  splitMotifs <- strsplit(reduceMotifs$names, split = ",")

  # Calculate y spacing for motif labels
  minSize <- max(countdf$Counts) / (length(specMotifs) / motif_y_space_factor)
  y1 <- rep(0, length(specMotifs))
  names(y1) <- specMotifs$labels

  for (i in 1:length(splitMotifs)) {
    if (i %% 2 == 0 & motif_stagger_labels_y) { # stagger y spacing in neighboring columns
      start_val <- minSize / 2
    } else {
      start_val <- 0
    }

    y1[splitMotifs[[i]]] <- seq(start_val, start_val + minSize * (length(splitMotifs[[i]]) - 1), by = minSize)
  }

  # TF label coordinates and labels
  x <- NULL
  y <- NULL
  tmp_motifdf <- data.frame(
    x1 = IRanges::start(specMotifs),
    x2 = IRanges::start(specMotifs) + IRanges::width(specMotifs),
    y = y1,
    name = specMotifs$labels
  ) %>%
    tidyr::pivot_longer(cols = c("x1", "x2"), names_to = NULL, values_to = "x") %>%
    dplyr::group_by(.data$name) %>%
    dplyr::mutate(labels = ifelse(max(x) == x, gsub("_.*", "", .data$name), NA))

  # Incoroprate Weights
  if (!is.null(motif_weights)) {
    assertthat::assert_that(is.numeric(motif_weight_colors))

    if (length(intersect(names(motif_weights), tmp_motifdf$labels)) == 0) {
      warning(sprintf(
        "None of the supplied motif weight names match expected motif labels. Example motif label format: %s",
        paste(utils::head(unique(stats::na.omit(tmp_motifdf$label)), 3), collapse = ", ")
      ))
    }

    tmp_motifdf <- tmp_motifdf %>%
      dplyr::mutate(mweight = ifelse(labels %in% names(motif_weights), motif_weights[labels], NA)) %>%
      dplyr::mutate(mweight = ifelse(!all(is.na(mweight)), max(mweight, na.rm = T), NA)) %>% # still grouped by name. this ensures weights are applied to each row of same motif
      dplyr::mutate(mweight = dplyr::case_when( # clip ends within color range
        .data$mweight > max(motif_weight_colors) ~ max(motif_weight_colors),
        .data$mweight < min(motif_weight_colors) ~ min(motif_weight_colors),
        is.na(.data$mweight) ~ as.numeric(NA),
        TRUE ~ .data$mweight
      ))

    # Plot
    p1 <- p1 +
      ggplot2::geom_line(
        data = tmp_motifdf,
        ggplot2::aes(x = x, y = y, group = name, color = mweight),
        alpha = motif_line_alpha,
        size = motif_line_size
      ) +
      # vertical labels
      ggrepel::geom_text_repel(
        data = tmp_motifdf[tmp_motifdf$y > 0 & !is.na(tmp_motifdf$labels), ], # removed the NA rows to prevent warnings for intentionally missing labels
        ggplot2::aes(x = x, y = y, label = labels, color = mweight),
        direction = "x",
        arrow = grid::arrow(length = grid::unit(0.5, "mm")),
        alpha = motif_lab_alpha,
        size = motif_lab_size,
        segment.size = 0.25,
        max.overlaps = 50,
        min.segment.length = 0
      ) +
      # motif labels along x-axis
      ggrepel::geom_text_repel(
        data = tmp_motifdf[tmp_motifdf$y == 0 & !is.na(tmp_motifdf$labels), ], # removed the NA rows to prevent warnings for intentionally missing labels
        ggplot2::aes(x = x, y = y, label = labels, color = mweight),
        arrow = grid::arrow(length = grid::unit(0.5, "mm")),
        alpha = motif_lab_alpha,
        size = motif_lab_size,
        direction = "y",
        segment.size = 0.25,
        min.segment.length = 0,
        nudge_y = -max(countdf$Counts) / 20
      ) +
      ggplot2::ylim(-max(countdf$Counts) / 10, max(countdf$Counts)) +
      ggplot2::xlim(min(countdf$Locus), max(countdf$Locus))
  } else {
    # tmp_motifdf$mweight <-  1

    # Plot
    p1 <- p1 +
      ggplot2::geom_line(
        data = tmp_motifdf,
        ggplot2::aes(x = x, y = y, group = name),
        alpha = motif_line_alpha,
        size = motif_line_size
      ) +
      # vertical labels
      ggrepel::geom_text_repel(
        data = tmp_motifdf[tmp_motifdf$y > 0 & !is.na(tmp_motifdf$labels), ], # removed the NA rows to prevent warnings for intentionally missing labels
        ggplot2::aes(x = x, y = y, label = labels),
        direction = "x",
        arrow = grid::arrow(length = grid::unit(0.5, "mm")),
        alpha = motif_lab_alpha,
        size = motif_lab_size,
        segment.size = 0.25,
        max.overlaps = 50,
        min.segment.length = 0
      ) +
      # motif labels along x-axis
      ggrepel::geom_text_repel(
        data = tmp_motifdf[tmp_motifdf$y == 0 & !is.na(tmp_motifdf$labels), ], # removed the NA rows to prevent warnings for intentionally missing labels
        ggplot2::aes(x = x, y = y, label = labels),
        arrow = grid::arrow(length = grid::unit(0.5, "mm")),
        alpha =  motif_lab_alpha,
        size = motif_lab_size,
        direction = "y",
        segment.size = 0.25,
        min.segment.length = 0,
        nudge_y = -max(countdf$Counts) / 20
      ) +
      ggplot2::ylim(-max(countdf$Counts) / 10, max(countdf$Counts)) +
      ggplot2::xlim(min(countdf$Locus), max(countdf$Locus))
  }

  # Color text by motif weights
  if (!is.null(motif_weights)) {
    scaled_breaks <- breaks_to_scaledbreaks(motif_weight_colors, stats::na.omit(tmp_motifdf$mweight))

    p1 <- p1 + ggplot2::scale_color_gradient(
      colors = names(motif_weight_colors), values = scaled_breaks,
      na.value = "gray", # may want to parameterize this
      name = motif_weight_name
    )
  } # else {
  # p1 <- p1 #scale_color_gradient(low = "black", high = "black", na.value = "gray", name = motif_weight_name, guide = "none")
  # scale_color_manual(values = "gray", breaks = 1, na.value = "gray", name = motif_weight_name, guide = "none")
  # scale_color_discrete("gray", name = motif_weight_name, guide = "none")
  # }

  return(p1)
}

#' Common theme for gene plots
.gene_plot_theme <- list(
  axis.text.y = ggplot2::element_blank(),
  axis.ticks.y  = ggplot2::element_blank(),
  panel.grid.minor.y =  ggplot2::element_blank(),
  panel.grid.major.y =  ggplot2::element_blank(),
  plot.margin = grid::unit(c(0, .5, .5, .5), "cm")
)

#' Generate a link plot from a dataframe of co-accessible links
#'
#' @param regionGRanges GRanges containing the regions to plot
#' @param legend.position legend.position
#' @param relativeHeights named list of tracks and relative track heights
#' @param linkdf Dataframe of co-accessible links from getCoAccessibleLinks
#'
#' @noRd
get_link_plot <- function(regionGRanges, legend.position = NULL,
                          relativeHeights, linkdf) {
  start <- end <- y <- Correlation <- NULL

  linkdf2 <- dplyr::filter(linkdf, .data$start + 250 > GenomicRanges::start(regionGRanges) & end - 250 < GenomicRanges::end(regionGRanges))

  ## Set Curvature to fit window
  if (is.null(legend.position) & relativeHeights["Links"] / 3 <= 0.5) {
    curveVal <- relativeHeights["Links"] / 3 - 0.05
  } else if (is.null(legend.position)) {
    curveVal <- 0.5
  } else if (relativeHeights["Links"] / 3 <= 0.5) {
    curveVal <- relativeHeights["Links"] / 3 - 0.2
  } else {
    curveVal <- 0.4
  }

  p5 <- ggplot2::ggplot() +
    ggplot2::geom_curve(ggplot2::aes(
      x = start + 250, xend = end - 250,
      y = y, yend = y, color = Correlation
    ),
    curvature = curveVal,
    data = cbind(linkdf2, y = rep(0, dim(linkdf2)[1]))
    ) +
    ggplot2::theme_minimal() +
    ggplot2::ylab(NULL) +
    ggplot2::xlab(NULL) +
    ggplot2::ylim(-1, 0) +
    ggplot2::scale_colour_viridis_c(breaks = c(
      ceiling(10 * min(linkdf2$Correlation)) / 10,
      0,
      floor(10 * max(linkdf2$Correlation)) / 10
    )) +
    #ggplot2::coord_cartesian(clip = "off", ylim = c(-0.75, 0)) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank()
    )

  return(p5)
}
