#' Data displayer
#'
#' @param windowed_shift GRanges containing windowed data (as loaded by import.smvshft)
#' @param selected_parent Name of the selected-for population
#' @param backcrossed_parent Name of the backcrossed-too population
#' @param contigs What contigs to display
#' @param main_title What to call the plot
#' @param ref_gen Name of the reference genome
#' @param primary_aesthetic Primary aesthetic
#' @param envelope_aesthetic envelope aesthetic
#' @param ancestral_aesthetic ancestral aesthetic
#'
#' @return a ggbio plot object
#' @export
#'
#' @examples
#'\donttest{
#' windowed_shifts.filename <- system.file("extdata",
#' "windowed_shifts.example_data.bed", package = "PopPsiSeqR")
#' windowed_shifts.bg <- import.smvshift(windowed_shifts.filename)
#' windowedFrequencyShift.plotter(windowed_shifts.bg)
#' }
windowedFrequencyShift.plotter <- function(windowed_shift, selected_parent="sim", backcrossed_parent = "sec", contigs = c("chr2L","chr2R","chr3L","chr3R"), main_title = "popPsiSeq results", ref_gen = "droSim1", primary_aesthetic = ggplot2::aes(), envelope_aesthetic = ggplot2::aes() , ancestral_aesthetic = ggplot2::aes()){

  windowed_shift.autosomal.df <- windowed_shift %>% as.data.frame() %>% dplyr::filter(.data$seqnames %in% contigs)
  windowed_shift.autosomal.df$experiment <- windowed_shift.autosomal.df[[paste("avg_",selected_parent,"ward_AFshift", sep="")]]
  windowed_shift.autosomal.df$max_selection <- windowed_shift.autosomal.df[[paste("max_",selected_parent,"ward_AFshift", sep="")]]
  windowed_shift.autosomal.df$min_selection <- windowed_shift.autosomal.df[[paste("min_",selected_parent,"ward_AFshift", sep="")]]
  windowed_shift.autosomal.df$parental_difference <- windowed_shift.autosomal.df[[paste(selected_parent,"_",backcrossed_parent,"_difference", sep="")]]

  windowed_shift.autosomal.bg <- windowed_shift.autosomal.df %>% GenomicRanges::GRanges()


  primary_aesthetic$y<- ggplot2::aes(y=.data$experiment)$y
  if ("colour" %in% names(primary_aesthetic ) | "color" %in% names(primary_aesthetic ) ) {
    figure.out <- ggbio::autoplot(windowed_shift.autosomal.bg, primary_aesthetic, geom='line')
  } else {
    figure.out <- ggbio::autoplot(windowed_shift.autosomal.bg, primary_aesthetic, color = "black" , geom='line')
  }


  envelope_aesthetic$y <- ggplot2::aes(y=.data$max_selection)$y
  figure.out <-  figure.out + ggplot2::geom_line(envelope_aesthetic, linetype = "dashed")
  envelope_aesthetic$y <- ggplot2::aes(y=.data$min_selection)$y
  figure.out <-  figure.out + ggplot2::geom_line(envelope_aesthetic, linetype = "dashed")


  ancestral_aesthetic$y <- ggplot2::aes(y= .data$parental_difference/2)$y
  figure.out <-  figure.out + ggplot2::geom_line(ancestral_aesthetic, linetype = "dotted")
  ancestral_aesthetic$y <- ggplot2::aes(y= -.data$parental_difference/2)$y
  figure.out <-  figure.out + ggplot2::geom_line(ancestral_aesthetic, linetype = "dotted")

  figure.out <-  figure.out + ggplot2::geom_hline(yintercept = 0, alpha = 0.2)  + ggplot2::facet_wrap(~.data$seqnames, scales = "free_x") + ggplot2::labs(y="Selection Allele Frequency Shift", title = main_title, subtitle = " ", caption ="thick dashed line - hard bounds; fine dotted line - parent populations ",x= paste("coordinate (", ref_gen," reference genome)", sep = "" ) ) + ggbio::theme_clear() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))

  #      figure.out <- figure.out + ggplot2::geom_line(ggplot2::aes(y=min_selection), color ="black", linetype = "dashed")  + ggplot2::geom_line(ggplot2::aes(y=parental_difference/2), color ="black", linetype = "dotted")  + ggplot2::geom_line(ggplot2::aes(y=-parental_difference/2), color ="black", linetype = "dotted")  +ggplot2::geom_hline(yintercept = 0, alpha = 0.2)  + ggplot2::facet_wrap(~seqnames, scales = "free_x") + ggplot2::labs(y="Selection Allele Frequency Shift", title = main_title, subtitle = " ", caption ="thick dashed line - hard bounds; fine dotted line - parent populations ",x= paste("coordinate (", ref_gen," reference genome)", sep = "" ) ) + ggbio::theme_clear() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))



  return(figure.out)

}


