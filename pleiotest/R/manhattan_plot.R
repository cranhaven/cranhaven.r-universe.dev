#' @title Single Trait Manhattan plot
#' @description Manhattan plot of results from mt_gwas function.
#' @param mt_gwas_results Object returned by mt_gwas
#' @param trait integer indicating the position of the trait (see: names(mt_gwas_results)) to be plotted.
#' @param bp_positions dataframe with SNPs base pair positions. colnames msut be 'chr' and 'position', rownames must be SNP identifiers matching names in mt_gwas.
#' @param ... further graphical parameters. Options include: title=, bty=, pch=, cex.lab=, and cex.main=.
#' @return Manhattan plot
#' @export
manhattan_plot <- function(mt_gwas_results, trait, bp_positions, ...){
  colnames(bp_positions) <- c('chr', 'position')
  result_trait <- mt_gwas_results[[trait]]

  if(length(unlist(strsplit(rownames(result_trait)[1], '_'))) == 2){
    rownames(result_trait) <- sapply(strsplit(rownames(result_trait), '_'), function(x) x[1])
  }
  matched_bp <- match(rownames(result_trait), rownames(bp_positions))
  result_trait <- cbind(result_trait, bp_positions[matched_bp,])
  result_trait <- result_trait[order(result_trait$chr, result_trait$position),]
  my_colors <- as.factor(result_trait$chr)
  levels(my_colors) <- rep(c('blue1', 'darkorange3'), length.out = 26)
  graphics::plot(1:nrow(result_trait), -log10(result_trait$`p value`), col = as.character(my_colors),
       cex = .8, xaxt = 'n', xlab = 'chromosome', ylab = '-log10(p value)', ...)
  x_axis <- stats::aggregate(1:nrow(result_trait) ~ result_trait$chr, FUN = function(x) round(mean(x)))
  graphics::axis(1, x_axis[,2], x_axis[,1])
}
