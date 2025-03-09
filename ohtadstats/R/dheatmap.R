#' Heatmap Plot
#' 
#' Plots a matrix of D statistics, output from dwrapper, as a heatmap.
#' 
#' @param d_matrix A matrix of D statistics or a matrix of D statistic ratios. 
#' @param colors An optional color vector. Optionally modify the color scheme of the heatmap. If mode = 'binned', must be of length 5.
#' @param mode A string indicating desired coloring scheme. The option "linear" scales
#' colors linearly, "truncated" truncates values greater than 1, and "binned" returns
#' a discretedistribution of colors.
#' @param tick.labels A logical indicating whether or not marker labels should be drawn. 
#' @param nbins An integer specifying the number of bins to be used. Only relevent if mode is "binned".
#' 
#' @return A color plot
#'
#' @details
#' The d_matrix input should be one of the matrices output by dwrapper. Options are d2it_mat, d2is_mat, d2st_mat, dp2st_mat, dp2is_mat, npops_mat, ratio1, and ratio2.
#' More customized plots can be developed using the "levelplot" package.
#' 
#' @examples
#' data(miyashita_langley_data)
#' miyashita_langley_subset <- miyashita_langley_data[,1:15]
#' ml_results <- dwrapper(miyashita_langley_subset)
#' dheatmap(ml_results[["d2it_mat"]], mode = 'linear')
#' 
#' \dontrun{
#' data(miyashita_langley_data)
#' ml_results <- dwrapper(miyashita_langley_data)
#' dheatmap(ml_results[["d2it_mat"]], mode = 'linear')
#' }
#' 
#' @export
dheatmap <- function(d_matrix, colors = c("white", "lightblue", "blue", "darkblue", "black"), mode = "linear", tick.labels = TRUE, nbins = 5){
  d_matrix <- t(d_matrix)[,dim(d_matrix)[1]:1]
	if (mode == 'linear'){
    heatmaps <- lattice::levelplot(d_matrix,
                          col.regions = grDevices::colorRampPalette(colors),
                          space = "rgb",
                          xlab = "Marker", ylab = "Marker",
                          scales = list(x = list(rot=90), draw = tick.labels))
  }
  else if (mode == 'truncated'){
    d_matrix[which(d_matrix > 1)] <- 1
    heatmaps <- lattice::levelplot(d_matrix,
                          col.regions = grDevices::colorRampPalette(colors),
                          space = "rgb",
                          xlab = "Marker", ylab = "Marker",
                          scales = list(x = list(rot=90), draw = tick.labels),
                          colorkey=list(space="right", col=colors, at=c(0,0.25,0.5,0.75,1,1.1), labels=c("0","0.25","0.5","0.75","1",">1.0")))
  }
  else if (mode == 'binned'){ # Modify to use the 'cuts' attribute
    if (length(colors) >= nbins){
    d_matrix[which(d_matrix > 1)] <- 2
    dmax <- max(d_matrix[which(d_matrix != Inf)], na.rm = TRUE)
    heatmaps <- lattice::levelplot(d_matrix,
                          col.regions = grDevices::colorRampPalette(colors),
                          space = 'rgb',
                          xlab = "Marker", ylab = "Marker",
                          scales = list(x = list(rot=90), draw = tick.labels),
                          cuts = nbins - 1,
                          colorkey = list(space = "right",
                                          col = colors, 
                                          at = seq(from = 0, to = dmax, by = dmax/nbins),
                                          labels = as.character(seq(from = 0, to = dmax, by = dmax/nbins))))
    }
    else{
      stop('Color vector must be greater than or equal to the desired number of bins.')
    }
  }
  else {
    stop('Please use a valid mode. "linear", "truncated", or "binned".')
  }
  return(heatmaps)
}
