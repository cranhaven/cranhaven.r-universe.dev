#' Complex network analysis
#' 
#' @description `cnet` plots the canonical network map of a single classification scheme. 
#'
#' @param x regs object. 
#' @param n number of regions.
#' @param thres the cross-correlation threshold of the network.
#' @return plot object
#' 
#' @details The `cnet` function estimates the cross-correlation matrix of the average time series of 
#' each region and plots a map linking the regions with cross-correlations above the selected threshold.
#' 
#' @examples
#' \donttest{
#' dummy <- owda[Time <= 1600]
#' inp_som <- sominp(dummy)
#' my_som <- somspa(inp_som, rlen = 100, grid = somgrid(3, 3, "hexagonal"))
#' my_regions <- somregs(my_som, nregions = 6) 
#' cnet(my_regions, n = 5, thres = 0.2)}
#' @importFrom graphics par plot points segments text
#' @importFrom reshape2 melt
#' @export

cnet <- function(x, n, thres){
  aa_cor <- reshape2::melt(cor_regs(x, n))
  aa_cor <- data.table(aa_cor[aa_cor$value != 1,])
  aa_cor_thres <- aa_cor[value > thres]
  
  aa_coords <- unique(x$regions[, .(lat = mean(lat), lon = mean(lon)), x$regions[[9 + n]]])
  colnames(aa_coords)[1] = "region"
  
  aa_merge = merge(aa_coords, aa_cor_thres, by.x = "region", by.y = "Var1")
  aa_cor_thres <- merge(aa_merge, aa_coords, by.y = "region", by.x = "Var2")
  colnames(aa_cor_thres) = c('reg.1', 'reg.2', 'lat.2', 'lon.2', 'value', 'lat.1', 'lon.1')
  
  plot(x, n)
  segments(aa_cor_thres$lon.1, aa_cor_thres$lat.1, 
           aa_cor_thres$lon.2, aa_cor_thres$lat.2, 
           col = "black", lwd = 2)
  points(aa_coords$lon, aa_coords$lat, cex = 3, pch = 16)
  text(aa_coords$lon, aa_coords$lat, labels = aa_coords$region, cex = 0.7, col = "grey90")
}

globalVariables("value")