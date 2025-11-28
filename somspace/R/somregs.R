#' Classify SOM into regions
#' 
#' @description `somregs` applies hierarchical cluster analysis to the Self-Organizing Map 
#' to form regions with homogeneous characteristics (classification scheme).
#'
#' @param x A `somsp` object.
#' @param nregions The maximum number of classifications schemes to be determined starting from 2. 
#' @param ... Other arguments passed to methods from `hclust` function which is used to determine the regions.
#' 
#' @details `nregions` must be at least two, i.e., a classification scheme with two regions, and smaller than
#' the number of SOM nodes. In the latter case, each SOM node corresponds to a region. 
#' The resulting `regs` object can be plotted by `plot` and `plot_ts`. 
#' If `plot` is used, three additional arguments are needed; a set with the classification schemes 
#' that will be ploted, number of rows and number of columns of the plotted panels.
#' `plot_ts` plots all the time series of a given classification scheme.
#' 
#' @return A `regs` object, which contains: 
#' 
#' \itemize{
#' 
#' \item{A summary `data.table` which updates the `somsp` object with the region ids of all classification schemes
#' up to `nregions`. Each different classification scheme is stored as an individual region, e.g., `regions.2`, 
#' `regions.3`, etc.
#' to their corresponding winning unit, the number of points of each node, as well as the median 
#' latitude and longitude of each node coordinates and their standard deviation.}
#' 
#' \item{The original time series which created the SOM as a `data.table`, as in `somsp`.}
#' }
#' 
#' @seealso \code{\link{somsp}} 
#' @seealso \code{\link{somspa}} 
#' 
#' @examples
#' \donttest{
#' dummy <- owda[Time <= 1600]
#' inp_som <- sominp(dummy)
#' my_som <- somspa(inp_som, rlen = 100, grid = somgrid(4, 4, "hexagonal"))
#' my_regions <- somregs(my_som, nregions = 9) 
#' plot(my_regions, regions = c(2, 4, 6, 8), nrow = 2, ncol = 2) 
#' plot_ts(my_regions, n = 4)}
#' 
#' @export

somregs <- function(x, nregions, ...){
  out <- list()
  temp <- data.table(x$summary)
  for(i in 2:nregions){
    som_hc <- unname(cutree(hclust(dist(x$som$codes[[1]]), ...), i))
    temp[, paste0("regions.", i) := som_hc[temp$node]]
  }
  out$regions <- reg_rename(temp)
  out$input_dt <- x$input_dt
  class(out) <- "regs"
  return(out)
}

reg_rename <- function(dta) {
  reg_numbers_old <- dta[, grepl('regions', colnames(dta)), with = F] 
  reg_numbers_raw <- as.numeric(gsub('regions.', '', names(dta[, grepl('regions', colnames(dta)), with = F])))
  id <- 1:dim(dta)[1]
  reg_id <- list()
  
  for(j in 1:dim(reg_numbers_old)[2]) {
    tmp <- list()
    for(i in 1:max(reg_numbers_old[, j, with = F])) {
      tmp[[i]] <- id[which(reg_numbers_old[, j, with = F] == i)]
    }
    reg_id[[j]] <- tmp
    names(reg_id[[j]]) <- paste0(1:max(reg_numbers_old[, j,  with = F]))
  }
  
  for(i in 1:(dim(reg_numbers_old)[2] - 1)) {
    names(reg_id[[i + 1]][reg_id[[i + 1]] %in% reg_id[[i]]])
    names(reg_id[[i + 1]]) <- as.character(names(reg_id[[i + 1]]))
    names(reg_id[[i + 1]])[which(reg_id[[i + 1]] %in% reg_id[[i]])] <- names(reg_id[[i]])[which(reg_id[[i]] %in% reg_id[[i + 1]])]
    
    if(length(reg_id[[i + 1]][!(reg_id[[i + 1]] %in% reg_id[[i]])]) != 2) next 
    if(length(reg_id[[i + 1]][!(reg_id[[i + 1]] %in% reg_id[[i]])][[1]]) >= length(reg_id[[i + 1]][!(reg_id[[i + 1]] %in% reg_id[[i]])][[2]])) {
      names(reg_id[[i + 1]])[!(reg_id[[i + 1]] %in% reg_id[[i]])][1] <- names(reg_id[[i]])[!(reg_id[[i]] %in% reg_id[[i + 1]])]
      names(reg_id[[i + 1]])[!(reg_id[[i + 1]] %in% reg_id[[i]])][2] <- reg_numbers_raw[i + 1]
    } else {
      names(reg_id[[i + 1]])[!(reg_id[[i + 1]] %in% reg_id[[i]])][1] <- reg_numbers_raw[i + 1]
      names(reg_id[[i + 1]])[!(reg_id[[i + 1]] %in% reg_id[[i]])][2] <- names(reg_id[[i]])[!(reg_id[[i]] %in% reg_id[[i + 1]])]
    }
  }
  
  reg_numbers_new <- as.data.frame(reg_numbers_old)
  
  for(i in 1:length(reg_id)) {
    for(j in 1:length(reg_id[[i]])) {
      reg_numbers_new[, i][reg_id[[i]][[j]]] <- as.numeric(names(reg_id[[i]])[j])
    }
  }
  
  out <- as.data.frame(dta)
  out[, grepl('regions', colnames(dta))] <- reg_numbers_new
  
  return(data.table(out))
}
