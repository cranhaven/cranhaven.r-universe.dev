########################################################################################################################################################
##' @title Generic function to plot curves
##'
##' @description This function is a generic function to plot curves.
##'
##' @param x An object for which the curves are to be plotted.
##' @param ... Additional arguments affecting the output.
##'
##'
##' @examples
##' \donttest{
##' library(sp)
##' data("map_sites")
##' data("funi_data")
##' coords <- coordinates(map_sites)
##'
##' res_npfss <- SpatialScan(method = "NPFSS", data = funi_data, sites_coord = coords, system = "WGS84",
##' mini = 1, maxi = nrow(coords)/2)$NPFSS
##'
##' plotCurves(x = res_npfss, add_mean = TRUE, add_median = TRUE)}
##' \dontshow{
##' library(sp)
##' data("map_sites")
##' data("funi_data")
##' indices <- c(51:75)
##' coords <- coordinates(map_sites[indices,])
##' res_npfss <- SpatialScan(method = "NPFSS", data = funi_data[indices,], sites_coord = coords,
##' system = "WGS84", mini = 1, maxi = nrow(coords)/2, MC = 99)$NPFSS
##' if(length(res_npfss$sites_clusters)>0){
##' plotCurves(x = res_npfss, add_mean = TRUE, add_median = TRUE)
##' }
##'
##' }
##'
##' @return No value returned, plots the curves.
##'
##' @seealso \code{\link{plotCurves.ResScanOutputUniFunct}} and \code{\link{plotCurves.ResScanOutputMultiFunct}}
##'
##' @export
##'
plotCurves <- function(x, ...){
  UseMethod("plotCurves",x)
}

########################################################################################################################################################
##' @title Plots the curves in the clusters detected by the univariate functional scan functions (PFSS, NPFSS, DFFSS or URBFSS)
##'
##' @description This function plot the curves in the clusters detected by the univariate functional scan functions (PFSS, NPFSS, DFFSS or URBFSS).
##'
##' @param x ResScanOutputUniFunct. Output of a univariate functional scan function (PFSS, NPFSS, DFFSS or URBFSS).
##' @param add_mean boolean. If TRUE it adds the global mean curve in black.
##' @param add_median boolean. If TRUE it adds the global median curve in blue.
##' @param colors character. The colors to plot the clusters' curves. If length(colors)==1 then all the clusters will be plotted in this color. Else there must be the same number of elements in colors than the number of clusters
##' @param only.MLC logical. Should we plot only the MLC or all the significant clusters?
##' @param ... Further arguments to be passed to or from methods.
##'
##' @examples
##' \donttest{
##' library(sp)
##' data("map_sites")
##' data("funi_data")
##' coords <- coordinates(map_sites)
##'
##' res_npfss <- SpatialScan(method = "NPFSS", data = funi_data, sites_coord = coords,
##' system = "WGS84", mini = 1, maxi = nrow(coords)/2)$NPFSS
##'
##' plotCurves(x = res_npfss, add_mean = TRUE, add_median = TRUE)}
##' \dontshow{
##' library(sp)
##' data("map_sites")
##' data("funi_data")
##' indices <- c(51:75)
##' coords <- coordinates(map_sites[indices,])
##' res_npfss <- SpatialScan(method = "NPFSS", data = funi_data[indices,],
##' sites_coord = coords,
##' system = "WGS84", mini = 1, maxi = nrow(coords)/2, MC = 99)$NPFSS
##' if(length(res_npfss$sites_clusters)>0){
##' plotCurves(x = res_npfss, add_mean = TRUE, add_median = TRUE)
##' }
##'
##' }
##'
##' @return No value returned, plots the curves.
##'
##' @export
##'
plotCurves.ResScanOutputUniFunct <- function(x, add_mean = FALSE, add_median = FALSE, colors = "red", only.MLC = FALSE, ...){

  if(is.logical(only.MLC) == FALSE){
    stop("only.MLC must be logical")
  }

  if(length(x$sites_clusters) == 0){
    stop("No significant cluster has been detected by the scan procedure")
  }

  if(length(colors) == 1){
    if(colors == "black" | colors == "grey" | colors == "blue"){
      stop("The color provided cannot be black, grey or blue")
    }
  }else{
    if(only.MLC == TRUE){
      stop("if only.MLC is TRUE only one color must be provided")
    }

    if(length(colors) != length(x$sites_clusters)){
      stop("colors must be of length 1 or with as many colors as the length of output_clusters")
    }else{
      if(sum(colors %in% c("black", "blue", "grey")) > 0){
        stop("The colors provided cannot be black, grey or blue")
      }
    }
  }

  if(is.null(x$times)){
    times <- seq(from = 0, to = 1, length.out = ncol(x$data))
  }else{
    if(length(x$times) != ncol(x$data)){
      stop("There must be the same number of times in times and in the data")
    }else{
      times <- x$times
    }
  }

  if(only.MLC == FALSE){
    for(i in 1:length(x$sites_clusters)){
      plot(NULL, ylim = range(x$data), xlim = range(times), xlab = "Time", ylab = "", main = paste("Cluster ", i, sep = ""), cex.lab = 0.8, cex.main = 0.8, cex.axis = 0.8)
      for(elem in 1:nrow(x$data)){
        lines(x = times, y = x$data[elem,], col = "grey", lwd = 1.2)
      }
      for(elem in x$sites_clusters[[i]]){
        if(length(colors)==1){
          lines(x = times, y = x$data[elem,], col = colors, lwd = 1.2)
        }else{
          lines(x = times, y = x$data[elem,], col = colors[i], lwd = 1.2)
        }
      }

      if(length(colors) == 1){
        col_legend <- c(colors, "grey")
      }else{
        col_legend <- c(colors[i], "grey")
      }
      text_legend <- c("Sites included in the cluster", "Sites not included in the cluster")
      lty_legend <- c(1,1)
      lwd_legend <- c(1.2,1.2)


      if(add_mean == TRUE){
        mean_curve <- colMeans(x$data)
        lines(x = times, y = mean_curve, col = "black", lwd = 1.5)
        col_legend <- c(col_legend, "black")
        text_legend <- c(text_legend, "Global mean")
        lty_legend <- c(lty_legend,1)
        lwd_legend <- c(lwd_legend, 1.5)
      }
      if(add_median == TRUE){
        median_curve <- colMedians(x$data)
        lines(x = times, y = median_curve, col = "blue", lwd = 1.5)
        col_legend <- c(col_legend, "blue")
        text_legend <- c(text_legend, "Global median")
        lty_legend <- c(lty_legend,1)
        lwd_legend <- c(lwd_legend, 1.5)
      }
      legend("topleft", legend = text_legend, col = col_legend, lty = lty_legend, lwd = lwd_legend, bty = "n", cex = 0.8)

    }
  }else{
    i <- 1
    plot(NULL, ylim = range(x$data), xlim = range(times), xlab = "Time", ylab = "", main = paste("Cluster ", i, sep = ""), cex.lab = 0.8, cex.main = 0.8, cex.axis = 0.8)
    for(elem in 1:nrow(x$data)){
      lines(x = times, y = x$data[elem,], col = "grey", lwd = 1.2)
    }
    for(elem in x$sites_clusters[[i]]){
      if(length(colors)==1){
        lines(x = times, y = x$data[elem,], col = colors, lwd = 1.2)
      }else{
        lines(x = times, y = x$data[elem,], col = colors[i], lwd = 1.2)
      }
    }

    if(length(colors) == 1){
      col_legend <- c(colors, "grey")
    }else{
      col_legend <- c(colors[i], "grey")
    }
    text_legend <- c("Sites included in the cluster", "Sites not included in the cluster")
    lty_legend <- c(1,1)
    lwd_legend <- c(1.2,1.2)


    if(add_mean == TRUE){
      mean_curve <- colMeans(x$data)
      lines(x = times, y = mean_curve, col = "black", lwd = 1.5)
      col_legend <- c(col_legend, "black")
      text_legend <- c(text_legend, "Global mean")
      lty_legend <- c(lty_legend,1)
      lwd_legend <- c(lwd_legend, 1.5)
    }
    if(add_median == TRUE){
      median_curve <- colMedians(x$data)
      lines(x = times, y = median_curve, col = "blue", lwd = 1.5)
      col_legend <- c(col_legend, "blue")
      text_legend <- c(text_legend, "Global median")
      lty_legend <- c(lty_legend,1)
      lwd_legend <- c(lwd_legend, 1.5)
    }
    legend("topleft", legend = text_legend, col = col_legend, lty = lty_legend, lwd = lwd_legend, bty = "n", cex = 0.8)


  }


}

########################################################################################################################################################
##' @title Plots the curves in the clusters detected by the multivariate functional scan functions (MPFSS, NPFSS, MDFFSS or MRBFSS)
##'
##' @description This function plot the curves in the clusters detected by the multivariate functional scan functions (MPFSS, NPFSS, MDFFSS or MRBFSS).
##'
##' @param x ResScanOutputMultiFunct. Output of a multivariate functional scan function (MPFSS, NPFSS, MDFFSS or MRBFSS).
##' @param add_mean boolean. If TRUE it adds the global mean curve in black.
##' @param add_median boolean. If TRUE it adds the global median curve in blue.
##' @param colors character. The colors to plot the clusters' curves. If length(colors)==1 then all the clusters will be plotted in this color. Else there must be the same number of elements in colors than the number of clusters
##' @param only.MLC logical. Should we plot only the MLC or all the significant clusters?
##' @param ... Further arguments to be passed to or from methods.
##'
##' @examples
##' \donttest{
##' library(sp)
##' data("map_sites")
##' data("fmulti_data")
##' coords <- coordinates(map_sites)
##'
##' res_npfss <- SpatialScan(method = "NPFSS", data = fmulti_data, sites_coord = coords,
##' system = "WGS84", mini = 1, maxi = nrow(coords)/2)$NPFSS
##'
##' plotCurves(x = res_npfss, add_mean = TRUE, add_median = TRUE)}
##' \dontshow{
##' library(sp)
##' data("map_sites")
##' data("fmulti_data")
##' indices <- c(51:75)
##' coords <- coordinates(map_sites[indices,])
##' res_npfss <- SpatialScan(method = "NPFSS", data = fmulti_data[indices],
##' sites_coord = coords,
##' system = "WGS84", mini = 1, maxi = nrow(coords)/2, MC = 99)$NPFSS
##' if(length(res_npfss$sites_clusters)>0){
##' plotCurves(x = res_npfss, add_mean = TRUE, add_median = TRUE)
##' }
##'
##' }
##'
##' @return No value returned, plots the curves.
##'
##' @export
##'
plotCurves.ResScanOutputMultiFunct <- function(x, add_mean = FALSE, add_median = FALSE, colors = "red", only.MLC = FALSE, ...){

  if(is.logical(only.MLC) == FALSE){
    stop("only.MLC must be logical")
  }

  if(length(x$sites_clusters) == 0){
    stop("No significant cluster has been detected by the scan procedure")
  }

  if(length(colors) == 1){
    if(colors == "black" | colors == "grey" | colors == "blue"){
      stop("The color provided cannot be black, grey or blue")
    }
  }else{
    if(only.MLC == TRUE){
      stop("if only.MLC is TRUE only one color must be provided")
    }
    if(length(colors) != length(x$sites_clusters)){
      stop("colors must be of length 1 or with as many colors as the number of clusters")
    }else{
      if(sum(colors %in% c("black", "blue", "grey")) > 0){
        stop("The colors provided cannot be black, grey or blue")
      }
    }
  }
  if(is.null(x$variable_names)){
    variable_names <- paste("var", c(1:nrow(x$data[[1]])), sep = "")
  }else{
    if(length(x$variable_names) != nrow(x$data[[1]])){
      stop("There must be the same number of elements in variable_names than the number of variables in data")
    }else{
      variable_names <- x$variable_names
    }
  }

  if(is.null(x$times)){
    times <- seq(from = 0, to = 1, length.out = ncol(x$data[[1]]))
  }else{
    if(length(x$times) != ncol(x$data[[1]])){
      stop("There must be the same number of times in times and in the data")
    }else{
      times <- x$times
    }
  }

  if(only.MLC == FALSE){
    for(i in 1:length(x$sites_clusters)){
      for(v in 1:nrow(x$data[[1]])){
        temp <- matrix(ncol = ncol(x$data[[1]]), nrow = length(x$data))
        for(l in 1:length(x$data)){
          temp[l,] <- x$data[[l]][v,]
        }

        plot(NULL, ylim = range(temp), xlim = range(times), xlab = "Time", ylab = variable_names[v], main = paste("Cluster ", i, sep = ""), cex.lab = 0.8, cex.main = 0.8, cex.axis = 0.8)
        for(elem in 1:nrow(temp)){
          lines(x = times, y = temp[elem,], col = "grey", lwd = 1.2)
        }

        for(elem in x$sites_clusters[[i]]){
          if(length(colors) == 1){
            lines(x = times, y = temp[elem,], col = colors, lwd = 1.2)
          }else{
            lines(x = times, y = temp[elem,], col = colors[i], lwd = 1.2)
          }
        }
        if(length(colors) == 1){
          col_legend <- c(colors, "grey")
        }else{
          col_legend <- c(colors[i], "grey")
        }
        text_legend <- c("Sites included in the cluster", "Sites not included in the cluster")
        lty_legend <- c(1,1)
        lwd_legend <- c(1.2,1.2)

        if(add_mean == TRUE){
          mean_curve <- colMeans(temp)
          lines(x = times, y = mean_curve, col = "black", lwd = 1.5)
          col_legend <- c(col_legend, "black")
          text_legend <- c(text_legend, "Global mean")
          lty_legend <- c(lty_legend,1)
          lwd_legend <- c(lwd_legend, 1.5)
        }
        if(add_median == TRUE){
          median_curve <- colMedians(temp)
          lines(x = times, y = median_curve, col = "blue", lwd = 1.5)
          col_legend <- c(col_legend, "blue")
          text_legend <- c(text_legend, "Global median")
          lty_legend <- c(lty_legend,1)
          lwd_legend <- c(lwd_legend, 1.5)
        }
        legend("topleft", legend = text_legend, col = col_legend, lty = lty_legend, lwd = lwd_legend, bty = "n", cex = 0.8)
      }
    }
  }else{
    i <- 1
    for(v in 1:nrow(x$data[[1]])){
      temp <- matrix(ncol = ncol(x$data[[1]]), nrow = length(x$data))
      for(l in 1:length(x$data)){
        temp[l,] <- x$data[[l]][v,]
      }

      plot(NULL, ylim = range(temp), xlim = range(times), xlab = "Time", ylab = variable_names[v], main = paste("Cluster ", i, sep = ""), cex.lab = 0.8, cex.main = 0.8, cex.axis = 0.8)
      for(elem in 1:nrow(temp)){
        lines(x = times, y = temp[elem,], col = "grey", lwd = 1.2)
      }

      for(elem in x$sites_clusters[[i]]){
        if(length(colors) == 1){
          lines(x = times, y = temp[elem,], col = colors, lwd = 1.2)
        }else{
          lines(x = times, y = temp[elem,], col = colors[i], lwd = 1.2)
        }
      }
      if(length(colors) == 1){
        col_legend <- c(colors, "grey")
      }else{
        col_legend <- c(colors[i], "grey")
      }
      text_legend <- c("Sites included in the cluster", "Sites not included in the cluster")
      lty_legend <- c(1,1)
      lwd_legend <- c(1.2,1.2)

      if(add_mean == TRUE){
        mean_curve <- colMeans(temp)
        lines(x = times, y = mean_curve, col = "black", lwd = 1.5)
        col_legend <- c(col_legend, "black")
        text_legend <- c(text_legend, "Global mean")
        lty_legend <- c(lty_legend,1)
        lwd_legend <- c(lwd_legend, 1.5)
      }
      if(add_median == TRUE){
        median_curve <- colMedians(temp)
        lines(x = times, y = median_curve, col = "blue", lwd = 1.5)
        col_legend <- c(col_legend, "blue")
        text_legend <- c(text_legend, "Global median")
        lty_legend <- c(lty_legend,1)
        lwd_legend <- c(lwd_legend, 1.5)
      }
      legend("topleft", legend = text_legend, col = col_legend, lty = lty_legend, lwd = lwd_legend, bty = "n", cex = 0.8)
    }

  }


}
