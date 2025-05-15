########################################################################################################################################################
##' @title Generic function to plot a summary
##'
##' @description This function is a generic function to plot a summary.
##'
##' @param x An object for which the summary is to be plotted.
##' @param ... Additional arguments affecting the summary produced.
##'
##' @examples
##' \donttest{
##' library(sp)
##' data("map_sites")
##' data("multi_data")
##' coords <- coordinates(map_sites)
##'
##' res_mnp <- SpatialScan(method = "MNP", data = multi_data, sites_coord = coords,
##' system = "WGS84", mini = 1, maxi = nrow(coords)/2,
##' variable_names = c("NO2", "O3", "PM10", "PM2.5"))$MNP
##'
##' plotSummary(x = res_mnp, type = "mean")}
##' \dontshow{
##' library(sp)
##' data("map_sites")
##' data("multi_data")
##' indices <- c(51:75)
##' coords <- coordinates(map_sites[indices,])
##' res_mnp <- SpatialScan(method = "MNP", data = multi_data[indices,],
##' sites_coord = coords, system = "WGS84", mini = 1, maxi = nrow(coords)/2,
##' MC = 99)$MNP
##' if(length(res_mnp$sites_clusters)>0){
##' plotSummary(x = res_mnp, type = "mean")
##' }
##' }
##'
##' @seealso \code{\link{plotSummary.ResScanOutputMulti}}, \code{\link{plotSummary.ResScanOutputUniFunct}} and \code{\link{plotSummary.ResScanOutputMultiFunct}}
##'
##' @return No value returned, plots the summary.
##'
##' @export
##'
plotSummary <- function(x, ...){
  UseMethod("plotSummary",x)
}

########################################################################################################################################################
##' @title Plots the mean or median spider chart of the clusters detected by a multivariate scan function (MG or MNP)
##'
##' @description This function plots the mean or median spider chart of the clusters detected by a multivariate scan function (MG or MNP).
##'
##' @param x ResScanOutputMulti. Output of a multivariate scan function (MG or MNP).
##' @param type character. "mean" or "median". If "mean": the means in the clusters are plotted in solid lines, outside the cluster in dots, the global mean is in black. If "median": the medians in the clusters are plotted in solid lines, outside the cluster in dots, the global median is in black.
##' @param colors character. The colors to plot the clusters' summaries. If length(colors)==1 then all the clusters will be plotted in this color. Else there must be the same number of elements in colors than the number of clusters
##' @param only.MLC logical. Should we plot only the MLC or all the significant clusters?
##' @param ... Further arguments to be passed to or from methods.
##'
##' @examples
##' \donttest{
##' library(sp)
##' data("map_sites")
##' data("multi_data")
##' coords <- coordinates(map_sites)
##'
##' res_mnp <- SpatialScan(method = "MNP", data=multi_data, sites_coord = coords,
##' system = "WGS84", mini = 1, maxi = nrow(coords)/2,
##' variable_names = c("NO2", "O3", "PM10", "PM2.5"))$MNP
##'
##' plotSummary(x = res_mnp, type = "mean")}
##' \dontshow{
##' library(sp)
##' data("map_sites")
##' data("multi_data")
##' indices <- c(51:75)
##' coords <- coordinates(map_sites[indices,])
##' res_mnp <- SpatialScan(method = "MNP", data=multi_data[indices,],
##' sites_coord = coords,
##' system = "WGS84", mini = 1, maxi = nrow(coords)/2, MC = 99)$MNP
##' if(length(res_mnp$sites_clusters)>0){
##' plotSummary(x = res_mnp, type = "mean")
##' }
##' }
##'
##' @return No value returned, plots the spider chart.
##'
##' @export
##'
plotSummary.ResScanOutputMulti <- function(x, type = "mean", colors = "red", only.MLC = FALSE, ...){

  if(is.logical(only.MLC) == FALSE){
    stop("only.MLC must be logical")
  }

  if(length(x$sites_clusters) == 0){
    stop("No significant cluster has been detected by the scan procedure")
  }

  if(length(colors) == 1){
    if(colors == "black"){
      stop("The color provided cannot be black")
    }
  }else{
    if(only.MLC == TRUE){
      stop("if only.MLC is TRUE only one color must be provided")
    }

    if(length(colors) != length(x$sites_clusters)){
      stop("colors must be of length 1 or with as many colors as the number of detected clusters")
    }else{
      if(sum(colors %in% c("black")) > 0){
        stop("The colors provided cannot be black")
      }
    }
  }

  if(length(type)!=1){
    stop("type must be of length 1")
  }else{
    if(!(type %in% c("mean", "median"))){
      stop("type must be mean or median")
    }
  }

  if(is.null(x$variable_names)){
    variable_names <- paste("var", c(1:ncol(x$data)), sep = "")
  }else{
    if(length(x$variable_names) != ncol(x$data)){
      stop("There must be the same number of elements in variable_names than the number of columns in data")
    }else{
      variable_names <- x$variable_names
    }
  }


  if(type == "mean"){
    if(only.MLC == FALSE){
      for(i in 1:length(x$sites_clusters)){
        mean_in <- colMeans(x$data[x$sites_clusters[[i]],, drop = FALSE])
        mean_out <- colMeans(x$data[-x$sites_clusters[[i]],, drop = FALSE])
        mean_overall <- colMeans(x$data)

        mini <- colMins(x$data)
        maxi <- colMaxs(x$data)

        data_frame <- data.frame(matrix(ncol=ncol(x$data), nrow=3))

        data_frame[1,] <- mean_in
        data_frame[2,] <- mean_out
        data_frame[3,] <- mean_overall

        colnames(data_frame) <- variable_names
        rownames(data_frame) <- c("mean_in", "mean_out", "mean_overall")

        data_frame <- rbind(maxi, mini, data_frame)

        if(length(colors)==1){
          radarchart(data_frame, pcol = c(colors, colors, "black"), plty = c(1,2,1), title = paste("Spider chart of the means for the cluster", i, sep = " "), vlcex = 0.8, cex.main = 0.8)
          legend("topleft", legend = c("Mean inside the cluster", "Mean outside the cluster", "Global mean"), col = c(colors, colors, "black"), lty = c(1,2,1), bty = "n", cex = 0.8)
        }else{
          radarchart(data_frame, pcol = c(colors[i], colors[i], "black"), plty = c(1,2,1), title = paste("Spider chart of the means for the cluster", i, sep = " "), vlcex = 0.8, cex.main = 0.8)
          legend("topleft", legend = c("Mean inside the cluster", "Mean outside the cluster", "Global mean"), col = c(colors[i], colors[i], "black"), lty = c(1,2,1), bty = "n", cex = 0.8)
        }
      }
    }else{
      i <- 1
      mean_in <- colMeans(x$data[x$sites_clusters[[i]],, drop = FALSE])
      mean_out <- colMeans(x$data[-x$sites_clusters[[i]],, drop = FALSE])
      mean_overall <- colMeans(x$data)

      mini <- colMins(x$data)
      maxi <- colMaxs(x$data)

      data_frame <- data.frame(matrix(ncol=ncol(x$data), nrow=3))

      data_frame[1,] <- mean_in
      data_frame[2,] <- mean_out
      data_frame[3,] <- mean_overall

      colnames(data_frame) <- variable_names
      rownames(data_frame) <- c("mean_in", "mean_out", "mean_overall")

      data_frame <- rbind(maxi, mini, data_frame)

      if(length(colors)==1){
        radarchart(data_frame, pcol = c(colors, colors, "black"), plty = c(1,2,1), title = paste("Spider chart of the means for the cluster", i, sep = " "), vlcex = 0.8, cex.main = 0.8)
        legend("topleft", legend = c("Mean inside the cluster", "Mean outside the cluster", "Global mean"), col = c(colors, colors, "black"), lty = c(1,2,1), bty = "n", cex = 0.8)
      }else{
        radarchart(data_frame, pcol = c(colors[i], colors[i], "black"), plty = c(1,2,1), title = paste("Spider chart of the means for the cluster", i, sep = " "), vlcex = 0.8, cex.main = 0.8)
        legend("topleft", legend = c("Mean inside the cluster", "Mean outside the cluster", "Global mean"), col = c(colors[i], colors[i], "black"), lty = c(1,2,1), bty = "n", cex = 0.8)
      }

    }

  }else{
    if(only.MLC == FALSE){
      for(i in 1:length(x$sites_clusters)){
        med_in <- colMedians(x$data[x$sites_clusters[[i]],, drop = FALSE])
        med_out <- colMedians(x$data[-x$sites_clusters[[i]],, drop = FALSE])
        med_overall <- colMedians(x$data)

        mini <- colMins(x$data)
        maxi <- colMaxs(x$data)

        data_frame <- data.frame(matrix(ncol=ncol(x$data), nrow=3))

        data_frame[1,] <- med_in
        data_frame[2,] <- med_out
        data_frame[3,] <- med_overall

        colnames(data_frame) <- variable_names
        rownames(data_frame) <- c("median_in", "median_out", "median_overall")

        data_frame <- rbind(maxi, mini, data_frame)

        if(length(colors)==1){
          radarchart(data_frame, pcol = c(colors, colors, "black"), plty = c(1,2,1), title = paste("Spider chart of the medians for the cluster", i, sep = " "), vlcex = 0.8, cex.main = 0.8)
          legend("topleft", legend = c("Median inside the cluster", "Median outside the cluster", "Global median"), col = c(colors, colors, "black"), lty = c(1,2,1), bty = "n", cex = 0.8)
        }else{
          radarchart(data_frame, pcol = c(colors[i], colors[i], "black"), plty = c(1,2,1), title = paste("Spider chart of the medians for the cluster", i, sep = " "), vlcex = 0.8, cex.main = 0.8)
          legend("topleft", legend = c("Median inside the cluster", "Median outside the cluster", "Global median"), col = c(colors[i], colors[i], "black"), lty = c(1,2,1), bty = "n", cex = 0.8)
        }
      }
    }else{
      i <- 1
      med_in <- colMedians(x$data[x$sites_clusters[[i]],, drop = FALSE])
      med_out <- colMedians(x$data[-x$sites_clusters[[i]],, drop = FALSE])
      med_overall <- colMedians(x$data)

      mini <- colMins(x$data)
      maxi <- colMaxs(x$data)

      data_frame <- data.frame(matrix(ncol=ncol(x$data), nrow=3))

      data_frame[1,] <- med_in
      data_frame[2,] <- med_out
      data_frame[3,] <- med_overall

      colnames(data_frame) <- variable_names
      rownames(data_frame) <- c("median_in", "median_out", "median_overall")

      data_frame <- rbind(maxi, mini, data_frame)

      if(length(colors)==1){
        radarchart(data_frame, pcol = c(colors, colors, "black"), plty = c(1,2,1), title = paste("Spider chart of the medians for the cluster", i, sep = " "), vlcex = 0.8, cex.main = 0.8)
        legend("topleft", legend = c("Median inside the cluster", "Median outside the cluster", "Global median"), col = c(colors, colors, "black"), lty = c(1,2,1), bty = "n", cex = 0.8)
      }else{
        radarchart(data_frame, pcol = c(colors[i], colors[i], "black"), plty = c(1,2,1), title = paste("Spider chart of the medians for the cluster", i, sep = " "), vlcex = 0.8, cex.main = 0.8)
        legend("topleft", legend = c("Median inside the cluster", "Median outside the cluster", "Global median"), col = c(colors[i], colors[i], "black"), lty = c(1,2,1), bty = "n", cex = 0.8)
      }

    }

  }

}

########################################################################################################################################################
##' @title Plots the mean or median curves in the clusters detected by a univariate functional scan procedure (PFSS, NPFSS, DFFSS or URBFSS)
##'
##' @description This function plots the mean or median curves in the clusters detected by a univariate functional scan procedure (PFSS, NPFSS, DFFSS or URBFSS).
##'
##' @param x ResScanOutputUniFunct. Output of a univariate functional scan function (PFSS, NPFSS, DFFSS or URBFSS).
##' @param type character. "mean" or "median". If "mean": the mean curves in the clusters are plotted in solid lines, outside the cluster in dots, the global mean curve is in black. If "median": the median curves in the clusters are plotted in solid lines, outside the cluster in dots, the global median curve is in black.
##' @param colors character. The colors to plot the clusters' summary curves. If length(colors)==1 then all the clusters will be plotted in this color. Else there must be the same number of elements in colors than the number of clusters
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
##' plotSummary(x = res_npfss, type = "median")}
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
##' plotSummary(x = res_npfss, type = "mean")
##' }
##'
##' }
##'
##' @return No value returned, plots the curves.
##'
##' @export
##'
plotSummary.ResScanOutputUniFunct <- function(x, type = "mean", colors = "red", only.MLC = FALSE, ...){

  if(is.logical(only.MLC) == FALSE){
    stop("only.MLC must be logical")
  }

  if(length(x$sites_clusters) == 0){
    stop("No significant cluster has been detected by the scan procedure")
  }

  if(length(colors) == 1){
    if(colors == "black" | colors == "grey"){
      stop("The color provided cannot be black or grey")
    }
  }else{

    if(only.MLC == TRUE){
      stop("if only.MLC is TRUE only one color must be provided")
    }

    if(length(colors) != length(x$sites_clusters)){
      stop("colors must be of length 1 or with as many colors as the number of clusters detected")
    }else{
      if(sum(colors %in% c("black", "grey")) > 0){
        stop("The colors provided cannot be black or grey")
      }
    }
  }

  if(length(type)!=1){
    stop("type must be of length 1")
  }else{
    if(!(type %in% c("mean", "median"))){
      stop("type must be mean or median")
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

      col_legend <- c("grey")
      text_legend <- c("Sites")
      lty_legend <- c(1)
      lwd_legend <- c(1.2)

      if(type == "mean"){
        mean_curve <- colMeans(x$data)
        lines(x = times, y = mean_curve, col = "black", lwd = 1.5)
        if(length(colors)==1){
          lines(x = times, y = colMeans(x$data[x$sites_clusters[[i]],,drop = FALSE]), col = colors, lwd = 1.2)
          lines(x = times, y = colMeans(x$data[-x$sites_clusters[[i]],, drop = FALSE]), col = colors, lwd = 1.2, lty = 2)
          col_legend <- c(col_legend, "black", colors, colors)
          text_legend <- c(text_legend, "Global mean", "Mean inside the cluster", "Mean outside the cluster")
          lty_legend <- c(lty_legend, 1, 1, 2)
          lwd_legend <- c(lwd_legend, 1.5, 1.2, 1.2)
        }else{
          lines(x = times, y = colMeans(x$data[x$sites_clusters[[i]],, drop = FALSE]), col = colors[i], lwd = 1.2)
          lines(x = times, y = colMeans(x$data[-x$sites_clusters[[i]],, drop = FALSE]), col = colors[i], lwd = 1.2, lty = 2)
          col_legend <- c(col_legend, "black", colors[i], colors[i])
          text_legend <- c(text_legend, "Global mean", "Mean inside the cluster", "Mean outside the cluster")
          lty_legend <- c(lty_legend, 1, 1, 2)
          lwd_legend <- c(lwd_legend, 1.5, 1.2, 1.2)
        }
      }else{
        median_curve <- colMedians(x$data)
        lines(x = times, y = median_curve, col = "black", lwd = 1.5)
        if(length(colors)==1){
          lines(x = times, y = colMedians(x$data[x$sites_clusters[[i]],, drop = FALSE]), col = colors, lwd = 1.2)
          lines(x = times, y = colMedians(x$data[-x$sites_clusters[[i]],, drop = FALSE]), col = colors, lwd = 1.2, lty = 2)
          col_legend <- c(col_legend, "black", colors, colors)
          text_legend <- c(text_legend, "Global median", "Median inside the cluster", "Median outside the cluster")
          lty_legend <- c(lty_legend, 1, 1, 2)
          lwd_legend <- c(lwd_legend, 1.5, 1.2, 1.2)
        }else{
          lines(x = times, y = colMedians(x$data[x$sites_clusters[[i]],, drop = FALSE]), col = colors[i], lwd = 1.2)
          lines(x = times, y = colMedians(x$data[-x$sites_clusters[[i]],, drop = FALSE]), col = colors[i], lwd = 1.2, lty = 2)
          col_legend <- c(col_legend, "black", colors[i], colors[i])
          text_legend <- c(text_legend, "Global median", "Median inside the cluster", "Median outside the cluster")
          lty_legend <- c(lty_legend, 1, 1, 2)
          lwd_legend <- c(lwd_legend, 1.5, 1.2, 1.2)
        }
      }
      legend("topleft", legend = text_legend, col = col_legend, lty = lty_legend, lwd = lwd_legend, bty = "n", cex = 0.8)
    }
  }else{
    i <- 1
    plot(NULL, ylim = range(x$data), xlim = range(times), xlab = "Time", ylab = "", main = paste("Cluster ", i, sep = ""), cex.lab = 0.8, cex.main = 0.8, cex.axis = 0.8)
    for(elem in 1:nrow(x$data)){
      lines(x = times, y = x$data[elem,], col = "grey", lwd = 1.2)
    }

    col_legend <- c("grey")
    text_legend <- c("Sites")
    lty_legend <- c(1)
    lwd_legend <- c(1.2)

    if(type == "mean"){
      mean_curve <- colMeans(x$data)
      lines(x = times, y = mean_curve, col = "black", lwd = 1.5)
      if(length(colors)==1){
        lines(x = times, y = colMeans(x$data[x$sites_clusters[[i]],,drop = FALSE]), col = colors, lwd = 1.2)
        lines(x = times, y = colMeans(x$data[-x$sites_clusters[[i]],, drop = FALSE]), col = colors, lwd = 1.2, lty = 2)
        col_legend <- c(col_legend, "black", colors, colors)
        text_legend <- c(text_legend, "Global mean", "Mean inside the cluster", "Mean outside the cluster")
        lty_legend <- c(lty_legend, 1, 1, 2)
        lwd_legend <- c(lwd_legend, 1.5, 1.2, 1.2)
      }else{
        lines(x = times, y = colMeans(x$data[x$sites_clusters[[i]],, drop = FALSE]), col = colors[i], lwd = 1.2)
        lines(x = times, y = colMeans(x$data[-x$sites_clusters[[i]],, drop = FALSE]), col = colors[i], lwd = 1.2, lty = 2)
        col_legend <- c(col_legend, "black", colors[i], colors[i])
        text_legend <- c(text_legend, "Global mean", "Mean inside the cluster", "Mean outside the cluster")
        lty_legend <- c(lty_legend, 1, 1, 2)
        lwd_legend <- c(lwd_legend, 1.5, 1.2, 1.2)
      }
    }else{
      median_curve <- colMedians(x$data)
      lines(x = times, y = median_curve, col = "black", lwd = 1.5)
      if(length(colors)==1){
        lines(x = times, y = colMedians(x$data[x$sites_clusters[[i]],, drop = FALSE]), col = colors, lwd = 1.2)
        lines(x = times, y = colMedians(x$data[-x$sites_clusters[[i]],, drop = FALSE]), col = colors, lwd = 1.2, lty = 2)
        col_legend <- c(col_legend, "black", colors, colors)
        text_legend <- c(text_legend, "Global median", "Median inside the cluster", "Median outside the cluster")
        lty_legend <- c(lty_legend, 1, 1, 2)
        lwd_legend <- c(lwd_legend, 1.5, 1.2, 1.2)
      }else{
        lines(x = times, y = colMedians(x$data[x$sites_clusters[[i]],, drop = FALSE]), col = colors[i], lwd = 1.2)
        lines(x = times, y = colMedians(x$data[-x$sites_clusters[[i]],, drop = FALSE]), col = colors[i], lwd = 1.2, lty = 2)
        col_legend <- c(col_legend, "black", colors[i], colors[i])
        text_legend <- c(text_legend, "Global median", "Median inside the cluster", "Median outside the cluster")
        lty_legend <- c(lty_legend, 1, 1, 2)
        lwd_legend <- c(lwd_legend, 1.5, 1.2, 1.2)
      }
    }
    legend("topleft", legend = text_legend, col = col_legend, lty = lty_legend, lwd = lwd_legend, bty = "n", cex = 0.8)

  }



}

########################################################################################################################################################
##' @title Plots the mean or median curves in the clusters detected by a multivariate functional scan procedure (MPFSS, NPFSS, MDFFSS or MRBFSS)
##'
##' @description This function plots the mean or median curves in the clusters detected by a multivariate functional scan procedure (MPFSS, NPFSS, MDFFSS or MRBFSS).
##'
##' @param x ResScanOutputMultiFunct. Output of a multivariate functional scan function (MPFSS, NPFSS, MDFFSS or MRBFSS).
##' @param type character. "mean" or "median". If "mean": the mean curves in the clusters are plotted in solid lines, outside the cluster in dots, the global mean curve is in black. If "median": the median curves in the clusters are plotted in solid lines, outside the cluster in dots, the global median curve is in black.
##' @param colors character. The colors to plot the clusters' summary curves. If length(colors)==1 then all the clusters will be plotted in this color. Else there must be the same number of elements in colors than the number of clusters
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
##' plotSummary(x = res_npfss, type = "median")}
##' \dontshow{
##' library(sp)
##' data("map_sites")
##' data("fmulti_data")
##' indices <- c(51:75)
##' coords <- coordinates(map_sites[indices,])
##' res_npfss <- SpatialScan(method = "NPFSS", data = fmulti_data[indices],
##' sites_coord = coords, system = "WGS84", mini = 1, maxi = nrow(coords)/2, MC = 99)$NPFSS
##' if(length(res_npfss$sites_clusters)>0){
##' plotSummary(x = res_npfss, type = "mean")
##' }
##'
##' }
##'
##' @return No value returned, plots the curves.
##'
##' @export
##'
plotSummary.ResScanOutputMultiFunct <- function(x, type = "mean", colors = "red", only.MLC = FALSE, ...){

  if(is.logical(only.MLC) == FALSE){
    stop("only.MLC must be logical")
  }

  if(length(x$sites_clusters) == 0){
    stop("No significant cluster has been detected by the scan procedure")
  }

  if(length(colors) == 1){
    if(colors == "black" | colors == "grey"){
      stop("The color provided cannot be black or grey")
    }
  }else{

    if(only.MLC == TRUE){
      stop("if only.MLC is TRUE only one color must be provided")
    }

    if(length(colors) != length(x$sites_clusters)){
      stop("colors must be of length 1 or with as many colors as the number of clusters detected")
    }else{
      if(sum(colors %in% c("black", "grey")) > 0){
        stop("The colors provided cannot be black or grey")
      }
    }
  }

  if(length(type)!=1){
    stop("type must be of length 1")
  }else{
    if(!(type %in% c("mean", "median"))){
      stop("type must be mean or median")
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

        col_legend <- c("grey")
        text_legend <- c("Sites")
        lty_legend <- c(1)
        lwd_legend <- c(1.2)


        if(type == "mean"){
          mean_curve <- colMeans(temp)
          lines(x = times, y = mean_curve, col = "black", lwd = 1.5)
          if(length(colors)==1){
            lines(x = times, y = colMeans(temp[x$sites_clusters[[i]],, drop = FALSE]), col = colors, lwd = 1.2)
            lines(x = times, y = colMeans(temp[-x$sites_clusters[[i]],,drop = FALSE]), col = colors, lwd = 1.2, lty = 2)

            col_legend <- c(col_legend, "black", colors, colors)
            text_legend <- c(text_legend, "Global mean", "Mean inside the cluster", "Mean outside the cluster")
            lty_legend <- c(lty_legend, 1, 1, 2)
            lwd_legend <- c(lwd_legend, 1.5, 1.2, 1.2)

          }else{
            lines(x = times, y = colMeans(temp[x$sites_clusters[[i]],, drop = FALSE]), col = colors[i], lwd = 1.2)
            lines(x = times, y = colMeans(temp[-x$sites_clusters[[i]],, drop = FALSE]), col = colors[i], lwd = 1.2, lty = 2)
            col_legend <- c(col_legend, "black", colors[i], colors[i])
            text_legend <- c(text_legend, "Global mean", "Mean inside the cluster", "Mean outside the cluster")
            lty_legend <- c(lty_legend, 1, 1, 2)
            lwd_legend <- c(lwd_legend, 1.5, 1.2, 1.2)
          }
        }else{
          median_curve <- colMedians(temp)
          lines(x = times, y = median_curve, col = "black", lwd = 1.5)
          if(length(colors)==1){
            lines(x = times, y = colMedians(temp[x$sites_clusters[[i]],, drop = FALSE]), col = colors, lwd = 1.2)
            lines(x = times, y = colMedians(temp[-x$sites_clusters[[i]],, drop = FALSE]), col = colors, lwd = 1.2, lty = 2)
            col_legend <- c(col_legend, "black", colors, colors)
            text_legend <- c(text_legend, "Global median", "Median inside the cluster", "Median outside the cluster")
            lty_legend <- c(lty_legend, 1, 1, 2)
            lwd_legend <- c(lwd_legend, 1.5, 1.2, 1.2)
          }else{
            lines(x = times, y = colMedians(temp[x$sites_clusters[[i]],, drop = FALSE]), col = colors[i], lwd = 1.2)
            lines(x = times, y = colMedians(temp[-x$sites_clusters[[i]],, drop = FALSE]), col = colors[i], lwd = 1.2, lty = 2)
            col_legend <- c(col_legend, "black", colors[i], colors[i])
            text_legend <- c(text_legend, "Global median", "Median inside the cluster", "Median outside the cluster")
            lty_legend <- c(lty_legend, 1, 1, 2)
            lwd_legend <- c(lwd_legend, 1.5, 1.2, 1.2)
          }
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

      col_legend <- c("grey")
      text_legend <- c("Sites")
      lty_legend <- c(1)
      lwd_legend <- c(1.2)


      if(type == "mean"){
        mean_curve <- colMeans(temp)
        lines(x = times, y = mean_curve, col = "black", lwd = 1.5)
        if(length(colors)==1){
          lines(x = times, y = colMeans(temp[x$sites_clusters[[i]],, drop = FALSE]), col = colors, lwd = 1.2)
          lines(x = times, y = colMeans(temp[-x$sites_clusters[[i]],,drop = FALSE]), col = colors, lwd = 1.2, lty = 2)

          col_legend <- c(col_legend, "black", colors, colors)
          text_legend <- c(text_legend, "Global mean", "Mean inside the cluster", "Mean outside the cluster")
          lty_legend <- c(lty_legend, 1, 1, 2)
          lwd_legend <- c(lwd_legend, 1.5, 1.2, 1.2)

        }else{
          lines(x = times, y = colMeans(temp[x$sites_clusters[[i]],, drop = FALSE]), col = colors[i], lwd = 1.2)
          lines(x = times, y = colMeans(temp[-x$sites_clusters[[i]],, drop = FALSE]), col = colors[i], lwd = 1.2, lty = 2)
          col_legend <- c(col_legend, "black", colors[i], colors[i])
          text_legend <- c(text_legend, "Global mean", "Mean inside the cluster", "Mean outside the cluster")
          lty_legend <- c(lty_legend, 1, 1, 2)
          lwd_legend <- c(lwd_legend, 1.5, 1.2, 1.2)
        }
      }else{
        median_curve <- colMedians(temp)
        lines(x = times, y = median_curve, col = "black", lwd = 1.5)
        if(length(colors)==1){
          lines(x = times, y = colMedians(temp[x$sites_clusters[[i]],, drop = FALSE]), col = colors, lwd = 1.2)
          lines(x = times, y = colMedians(temp[-x$sites_clusters[[i]],, drop = FALSE]), col = colors, lwd = 1.2, lty = 2)
          col_legend <- c(col_legend, "black", colors, colors)
          text_legend <- c(text_legend, "Global median", "Median inside the cluster", "Median outside the cluster")
          lty_legend <- c(lty_legend, 1, 1, 2)
          lwd_legend <- c(lwd_legend, 1.5, 1.2, 1.2)
        }else{
          lines(x = times, y = colMedians(temp[x$sites_clusters[[i]],, drop = FALSE]), col = colors[i], lwd = 1.2)
          lines(x = times, y = colMedians(temp[-x$sites_clusters[[i]],, drop = FALSE]), col = colors[i], lwd = 1.2, lty = 2)
          col_legend <- c(col_legend, "black", colors[i], colors[i])
          text_legend <- c(text_legend, "Global median", "Median inside the cluster", "Median outside the cluster")
          lty_legend <- c(lty_legend, 1, 1, 2)
          lwd_legend <- c(lwd_legend, 1.5, 1.2, 1.2)
        }
      }
      legend("topleft", legend = text_legend, col = col_legend, lty = lty_legend, lwd = lwd_legend, bty = "n", cex = 0.8)
    }

  }



}
