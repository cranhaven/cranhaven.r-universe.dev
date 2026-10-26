#' Plots temporal snapshots of data for specific spatial locations using a dataframe or a stars object.
#'
#' This function plots temporal snapshos for specific spatial locations. The location id sample
#' need to be given as a function argument.
#'
#'@inheritParams spatial_snapshots
#'@inheritParams spatial_snapshots.data.frame
#'@param id_col The column of the location id.
#'@param id_sample The sample of location ids to be plotted
#'@param ... Other arguments currently ignored.
#'@param xvals For stars objects: the set of xvalues to plot.
#'@param yvals For stars objects: the set of yvalues to plot. These two lengths need to be the same.
#'@param precision For stars objects: set to 0, if the given values are compared with the integer values in the stars object.
#'
#'@return A ggplot.
#'
#'@examples
#'# Dataframe example
#'library(dplyr)
#'data(NOAA_df_1990)
#'Tmax <- filter(NOAA_df_1990,
#'              proc == "Tmax" &
#'              month %in% 5:9 &
#'              year == 1993)
#'Tmax_ID <- unique(Tmax$id)
#'Tmax$t <- Tmax$julian - min(Tmax$julian) + 1
#'ids <- sample(Tmax_ID, 10)
#'temporal_snapshots(Tmax,
#'                   t_col = 't',
#'                   z_col = 'z',
#'                   id_col = 'id',
#'                   id_sample = ids)
#'
#'
#'# stars example
#'library(stars)
#'tif = system.file("tif/L7_ETMs.tif", package = "stars")
#'x <- read_stars(tif)
#'xvals <- c(288876.0,289047.0)
#'yvals <- c(9120405, 9120006)
#'temporal_snapshots(x,
#'                   xvals = xvals,
#'                   yvals = yvals)
#'@export
temporal_snapshots <- function(x,
                               xlab = "x",
                               ylab = "y",
                               title = "",
                               ...){
  UseMethod("temporal_snapshots")
}

#' @rdname temporal_snapshots
#' @export
temporal_snapshots.data.frame <- function(x,
                                         xlab="Time",
                                         ylab ="Value",
                                         title = "",
                                         t_col,
                                         z_col,
                                         id_col,
                                         id_sample,
                                         ...){

  if(missing(x)){
    stop("Empty dataframe x. Please give a proper input.")
  }

  if(missing(t_col)){
    stop("Time column not specified. Use t_col to specify time.")
  }

  if(missing(z_col)){
    stop("Variable to plot is not specified. Use z_col to specify variable.")
  }

  if(missing(id_col)){
    stop("Location id column is not specified. Use id_col to specify location id.")
  }

  if(missing(id_sample)){
    stop("Location id sample is not specified. Use id_sample to specify a sample of location ids.")
  }

  df <- x
  id <- df[ ,id_col]
  rows <- which(id %in% id_sample)
  id <- df[rows, id_col]
  z <- df[rows ,z_col]
  t <- df[rows ,t_col]

  df2 <- data.frame(t = t, z = z, id = id)

  ggplot(df2) +
    geom_line(aes(x = t, y = z)) +                         # line plot of z against t
    facet_wrap(~id) +                                      # facet by station
    xlab(xlab) +                                           # x label
    ylab(ylab) +                                           # y label
    theme(panel.spacing = unit(1, "lines")) +              # facet spacing
    theme_bw() +                                           # black and white theme
    ggtitle(title)                                         # add title
}


#' @rdname temporal_snapshots
#' @export
temporal_snapshots.stars <- function(x,
                                     xlab="Time",
                                     ylab ="Value",
                                     title = "",
                                     xvals,
                                     yvals,
                                     precision = 0,
                                     ...){

  if(length(xvals)!=length(yvals)){
    stop("The number of x and y locations need to be the same.i.e, length(xvals) = length(yvals) ")
  }

  xt <- tidyr::as_tibble(x)
  xx <- yy<- x <- y <- t <- value <- NULL

  colnames(xt) <- c("x", "y", "t", "value")
  xt <- xt %>%
    dplyr::mutate(xx = round(x, precision), yy = round(y, precision))
  xt2 <- xt %>%
    dplyr::filter(xx %in% xvals & yy %in% yvals) %>%
    dplyr::group_by(xx, yy) %>%
    dplyr::mutate(id = paste(x,y,sep="-"))


  ggplot(xt2) +
    geom_line(aes(x = t, y = value)) +                         # line plot of z against t
    facet_wrap(~id) +                                      # facet by station
    xlab(xlab) +                                           # x label
    ylab(ylab) +                                           # y label
    theme(panel.spacing = unit(1, "lines")) +              # facet spacing
    theme_bw() +                                           # black and white theme
    ggtitle(title)
}
