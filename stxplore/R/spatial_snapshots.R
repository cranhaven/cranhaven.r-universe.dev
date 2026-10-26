#' Plots spatial snapshots of data through time using a dataframe or a stars object.
#'
#' @description
#' This function can take either a stars object or a dataframe. Input arguments differ
#' for each case.
#'
#' For dataframes, usage involves latitude and longitude. However, x and y coordinates
#' can be given instead of longitude and latitude. If x and y are given instead of longitude
#' and latitude, the country borders will not be shown.
#'
#'
#' @param x A stars object or a dataframe. Arguments differ according to the input type.
#' @param xlab The x label.
#' @param ylab The y label.
#' @param title The graph title.
#' @param palette The color palette. Default is \code{Spectral}.
#' @param legend_title The title for the legend.
#' @param lat_col For dataframes: the column or the column name giving the latitude. The y coordinate can be used instead of latitude.
#' @param lon_col For dataframes: the column or the column name giving the longitude. The x coordinate can be used instead of longitude.
#' @param t_col For dataframes: the time column. Time must be a set of discrete integer values.
#' @param z_col For dataframes: the The quantity of interest that will be plotted. Eg. temperature.
#' @param ifxy For dataframes: if \code{TRUE} then the country borders are not drawn as longitude and latitude are unknown.
#' @param ... Other arguments currently ignored.
#'
#' @return A ggplot object.
#'
#' @examples
#' library(dplyr)
#' # Dataframe example
#' data(NOAA_df_1990)
#' Tmax <- filter(NOAA_df_1990,
#'   proc == "Tmax" &
#'   month == 5 &
#'   year == 1993 &
#'   id < 4000)
#' Tmax$t <- Tmax$julian - min(Tmax$julian) + 1
#' Tmax_days <- subset(Tmax, t %in% c(1, 15))
#' spatial_snapshots(Tmax_days,
#'   lat_col = 'lat',
#'   lon_col = 'lon',
#'   t_col = 't',
#'   z_col = 'z',
#'   title = "Maximum Temperature for 2 days ")
#'
#' # stars example
#' library(stars)
#' tif = system.file("tif/L7_ETMs.tif", package = "stars")
#' x <- read_stars(tif)
#' x2 <- x %>% slice(band, 1:2)
#' spatial_snapshots(x2)
#'
#' @importFrom rlang .data
#' @importFrom stars geom_stars
#' @importFrom rlang sym
#'
#' @export
spatial_snapshots <- function(x,
                              xlab = "x",
                              ylab = "y",
                              title = "",
                              palette = "Spectral",
                              legend_title = "z",
                              ...){
  UseMethod("spatial_snapshots")
}

#' @rdname spatial_snapshots
#' @export
spatial_snapshots.data.frame <- function(x,
                     xlab = "Longitude",
                     ylab = "Latitude",
                     title = "",
                     palette = "Spectral",
                     legend_title = "z",
                     lat_col,
                     lon_col,
                     t_col,
                     z_col,
                     ifxy = FALSE,
                     ...){
  if(missing(x)){
    stop("Empty dataframe x. Please give a proper input.")
  }


  if(missing(lat_col)){
    stop("Latitude column not specified. Use lat_col to specify latitude.")
  }

  if(missing(lon_col)){
    stop("Longitude column not specified. Use lon_col to specify longitude.")
  }

  if(missing(t_col)){
    stop("Time column not specified. Use t_col to specify time.")
  }

  if(missing(z_col)){
    stop("Variable to plot is not specified. Use z_col to specify variable.")
  }

  df <- x
  lat <- df[ ,lat_col]
  lon <- df[ ,lon_col]
  z <- df[ ,z_col]
  t <- df[ ,t_col]

  df2 <- data.frame(lat = lat, lon = lon, z = z, t = t)

  bbox <- ggmap::make_bbox(lat = lat, lon = lon, data = df2)
  if(!ifxy){
    ggplot(data = df2) +                                      # plot points
      geom_point(aes(x = lon, y = lat,                       # lon and lat with colour defined by z
                     colour = z),size = 2) +                 # and size of points larger
      scale_colour_distiller(palette = palette,
                             guide = "colourbar",
                             legend_title) +
      xlab(xlab) +  ylab(ylab) +                             # label x and y axes
      geom_path(aes(x = .data$long, y = .data$lat, group = .data$group),       # add in a map of the world
                data = map_data("world")) +
      facet_wrap(~t) +                                       # separate plots based on time (facets)
      coord_fixed(xlim = c(bbox[1], bbox[3]),                # zoom in to the bounding box
                  ylim = c(bbox[2], bbox[4])) +
      theme_bw() +                                           # black and white theme
      labs(color=z_col) +
      ggtitle(title)
  }else{
    ggplot(data = df2) +                                      # plot points
      geom_point(aes(x = lon, y = lat,                       # lon and lat with colour defined by z
                     colour = z),size = 2) +                 # and size of points larger
      scale_colour_distiller(palette = palette,
                             guide = "colourbar",
                             legend_title) +
      xlab(xlab) +  ylab(ylab) +                             # label x and y axes
      facet_wrap(~t) +                                       # separate plots based on time (facets)
      coord_fixed(xlim = c(bbox[1], bbox[3]),                # zoom in to the bounding box
                  ylim = c(bbox[2], bbox[4])) +
      theme_bw() +                                           # black and white theme
      labs(color=z_col) +
      ggtitle(title)+
      guides(fill = guide_legend(title = legend_title))
  }

}

#' @rdname spatial_snapshots
#' @export
spatial_snapshots.stars <- function(x,
                                    xlab = "x",
                                    ylab = "y",
                                    title = "",
                                    palette = "Spectral",
                                    legend_title = "z",
                                    ...){

  dim_names <- dimnames(x)
  variable <- dim_names[3]
  time_vals <- stars::st_get_dimension_values(x, 3)
  xx <- stars::st_set_dimensions(x, 3, as.factor(time_vals))

  ggplot() +
    geom_stars(data = xx) +
    facet_wrap(sym(variable)) +
    xlab(xlab) +
    ylab(ylab) +
    scale_fill_distiller(palette = palette,
                         guide = "colourbar",
                         legend_title)


}
