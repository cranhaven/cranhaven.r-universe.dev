#' Computes spatial empirical means using a dataframe or a stars object
#'
#' @description
#' This function computes spatial empirical means by latitude and longitude averaged over time.
#' This function can take either a stars object or a dataframe. Input arguments differ for each case.
#' The autoplot function can plot this object.
#'
#' The variations are
#'  * 'spatial_means.data.frame()' if the input is a dataframe
#'  * 'spatial_means.stars()' if the input is a stars object
#'  * 'autoplot.spatialmeans()' to plot the outputs.
#'
#'
#' @inheritParams spatial_snapshots
#' @inheritParams spatial_snapshots.data.frame
#' @param object For autoplot: the output from the `spatial_means' function.
#' @param ylab For autoplot: the ylabel.
#' @param xlab1 For autoplot: The xlabel for the first plot.
#' @param xlab2 For autuoplot: The xlabel for the second plot.
#' @param ... Other arguments currently ignored.
#'
#' @return A spatialmeans object contaiing spatial averages and the original data.
#'
#' @examples
#' # dataframe example
#' data(NOAA_df_1990)
#' library(dplyr)
#' Tmax <- filter(NOAA_df_1990,                      # subset the data
#'               proc == "Tmax" &                   # extract max temperature
#'                 month %in% 5:9 &                 # May to July
#'                 year == 1993)                    # year 1993
#' Tmax$t <- Tmax$julian - min(Tmax$julian) + 1      # create a new time variable starting at 1
#' sp_df <- spatial_means(Tmax,
#'        lat_col = "lat",
#'        lon_col = "lon",
#'        t_col = "t",
#'        z_col = "z")
#' autoplot(sp_df)
#'
#' # stars examples
#' library(stars)
#' tif = system.file("tif/olinda_dem_utm25s.tif", package = "stars")
#' x <- read_stars(tif)
#' sp_means <- spatial_means(x)
#' autoplot(sp_means)
#'
#' @export
spatial_means <- function(x,
                          ...){
  UseMethod("spatial_means")
}

#' @rdname spatial_means
#' @export
spatial_means.data.frame <- function(x,
                   lat_col,
                   lon_col,
                   t_col,
                   z_col,
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
  y <- df[ ,lat_col]
  x <- df[ ,lon_col]
  z <- df[ ,z_col]
  t <- df[ ,t_col]
  value <- mu_emp <- NULL

  df2 <- data.frame(y = y, x = x, value = z, t = t)

  spat_av <- dplyr::group_by(df2, x, y) %>%           # group by lon-lat
    dplyr::summarise(mu_emp = mean(value, na.rm = TRUE))                     # mean in each lon-lat box

  if(dim(df2)[1] == dim(spat_av)[1]){
    message("Only one observation of each location is present. The mean is the original value itself!")
  }
  ## Spatial Empirical Means

  structure(list(
    spatial_avg = spat_av,
    data = df2,
    data_stars = NULL,
    call = match.call()
  ), class='spatialmeans')

}


#' @rdname spatial_means
#' @export
spatial_means.stars <- function(x,
                                ...){
  if(missing(x)){
    stop("Empty stars object x. Please give a proper input.")
  }

  if(length(dim(x)) > 3){
    stop("The stars object can have x, y and time values (3 dimensions) or x & y values (2 dimensions)
         It doesn't make sense to average over more dimensions.")
  }


  st_mean <- stars::st_apply(x, c("x", "y"), function(x) mean(x, na.rm = TRUE))

  df <- dplyr::as_tibble(x)

  x <- y <- value <- t <- NULL
  if(dim(df)[2] == 3){
    # X and Y only
    colnames(df) <- c("x", "y", "value")
  }else{
    # X, Y and Time y
    colnames(df) <- c("x", "y", "t", "value")
  }


  mu_emp <- NULL

 spat_av <- dplyr::group_by(df, x, y) %>%           # group by lon-lat
    dplyr::summarise(mu_emp = mean(value, na.rm = TRUE))                     # mean in each lon-lat box

 if(dim(df)[1] == dim(spat_av)[1]){
   message("Only one observation of each location is present. The mean is the original value itself!")
 }


  ## Spatial Empirical Means

  structure(list(
    spatial_avg = spat_av,
    data = df,
    data_stars = st_mean,
    call = match.call()
  ), class='spatialmeans')

}

#' @rdname spatial_means
#' @export
autoplot.spatialmeans <- function(object,
                                  ylab = "Mean Value",
                                  xlab1 = "Latitude",
                                  xlab2 = "Longitude",
                                  title = "Spatial Empirical Means",
                                  ...){

  x <- y <- mu_emp <- NULL

  spat_av <- object$spatial_avg
  lat_means <- ggplot(spat_av) +
    geom_point(aes(y, mu_emp)) +
    theme_bw() +
    xlab(xlab1) +
    ylab(ylab)
  lon_means <- ggplot(spat_av) +
    geom_point(aes(x, mu_emp)) +
    theme_bw() +
    xlab(xlab2) +
    ylab(ylab)


  if(!is.null(object$data_stars)){
    xx <- object$data_stars
    st_fig <- ggplot() +
      geom_stars(data = xx) +
      scale_fill_distiller(palette = "Spectral",
                           guide = "colourbar")

    ll_means <- gridExtra::grid.arrange(lat_means, lon_means, st_fig, layout_matrix = matrix(c(1, 3, 2, 3), nrow = 2), top = title)
  }else{
    ll_means <- gridExtra::grid.arrange(lat_means, lon_means, nrow = 1, ncol = 2, top = title)

  }



  ll_means
}
