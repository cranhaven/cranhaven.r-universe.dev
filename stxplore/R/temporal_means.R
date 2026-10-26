#' Computes temporal empirical means using a dataframe or a stars object.
#'
#' @description
#' This function computes temporal empirical means averaged per time unit.
#' This function can take either a stars object or a dataframe. Input arguments
#' differ for each case. The function autoplot plots the output.
#'
#' @inheritParams temporal_snapshots.data.frame
#' @inheritParams spatial_snapshots.data.frame
#' @inheritParams temporal_snapshots
#' @param object For autoplot: the output of the function `temporal_means'.
#' @param legend_title For autoplot: the title for the legend.
#'
#' @return An object of class temporalmeans containing the averages and the original data
#' in two dataframes.
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
#' tem <- temporal_means(Tmax,
#'        t_col = 'date',
#'        z_col = 'z',
#'        id_col = 'id')
#'autoplot(tem)
#'
#' # stars example
#' library(stars)
#' library(dplyr)
#' library(units)
#' # Example
#' prec_file = system.file("nc/test_stageiv_xyt.nc", package = "stars")
#' prec <- read_ncdf(prec_file)
#' temporal_means(prec)
#' @export
temporal_means <- function(x,
                           ...){
  UseMethod("temporal_means")
}

#' @rdname temporal_means
#' @export
temporal_means.data.frame <- function(x,
                                      t_col,
                                      z_col,
                                      id_col,
                                      ...){
  if(missing(x)){
    stop("Empty dataframe x. Please give a proper input.")
  }

  if(missing(t_col)){
    stop("Date/time column not specified. Use t_col to specify time.")
  }

  if(missing(z_col)){
    stop("Variable to plot is not specified. Use z_col to specify variable.")
  }

  if(missing(id_col)){
    stop("Location id column is not specified. Use id_col to specify location id.")
  }

  df <- x
  z <- df[ ,z_col]
  t <- df[ ,t_col]
  id <- df[ ,id_col]
  meanz <- NULL
  time <- NULL

  df2 <- data.frame(z = z, time = t, id = id)

  df_av <- dplyr::group_by(df2, time) %>%
    dplyr::summarise(meanz = mean(z))

  structure(list(
    data = df2,
    averages = df_av,
    call = match.call()
  ), class='temporalmeans')
}


#' @rdname temporal_means
#' @export
temporal_means.stars <- function(x,
                                 ...){

  if(missing(x)){
    stop("Empty stars object x. Please give a proper input.")
  }

  if(length(dim(x)) !=3){
    stop("To compute temporal means, you need a 3D raster! Not more or less dimensions. ")
  }

  df <- dplyr::as_tibble(x)

  id <- x <- y <- time <- value <- NULL

  descriptive_name <- colnames(df)[4]
  colnames(df) <- c("x", "y", "time", "value")
  df2 <- df %>%
    dplyr::mutate(id = paste(x, y, sep ="-")) %>%
    dplyr::select(value, time, id)

  df_av <- dplyr::group_by(df, time) %>%
    dplyr::summarise(meanz = mean(value, na.rm = TRUE))

  structure(list(
    data = df2,
    averages = df_av,
    call = match.call()
  ), class='temporalmeans')
}

#' @rdname temporal_means
#' @export
autoplot.temporalmeans <- function(object,
                                   ylab = "Value",
                                   xlab ="Time",
                                   legend_title ="",
                                   title = "Temporal Empirical Means",
                                   ...){

  df <- as.data.frame(object$data)
  df_av <- as.data.frame(object$averages)

  num_obs <- dim(df)[1]
  if(num_obs > 50000){  # More than 50000 data points
    ss <- sample(num_obs, 50000)
    df <- df[ss, ]
  }

  meanz <- time <- NULL

  p <- ggplot() +
    geom_line(data = df,
              aes(x = df[ ,2],
                  y = df[ ,1],
                  group = df[ ,3],
                  colour = "blue"),
              alpha = 0.04) +
    geom_line(data = df_av,
              aes(x = time,
                  y = meanz,
                  colour = "black")) +
    xlab(xlab) +
    ylab(ylab) +
    theme_bw() +
    scale_color_manual(name = legend_title,
                       values = c(blue = "blue", black = "black"),
                       labels = c("Observed", "Average"),
                       guide = "legend") +
    ggtitle(title)
  p

}

