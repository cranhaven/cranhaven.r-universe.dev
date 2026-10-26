#' Computes empirical orthogonal functions using a dataframe or a stars object.
#'
#' Computes empirical orthogonal functions of the data. Function autoplot can plot the output.
#'
#' @inheritParams semivariogram.data.frame
#' @inheritParams spatial_snapshots
#' @param object For autoplot: the output of the function `emp_orth_fun'.
#' @param EOF_num For autoplot: the number of Empirical Orthogonal Functions (EOFs) to plot.
#' @param only_EOF For autoplot: if \code{TRUE}, only the spatial EOF function would be plotted.
#' @param only_TS For autoplot: if \code{TRUE}, only the PC time series would be plotted. If both are set to \code{FALSE},
#'                 both plots would be displayed. Both cannot be set to \code{TRUE}.
#' @param ... Other arguments currently ignored.
#'
#' @returns An emporthfun object with temporal PCs and spatial EOFs.
#'
#' @examples
#' # dataframe example
#' data(SSTlonlatshort)
#' data(SSTdatashort)
#' data(SSTlandmaskshort)
# Take first 396 months (33 years) and delete land
#' delete_rows <- which(SSTlandmaskshort  ==  1)
#' SSTdatashort   <- SSTdatashort[-delete_rows, 1:396]
#' emp1 <- emp_orth_fun(SSTlonlatshort[-delete_rows,  ],
#'                      SSTdatashort)
#' autoplot(emp1,
#'          EOF_num = 1)
#'
#'
#' # stars example
#' library(dplyr)
#' library(stars)
#' # Create a stars object from a data frame
#' precip_df <- NOAA_df_1990[NOAA_df_1990$proc == 'Precip', ] %>%
#'   filter(date >= "1992-02-01" & date <= "1992-02-05")
#' precip <- precip_df[ ,c('lat', 'lon', 'date', 'z')]
#' st_precip <- st_as_stars(precip, dims = c("lon", "lat", "date"))
#' emp <- emp_orth_fun(st_precip)
#' autoplot(emp, only_TS = TRUE)
#'
#' @export
emp_orth_fun <- function(x,
                         ...){
  UseMethod("emp_orth_fun")
}


#' @rdname emp_orth_fun
#' @export
emp_orth_fun.data.frame <- function(x,
                                    values_df,
                                    ...){

  if(missing(x)){
    stop("Empty dataframe x. Please give a dataframe with latitude and longitude.")
  }

  locations_df <- x

  if(missing(values_df)){
    stop("Empty dataframe values_df. Please give an N x T dataframe with values of the quantity you are interested in.  ")
  }

  EOF <- nPC <- NULL

  # Put data into space-time format
  Z  <- t(values_df)

  spat_mean <-  apply(values_df, 1, function(x) mean(x, na.rm = TRUE))
  nT  <- ncol(values_df)

  # Subtract and standardise and compute the SVD
  Zt <- 1/sqrt(nT - 1)*(Z  - outer(rep(1,nT), spat_mean))
  E <- svd(Zt)

  # Extract the matrix V which contains the EOFs in space-time format and append
  # lat lon to it.
  V <- E$v
  colnames(E$v)  <- paste0("EOF",  1:ncol(values_df))        # label each column
  EOFs <- cbind(locations_df, E$v)

  # Extract the principal component time series,
  # U in wide-table format
  TS1  <- data.frame(E$u) %>%	                               # convert U matrix to data frame
    dplyr::mutate(t  =  1:nrow(E$u))
  TS <- TS1 %>% tidyr::pivot_longer(cols = 1:(dim(TS1)[2]-1),
                             names_to ="EOF",
                             values_to ="PC")                           # put all columns (except times)
  # into long-table format with
  # EOF-PC as a key-value pair

  TS$nPC <- TS$PC * sqrt(nT-1)                             # Normalize the time series


  structure(list(
    eofs = EOFs,
    pcts = TS,
    data_loc = locations_df,
    data_vals = values_df,
    call = match.call()
  ), class='emporthfun')

}


#' @rdname emp_orth_fun
#' @export
emp_orth_fun.stars <- function(x,
                               ...){

  if(missing(x)){
    stop("Empty stars object x. Please give a stars object with 3 dimensions.")
  }

  x <- stats::setNames(x, "dat")

  # get x, y and t values
  x1 <- stars::st_get_dimension_values(x, 1)
  x2 <- stars::st_get_dimension_values(x, 2)
  t_vals <- stars::st_get_dimension_values(x, 3)

  times_df <- data.frame(time = t_vals)

  # make an xy grid
  gridxy <- meshgrid2d(x1, x2)
  locations_df <- data.frame(lon = gridxy[ ,1], lat = gridxy[ ,2])
  # flatten the stars to 2D
  x_sf <- stars::st_xy2sfc(x, as_points = TRUE, na.rm = FALSE)
  values_df <- x_sf$dat

  good_loc_vals <- which(apply(values_df, 1, function(x) sum(is.na(x))) ==0 )
  values_df2 <- values_df[good_loc_vals, ]
  locations_df2 <- locations_df[good_loc_vals, ]

  emp_orth_fun.data.frame(locations_df2,
                          values_df2)
}


#' @rdname emp_orth_fun
#' @export
#' @export
autoplot.emporthfun <- function(object,
                                EOF_num = 1,
                                palette = "Spectral",
                                only_EOF = FALSE,
                                only_TS = FALSE,
                                ...){
  EOF <- NULL
  EOFs <- object$eofs
  TS <- object$pcts
  nPC <- object$pcts$nPC

  variable <- paste("EOF",EOF_num, sep="")
  lon <- 'lon'
  lat <- 'lat'
  # Plot EOF
  eof_p <- ggplot(EOFs)  +  geom_tile(ggplot2::aes_string(x = lon, y = lat, fill = variable))  +
    scale_fill_distiller(palette = palette, guide = "colourbar") +
    theme_bw()  +
    xlab(expression(paste("Longitude", degree)))  +
    ylab(expression(paste("Latitude", degree))) +
    ggtitle(paste("EOF_",EOF_num, sep=""))

  # Plot PC of that EOF
  varriable2 <- paste("X", EOF_num, sep="")
  TS_EOF <- subset(TS, EOF == varriable2)
  pcts_eof <- ggplot(TS_EOF, aes(x=t, y=nPC)) + geom_line(size = 0.8) +
    xlab("Time (t)") +
    ylab("Normalized Principal Component") +
    ggtitle(paste('PC Time series for EOF_', EOF_num, sep=""))

  if(only_EOF){
    eof_p
  }else if(only_TS){
    pcts_eof
  }else{
    gridExtra::grid.arrange(eof_p, pcts_eof)
  }

}
