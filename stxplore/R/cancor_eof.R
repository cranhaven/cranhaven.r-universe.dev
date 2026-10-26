#' Performs CCA  using Empirical Orthogonal Functions (EOFs) from a lagged dataset
#'
#'@description
#' Performs Canonical Correlation Analysis (CCA) using Empirical Orthogonal Function analysis using
#' in a dataframe or a stars object. The autoplot function can plot the outputs.
#'
#' The variations are
#'  *  `cancor_eof.data.frame()`  if the input is a dataframe
#'  *  `cancor_eof.stars()` if the input is a stars object
#'  *  `autoplot.cancoreof()` to plot the outputs.
#'
#'
#'
#' @inheritParams emp_orth_fun
#' @inheritParams emp_orth_fun.data.frame
#' @param lag Specifies the lag to be used.
#' @param n_eof The number of EOFs to be used.
#' @param object autoplot parameter: the output of the function `cancor_eof'.
#' @param line_plot autoplot parameter: if set to \code{TRUE}, then the line plot is included.
#' @param space_plot autoplot parameter: if set to \code{TRUE}, the space splot is included.
#' @param palette autoplot parameter: the color palette to use for plotting.
#' @param xlab autoplot parameter:: he label on the x-axis for the line plot.
#' @param ... Other arguments currently ignored.
#'
#' @return A cancoreof object with CCA output, EOF output, original data and cancor object from `stats`.
#'
#' @examples
#' # Dataframe example
#' data(SSTlonlatshort)
#' data(SSTdatashort)
#' cancor_df <- cancor_eof(x = SSTlonlatshort,
#'            lag = 7,
#'            n_eof = 8,
#'            values_df = SSTdatashort)
#' autoplot(cancor_df)
#'
#' # Stars example
#' library(dplyr)
#' library(stars)
#' # Create a stars object from a data frame
#' precip_df <- NOAA_df_1990[NOAA_df_1990$proc == 'Precip', ] %>%
#'   filter(date >= "1992-02-01" & date <= "1992-02-28")
#' precip <- precip_df[ ,c('lat', 'lon', 'date', 'z')]
#' st_precip <- st_as_stars(precip, dims = c("lon", "lat", "date"))
#' cancor_st <- cancor_eof(st_precip)
#' autoplot(cancor_st, line_plot = TRUE, space_plot = FALSE)
#'
#'
#' @export cancor_eof
cancor_eof <- function(x,
                       lag,
                       n_eof,
                       ...){
  UseMethod("cancor_eof")
}


#' @rdname cancor_eof
#' @export
cancor_eof.data.frame <- function(x,
                                  lag = 7,
                                  n_eof = 10,
                                  values_df,
                                  ...){

  # option 4 - using the same dataset - using eof lagged
  if(missing(x)){
    stop("Empty dataframe x. Please give a dataframe with latitude and longitude.")
  }
  locations_df <- x

  if(missing(values_df)){
    stop("Empty dataframe values_df. Please give an N x T dataframe with values of the quantity you are interested in.  ")
  }
  message("Canonical Correlation Analysis is carried out using a lagged dataset.")

  SW1 <- Variable <- lat <- lon <- ts <- EOF <- nPC <- NULL

  # Put data into space-time format
  Z  <- t(values_df)

  spat_mean <-  apply(values_df, 1, mean)
  nT  <- ncol(values_df)

  # Subtract and standardise and compute the SVD
  Zt <- 1/sqrt(nT - 1)*(Z  - outer(rep(1,nT), spat_mean))
  E <- svd(Zt)

  # Extract the matrix V which contains the EOFs in space-time format and append
  # lat lon to it.
  V <- E$v
  colnames(E$v)  <- paste0("EOF",  1:ncol(values_df))        # label each column
  EOFs <- cbind(locations_df, E$v)

  nEOF <-  n_eof
  EOFset1  <- E$u[1:(nT-lag),  1:nEOF]  * sqrt(nT - 1)
  EOFset2  <- E$u[(lag+1):nT,  1:nEOF]  * sqrt(nT - 1)

  cc  <-  cancor(EOFset1, EOFset2)

  # Compute the  series of the first canonical variables
  CCA_df  <- data.frame(t  =  rep(1:(nT-lag), 2),
                        ts  =  c((EOFset1 %*%  cc$xcoef[,1])[,1],
                                 (EOFset2 %*%  cc$ycoef[,1])[,1]),
                        Variable = c(rep("CCA1", nT-lag), rep("CCA2", nT-lag)))

  # Visualizing the Linear Weights
  EOFs_CCA <- data.frame(lon = EOFs$lon, lat = EOFs$lat)
  EOFs_CCA$SW1 <- as.numeric(as.matrix(EOFs[,3:(2+n_eof)]) %*% cc$xcoef[,1])
  EOFs_CCA$SW2 <- as.numeric(as.matrix(EOFs[,3:(2+n_eof)]) %*% cc$ycoef[,1])



  # Compute the  series of the first canonical variables
  CCA_df  <- data.frame(t  =  rep(1:(nT-lag), 2),
                        ts  =  c((EOFset1 %*%  cc$xcoef[,1])[,1],
                                 (EOFset2 %*%  cc$ycoef[,1])[,1]),
                        Variable = c(rep("CCA1", nT-lag), rep("CCA2", nT-lag)))

  structure(list(
    cancor_df = CCA_df,
    eofs_df = EOFs_CCA,
    locations_df = locations_df,
    values_df = values_df,
    cancor_obj = cc,
    call = match.call()
  ), class='cancoreof')


}

#' @rdname cancor_eof
#' @export
cancor_eof.stars <- function(x,
                             lag = 7,
                             n_eof = 10,
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

  cancor_eof.data.frame(x = locations_df2,
                        lag = lag,
                        n_eof = n_eof,
                        values_df =  values_df2)
}


#' @rdname cancor_eof
#' @export
autoplot.cancoreof <- function(object,
                               line_plot = TRUE,
                               space_plot = TRUE,
                               palette = 'Spectral',
                               xlab = "Time",
                               ...){

  SW1 <- t <- ts <- Variable <- lon <- lat <- NULL
  CCA_df <- object$cancor_df
  EOFs_CCA <- object$eofs_df

  sst_cca <- ggplot(CCA_df, aes(x=t, y=ts, colour = Variable)) + geom_line(size = 0.8) +
    scale_color_manual(values = c("dark blue", "dark red")) +
    ylab("CCA  variables")  +
    xlab(xlab)  +
    theme_bw() +
    ggtitle("Canonical Correlation Analysis")

  # Plotting Weights as Spatial Maps: First Canoncial Variable

  spwts_p <- ggplot(EOFs_CCA, aes(lon, lat)) +
    geom_tile(aes(fill = SW1)) +
    scale_fill_distiller(palette = palette, guide = "colourbar") +
    scale_y_reverse() + ylab("Latitude") +
    xlab("Longitude") +
    ggtitle("Spatial Weights: Canonical Variable")  +
    theme_bw()

  if(line_plot & space_plot){
    gridExtra::grid.arrange(sst_cca, spwts_p)
  }else if(line_plot){
    sst_cca
  }else if(space_plot){
    spwts_p
  }
}
