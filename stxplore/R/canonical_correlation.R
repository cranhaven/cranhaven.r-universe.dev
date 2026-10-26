#' Computes transformed variables from Canonical Correlation Analysis using a dataframe or a stars object
#'
#' Computes Canonical Correlation Analysis (CCA) using 2 datasets. The autoplot function
#' plots the output.
#'
#' @param x1 The first dataframe or stars object.
#' @param x2 The second dataframe or stars objext. The dimensions of both datasets need to be the same.
#' @param ... Other arguments that need to be used for datafames or currently ignored.
#' @param object  For autoplot: the output of the function `cannonical_correlation'.
#' @param xlab For autoplot: the xlabel to appear on CCA plot.
#' @param ... Other arguments currently ignored.
#'
#' @return A canonical correlation object.
#'
#' @examples
#' # Dataframe example
#' df1 <- SSTdatashort[1:100, ]
#' df2 <- SSTdatashort[401:500, ]
#' ccor <- canonical_correlation(df1, df2)
#' autoplot(ccor)
#'
#' # stars example
#' library(stars)
#' tif = system.file("tif/olinda_dem_utm25s.tif", package = "stars")
#' x <- read_stars(tif)
#' x1 <- x[[1]][1:50, 1:50]
#' x2 <- x[[1]][51:100, 1:50]
#' stx1 <- st_as_stars(x1)
#' stx2 <- st_as_stars(x2)
#' canonical_correlation(stx1, stx2)
#'
#' @importFrom stats cancor
#' @export
canonical_correlation <- function(x1,
                                  x2,
                                  ...){
  UseMethod("canonical_correlation")
}

#' @rdname canonical_correlation
#' @export
canonical_correlation.data.frame <- function(x1,
                                             x2,
                                             ...){
  # option 1 - using 2 separate datasets - using raw
  if(missing(x1)){
    stop("Empty dataframe df1. Please give a dataframe with N1 rows and N2 columns.")
  }

  if(missing(x2)){
    stop("Empty dataframe df2. Please give a dataframe with N1 rows and N2 columns.")
  }

  if(!identical(dim(x1),dim(x2))){
    stop("The datasets need to have the same dimensions.")
  }

  df1 <- x1
  df2 <- x2
  t <- ts <- Variable <- NULL

  nn <- dim(df1)[1]
  dd <- dim(df1)[2]
  if(nn < dd){
    message('Rows less than columns. Using the transpose to perform CCA')
    df1 <- t(df1)
    df2 <- t(df2)
  }

  nn <- dim(df1)[1]
  cc <- cancor(df1, df2)

  df1cc <- as.matrix(df1) %*% cc$xcoef[ ,1]
  df2cc <- as.matrix(df2) %*% cc$ycoef[ ,1]
  # Compute the  series of the first canonical variables
  CCA_df  <- data.frame(t  =  rep(1:nn, 2),
                        ts  =  c(df1cc, df2cc),
                        Variable = c(rep("CCA1-df1", nn), rep("CCA1-df2", nn)))


  structure(list(
    cancor_df = CCA_df,
    df1 = df1,
    df2 = df2,
    call = match.call()
  ), class='cancor')

}

#' @rdname canonical_correlation
#' @export
canonical_correlation.stars <- function(x1,
                                        x2,
                                        ...){
  # option 1 - using 2 separate datasets - using raw
  if(missing(x1)){
    stop("Empty stars object x1. Please give a stars object with dimensions N1xN2.")
  }

  if(missing(x2)){
    stop("Empty stars object x2. Please give a stars object with dimensions N1xN2.")
  }

  if(!identical(dim(x1),dim(x2))){
    stop("The datasets need to have the same dimensions.")
  }

  df1 <- dplyr::as_tibble(x1)
  df2 <- dplyr::as_tibble(x2)

  canonical_correlation.data.frame(df1, df2)
}

#' @rdname canonical_correlation
#' @export
autoplot.cancor <- function(object,
                            xlab = "Time",
                            ...){

  t <- ts <- Variable <- NULL
  CCA_df <- object$cancor_df

  # Now plot
  sst_cca <- ggplot(CCA_df, aes(x=t, y=ts, colour = Variable)) + geom_line(size = 0.8) +
    scale_color_manual(values = c("dark blue", "dark red")) +
    ylab("CCA  variables")  +
    xlab(xlab)  +
    theme_bw() +
    ggtitle("Canonical Correlation Analysis")

  sst_cca

}


