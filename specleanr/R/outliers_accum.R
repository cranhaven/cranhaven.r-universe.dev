
#' Identify if enough methods are selected for the outlier detection.
#'
#' @param x \code{datacleaner}. The output from the outlier detection in \code{multidetect} function.
#' @param boots \code{interger}. The number of bootstraps to sample the outliers obtained during outlier
#'        detection process. Start from a lower number such as 10 and increase serially to get a smoother
#'        curve. High bootstrap may lead to crashing the Generalized Additive Model used to fit the
#'        bootstraps and cumulative number of outliers.
#' @param seed \code{integer} To fix the random sampling during bootstrapping.
#' @param select \code{vector}. If more than 10 groups are considered, then the at least should be seclected to hvae meaningful
#'    visualization.
#' @param ncol \code{integer}. Number of columns if the groups are greater 4, to allow effective vizualisation.
#' @param linecolor \code{string} A parameter to indicate the color of the lines. The default is 'purple'.
#' @param sci \code{logical}. If \code{sci} is TRUE, then the species names will be italised otherwise normal names will displayed. Default \code{FALSE}
#' @param xlab,ylab \code{string}. inherited from ggplot2 to changes x and y axis texts.
#' @param scales \code{string} Define if the x oy y axis will be shared or free. check \code{ggplot2} for details.
#'
#' @importFrom mgcv gam
#'
#' @return ggplot2 output with cumulative number of outliers and number of methods used.
#'
#' @export
#'
ggoutlieraccum <- function(x, boots = 5,
                           select = NULL, ncol = 3,
                           linecolor = 'blue',
                           seed = 1134, sci=FALSE,
                           xlab ='Number of methods',
                           ylab = 'Number of outliers',
                           scales = 'free'){

  var <- x@varused
  #get outliers
  spresult <- x@result

  if(isFALSE(boots%%1==0) | isFALSE(seed%%1==0)) stop("The seed or boots parameters must be integers.")

  #for single species assign a pseudo name to enable processing downward

  if(x@mode==FALSE) spout <- list(sp = spresult) else spout <- spresult


  if(length(unique(names(spout)))>=20 && is.null(select)) {

    stop("Provide a vector of maximum 20 group variables to display. Use the select parameter.")

  }else if(!is.null(select)){

    inOut <- names(x@result)%in%select

    inGroup <- names(x@result)[which(inOut==TRUE)]

    if(length(inGroup)<=0) stop("Groups indicated in select parameter do not exist in the groups in the original data.", call. = FALSE)

    if(length(inGroup)<length(select)) warning("Some groups were dropped as they were not in the dataset.", call. = FALSE)

    spout <- spout[inGroup]

  }else{
    spout
  }

  xs <- sapply(names(spout), function(yy){

    #get data based on names

    xd <- spout[[yy]]

    #first check for null values for methods that were not successful
    checkNA <- sapply(xd, nrow)

    #remove methods that didn't execute

    spnull <- xd[!sapply(checkNA,is.null)]

    #check if any method returned no outliers but will be retained while computing absolute outliers.
    len <- sapply(spnull, nrow)

    if(any(len==0)) cleanout <- spnull[len !=0] else cleanout <-  spnull #replace the list with empty data with y

    lgetout <- sapply(cleanout, function(xx)xx[[var]])

    bts <- seq(1, boots, 1)

    set.seed(seed)

    samp <- lapply(bts, function(x){

      xc <- sample(lgetout, replace = FALSE)

      outlierc_csum <- Reduce(union, xc, init = c(), accumulate = TRUE)

      outlierc_csum_null <- outlierc_csum[!sapply(outlierc_csum, is.null)]

      outlierfreq <- sapply(outlierc_csum_null, length)

      methodstot <- seq(1, length(outlierfreq), 1)

      dftf <- as.data.frame(cbind(methodstot, outlierfreq))
    })
    xout <- do.call(rbind, samp)

    xout[,'variables'] <- yy

    xout
  }, simplify = FALSE, USE.NAMES = FALSE)

  xsout <- do.call(rbind, xs)

  ncolx <- ncol
  scales_format <- scales

  #assign either gam or loess

  check_packages('ggplot2')

  #instantiate the parameters

  methodstot = NULL; outlierfreq = NULL; variables = NULL

  modelchange <- function(formula, data, weights, ...) {

    if(nrow(data) <= 1000){

      return(loess(formula = y ~ x, data = data, ...))

    }else{
      gam(formula = y ~ s(x, bs = "cs"), data = data, ...)
    }
  }

  ggplot2::ggplot(data = xsout, ggplot2::aes(x=methodstot, outlierfreq, group = variables))+

    ggplot2::geom_smooth(method = modelchange, color = linecolor)+

    ggplot2::facet_wrap(~variables, ncol = ncolx, scales = scales_format)+

    {if(sci==TRUE) ggplot2::theme(strip.text = ggplot2::element_text(face = 'italic'))}+

    ggplot2::labs(x=xlab, y=ylab)
}






