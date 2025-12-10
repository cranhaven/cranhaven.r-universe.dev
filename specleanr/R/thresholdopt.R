
#' @title Determine the threshold using Locally estimated or weighted Scatterplot Smoothing.
#'
#' @inheritParams ocindex
#' @param data \code{Dataframe}. The reference dataframe were absolute outliers will be removed.
#' @param outliers \code{datacleaner}. Datacleaner output with outliers flagged in \code{multidetect} function.
#' @param plotsetting \code{list}. to show plot of loess fitted function with local and global maxima (optimal threshold and clean data).
#'        The list had two parameters. 1) plot to indicate the plot and group to provide the plot title.
#' @param var_col \code{string}. A column with species names if \code{dataset} for species is a dataframe not a list.
#'        See \code{\link{pred_extract}} for extracting environmental data.
#' @param verbose \code{logical}. If true, then messages about the outlier flagging will be displayed.
#' @param cutoff \code{numeric}. Ranging from 0.5 to 0.8 indicating the cutoff to initiate the
#'        LOESS model to optimize the identification of absolute outliers.
#' @param tloss \code{seqences} Indicates the sequence for tuning the the span parameter of the LOESS model.
#' @importFrom stats loess optimize
#' @importFrom graphics abline legend lines points title
#' @return Returns \code{numeric} of most suitable threshold at globalmaxima or localmaxima of the loess smoothing.
#'
#'
#'
search_threshold <- function(data, outliers,
                             sp = NULL,
                             plotsetting = list(plot= FALSE, group = NULL),
                             var_col = NULL,
                             warn=FALSE,
                             verbose=FALSE,
                             cutoff,
                             tloss = seq(0.1, 1, 0.1)){

  var <- outliers@varused

  if(length(var)>1) var <- sp else var

  #handle data groups with proportions less than 0.5 and groups with less than 2 methods with outliers.

  gcheck <- tryCatch(expr = ocindex(x= outliers, sp = sp, threshold = 0.1,
                                    absolute = TRUE, props = TRUE, warn = FALSE),
                     error = function(e) if(grepl('The methods with outliers', e$message)) NULL else 'unknown error')

  if(!is.null(gcheck)){

    maxtr <- max(gcheck$absolute_propn)

    if(maxtr >= cutoff){

      #get unique proportions
      uniqprop <- unique(gcheck$absolute_propn)

      if(length(uniqprop)<=2){
        NULL
      }else{

        el <- lapply(tloss, function(tt){

          ee <- try(ocindex(x= outliers, sp = sp, absolute = TRUE, threshold = tt, warn = warn),
                    silent = TRUE)

          if(!inherits(ee, 'try-error')) {

            varc <- unlist(data[, var])

            indx <- which(!varc %in% ee)

            datIn <- data[indx,]

            dt <- data.frame(th = tt, val= nrow(datIn))
          }
        })
        elout <-  Reduce(rbind, el)

        tl <- sapply(tloss, function(oo){

          lwrs <- tryCatch(expr = loess(val~th, data= elout, span = oo),#lowess(x = elout$th, y = elout$val, f = oo),#

                           error= function(e) NULL, warning=function(w) NULL)

          if(!is.null(lwrs)) vec <- data.frame(rmse = sqrt(mean((predict(lwrs)-unlist(elout$val))^2)), spans = oo) else vec <- data.frame(rmse = NA, spans = oo)

        }, simplify = FALSE, USE.NAMES = FALSE)

        spanout <- Reduce(rbind, tl)

        bestspan <- spanout$spans[which.min(spanout$rmse)]

        if(length(bestspan) != 0){

          #Get the maxima and minima ##thresholds when clean extracted data has reached highest.
          #Flat curve or where the slope of the line is the highest.: identify the first derivative
          #For the fitted data
          #completely, the f(x) = 0
          #maxima: a maximum where any shift the gradient decreases
          #https://stackoverflow.com/questions/12183137/calculate-min-max-slope-of-loess-fitted-curve-with-r

          fit <- loess(val ~ th, data = elout, span = bestspan)

          grid_x <- seq(min(elout$th), max(elout$th), length.out = 100)

          pred_y <- predict(fit, newdata = data.frame(th = grid_x))

          maxlog <- c(FALSE, diff(sign(diff(pred_y))) == -2, FALSE)

          l_max_x <- min(grid_x[maxlog], na.rm = TRUE)

          l_max_y <- min(pred_y[maxlog], na.rm = TRUE)

          opt <- optimize(function(x) -predict(fit, newdata = data.frame(th = x)),
                          interval = range(elout$th))

          g_max_x = opt$minimum

          g_max_y = -opt$objective

          #handle instance when g_max_x is less than the max threshold
          if(g_max_x<=cutoff || g_max_x>cutoff) {

            dydx <- diff(elout$val)/diff(elout$th)
            #get indices after the cutoff
            ctf <- which(elout$th[-1]>=cutoff+0.1)

            if(length(ctf)>1){
              #get deriv
              ths <- elout$th[-1][ctf] #index - 1
              vals <- elout$val[-1][ctf]
              dv <- dydx[ctf]
              g_max_x <- ths[which.min(dv)]
              g_max_y <- vals[which.min(dv)]
            }else{
              g_max_x
              g_max_y
            }
          }

          plotvars <- modifyList(x = list(plot=FALSE, group = NULL), plotsetting)

          # get variables
          group <- plotvars$group
          plot  <- plotvars$plot


          if(isTRUE(plot)){

            y1sub <- 10^(floor(log10(min(elout$val))) - 2)

            y1add <- 10^(floor(log10(max(elout$val))) - 2)

            plot(elout$th, elout$val, pch = 20, col = "grey15", main = " ",
                 mgp = c(2, 1, 0), xlab ='Thresholds', ylab = 'Records after outlier removal',
                 cex.lab  = 0.8,
                 cex.axis = 0.8,
                 cex.main = 0.7,
                 ylim = c(min(elout$val)-y1sub, max(elout$val)+y1add),
                 xlim = c(min(elout$th), max(elout$th)+0.05))

            lines(grid_x, pred_y, col = "grey40", lwd = 2)

            points(l_max_x, l_max_y, col = "purple", pch = 8, cex = 1.2)

            points(g_max_x, g_max_y, col = "red", pch = 8, cex = 1.2)

            abline(v=l_max_x, col='purple', lty = 2)

            abline(h=l_max_y, col='purple', lty = 4)

            abline(h=g_max_y, col='red', lty = 4)

            abline(v=g_max_x, col='red', lty = 4)

            if(!is.null(group)) title(group, line = 0.5)

            step <- 10^(floor(log10(l_max_y)) - 1)

            xmax <- if(g_max_x<=0.4)  g_max_x else g_max_x-0.4

            legend(x= xmax, y = l_max_y-step,
                   legend = c("Fitted curve", "Local maxima", "Global maxima"),
                   col    = c("grey40", "purple", "red"),
                   lty    = c(1, 0, 0),
                   pch    = c(NA, 19, 19),
                   lwd    = c(2, 0, 0),
                   y.intersp = 0.8,
                   x.intersp = 0.1,
                   pt.cex = 0.5,
                   bty = 'n')
          }
          return(c(localmaxima = l_max_x, globalmaxima = g_max_x))
        }else {
          NULL
        }
      }
    }else{
      NULL
    }
  }else{
    NULL
  }
}


#' @title Optimize threshold for clean data extraction.
#'
#' @inheritParams search_threshold
#' @param refdata \code{dataframe}. Species data frame from precleaned analysis.
#' @return Either a \code{list} or \code{dataframe} of cleaned records for multiple species.
#' @export
#'
#'

optimal_threshold <- function(refdata, outliers, var_col = NULL, warn=FALSE, verbose=FALSE,
                              plotsetting =  list(plot = FALSE, group = NULL),
                              cutoff = 0.6){

  #for a single species: clean data extraction

  if(deparse(substitute(refdata))!= outliers@dfname)stop('The reference dataset used in outlier detection and the output of outlier detection are different.')

  if(outliers@mode==FALSE){

    dfdata <- search_threshold(data = refdata, outliers = outliers, plotsetting = plotsetting, warn=warn, verbose=verbose, cutoff = cutoff)

  }else{

    if(is(refdata, 'list')){

      if(length(refdata)!= length(outliers@result)) stop('Number of species in in data and outlier detection are not equal')

      splist <- refdata

    } else if(is(refdata, 'data.frame')){

      if(is.null(var_col)) stop('Provide the column with species names in parameter, var_col .')

      splist <- split(refdata, f= refdata[,var_col])

      if(length(splist)!= length(outliers@result)) stop('Number of species in in data and outlier detection are not equal')

    }else{
      stop('Only list or dataframe of species occurences accepted or set the var_col parameter.')
    }
    spdata <- list()

    for (opt in seq_along(splist)) {

      spnames <- names(splist)[opt]

      if(is(refdata, 'data.frame'))  data2<- refdata[refdata[,var_col] == spnames, ] else  data2<- refdata[[spnames]]

      minmax <- search_threshold(data = data2, outliers = outliers, sp = spnames, plotsetting = plotsetting, var_col = var_col,
                                 warn=warn, verbose=verbose, cutoff = cutoff)

      if(!is.null(minmax)) spdata[[opt]] <- data.frame(localmaxima = unname(minmax[1]), globalmaxima= unname(minmax[2])) else spdata[[opt]] <- data.frame(localmaxima = NA, globalmaxima = NA)

      spdata[[opt]]['species'] <- spnames

      dfdata <- do.call(rbind, spdata)

    }
  }
  invisible(dfdata)
}



