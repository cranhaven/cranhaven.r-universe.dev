#' @title LDA_plot_activity
#'
#' @description generate clonogenic activity estimation plot (frequency of
#'   negative wells over the number of cells seeded) for data of limiting
#'   dilution assay (LDA) experiments. Input is an data object as returned by
#'   the preprocessing function LDA_prepare_plot().
#'
#' @param LDA_obj list returned from LDA_prepare_plot
#' @param xlim manually setting the xlim
#' @param uncertainty.band plotting uncertainty bands TRUE/FALSE
#'
#' @return none
#'
#' @examples
#' x <- data.frame("cells" = rep(c(10,50,100,250),times = 4),
#'                 "wells" = rep(25,16),
#'                 "positive" = c(2,5,10,20,1,2,6,11,3,4,8,22,1,1,7,12),
#'                 "group" = rep(c(rep("A",4),rep("B",4)),times = 2),
#'                 "replicate" = c(rep(1,8),rep(2,8)))
#' out <- LDA_prepare_plot(x)
#' LDA_plot_activity(out[[1]])
#' data(LDAdata)
#' Z1 <- subset.data.frame(LDAdata,subset = name == unique(LDAdata$name)[1])
#' out <- LDA_prepare_plot(Z1[,c("S-value","# Tested","# Clonal growth",
#'                               "Group","replicate")])
#' LDA_plot_activity(out[[1]])
#' @importFrom graphics "abline" "plot" "polygon" "lines"
#' @importFrom grDevices "rgb" "col2rgb"
#' @export
#'
LDA_plot_activity <- function(LDA_obj,
                     xlim = NULL,
                     uncertainty.band = FALSE){
  if (class(LDA_obj)[1] != "list"){
    stop("error: input must be list as returned by LDA_prepare_plot")
  }

  alpha_col <- 0.42

  x_sp <- NULL
  for (gi in seq_along(LDA_obj)){
    dazu <- rbind(LDA_obj[[gi]]$rep.data,
                  LDA_obj[[gi]]$mean.data)
    dazu$col <- LDA_obj[[gi]]$color
    x_sp <- rbind(x_sp,dazu)
  }

  # +------------------------+
  # | plot Part I : activity |
  # +------------------------+
  if (is.null(xlim)) {
    xlim <- c(0,max(x_sp$x))
  }

  plot(x = x_sp$x,
       y = log(1-x_sp$y),
       xlim = xlim,
       ylab = "ln fract. nonresp.",
       xlab = "cells seeded",
       pch = x_sp$pch,col = x_sp$col)

  for (gi in seq_along(LDA_obj)){
    lines(x = LDA_obj[[gi]]$model$line$x,
          y = LDA_obj[[gi]]$model$line$y,lwd=2,
          col = LDA_obj[[gi]]$color)
    if (uncertainty.band == TRUE){
      polygon(x = c(LDA_obj[[gi]]$model$unc.band$x,
                    rev(LDA_obj[[gi]]$model$unc.band$x)),
              y = c(LDA_obj[[gi]]$model$unc.band$y.ub,
                    rev(LDA_obj[[gi]]$model$unc.band$y.lb)),
              border = NA,
              col = rgb(red = col2rgb(LDA_obj[[gi]]$color)[1],
                        green = col2rgb(LDA_obj[[gi]]$color)[2],
                        blue = col2rgb(LDA_obj[[gi]]$color)[3],
                        alpha = 127,
                        maxColorValue = 255))
    }
    abline(h = -1,lty = 3,col = "grey42")
  }
}
