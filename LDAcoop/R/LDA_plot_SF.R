#' @title LDA_plot_SF
#'
#' @description generate clonogenic survival plot (estimated clonogenic
#'   survival over treatment) for data from limiting dilution assay (LDA).
#'   Input is an data object as returned by the preprocessing function
#'   LDA_prepare_plot().
#'
#' @param LDA_obj list returned from LDA_prepare_plot
#'
#' @return none
#'
#' @examples
#' x <- data.frame("cells" = rep(c(10,50,100,250),times = 4),
#'                 "wells" = rep(25,16),
#'                 "positive" = c(2,5,10,20,1,2,6,11,3,4,8,22,1,1,7,12),
#'                 "group" = rep(c(rep(0,4),rep(6,4)),times = 2),
#'                 "replicate" = c(rep(1,8),rep(2,8)))
#' out <- LDA_prepare_plot(x)
#' LDA_plot_SF(out[[2]])
#' data(LDAdata)
#' Z1 <- subset.data.frame(LDAdata,subset = name == unique(LDAdata$name)[1])
#' out <- LDA_prepare_plot(Z1[,c("S-value","# Tested","# Clonal growth",
#'                                 "Group","replicate")])
#' LDA_plot_SF(out[[2]])
#' @importFrom graphics "plot" "title" "axis"
#' @importFrom grDevices "rgb"
#' @importFrom Hmisc "errbar" "ceil"
#' @export
#'
LDA_plot_SF <- function(LDA_obj){
  if (class(LDA_obj)[1] != "data.frame"){
    stop("error: input must be of class data.frame")
  }

  LDA_obj$treat <- as.numeric(LDA_obj$treat)

  plot(
      x = LDA_obj$treat,
      y = log10(LDA_obj$sf),
      #main = SF[[sfi]]$name,
      col.main = rgb(
        red = 0, green = 148/255, blue = 64/255, alpha = 1,
        maxColorValue = 1
      ),
      las = 1,
      ylab = "",
      xaxt = "n",
      yaxt = "n",
      xlab = "treatment",
      ylim = c(floor(min(log10(LDA_obj$sf.msd))),0),
      col = LDA_obj$col,
      axes = FALSE,
      pch = 19
    )
    ytick <- floor(min(log10(LDA_obj$sf.msd), na.rm = TRUE)):
      ceil(max(log10(LDA_obj$sf.psd), na.rm = TRUE))
    axis(
      side = 2,
      at = ytick,
      las = 1,
      labels = paste0(10^ytick * 100, "%")
    )
    title(
      ylab = "clonogenic survival",
      line = 2.5
    )
    axis(
      side = 1,
      at = LDA_obj$treat,
      labels = LDA_obj$treat
    )
    with(data = LDA_obj,
         errbar(treat,
                log10(sf),
                log10(sf.msd),
                log10(sf.psd),
                col = LDA_obj$col,
                add = TRUE,
                pch = 1,
                errbar.col = LDA_obj$col
    )
  )
}

