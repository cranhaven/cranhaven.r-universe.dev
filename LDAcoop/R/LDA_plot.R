#' @title LDA_plot
#'
#' @description plot clonogenic activity and survival (at more than one
#'   treatment group) for data from limiting dilution assay (LDA) experiments.
#'
#' @param LDA_tab LDA data.frame
#'      ("cells", "wells", "positive", "group", "replicate")
#' @param uncertainty method for uncertainty calculation ("act", "ep")
#' @param xlim setting xlim of clonogenic activity plot
#' @param uncertainty.band plotting of uncertainty bands TRUE/FALSE
#'
#' @return none
#'
#' @examples
#' data(LDAdata)
#' Z1 <- subset.data.frame(LDAdata,subset = name == unique(LDAdata$name)[1])
#' LDA_plot(Z1[,c("S-value","# Tested","# Clonal growth","Group","replicate")])
#' @importFrom graphics "par"
#' @export
#'
LDA_plot <- function(LDA_tab,
                     uncertainty = "act",
                     xlim = NULL,
                     uncertainty.band = FALSE){
  if (class(LDA_tab)[1] != "data.frame"){
    stop("error: input must be of class data.frame")
  }
  if (!(uncertainty %in% c("ep","act"))){
    stop("error: uncertainty must be either
         'ep' (error propagation) or
         'act' (activity CIs)")
  }
  if (ncol(LDA_tab) == 3){
    LDA_tab$Group <- 0
  }

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  par(
    mar = c(2.5, 3.5, 0.5, 0.5),
    mgp = c(1.5, 0.5, 0)
  )
  if (length(unique(LDA_tab$Group)) > 1){
    par(mfrow = c(2,1))
  }
  LDA_PlotObj <- LDA_prepare_plot(LDA_tab,
                                  uncertainty = uncertainty)

  LDA_plot_activity(LDA_obj = LDA_PlotObj[[1]],
                    xlim = xlim,
                    uncertainty.band = uncertainty.band)
  if (length(unique(LDA_tab$Group)) > 1){
    LDA_plot_SF(LDA_obj = LDA_PlotObj[[2]])
  }
}
