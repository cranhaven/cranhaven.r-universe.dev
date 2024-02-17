#' Plot percent richness change for various percent flow reductions
#' @description Calculates and plots percent richness change resulting from streamflow reductions
#' @param stats A dataframe of ELF statistics
#' @param yaxis_thresh Value used for specifying y-axis max limit
#' @param xlabel Used to overwrite default x-axis label
#' @param ylabel Used to overwrite default y-axis label
#' @return Plot of percent decreases in richness from flow reductions
#' @import scales
#' @import ggplot2
#' @export elfchange
#' @examples
#' \donttest{
#' # We don't run this example by R CMD check, because it takes >10s
#'
#' # Generate plot of percent richness change for various percent flow reductions
#' watershed.df <- elfdata(watershed.code = '0208020104', ichthy.localpath = tempdir())
#' breakpt <- 500
#' elf <- elfgen(
#'    "watershed.df" = watershed.df,
#'    "quantile" = 0.95,
#'    "breakpt" = breakpt,
#'    "xlabel" = "Mean Annual Flow (ft3/s)",
#'    "ylabel" = "Fish Species Richness"
#'    )
#' elfchange(elf$stats, "yaxis_thresh" = 25)
#' }
elfchange <- function(stats,yaxis_thresh,xlabel = FALSE,ylabel = FALSE) {
  its <- seq(1, 500, 0.01)
  pct_list <- c(5, 10, 20, 30, 40, 50)

  table <- data.frame("xvalues" = its,
                      stringsAsFactors = FALSE)

  #message(paste("Processing Richness Change...",sep = ''))
  for (i in 1:length(pct_list)) {
    pct <- pct_list[i]
    elfchg.percent <- richness_change(stats, pct, its)
    elfchg.percent <- -elfchg.percent

    table_i = data.frame(elfchg.percent)
    names(table_i) <- c(paste("pct_chg_", pct_list[i], sep = ""))
    table <- cbind(table, table_i)
  }

  # default ymax if none provided
  if (missing(yaxis_thresh)) {
    yaxis_thresh <- 100
  }

  xaxis_title <- paste("Mean Annual Flow (ft3/s)",sep="")
  yaxis_title <- paste("Fish Species Richness",sep="")
  if (xlabel != FALSE) {xaxis_title <- xlabel}
  if (ylabel != FALSE) {yaxis_title <- ylabel}

  ptitle <- paste("Change in ",yaxis_title," at Various Percent Flow Reductions","\n","Watershed: ",stats$watershed,"\n",sep="")

  xaxis_title <- paste("\n",xaxis_title,sep="")
  yaxis_title <- paste("Percent Decrease in","\n",yaxis_title,"\n",sep="")

  #message(paste("Generating Plot Image...",sep = ''))
  xvalues <- NULL
  pct_chg_50 <- NULL
  pct_chg_40 <- NULL
  pct_chg_30 <- NULL
  pct_chg_20 <- NULL
  pct_chg_10 <- NULL
  pct_chg_5 <- NULL

  result <- ggplot(table , aes(x=xvalues, y=pct_chg_50)) +

    geom_line(data = table , aes(x=xvalues,y=pct_chg_50,color = "black"), na.rm=TRUE) +
    geom_line(data = table , aes(x=xvalues,y=pct_chg_40,color = "blue"), na.rm=TRUE) +
    geom_line(data = table , aes(x=xvalues,y=pct_chg_30,color = "green"), na.rm=TRUE)+
    geom_line(data = table , aes(x=xvalues,y=pct_chg_20,color = "red"), na.rm=TRUE)+
    geom_line(data = table , aes(x=xvalues,y=pct_chg_10,color = "violet"), na.rm=TRUE)+
    geom_line(data = table , aes(x=xvalues,y=pct_chg_5,color = "wheat"), na.rm=TRUE)+

    scale_color_manual(
      "Flow Reduction",
      values=c("black","blue","forestgreen","red","darkmagenta","sienna4"),
      labels=c("50%","40%","30%","20%","10%","5%")
    ) +

    ylim(0,yaxis_thresh) +

    scale_x_log10(
      limits = c(1,500),
      breaks = trans_breaks("log", function(x) {10^x})
    ) +
    annotation_logticks(sides = "b")+

    ggtitle(ptitle)+
    labs(x=xaxis_title,y=yaxis_title)+
    theme(axis.text.x = element_text(colour="grey20",size=15,hjust=.5,vjust=.5,face="plain"),
          axis.text.y = element_text(colour="grey20",size=15,hjust=.5,vjust=.5,face="plain"))

  return(result)
}
