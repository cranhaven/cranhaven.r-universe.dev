#' Function to display a bland plot in order to visually assess the agreement between CytOpt estimation
#' of the class proportions and the estimate of the class proportions provided through manual gating.
#'
#'@param proportions \code{data.frame} of (true and) estimated proportions from \code{CytOpt()}
#'@param title plot title. Default is \code{""}, i.e. no title.
#'@param xaxis_angle scalar indicating an angle to tilt the labels of x_axis. Default is \code{45}.
#'
#'@return a \code{\link[ggplot2]{ggplot}} object
#'
#'@importFrom reshape2 melt
#'@import ggplot2
#'@import MetBrewer
#'@export
#'
#'@examples
#'if(interactive()){
#'
#'res <- CytOpT(X_s = HIPC_Stanford_1228_1A, X_t = HIPC_Stanford_1369_1A, 
#'              Lab_source = HIPC_Stanford_1228_1A_labels,
#'              eps = 0.0001, lbd = 0.0001, n_iter = 10000, n_stoc=10,
#'              step_grad = 10, step = 5, power = 0.99, 
#'              method='minmax')
#'barplot_prop(res$proportions)
#'
#'}


barplot_prop <- function (proportions, title = "", xaxis_angle = 45){
  
  proportions$Population <- rownames(proportions)
  data2barplot <- reshape2::melt(proportions, id.vars="Population",
                                 value.name = "Proportion", variable.name = "Method")
  
  data2barplot$Method <- gsub("MinMax", "MinMax swapping",
                              gsub("Descent_ascent", "Descent-Ascent",
                                   gsub("Gold_standard","Gold standard", data2barplot$Method)))
  
  data2barplot$Method <- factor(data2barplot$Method,
                                levels = c("Gold standard", "Descent-Ascent", "MinMax swapping"))
  
  data2barplot$Population <- factor(data2barplot$Population,levels = unique(data2barplot$Population))
  
  cols <- met.brewer("Egypt", n=3)
  names(cols) <- c("Gold standard", "Descent-Ascent", "MinMax swapping")

  p <- ggplot(data=data2barplot, 
              aes_string(x="Population", y="Proportion", fill="Method")) +
    geom_bar(stat="identity", position=position_dodge(), colour="black") +
    scale_fill_manual("", values=cols, 
                      breaks=unique(data2barplot$Method)
                      #limits=c("Gold standard", "Descent-Ascent", "MinMax swapping"),
                      #drop=TRUE
    ) +
    ggtitle(title) +
    theme(legend.title=element_blank()) +
    theme_bw()
  
  if(!is.null(xaxis_angle)){
    p <- p + theme(axis.text.x = element_text(angle=xaxis_angle, hjust = 1))
  }
  return(p)
  
}
