#' @importFrom rlang .data

#' @export

ggVarImp <- function(importance, sort=TRUE, xlabel="Importance", ylabel="Variable", main="") {

  temp <- data.frame(var=names(importance), imp=importance, stringsAsFactor=FALSE)
  
  if(sort) temp <- temp[order(temp$imp),]
    
  p <- ggplot2::ggplot(temp, ggplot2::aes(x=.data$imp, y=factor(.data$var,levels=.data$var))) +
        ggplot2::geom_point() +
        ggplot2::geom_vline(ggplot2::aes(xintercept=0), size=0.2, linetype='longdash', color='darkgray') +
        ggplot2::theme_bw() +
        ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                       panel.grid.minor.x = ggplot2::element_blank(),
                       panel.grid.major.y = ggplot2::element_line(colour='darkgray', size=0.2, linetype='dashed')) +
        ggplot2::expand_limits(x=0) +
        ggplot2::xlab(xlabel) +
        ggplot2::ylab(ylabel) +
        ggplot2::ggtitle(main)
  
  p

}