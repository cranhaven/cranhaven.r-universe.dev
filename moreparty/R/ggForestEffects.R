#' @importFrom rlang .data
#' 
#' @export

ggForestEffects <- function(dt, vline=0, xlabel="", ylabel="", main="") {

  # names(dt)[which(sapply(dt, is.numeric))] <- "value"
  
  if(sum(duplicated(dt$cat))>0) stop("There should be no duplicated categories.")
  
  p <- ggplot2::ggplot(dt, ggplot2::aes(x = .data$value, y = factor(.data$cat,levels=.data$cat), color = .data$var)) + 
         ggplot2::geom_point(shape = 16) + 
         ggplot2::facet_grid(.data$var ~ ., scales = "free_y", space = "free_y") + 
         ggplot2::theme_bw() + 
         ggplot2::theme(panel.grid = ggplot2::element_blank(),
                        panel.grid.major.y = ggplot2::element_line(size=.1, color="grey70"),
                        legend.position = "none",
                        strip.text.y = ggplot2::element_text(angle = 0)) +
         ggplot2::geom_vline(ggplot2::aes(xintercept=vline), size=0.2, linetype='dashed', color='darkgray') +
         ggplot2::xlab(xlabel) +
         ggplot2::ylab(ylabel) +
         ggplot2::ggtitle(main)
  p

}