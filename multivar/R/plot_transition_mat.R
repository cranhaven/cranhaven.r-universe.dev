#' Plot arbitrary transition matrix.
#'
#' @param x Matrix. An arbitrary transition matrix.
#' @param title Character. A title for the plot.
#' @param subtitle Character. A subtitle for the plot.
#' @param ub Numeric. Upper bound on coefficient values for heatmap index. Default is 1.
#' @param lb Numeric. Lower bound on coefficient values for heatmap index. Default is -1.
#' @keywords var multivar lot
#'
#' @examples
#'
#' plot_transition_mat(matrix(rnorm(25),5,5), title= "Example")
#'
#' @export
plot_transition_mat <- function(x, title = NULL, subtitle = NULL, ub = 1, lb = -1){
  rows <- values <- NULL
  mats <- list("common" = x)
  df   <- setNames(reshape2::melt(mats$common), c('rows', 'vars', 'values'))
 
  if(!is.null(subtitle)){
    df$Subject <- subtitle
  }
  
  df$values[df$values == 0] <- NA

  zf_red <- rgb(255,0,90, maxColorValue=255)
  zf_green <- rgb(90, 168, 0, maxColorValue=255)
  zf_blue <- rgb(0, 152, 233, maxColorValue=255)
  zf_yellow <- rgb(242, 147, 24, maxColorValue=255)
  zf_back <- rgb(51,51,51, maxColorValue=255)
  #zf_fore <- rgb(249,242,215, maxColorValue=255)
  zf_fore <- "white"
  
  text_color <- zf_back
  grid_color <- zf_back
  plot_background <- "white"
  
  colfunc_low <- colorRampPalette(c(zf_red, zf_fore))
  colfunc_high <- colorRampPalette(c(zf_fore, zf_blue))
  colors_to_use <- c(colfunc_low(6)[1:3],zf_fore,colfunc_high(6)[4:6])
  
  limit <- max(abs(c(lb,ub))) * c(-1, 1)
  
  df$rows <- factor(df$rows,levels = rev(colnames(mats$common)))
 
  gg <- ggplot(df, aes(y=rows, x=vars, fill = values)) # original
  #gg <- gg + geom_tile(color=grid_color, size=.5) 
  gg <- gg + geom_tile() 
  gg <- gg + scale_fill_gradientn(colors=colors_to_use,limits=limit,na.value = zf_fore,guide = guide_colorbar(frame.colour = "black", ticks.colour = "black",ticks.linewidth = 1,frame.linewidth = 1))
  gg <- gg + coord_equal()
  gg <- gg + theme(panel.grid.minor=element_blank())
  gg <- gg + theme(panel.grid.major=element_blank())
  gg <- gg + theme(axis.text.x = element_text(angle=45,hjust=1))
  gg <- gg + theme(axis.ticks =element_blank())
  gg <- gg + theme(axis.text.x=element_text(size=10, color=text_color))
  gg <- gg + theme(axis.text.y=element_text(size=10, color=text_color))
  gg <- gg + theme(panel.border=element_blank())
  gg <- gg + theme(plot.title=element_text(hjust=0, color=text_color,face="bold"))
  gg <- gg + theme(strip.text=element_text(hjust=0, color=text_color,size=12,face="bold"))
  gg <- gg + theme(strip.background=element_rect(fill=plot_background, color=plot_background))
  gg <- gg + theme(panel.spacing.x=unit(0.5, "cm"))
  gg <- gg + theme(panel.spacing.y=unit(0.5, "cm"))
  gg <- gg + theme(legend.background=element_rect(fill=plot_background, color=plot_background)) 
  gg <- gg + theme(legend.title=element_text(size=12, color=text_color))
  gg <- gg + theme(legend.title.align=1)
  gg <- gg + theme(legend.text=element_text(size=10, color=text_color))
  gg <- gg + theme(plot.background=element_rect(fill=plot_background,color=plot_background)) 
  gg <- gg + theme(panel.border=element_rect(fill = NA, colour='black',size=1))
  gg <- gg + theme(legend.text.align=1)
  gg <- gg + labs(fill='') 
  gg <- gg + labs(x=NULL, y=NULL, title=title)
  
  if(!is.null(subtitle)){
    gg + facet_wrap(~Subject, ncol=1)
    gg
  } else {
    gg
  }
  
  
}
