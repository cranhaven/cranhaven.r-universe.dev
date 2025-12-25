#' Plot data arising from cv.multivar.
#'
#' @param x Object. An object returned by multivar_sim.
#' @param plot_type Character. User can specify "common" to plot the common effects matrix, "unique" to plot the unique effects matrix, or "total" to plot the total effects matrix.
#' @param facet_ncol Numeric. Number of columns to use in the "unique" or "total" effects plot.
#' @param datasets Numeric. A vector containing the index of datasets to plot. Default is "all".
#' @param ub Numeric. Upper bound on coefficient values for heatmap index. Default is 1.
#' @param lb Numeric. Lower bound on coefficient values for heatmap index. Default is -1.
#' @keywords var multivar simulate plot
#'
#' @examples
#'
#' sim1  <- multivar_sim(
#'   k = 2,  # individuals
#'   d = 3,  # number of variables
#'   n = 20, # number of timepoints
#'   prop_fill_com = 0.1, # proportion of paths common
#'   prop_fill_ind = 0.1, # proportion of paths unique
#'   lb = 0.1,  # lower bound on coefficient magnitude
#'   ub = 0.9,  # upper bound on coefficient magnitude
#'   sigma = diag(1,3) # noise
#' )
#' 
#' model1 <- constructModel(data = sim1$data, weightest = "ols")
#' fit1 <- cv.multivar(model1)
#' plot_results(fit1, plot_type = "common")
#'
#' @export
plot_results <- function(x, plot_type = "common", facet_ncol=3, datasets = "all",ub = 1, lb = -1){
  
  rows <- values <- NULL
  MSFE_mean    <- colMeans(x$MSFE)
  msfe_min_idx <- which.min(MSFE_mean)
  B <- x$beta[,,msfe_min_idx]

  mats <- breakup_transition(B, x$obj@Ak, x$obj@ndk, x$obj@intercept,x$obj@thresh)
    
  if(plot_type == "common"){
    
    df <- setNames(melt(mats$common), c('rows', 'vars', 'values'))
  }
  
  if(plot_type == "unique"){
    if(datasets[1] == "all"){
      df <- as.data.frame(do.call("rbind",lapply(seq_along(mats$unique), function(j){
        mat <- mats$unique[[j]]
        df <- setNames(melt(mat), c('rows', 'vars', 'values'))
        df$Subject <- paste0("Dataset 1 ", j)
        df
      })))
    } else {
      df <- as.data.frame(do.call("rbind",lapply(datasets, function(j){
        mat <- mats$unique[[j]]
        df <- setNames(melt(mat), c('rows', 'vars', 'values'))
        df$Subject <- paste0("Dataset 1 ", j)
        df
      })))
    }
  }
  
  if(plot_type == "total"){
    if(datasets[1] == "all"){
      df <- as.data.frame(do.call("rbind",lapply(seq_along(mats$total), function(j){
        mat <- mats$total[[j]]
        df <- setNames(melt(mat), c('rows', 'vars', 'values'))
        df$Subject <- paste0("Dataset ", j)
        df
      })))
    } else {
      df <- as.data.frame(do.call("rbind",lapply(datasets, function(j){
        mat <- mats$total[[j]]
        df <- setNames(melt(mat), c('rows', 'vars', 'values'))
        df$Subject <- paste0("Dataset ", j)
        df
      })))
    }
  }

  df$values[df$values == 0] <- NA
  
  max_val <- max(df$values)
  min_val <- min(df$values)


  zf_red <- rgb(255,0,90, maxColorValue=255)
  zf_green <- rgb(90, 168, 0, maxColorValue=255)
  zf_blue <- rgb(0, 152, 233, maxColorValue=255)
  zf_yellow <- rgb(242, 147, 24, maxColorValue=255)
  zf_back <- rgb(51,51,51, maxColorValue=255)
  zf_fore <- rgb(249,242,215, maxColorValue=255)
  zf_fore <- "white"
  
  text_color <- zf_back
  grid_color <- zf_back
  plot_background <- "white"
  
  colfunc_low <- colorRampPalette(c(zf_red, zf_fore))
  colfunc_high <- colorRampPalette(c(zf_fore, zf_blue))
  colors_to_use <- c(colfunc_low(6)[1:3],zf_fore,colfunc_high(6)[4:6])
  
  df$rows <- factor(df$rows,levels = rev(colnames(mats$common)))
  
  limit <- max(abs(c(lb,ub))) * c(-1, 1)
 
  gg <- ggplot(df, aes(y=rows, x=vars, fill = values)) # original
  #gg <- gg + geom_tile(color=grid_color, size=.5) 
  gg <- gg + geom_tile() 
  #gg <- gg + scale_fill_gradientn(colors=colors_to_use,limits=c(-1,1),na.value = zf_fore)
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
  
  if(plot_type == "common"){
     gg <- gg + labs(x=NULL, y=NULL, title="Common Transition Matrix")
  }
  
  if(plot_type == "unique"){
     gg <- gg + labs(x=NULL, y=NULL, title="Unique Transition Matrices")
     gg <- gg + facet_wrap(~Subject, ncol=facet_ncol)
  }
  
  if(plot_type == "total"){
     gg <- gg + labs(x=NULL, y=NULL, title="Total Transition Matrices")
     gg <- gg + facet_wrap(~Subject, ncol=facet_ncol)
  }
  
  gg

  
}
