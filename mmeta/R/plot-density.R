



densityOverlay <- function(density_df, measure_name, 
                           group_name = group_name,
                           xlim = xlim, 
                           add_vertical = add_vertical,
                           by = by){
 

  density_df[group_name] <- density_df$group
  ### create objects
  obj_create_string <- paste('ggplot2_obj <- ggplot(data=density_df, aes(x=x, y=y,group = ', 
                             group_name, ' )) + ' , sep = '')
  if(by == 'line_type'){
    geom_line_string <- paste('geom_line(aes(linetype=',group_name,'), linewidth = 1) ',sep = )
  } else {
    geom_line_string <- paste('geom_line(aes(color=',group_name,'), linewidth = 1) ',sep = )
  }
  eval( parse(text= paste(obj_create_string, geom_line_string)))
  
  
  ## additonal atrributes
  ggplot2_obj <- ggplot2_obj +
    ylab("Density") +
    xlab(measure_name) +
    theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5)) +
    theme(panel.background = element_blank()) +
    theme(legend.key = element_rect(fill = alpha("white", 0.0))) +
    theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))
  
 
 if(!is.null(xlim)){
   ggplot2_obj <- ggplot2_obj + xlim(xlim[1], xlim[2])
 }
 
 if(!is.null(add_vertical)){
   ggplot2_obj <- ggplot2_obj + geom_vline(xintercept = add_vertical )
 }
 return(ggplot2_obj)
 
}


densitySideBySide  <- function(density_df, measure_name, 
                               group_name = group_name,
                               xlim = xlim, 
                               add_vertical = add_vertical ){
  
  density_df[group_name] <- density_df$group

  ### create objects
  obj_create_string <- paste('ggplot2_obj <- ggplot(data=density_df, aes(x=x, y=y,group = ', 
                             group_name, ' )) + ' , sep = '')
  facet_warp_string <- paste('facet_wrap(~ ' ,group_name,') ',sep = )
  eval( parse(text= paste(obj_create_string, facet_warp_string)))
  ## additional attributes
    ggplot2_obj <- ggplot2_obj + 
      geom_line(linewidth = 1) +
      ylab("Density") +
      xlab(measure_name) +
      theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5)) +
      theme(panel.background = element_blank()) +
      theme(legend.key = element_rect(fill = alpha("white", 0.0))) +
      theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))
  
  ## add xlim attributes
  if(!is.null(xlim)){
    ggplot2_obj <- ggplot2_obj + xlim(xlim[1], xlim[2])
  }
  
  if(!is.null(add_vertical)){
    ggplot2_obj <- ggplot2_obj + geom_vline(xintercept = add_vertical )
  }
  return(ggplot2_obj)
  
}


