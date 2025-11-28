#' @export

plot.regs <- function(x, regions, nrow = 1, ncol = 1, ...){
  colset_light_qual <- c("#8dd3c7",   "#fdb462", "#bebada", "#fb8072", 
                         "#80b1d3",  "#b3de69", "#ffed6f","#bc80bd",      
                         "#d9d9d9",  "#fccde5","#ccebc5", "#a1d6e2")
  palette_light_qual <- colorRampPalette(colset_light_qual)
  colset_mid <- c( "#4D648D", "#337BAE", "#97B8C2",  "#739F3D", "#ACBD78",  
                   "#F4CC70", "#EBB582",  "#BF9A77",
                   "#E38B75", "#CE5A57",  "#D24136", "#785A46" )
  colset_mid_qual <- colset_mid[c(11, 2, 4, 6,  1, 8, 10, 5, 7, 3, 9, 12)]
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  nnodes = max(x$regions$node)
  my_col = c(colset_light_qual, colset_mid_qual)
  
  par(mfrow = c(nrow, ncol), mar = c(2, 2, 2, 2), ps = 12, bg = "white", mgp = c(3, 0.2, 0))
  for(i in regions){
    print(plot(lat ~ lon, 
               data = x$regions, 
               pch = 15, 
               col = my_col[as.matrix(x$regions)[, 9 + i]]))
    maps::map("world", add = TRUE)
  }
  par(mfrow = c(1, 1))
} 




