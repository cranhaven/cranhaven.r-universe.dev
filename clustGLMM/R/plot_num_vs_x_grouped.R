plot_num_vs_x_grouped <- 
function(data, y = "y", x = "x", group, id = "i", units, xbreaks,
         add_lowess = TRUE, alpha = 0.7, legend_placement = "topright"){
  stopifnot(is.data.frame(data))
  stopifnot(is.character(y), length(y) == 1)
  stopifnot(is.character(x), length(x) == 1)
  stopifnot(is.character(id), length(id) == 1)
  stopifnot(is.logical(add_lowess), length(add_lowess) == 1, !is.na(add_lowess))
    
  if(missing(group)){
    group <- "g"
    data[,group] <- rep(0, dim(data)[1])
    if (missing(legend_placement)) legend_placement <- NULL
  } else {
    stopifnot(is.character(group), length(group) == 1)
  }
  stopifnot(all(c(y, x, group, id) %in% colnames(data)))
  
  if(missing(units)){
    units <- unique(data[,id])
  }
  
  isxfactor <- is.element("factor", class(data[,x]))
  alpha <- ifelse(isxfactor, alpha, 1)
  
  gs = sort(unique(data[data[,group] != 0,group]))
  G = length(gs)
  # color definition
  has0 <- (sum(data[,group] == 0) > 1)
  COL <- rainbow_hcl(G, c = 80, l = 70, alpha = alpha)
  dCOL <- rainbow_hcl(G, c = 80, l = 50, alpha = alpha)
  names(COL) <- names(dCOL) <- ggs <- as.character(gs)
  if(has0){
    sg3 <- apply(col2rgb("slategray3"), 2, 
                 function(curcoldata){rgb(red=curcoldata[1], 
                                          green=curcoldata[2],
                                          blue=curcoldata[3],
                                          alpha=255*alpha, 
                                          maxColorValue=255)})
    sg <- apply(col2rgb("slategray"), 2,
                function(curcoldata){rgb(red=curcoldata[1], 
                                         green=curcoldata[2],
                                         blue=curcoldata[3],
                                         alpha=255*alpha, 
                                         maxColorValue=255)})
    COL <- c(sg3, COL)
    dCOL <- c(sg, dCOL)
    #COL <- c("grey70", COL)
    #dCOL <- c("grey40", dCOL)
    names(COL) <- names(dCOL) <- c("0", gs)
    ggs <- c(0, gs)
  }
  
  if(isxfactor){
    plot(x = c(0,1), y = c(0,1), type = "n", 
         xlim = 0.5+c(0,nlevels(data[,x])),
         ylim = range(data[,y], na.rm=TRUE),
         xlab = x,
         ylab = y,
         xaxt = "n")
    axis(1, at = 1:nlevels(data[,x]), labels = levels(data[,x]))
    jitter <- seq(-0.2, 0.2, length.out = length(ggs))
    for(ig in 1:length(ggs)){
      g <- ggs[ig]
      subdata <- data[data[,group] == g, ]
      boxplot(subdata[,y] ~ factor(subdata[,x]),
              col = COL[as.character(g)],
              border = dCOL[as.character(g)],
              add = TRUE, xaxt = "n", yaxt = "n",
              at = 1:nlevels(data[,x]) + jitter[ig])
    }
  }else{
    plot(x = c(0,1), y = c(0,1), type = "n", 
         xlim = range(data[,x], na.rm=TRUE),
         ylim = range(data[,y], na.rm=TRUE),
         xlab = x,
         ylab = y)
    for(i in units){
      datai <- data[data[[id]] == i, ]
      datai <- datai[order(datai[,x]),] # order by x value
      if(dim(datai)[1] > 1){
        lines(datai[,x], datai[,y], col = COL[as.character(datai[1,group])], 
              lty = 1, lwd = 1)
      }else{
        points(datai[,x], datai[,y], col = COL[as.character(datai[1,group])], 
               pch = 16, cex = 0.8)
      }
    }
    
    # add lowess curves for each group
    if(add_lowess){
      for(g in ggs){
        gdata <- data[data[,group]==g,]
        gdata <- gdata[order(gdata[,x]),]
        lws <- try(loess.smooth(gdata[,x], gdata[,y]), TRUE)
        if(!inherits(lws, "try-error")){
          lines(lws, col = dCOL[as.character(g)], lwd = 5)
        }
      }
    }
  }
  if (!is.null(legend_placement)) {
      legend(legend_placement, names(dCOL), col = dCOL, lty=1, bty="n", lwd = 2)
  }
}
