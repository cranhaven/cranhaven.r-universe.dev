plot_cat_vs_x_grouped <-
function(data, y = "y", x = "x", group, xbreaks, main = NA, margin = 0.05, reverse = FALSE){
  stopifnot(is.data.frame(data))
  stopifnot(is.character(y), length(y) == 1)
  stopifnot(is.character(x), length(x) == 1)
  stopifnot(is.logical(reverse), length(reverse) == 1, !is.na(reverse))
    
  YLAB <- "Group"
  if(missing(group)){
    group <- "g"
    data[,group] <- rep(0, dim(data)[1])
    YLAB <- ""
  } else {
    stopifnot(is.character(group), length(group) == 1)
  }
  stopifnot(all(c(y, x, group) %in% colnames(data)))
  
  gs = sort(unique(data[data[,group] != 0,group]))
  G = length(gs)
  # color definition
  ncat <- nlevels(factor(data[,y]))
  is0 <- sum(data[,group] == 0) > 0
  G0 <- ifelse(is0, length(gs) + 1, length(gs))
  mCOL <- matrix(0, ncol = G0, nrow = ncat)
  seql <- seq(40, 80, length.out = ncat)
  if(is0){
    labels <- c(0,gs)
    # mCOL[,1] <- grey.colors(ncat, start = 0.25, end = 0.75)
    mCOL[,1] <- slategrey.colors(ncat, start = 0.0, end = 0.9)
    if(G > 0){
      for(l in 1:ncat){
        mCOL[l,2:(G+1)] <- rainbow_hcl(G, c = 80, l = seql[l])
      }
    }
  }else{
    labels <- gs  
    for(l in 1:ncat){
      mCOL[l,] <- rainbow_hcl(G, c = 80, l = seql[l])
    }
  }

  if(is(data[, x], "factor")) {
    fx <- factor(data[,x])
    xbreaks <- 0:nlevels(fx) + 0.5
    plot(0, G0, type = "n",
         xlim = range(data[,x]), ylim = c(0-G0*0.03,G0), 
         xlab = x, ylab = YLAB,
         main = main,
         xaxt = "n", yaxt = "n")
    axis(1, at = 1:nlevels(fx))
  }else{
    plot(0, G0, type = "n",
         xlim = range(data[,x]), ylim = c(0-G0*0.03,G0), 
         xlab = x, ylab = YLAB,
         main = main,
         xaxt = ifelse(missing(xbreaks),"s","n"), yaxt = "n")
    if(missing(xbreaks)){
      xbreaks <- seq(from = min(data[,x], na.rm = TRUE), 
                     to = max(data[,x], na.rm = TRUE), 
                     length.out = 11)
    }else{
      axis(1, at = xbreaks)
    }
    fx <- cut(data[,x], breaks = xbreaks)
  }
  if (YLAB != "") {
      axis(2, 0:(G0-1)+0.5, labels = labels, las = 2)
  }
  for(g in 1:G0){
    if(is0){
      gg <- ifelse(g==1, 0, gs[g-1])
    }else{
      gg <- gs[g]
    }
    TAB <- table(fx[data[,group]==gg], data[data[,group]==gg,y])
    cumTAB <- matrix(0, nrow = nlevels(fx), ncol = dim(TAB)[2]+1)
    for(j in 1:nlevels(fx)){
      if(reverse){
        cumTAB[j,] <- c(0, cumsum(rev(TAB[j,])/sum(TAB[j,])))
      }else{
        cumTAB[j,] <- c(0, cumsum(TAB[j,]/sum(TAB[j,])))
      }
    }
    # now scale it into interval [g-1+margin, g-margin] of length 1-2*margin
    cumTAB <- g - 1 + margin + cumTAB*(1-2*margin)
    for(j in 1:nlevels(fx)){
      rect(xleft = xbreaks[j], xright = xbreaks[j+1],
           ybottom = cumTAB[j,1:(dim(cumTAB)[2]-1)], ytop = cumTAB[j,2:dim(cumTAB)[2]],
           col = mCOL[,g])
    }
  }
  
  if(reverse){
    legend("bottom", legend = rev(0:(ncat-1)), pt.bg = mCOL[,1], pch = 22, ncol = ncat, bty = "n")
  }else{
    legend("bottom", legend = 0:(ncat-1), pt.bg = mCOL[,1], pch = 22, ncol = ncat, bty = "n")
  }
  
}
