# rm(list = ls())
# 
# library(ggedit)
# library(oceanmap)
# library(plotly)
# 
# r <- regions("lion")
# ggobj <- ggplotmap(lon = r$xlim,lat=r$ylim,grid.res = 8)


ggplotmaply <- function(ggobj, fixedrange=F, grid=F,expand=3){
  p <- ggobj
  
  layer.types <- unlist(lapply(p$layers, function(x) class(x$geom)))
  if(any(grepl("Rect",layer.types))) p <- ggedit::remove_geom(p,"rect",1)
  if(any(grepl("Rect",layer.types))) p <- ggedit::remove_geom(p,"rect",1)
  if(any(grepl("GeomSf",layer.types))) p <- ggedit::remove_geom(p,"GeomSf",1)
  p2 <- p + theme(panel.grid.major= element_blank())
  f <- f0 <- plotly::ggplotly(p2)
  
  xticks <- f$x$layout$xaxis$tickvals
  xdiff <- diff(xticks)
  ii <- which(xdiff > 0)
  if(length(ii) > 0){
    # xticks <- xticks[ii+1]
    xdiff <- xdiff[ii[1]]
    xticks <- xticks[which(xdiff > 0)+1]
    xlim <- c(xticks[1]-(expand*xdiff),xticks[1]+(expand*xdiff))
    
    ytickticks_new <- yticks <- f$x$layout$yaxis$tickvals
    ydiff <- diff(yticks)
    jj <- which(ydiff > 0)
    if(length(jj) > 0){
      yticks <- yticks[jj+1]
      ydiff <- ydiff[jj[1]]
      ylim <- c(yticks[1]-(expand*ydiff),yticks[1]+(expand*ydiff))
    }
    p <- ggplotmap(add_to = p2, lon = xlim, lat=ylim,grid.res = xdiff)
  }else{
    xlim <- f0$x$layout$xaxis$range
    xdiff <- diff(xlim)
    xlim <- c(xlim[1]-(2*xdiff),xlim[2]+(2*xdiff))
    
    ylim <- f0$x$layout$yaxis$range
    ydiff <- diff(ylim)
    ylim <- c(ylim[1]-(2*ydiff),ylim[2]+(2*ydiff))
    p <- ggplotmap(add_to = p2, lon = xlim, lat=ylim)
  }
  p2 <- ggedit::remove_geom(p,"rect",1)
  p2 <- ggedit::remove_geom(p2,"rect",1)
  if(!grid) p2 <- p2 + theme(panel.grid.major= element_blank())
  f <- plotly::ggplotly(p2)
  
  ax <- list(
    showline = TRUE,
    mirror = "ticks",
    linecolor = plotly::toRGB("black"),
    linewidth = 2,
    tickmode = "array",
    range = f0$x$layout$xaxis$range,
    fixedrange=fixedrange
  )
  
  ay <- list(
    showline = TRUE,
    mirror = "ticks",
    linecolor = plotly::toRGB("black"),
    linewidth = 2,
    tickmode = "array",
    range = f0$x$layout$yaxis$range,
    fixedrange=fixedrange
  )
  
  f <- plotly::layout(f, xaxis = ax, yaxis = ay)
  return(f)
}

