# rm(list = ls())
# 
# library(ggedit)
# library(oceanmap)
# library(plotly)
# library(RchivalTag)

# ## example 1a) line plot from several csv-files:
# csv_file <- system.file("example_files/15P1019-104659-1-GPE3.csv",package="RchivalTag")
# pos <- get_geopos(csv_file)  ## show tracks as line plot
# ggplot_geopos(pos)
# p <- ggplot_geopos(pos,type = "l")


ggplotly_geopos <- function(ggobj, fixedrange=F, grid=F,expand=10){
  p <- ggobj
  
  layer.types <- unlist(lapply(p$layers, function(x) class(x$geom)))
  if(any(grepl("Rect",layer.types))) p <- ggedit::remove_geom(p,"rect",1)
  if(any(grepl("Rect",layer.types))) p <- ggedit::remove_geom(p,"rect",1)
  if(any(grepl("GeomSf",layer.types))) p <- ggedit::remove_geom(p,"GeomSf",1)
  p2 <- p + theme(panel.grid.major= element_blank())
  f <- f0 <- plotly::ggplotly(p2,dynamicTicks=F)
  
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
    p <- oceanmap::ggplotmap(add_to = p2, lon = xlim, lat=ylim,grid.res = xdiff)
  }else{
    xlim <- f0$x$layout$xaxis$range
    xdiff <- diff(xlim)
    xlim <- c(xlim[1]-(2*xdiff),xlim[2]+(2*xdiff))
    
    ylim <- f0$x$layout$yaxis$range
    ydiff <- diff(ylim)
    ylim <- c(ylim[1]-(2*ydiff),ylim[2]+(2*ydiff))
    p <- oceanmap::ggplotmap(add_to = p2, lon = xlim, lat=ylim)
  }
  p2 <- ggedit::remove_geom(p,"rect",1)
  p2 <- ggedit::remove_geom(p2,"rect",1)
  if(!grid) p2 <- p2 + theme(panel.grid.major= element_blank())
  f <- plotly::ggplotly(p2, tooltip=c("group","text"),dynamicTicks=F)
  
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














### old code:


# p2 <- ggplotmap(lon = c(0,expand),lat=r$ylim,add_to = p2)
# p2 <- ggedit::remove_geom(p2,"rect",1)
# p2 <- ggedit::remove_geom(p2,"rect",1)
# 
# my.ggp.yrange <- ggplot_build(p2)$layout$  panel_scales_x[[1]]$y.range
# my.ggp.xrange <- ggplot_build(p2)$layout$panel_ranges[[1]]$x.range
# f
# 
# 
# 
# xtickticks_new <- xticks <- f$x$layout$xaxis$tickvals
# xticktext_new <- f$x$layout$xaxis$ticktext
# xdiff <- diff(xticks)
# ii <- which(xdiff > 0)
# if(length(ii) > 0){
#   xticks <- xticks[ii+1]
#   xdiff <- xdiff[ii[1]]
#   xticks <- xticks[which(xdiff > 0)+1]
#   xticks_new <- c(xticks[1]-(expand:1)*xdiff,xticks,xticks[length(xticks)]+(1:expand)*xdiff)
#   ie <- which(xticks_new >0 & xticks_new <=180)
#   iw <- which(xticks_new <0)
#   xticktext_new <- paste(xticks_new,"&#176;")
#   if(length(iw) > 0){
#     xticktext_new[iw] <- paste(-xticks_new[iw],"&#176; W")
#   }
#   iw2 <- which(xticks_new >180)
#   if(length(iw2) > 0){
#     xticktext_new[iw2] <- paste(360-xticks_new[iw2],"&#176; W")
#   }
#   if(length(ie) > 0){
#     xticktext_new[ie] <- paste(xticks_new[ie],"&#176; E")
#   }
# }
# 
# 
# 
# ytickticks_new <- yticks <- f$x$layout$yaxis$tickvals
# yticktext_new <- f$x$layout$yaxis$ticktext
# ydiff <- diff(yticks)
# jj <- which(ydiff > 0)
# if(length(jj) > 0){
#   yticks <- yticks[jj+1]
#   ydiff <- ydiff[jj[1]]
#   yticks_new <- c(yticks[1]-(expand:1)*ydiff,yticks,yticks[length(yticks)]+(1:expand)*ydiff)
#   iN <- which(yticks_new >0)
#   iS <- which(yticks_new <0)
#   yticktext_new <- paste(yticks_new,"&#176;")
#   if(length(iN) > 0){
#     yticktext_new[iN] <- paste(yticks_new[iN],"&#176; N")
#   }
#   if(length(iS) > 0){
#     yticktext_new[iS] <- paste(-yticks_new[iS],"&#176; S")
#   }
# }
# 
# ax <- list(
#   showline = TRUE,
#   mirror = "ticks",
#   linecolor = plotly::toRGB("black"),
#   linewidth = 2,
#   tickvals = xticks_new,
#   ticktext= xticktext_new,
#   tickmode = "array",
#   fixedrange=fixedrange
# )
# 
# ay <- list(
#   showline = TRUE,
#   mirror = "ticks",
#   linecolor = plotly::toRGB("black"),
#   linewidth = 2,
#   tickvals = yticks_new,
#   ticktext= yticktext_new,
#   tickmode = "array",
#   fixedrange=fixedrange
# )
# 
# f %>% layout( xaxis = ax, yaxis = ay)
