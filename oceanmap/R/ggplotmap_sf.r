ggplotmap <- function(region=v_area, lon=xlim, lat=ylim, add_to, asp, 
                         grid=T, grid.res, resolution=0, 
                         main, axes=T, axeslabels=axes, ticklabels=T,#, cex.lab=0.8, cex.ticks=0.8, las=1, add=F,
                         fill.land=T, col.land="grey", col.bg=NA, border='black', col.scale="black",bwd=1.5, v_area, xlim, ylim){
  worldHiresMapEnv <- NULL
  if(!missing(xlim) & !missing(ylim))  {
    ext <- extent(c(xlim,ylim))
    lon <- ext[1:2]; lat <- ext[3:4]
  }
  
  show.plot <- T
  if(is.na(col.land) & is.na(fill.land)){
    fill.land <- F
    show.plot <- F
    bwd <- 0
  }
  
  if(!missing(region)){ #' if region information is given
    if(is(region, 'character')) {
      r <- regions(region) # get regions defintions/2 (extent and name)
      #       center <- r$center ## no longer needed!
      if(missing(grid.res)) grid.res <- r$grid.res[1]
    }else{
      if(grepl('Raster', class(region)) | grepl('Extent', class(region))){
        extent.vector <- as.vector(t(sp::bbox(extent(region))))
        r <- data.frame(xlim=extent.vector[1:2],ylim=extent.vector[3:4])
      }else{
        if(missing(lon) | missing(lat)) stop('error in plotmap: provide region information as name or lon, lat data')
      }
    }
  }else{
    if(missing(lat) | missing(lon)){
      if(!missing(add_to)){
      lon <- ggplot_build(add_to)$layout$panel_scales_x[[1]]$range$range
      lat <- ggplot_build(add_to)$layout$panel_scales_y[[1]]$range$range
      }
    }
  }
  if(!missing(lon) & !missing(lat))  r <- data.frame(xlim=lon,ylim=range(lat))
  
  if(missing(grid.res)) grid.res <- .get.grid.res(r)
  
  if(!fill.land) col.land <- NA
  
  ## old_old:
  #   if(center == 'W'){
  #     if(any(r$xlim < 0)){
  #       r$xlim <- range(r$xlim)
  #       r$xlim <- r$xlim[2:1]
  #       r$xlim[2] <- 180+180+r$xlim[2]
  #     }
  #   }  
  
  # new:
  # if(any(r$xlim < 0)){
  #   r$xlim <- r$xlim+360
  # }
  
  ## old:
  if(r$xlim[1] > r$xlim[2]){
    #     r$xlim <- range(180+180+r$xlim)
    #     r$xlim[r$xlim > 360] <- 360
    if(r$xlim[2] < 0 & r$xlim[1] == abs(r$xlim[2])) r$xlim[2] <- r$xlim[1]+360
    
    #     if(r$xlim[1] == 180 & r$xlim[2] == -180) r$xlim[2] <- 180+360
    
    if(any(r$xlim < 0)){
      r$xlim <- range(r$xlim)
      r$xlim <- r$xlim[2:1]
      r$xlim[2] <- 180+180+r$xlim[2]
    }
  }  
  if(any(r$xlim > 540)) r$xlim <- r$xlim -360
  
  ### calculate default projection from map-package
  xrange <- r$xlim; yrange <- r$ylim; 
  aspect <- c(cos((mean(yrange) * pi)/180), 1)
  #     plot.window(xrange, yrange, asp = 1/aspect[1])  
  if(missing(asp)) asp <- 1/aspect[1]
  ###
  
  if(any(r$xlim <= -180)) r$xlim <- r$xlim+360
  
  # if(fill.land){
    if(any(r$xlim > 180)) {
      #       data("worldmap", envir=environment())
      worldmap <- .get.worldmap(resolution)
      # worldmap <- 'worldHires'
      # m <- map_data(worldmap, xlim = r$xlim, ylim = r$ylim)
      # m <- maps::map(worldmap, xlim = r$xlim, ylim = r$ylim, resolution=resolution,plot = F)
      # m <- try(maps::map(worldmap, fill=fill.land, col=col.land,xlim=r$xlim,ylim=r$ylim,add=T,resolution=resolution,border=border))
    }else{
      data('worldHiresMapEnv',envir=environment(),package="mapdata")
      worldmap <- 'worldHires'
      
      # m <- map_data(worldmap, xlim = r$xlim, ylim = r$ylim, resolution=resolution)
      # try(maps::map(database='worldHires', fill=fill.land, col=col.land,xlim=r$xlim,ylim=r$ylim,add=T,resolution=resolution,border=border))
    }
    m <- maps::map(worldmap, xlim = r$xlim, ylim = r$ylim, plot = F, exact = F, fill = TRUE)
    wrld2 = sf::st_as_sf(m)
    # m <- .fortify.map(m)
    #### old code
    #   if(any(r$xlim > 180)) {
    #     #     data(world2HiresMapEnv, envir = environment())
    #     #     worlddb <- 'world2Hires'
    #     m1 <- maps::map('worldHires', xlim=c(100, 180),plot=F)
    #     m2 <- maps::map('worldHires', xlim=c(-180, -60),plot=F)
    #     m2$names <- m2$names[which(m2$names %in% c("USA:Alaska:Mitkof Island"))]
    #     maps::map('world2Hires', c(m1$names, m2$names), fill=fill.land, col=col.land,xlim=r$xlim,ylim=r$ylim,add=T,resolution=resolution,border=border)
    #   }else{
    # worlddb <- 'worldHires'
    #     maps::map(database=worlddb, fill=fill.land, col=col.land,xlim=r$xlim,ylim=r$ylim,add=T,resolution=resolution,border=border)
    #   }
  # }
    
    
  
  
  
  if(axeslabels){
    xlab <- "Longitude"
    ylab <- "Latitude"
  }else{
    xlab <- ""
    ylab <- ""
  }
  # wrld2 = st_as_sf(map('world2', plot=F, fill=T,resolution=0))
  if(missing(add_to)) add_to <- ggplot()
  a <- add_to
  if(fill.land){
    a <- a + 
      geom_sf(data=wrld2, fill=col.land,colour=border) +
      geom_polygon(bg=col.land,colour=border)
  }else{
    a <- a + geom_sf(data=wrld2, fill=NA,colour=NA)
  }
  
  # if(ticklabels){
    a <- a + coord_sf(xlim=r$xlim, ylim=r$ylim,expand = F)
  # }
    # else{
  #   a <- a + coord_sf(xlim=r$xlim, ylim=r$ylim,expand = F, datum=NA)
  # }
  
  r$xlim <- xrange <- a$coordinates$limits$x
  
  # save(a,file="~/Desktop/test.rd")
  # load("~/Desktop/test.rd",verbose=T)
  # coord_quickmap(xlim = r$xlim, ylim = r$ylim, expand = FALSE) 
  
  aspect <- c(cos((mean(yrange) * pi)/180), 1)
  #     plot.window(xrange, yrange, asp = 1/aspect[1])  
  if(missing(asp)) asp <- 1/aspect[1]
  
  fx <- fy <- f <- bwd*min(diff(xrange), diff(yrange))/100
  fy <- fy/asp
  
  
  ##" move borders a bit to avoid problems superimposing grids
  # xrange <- c(xrange[1],xrange[2]) 
  # yrange <- c(yrange[1],yrange[2])
  sf = grid.res/2
  xmin <- ceiling(xrange[1]/sf)*sf # round up
  xmax <- floor(xrange[2]/sf)*sf
  ymin <- ceiling(yrange[1]/sf)*sf
  ymax <- floor(yrange[2]/sf)*sf
  color <- c("white","black")
  x1 <- c(xrange[1],seq(xmin,xmax,sf))
  x2 <- c(seq(xmin,xmax,sf),xrange[2])
  
  rects.long <- data.frame(x_start = x1, x_end = x2, y_start = yrange[1], y_end = yrange[1]+fy)
  rects.long <- rbind(rects.long, data.frame(x_start = x1, x_end = x2, y_start = yrange[2], y_end = yrange[2]-fy))
  
  rects.long$color <- if(.is.even(nrow(rects.long)/2)) { #even/odd test
    rep(c("black", "white"), nrow(rects.long)/2) #pattern for even
  } else {
    rep(c(rep(c("black", "white"), (nrow(rects.long) - 2)/4),"black"), 2)} #odd
  
  
  y1 <- c(yrange[1],seq(ymin,ymax,sf))
  y2 <- c(seq(ymin,ymax,sf),yrange[2])
  
  rects.lat <- data.frame(x_start = xrange[1], x_end = xrange[1]+fx, y_start = y1, y_end = y2)
  rects.lat <- rbind(rects.lat, data.frame(x_start = xrange[2]-fx, x_end = xrange[2], y_start = y1, y_end = y2))
  
  rects.lat$color <- if(.is.even(nrow(rects.lat)/2)) { #even/odd test
    rep(c("black", "white"), nrow(rects.lat)/2) #pattern for even
  } else {
    rep(c(rep(c("black", "white"), (nrow(rects.lat) - 2)/4),"black"), 2)} #odd
  
  #combine latitude and longitude grid
  rects.grid <- rbind(rects.lat, rects.long)
  
  #split into black dataframe and white dataframe
  rects.black <- rects.grid[rects.grid$color == "black",]
  rects.white <- rects.grid[rects.grid$color == "white",]
  
  #define axis breaks to match grid
  # axis.breaks.x <- seq(min.long, max.long, interval.long)
  # axis.breaks.y <- seq(min.lat, max.lat, interval.lat)
  if(missing(main)) main <- ""
  
  a <- a +
    #use geom_rect() to add border grid
    geom_rect(data = rects.white, inherit.aes = FALSE, #white grid rectangles
              aes_(xmin = ~ x_start, xmax = ~ x_end, ymin = ~ y_start, ymax = ~ y_end), 
              color = col.scale, fill = "white") +
    geom_rect(data = rects.black, inherit.aes = FALSE, #black grid rectangles
              aes_(xmin = ~ x_start, xmax = ~ x_end, ymin = ~ y_start, ymax = ~ y_end),
              color = col.scale, fill = col.scale)  + 
    labs(title=main, y=ylab, x = xlab)
  # + theme_minimal() #theme edits to make plot look like a map
  # theme(axis.title = element_blank(),
  # legend.position = "none")
  
  x <- c(ceiling(r$xlim[1]/grid.res)*grid.res, floor(r$xlim[2]/grid.res)*grid.res)
  y <- floor(r$ylim/grid.res)*grid.res
  
  xlabels <- seq(x[1],x[2],grid.res)
  xlabels <- xlabels[xlabels >= r$xlim[1] & xlabels <= r$xlim[2]]
  ylabels <- seq(y[1],y[2],grid.res)
  ylabels <- ylabels[ylabels >= r$ylim[1] & ylabels <= r$ylim[2]]
  at.ylabels <- ylabels
  
  # EW <- rep(" E",length(xlabels))
  # EW[xlabels < 0 | xlabels > 180] <- " W"
  at.xlabels <- xlabels
  xlabels[xlabels > 180] <- xlabels[xlabels > 180] - 360
  # NS <- rep(" N",length(ylabels))
  # NS[ylabels < 0] <- " S"
  if (length(ticklabels) == 1) ticklabels <- rep(ticklabels,2)

  
  xticklabels <- unlist(lapply(at.xlabels, function(x) ifelse(x < 0, paste(abs(x),"\u00B0E"), ifelse(x > 0, paste(x,"\u00B0W"),x))))
  yticklabels <- unlist(lapply(at.ylabels, function(x) ifelse(x < 0, paste(abs(x),"\u00B0S"), ifelse(x > 0, paste(x,"\u00B0N"),x))))
  
  # xticklabels <- unlist(lapply(at.xlabels, function(x) ifelse(x < 0, parse(text=paste0(abs(x),"^o", "*E")), ifelse(x > 0, parse(text=paste0(x,"^o", "*W")),x))))
  # yticklabels <- unlist(lapply(at.ylabels, function(x) ifelse(x < 0, parse(text=paste0(abs(x),"^o", "*S")), ifelse(x > 0, parse(text=paste0(x,"^o", "*N")),x))))
  
  
  # print(at.ylabels)
  # print(yticklabels)
  # print(replace(at.ylabels,values = yticklabels))
  if(!all(c(0,360) %in% at.xlabels)){
    if(ticklabels[1]) {
      a <- a + scale_x_continuous(breaks = at.xlabels, expand = c(0, 0))
    }else{
      a <- a + scale_x_continuous(breaks = at.xlabels,labels = replace(at.xlabels,values = ""), expand = c(0, 0))
    }
    # print((at.ylabels))
    # print((yticklabels))
    if(ticklabels[2]) {
      a <- a + scale_y_continuous(breaks = at.ylabels, expand = c(0, 0))
    }else{
      a <- a + scale_y_continuous(breaks = at.ylabels,labels = replace(at.ylabels,values = ""), expand = c(0, 0))
    }
  }
  
  # if(!ticklabels[1]) xlabels <- replace(xlabels,values = "")
  # if(!ticklabels[2]) ylabels <- replace(ylabels,values = "")
  # 
  # if(!all(c(0,360) %in% at.xlabels)){
  #   # if(ticklabels) {
  #   a <- a + scale_x_continuous(breaks = at.xlabels,labels = xlabels) +
  #     scale_y_continuous(breaks = at.yabels,labels = ylabels)
  #   # }
  # }
  # 
  
  # if(ticklabels) a <- a + scale_x_continuous(breaks = xlabels) +
  # scale_y_continuous(breaks = ylabels)
  
  # 
  a <- a + theme(panel.grid.minor = element_blank(), 
                 panel.background = element_blank(), 
                 panel.ontop = T)
  if(grid) {
    a <- a + theme(panel.grid.major = element_line(colour = "black", linetype = "dotted"))
  }else{
    a <- a + theme(panel.grid.major = element_blank())
  }
  if(!is.na(col.bg)) a <- a + theme(panel.background = element_rect(fill=col.bg, colour=col.bg), 
                                    panel.ontop = F)
  return(a)
}

# 
# .fortify.map <- function (model) 
# {
#   df <- data.frame(list(long = model$x, lat = model$y, 
#                             group = cumsum(is.na(model$x) & is.na(model$y)) + 1, 
#                             order = seq_along(model$x)), n = length(model$x))
#   names <- do.call("rbind", lapply(strsplit(model$names, "[:,]"), 
#                                    "[", 1:2))
#   df$region <- df$subregion <- "NA"
#   df$region[1:length(names)] <- names[df$group[1:length(names)], 1]
#   df$subregion[1:length(names)] <- names[df$group[1:length(names)], 2]
#   df[stats::complete.cases(df$lat, df$long), ]
# }


.is.even <- function(x) x %% 2 == 0
