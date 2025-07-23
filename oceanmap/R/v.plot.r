.v.plot <- function(b, minv, maxv, zlim, adaptive.vals, replace.na, main, cb.title, cb.xlab, pal, cb.ticks.srt, nticks,
                    sidelabels, Ylab, axeslabels, ticklabels, show.colorbar, cex.lab, cex.ticks, cex.cb.title, cex.cb.xlab, cb.xlab.line, cex.cb.ticks,
                    subplot, xpos, Save, plotfolder, plotname, fileformat, 
                    param, param_def, file_def, r, outfile.name,
                    v_area,v_image,v_contour,levels, contour.labels, v_arrows, scale_arrow, suffix,
                    fill.land, col.land, col.bg, border, grid, grid.res, bwd,las, asp, verbose,...){ # further arguments passed to plotmap
  
  #   cat('\nrunning .v.plot')
  ## set folder to plot in
  if(!missing(zlim)){
    minv <- zlim[1] 
    maxv <- zlim[2]
    adaptive.vals <- F
  }
  cmap <- cmap_topo <- NULL
  rm(cmap);   rm(cmap_topo)
  data("cmap", envir=environment())
  
  if(missing(pal)) pal <- as.character(param_def$pal1)
  #   if(is.na(pal) | !(pal %in% names(cmap))){
  # warning(paste('"pal" not defined, "jet" selected! available color maps:\n',paste(names(cmap),collapse='\n')))
  # }
  if(is.na(pal[1])){
    pal <- "jet"
    warning(paste('"pal" not defined, "jet" selected from available color maps:\n',paste(names(cmap),collapse='\n')))    
  }
  if(!(pal[1] %in% names(cmap))){
    warning(paste('presonalized color palette ("pal") selected! Please check also available color maps:\n',paste(names(cmap),collapse='\n')))
    cmap.data <- pal
  }else{
    cmap.data <- cmap[[pal]]
  }
  
  
  ## set file suffix
  
  if(missing(suffix)) suffix <- ''#paste0(".",pal)
  
  #   cat('\nrunning.v.plot')
  if(replace.na) b[!is.finite(b)] <- min(b[],na.rm=T)
  ext <- t(sp::bbox(raster::extent(b))) #reset area extent
  r$xlim <- ext[,1]
  r$ylim <- ext[,2]
  if(!subplot)
  {
    if(Save){
      if(!missing(plotname)) outfile.name <- plotname
      if(!grepl(fileformat, outfile.name)) outfile.name <- paste0(outfile.name,suffix,".",fileformat)
      if(missing(plotfolder)) plotfolder <- "."
      plotfolder <- .check.folder(plotfolder)
      
      outfile <- paste0(plotfolder,"",outfile.name)    
      
      if(fileformat == "eps" | fileformat == "ps"){ 
        setEPS()
        postscript(file=outfile,width=r$figdim[1],height=r$figdim[2])
      }else{
        png(filename=outfile,width=r$figdim[1],height=r$figdim[2],units ="in",res=300)
      }
    }else{
      dev.new(width=r$figdim[1],height=r$figdim[2],xpos=xpos)#,width=14.273574)#11.293835  5.649644
    }
    rescale.plot <- any(c(!missing(main),sidelabels,axeslabels))
    
    grads <- c(abs(diff(r$xlim)),abs(diff(r$ylim)))
    grads <- grads/min(grads)
    
    par(mar=c(2+2*rescale.plot, 2+2*rescale.plot, 2, 2)) # y1,x1,y2,x2
    #     par(mar=c(0+2*rescale.plot, 2, 0+4*rescale.plot, 2-2*rescale.plot)) # y1,x1,y2,x2
    
    cb.r <- r$cbx[2] > r$xlim[2]
    cb.b <- r$cby[1] < r$ylim[1]
    
    if(any(c(cb.r,cb.b))){
      
      par(mar=c(cb.b*8*grads[2], 2*grads[1], 0.5*grads[2], cb.r*6*grads[1])) # y1,x1,y2,x2
    }
  }
  
  ### apply default projection from map-package
  xrange <- r$xlim; yrange <- r$ylim; myborder <- 0.01
  aspect <- c(cos((mean(yrange) * pi)/180), 1)
  #     plot.window(xrange, yrange, asp = 1/aspect[1])
  if(missing(asp)) asp <- 1/aspect[1]
  ###
  
  
  if(any(param %in% c('uz','vz', 'wu', 'wv'))){
    path1 <- getwd()
    if(grepl('w',param)) {
      param2 <- ifelse(param == 'wu', 'wv', 'wu')
      u <- 'wu'
      v <- 'wv'
    }else{
      param2 <- ifelse(param == 'uz', 'vz', 'uz')
      u <- 'uz'
      v <- 'vz'
    }
    b[] <- b[]*scale_arrow
    values <- c(setNames(list(b),param)) # assign values of first file
    file_def$parameter <- param2
    path_parts <- unlist(strsplit(path1,param))
    path2 <- paste(path_parts[1], param2,path_parts[2],sep="")
    file2 <- name_join(file_def)
    folder <- path2
    folder <- paste0(folder, "/"); folder <- gsub('//','/',folder)
    sstring <- file2
    if(!file.exists(file2)) sstring <- paste0(folder,file2)
    b2 <- readbin(sstring)
    b2[] <- b2[]*scale_arrow
    values <- c(values,setNames(list(b2),param2))
    
    x <- getValues(init(values[[u]], v='x'),format='matrix')
    y <- getValues(init(values[[v]], v='y'),format='matrix')
    x2 <- x[seq(4,dim(values[[u]])[1],8),seq(4,dim(values[[u]])[2],8)]
    y2 <- y[seq(4,dim(values[[v]])[1],8),seq(4,dim(values[[v]])[2],8)]
    
    uz <- getValues(values[[u]],format='matrix')
    vz <- getValues(values[[v]],format='matrix')
    uz2 <- uz[seq(4,dim(values[[u]])[1],8),seq(4,dim(values[[u]])[2],8)]
    vz2 <- vz[seq(4,dim(values[[v]])[1],8),seq(4,dim(values[[v]])[2],8)]
    
    # calculate speed
    speed <- sqrt(uz2[]^2+vz2[]^2)
    speed <- t(as.matrix(speed)[dim(speed)[1]:1,])
    b <- raster(t(speed)[ncol(speed):1,])
    raster::extent(b) <- c(range(x2),range(y2))
    
    # set default plot option for current fields
    if(missing(v_arrows)) v_arrows <- T
    if(missing(v_image)) v_image <- F
    if(v_image) {
      param <- "speed"
      minv <- range(speed[speed > 0],na.rm=T)[1]
    }else {
      plotmap(v_area,fill.land=fill.land,col.land=col.land,col.bg=col.bg,border=border,grid=grid,grid.res=grid.res,
              axeslabels=axeslabels,ticklabels=ticklabels,cex.lab=cex.lab,cex.ticks=cex.ticks,las=las,bwd=bwd) # plot landmask, grid and scale border
      # question if arrows or contours should be plotted follows later on
    }
  }
  bcont <- b # assign values for contour plot
  
  if(!(any(param %in% c('uz','vz', 'wu', 'wv')))){
    #       if (param != 'uz' & param != 'vz'){
    
    #         cmap <- rgb(as.matrix(read.table(cmap_file,header=F,sep=',')/255)) # load colormap
    #         cmap.data <- cmap
    if(missing(zlim)) {
      zlim <- switch((param == "bathy2")+1, c(param_def$minv,param_def$maxv), range(b[], na.rm = T)) # set default colorbar values
      
      
      if(adaptive.vals & missing(minv) & missing(maxv)){
        minv <- min(b[],na.rm=T)
        maxv <- max(b[],na.rm=T)
      }
      if(missing(minv)){
        minv <- min(b[],na.rm=T)
      }
      if(missing(maxv)){
        maxv <- max(b[],na.rm=T)
      }
      
      # replace colorbar values if specified or from file
      if(!missing(minv)) {
        if(!is.numeric(minv)){ zlim[1] <- trunc(min(b[],na.rm=T))
        }else{zlim[1] <- minv}      
      }
      
      if(!missing(maxv)) {
        if(!is.numeric(maxv)){ zlim[2] <- ceiling(max(b[],na.rm=T))
        }else{zlim[2] <- maxv}
      }
    }
    if(any(is.na(zlim))) stop("no valid zlim values, please revise minv/maxv or adaptive.vals!")
    
    # cust.colorbar scale
    #     if(!missing(nticks)){
    #       ticks <- labels <- unique(c(seq(zlim[1],zlim[2],by=nticks),zlim[2]))
    #       ticks.xpos <- seq(r$cbx[1],r$cbx[2],length=length(ticks)) # set tick positions
    #     }else{
    if(param_def$log == 1) # logarithmic scale
    {
      ticks <- unique(c(seq(0.00001,0.0001,length=10),seq(0.0001,0.001,length=10),seq(0.001,0.01,length=10),seq(0.01,0.1,length=10),seq(0.1,1,length=10),1:10,seq(10,100,length=10)))
      zlim <- log10(zlim)
      labels <- apply(as.matrix(ticks), 1, FUN=function(x){formatC(x,format="f", digits=ifelse(log10(x) >= 0 ,0,ceiling(abs(log10(x)))))})       
      labels[!grepl("1", as.character(ticks))] <- ""
      ticks <- log10(ticks)
      labels <- labels[ticks >= zlim[1] & ticks <= zlim[2]]  # select ticks to plot
      ticks <- ticks[ticks >= zlim[1] & ticks <= zlim[2]]  # select ticks to plot
      ticks.xpos <- r$cbx[1]+(r$cbx[2]-r$cbx[1])*(ticks+(0-ticks[1]))/(abs(zlim[2])+abs(ticks[1])) # set tick positions
      #ticks.xpos <- r$cbx[1]+(r$cbx[2]-r$cbx[1])*(ticks+2)/(abs(zlim[2])+abs(ticks[1])) # set tick positions
      b <- log10(b)
      
    }else{ # linear axis      
      rf <- 10^digits(zlim[2])
      ticks <- ((zlim[1]*rf):(zlim[2]*rf))/rf
      labels <- ticks
      labels[(ticks*rf)%%c(5,10)[1+(length(ticks) > 51)] != 0] <- ""
      ticks <- ticks[ticks <= zlim[2]]  # select ticks to plot
      if(zlim[2] %% 10 == 0){
        if(zlim[2] <= 100){
          if(zlim[2] < 10) {
          }
          if(zlim[2] == 50) labels <- ticks <- seq(0,50,10)
          if(zlim[2] == 100) labels <- ticks <- seq(0,100,25)
        }else{
          if(zlim[2] > 100 & zlim[2] <= 500) fac <- 50
          if(zlim[2] > 500 & zlim[2] <= 1000) fac <- 100
          if(zlim[2] > 1000 & zlim[2] <= 2500) fac <- 250
          if(zlim[2] > 2500) fac <- 500
          if(diff(zlim) > 2500) fac <- 1000
          
          labels <- ticks <- seq(round(zlim[1]/fac)*fac,zlim[2],fac)
        }
      }else{
        labels <- ticks <- pretty(zlim,n=nticks)
      }
      ticks.xpos <- seq(r$cbx[1],r$cbx[2],length=length(ticks)) # set tick positions
    }
    zlim <- range(ticks)
    
    #     }
    
    b[b < zlim[1]] <- zlim[1]
    b[b > zlim[2]] <- zlim[2]
    
    # image.plot(b, zlim=zlim, axis.args=list(at=ticks,labels=labels),col=cmap.data)
    # usr <- par("usr")
    # rect(usr[2], usr[3], 100,usr[4], col="white", border="white",xpd=T)
    # rect(usr[1], usr[3], usr[2], usr[4], col=rgb(cbind(80,80,80)/255), border="black")
    # image.plot(b, zlim=zlim, axis.args=list(at=ticks,labels=labels),col=cmap.data,add=T)
    # 
    # plot( 1:10, (1:10)*10, type="n", bty="n")
    # colorbar.plot( 2, 75, 1:256, horizontal=T, col=cmap.data,axis.args=list(at=ticks,labels=labels))
    
    # create black background (for NaN Values), only needed for non arrows
    if(!v_image) {
      b <- matrix(NA,nrow(b),ncol(b))
      zlim <- c(0,1)
    }
    raster::image(b, xlim=r$xlim,ylim=r$ylim,xlab="", ylab="",axes=F,asp=asp,zlim=zlim)
    
    if(v_image){
      if(missing(col.bg)) col.bg <- rgb(cbind(80,80,80)/255)
      par(bg=col.bg)
      usr <- par("usr")
    }
    
    if(file_def$source == 'dekkar'){
      data(sysdata, envir=environment()) # load medm9_proj projection file
      lon <- unlist(medm9_proj[['lon']])
      lat <- unlist(medm9_proj[['lat']])
      
      b <- t(as.matrix(b)[dim(b)[1]:1,])
      image.plot(lon,lat,b, xlab="", ylab="",xlim=r$xlim,ylim=r$ylim,asp=asp,col=cmap.data,zlim=zlim,add=T,legend.mar=1)
    }else{
      if(param == "bathy" & any(zlim < 0)) {
        
        data("cmap_topo", envir=environment())
        #         print("sdsds")
        jj <- which(cmap_topo$elev >= zlim[1] & cmap_topo$elev <= zlim[2])
        cmap.data <- cmap_topo$col[jj]
        pal <- cmap_topo[jj,]
        fill.land <- FALSE
        #         if(missing(border)) border <- "black" # does not work if fill.land is set F!
        if(missing(cb.xlab)) cb.xlab <- "Elevation (m)"
      }
      raster::image(b, xlab="", ylab="",xlim=r$xlim,ylim=r$ylim,asp=asp,col=cmap.data,zlim=zlim,add=T)
    }
  }
  if(!missing(levels)) v_contour <- T
  if(v_contour) {
    grid <- F
    if(missing(levels)){
      raster::contour(bcont,add=T,labels=contour.labels,lty="dashed")
    }else{
      raster::contour(bcont,add=T,levels=levels,labels=contour.labels,lty=2:(length(levels)+1))
    }
  }
  if(v_arrows){
    zlim_speed <- range(speed[is.finite(speed)])
    scale_arrows <- 2.5*zlim_speed[2]/0.4 # needed scale adaption for other regions?
    arrows(x2,y2,x2+scale_arrows*uz2,y2+scale_arrows*vz2, length = 0.05) # plot currents
  }
  
  plotmap(lon=r$xlim,lat=r$ylim,add=T,fill.land=fill.land,col.land=col.land,col.bg=col.bg,border=border,grid=grid,grid.res=grid.res,
          axeslabels=axeslabels,ticklabels=ticklabels,cex.lab=cex.lab,cex.ticks=cex.ticks,las=las,bwd=bwd) # plot landmask, grid and scale border
  
  if(missing(cb.xlab)){
    param <- param_def$name1
    u <- as.character(param_def$unit)#
    u <- gsub('%',"'%'",u) ### replace percentage sign if needed
    u
    unit.empty <- '~bgroup("(",unit,")")'
    units <- gsub('unit',u,unit.empty)
    cb.xlab <- parse(text=paste("'",param,"'",units,sep=""))    
    #     cb.xlab <- parse(text=paste("'",param_def$name1,"'",'~',param_def$unit,sep="")) #### old
  }
  if(missing(cb.title)) cb.title <- ""
  #   if(missing(cb.title) & !is.na(file_def$date1)  & !is.na(file_def$date2)) cb.title <- bindate2Title(file_def$timestep,file_def$date1,file_def$date2)
  if(v_image){
    # plot colorbar
    if(show.colorbar){
      set.colorbar(r$cbx,r$cby,pal=pal,ticks=ticks,labels=labels,gradient=r$align[1],oticks=r$align[2],
                   cb.title=cb.title,cb.xlab=cb.xlab,cb.xlab.line=cb.xlab.line,
                   cex.cb.title=cex.cb.title,cex.cb.xlab=cex.cb.xlab,cex.cb.ticks=cex.cb.ticks,cb.ticks.srt=cb.ticks.srt,...)
    }
  }else{
    # print parameter label
    #     text((r$cbx[1]+r$cbx[2])/2,r$cby[1]-4.5*abs(r$cby[2]-r$cby[1]),parse(text=cb.xlab),cex=0.8,xpd=T)
    
    # print date
    #     text((r$cbx[1]+r$cbx[2])/2,r$cby[2]+2*abs(r$cby[2]-r$cby[1]),cb.title,cex=0.9)
  }
  #   if(v_image){
  #     # plot colorbar
  #     color.legend(r$cbx[1],r$cby[1],r$cbx[2],r$cby[2],"",cmap.data,align="rb",gradient="x",xpd=T) #xl,yb,xr,yt
  #     # plot colorbar ticks and labels
  #     for(n in 1:length(ticks))
  #     {
  #       lines(c(ticks.xpos[n],ticks.xpos[n]),c(r$cby[1],r$cby[1]-0.75*abs(r$cby[2]-r$cby[1])),xpd=T)
  #       text(ticks.xpos[n],r$cby[1]-2*abs(r$cby[2]-r$cby[1]),labels[n],cex=0.7,xpd=T)
  #     }
  #   }
  
  #   
  
  if(sidelabels){# plot side labels (month and year)
    if(missing(main)) text(mean(r$xlim),r$ylim[2]+0.1*diff(r$ylim),bindate2main(file_def$timestep,file_def$date1,file_def$date2),cex=1.7,font=2,xpd=T)
    if(Ylab == F){Ylab <- bindate2ylab(file_def$timestep,file_def$date1,file_def$date2)}else{Ylab <- Ylab}
    text(r$xlim[1]-0.18*diff(r$ylim),mean(r$ylim),Ylab,cex=1.7,font=2,cb.ticks.srt=90,xpd=T)
  }
  if(!missing(main)) text(mean(r$xlim),r$ylim[2]+0.1*diff(r$ylim),main,cex=1.7,font=2,xpd=T)
  
  plotinfolder <- c(paste(", in folder: ",plotfolder,sep="")," ")
  if(verbose) cat(paste0("\nprinting file: ", outfile.name, plotinfolder[2-as.numeric(Save)],"\n")) # display files to print
  
  if(Save){
    dev.off()
    #     if(Return) return(outfile.name)
  }
}
