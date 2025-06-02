##########################
##########################
##speedEm calculations
##########################
##########################

##########################
#functions to calculate and handle emSpeed data
##########################
#includes 
##########################
#(exported)
#fitSpeedEm
#speedEmPlot 
#panel.speedEmPlot1
#(unexported)
#pems_speedEm1

##########################
#to do
##########################
#schedule time to think about other functions
#tidy code
###########################
#comments
##########################
#pems_... are internal functions


##########################
##########################
##fitEmSpeed
##########################
##########################

#kr 16/02/2021 v 0.1.0
#kr 16/02/2021 v 0.0.3  (shifted to package)

#what it does
##########################
# calculates emSpeed relationships

#note
####################################
#emSpeed functions are not instantaneous terms 
#so need think about resolution at which they can be depicted 

#urgent
##########################
#

#to do
##########################
#tidy this

#might not be right name
fitSpeedEm <- function(em, time, speed, engine.on = NULL,
                       data = NULL, method = 1, min.speed = 5,
                       bin.size = NULL, 
                       ..., fun.name="fitEmSpeed"){
  
  #get inputs
  time <- getPEMSElement(!!enquo(time), data, if.missing="stop",
                         units="s", fun.name=fun.name)
  speed <- getPEMSElement(!!enquo(speed), data, if.missing="stop",
                       units="km/h", fun.name=fun.name)
  em <- getPEMSElement(!!enquo(em), data, if.missing="stop",
                       units="g/s", fun.name=fun.name)
  engine.on <- getPEMSElement(!!enquo(engine.on), data,
                              if.null="return",
                              if.missing="return",
                              fun.name=fun.name)
  
  #not sure engine is useful here....
  #engine.on reset is not supplied
  if(is.null(engine.on) || length(engine.on)<1){
    engine.on <- min(time, na.rm=TRUE)
  }
  
  #foreshorten ranges based on engine.on time 
  #doing this here (and add back in) so code does  
  #need to repeated in each pems_coldStart... method
  eng.on.row <- pems_findNearID(time, engine.on)
  if(eng.on.row>1){
    em[1:eng.on.row] <- NA
    speed[1:eng.on.row] <- NA
  }
  
  #switch might not best option
  #also want an option for this to be function
  if(is.numeric(method)){
    switch(method,
           "1" = {method <- pems_speedEm1},
           "2" = {method <- pems_speedEm2}
    )
  }
  if(!is.function(method)){
    stop("method unknown or suspect")
  }
  ans <- method(speed, em, time, min.speed=min.speed,
                bin.size=bin.size)
  ans$engine.on <- pems.element(rep(engine.on, nrow(ans)),
                                name="engine.on", 
                                units=units(time))
  #output
  ans
}

#speedEm method 1
#build with row binning
pems_speedEm1 <- function(speed, em, time, min.speed = 5,
                          bin.size = NULL,
                          ...){
  #speedEm model 1
  em2 <- em
  #assuming first point is 1 time unit
  #used median(diff(timp)) elsewhere...
  #rationalise this?
  em2[is.na(em2)] <- 0
  em2 <- em2 * diff(c(1, time))
  dist <- calcDistance(speed, time, units="km")
  d <- data.frame(dist=dist, em2=em2, speed=speed)
  d$temp <- d$temp/d$dist
  if(!is.null(bin.size)){
    d$grp <- rep(1:nrow(d), each=ceiling(bin.size))[1:nrow(d)]
    d <- dplyr::summarise(dplyr::group_by(d, grp),
                     dist=sum(dist, na.rm=TRUE),
                     temp=mean(temp, na.rm=TRUE),
                     speed=mean(speed, na.rm=TRUE))
    d <- d[is.finite(rowSums(d)),]
    d <- na.omit(d)
    d <- d[,names(d)!="grp"]
  }
  d <- pems(d, units=c(units(dist), "g/km", units(speed)))
  d <- d[d$speed >= min.speed,]
  d
}








##########################
##########################
##speedEmPlot
##########################
##########################

#kr 12/02/2021 v 0.1.0
#kr 01/02/2021 v 0.0.8  (shifted to package)

#what it does
##########################
# plots a speed emission trend for 
#    the supplied em and speed (and time) 
#    data-series

#urgent
##########################
#plot.type and panel should be rationalised
#  dont need both in coldStartPlot

#to do
##########################
#tidy this

#comments
##########################
#

speedEmPlot <- function(speed, em = NULL, time = NULL, 
                        ..., data = NULL, engine.on = NULL,
                        min.speed = 5, bin.size = NULL,  
                        plot.type = 1, method = 1,
                        fun.name="speedEmPlot"){
  
  #setup
  extra.args <- list(...)
  settings <- calcChecks(fun.name, ..., data = data)
  
  #get time, etc, locally
  time <- getPEMSElement(!!enquo(time), data,
                         if.missing="stop",
                         units="s", fun.name=fun.name)
  em <- getPEMSElement(!!enquo(em), data, if.missing="stop",
                       units="g/s", fun.name=fun.name)
  speed <- getPEMSElement(!!enquo(speed), data, if.missing="stop",
                       units="km/h", fun.name=fun.name)
  
  engine.on <- getPEMSElement(!!enquo(engine.on), data,
                              if.null="return",
                              fun.name=fun.name)
  #names
  xlab <- if(is.null(attributes(speed)$name)) {
    "speed"
  } else {
    attributes(speed)$name
  }
  ylab <- if(is.null(attributes(em)$name)) {
    "em"
  } else {
    attributes(em)$name
  }
    
  #accum 
  #multi.y handling
  if("sub.id" %in% names(attributes(em))){
    temp <- 1:length(attributes(em)$sub.id)
    y.ref <- lapply(temp, function(x){
      rep(attributes(em)$sub.nm[x], attributes(em)$sub.id[x])
    })
    y.ref <- do.call(c, y.ref)
  } else {
    y.ref <- rep("default", length(em))
  }
  ans <- lapply(unique(y.ref), function(x){
    temp <- em[y.ref==x]
    ans <- fitSpeedEm(temp, time, speed, engine.on=engine.on,
                      min.speed=min.speed,  method=method,
                      bin.size = bin.size)
    ans$y.ref <- rep(x, nrow(ans))
    ans
  })
  ans <- do.call(rbind, ans)
  #this needs to track cpe groups/y.ref
  test <- unique(ans$y.ref)
  if(length(test)>1){
    #add cpe terms
    attributes(ans$temp)$sub.nm <- test
    test <- length(test)
    attributes(ans$temp)$sub.id <- rep(length(ans$y.ref)/test,
                                       test)
  }
  
  #reset extra.args for data rework here
  #terms going to all plot
  
  plot.type <- as.character(plot.type)
  if(!plot.type %in% c("1", "2")){
    stop("In calcSpeedPlot(): plot.type unknown", 
         call. = FALSE)
  }
  
  #plot 1 scatter with loess
  if(plot.type=="1"){
    extra.args <- loa::listUpdate(list(x=ans$speed, y=ans$temp, 
                                  data=NULL, xlab=xlab, 
                                  ylab=ylab, report=FALSE,
                                  scheme=pems.utils::pems.scheme),
                             extra.args)
    plt <- rlang::exec(pemsPlot, !!!extra.args)
    if(loa::isGood4LOA(extra.args$loess)){
###########################
      #need to think about handling of this
      temp <- do.call(loa::listLoad, loa::listUpdate(extra.args, 
                        list(load="loess")))$loess
      temp <- loa::listUpdate(list(lattice.plot=plt,
                              type="n"), temp)
      plt <- do.call(loa::add.XYLOESSFit, temp)
    }
  }
  
  #plot2 boxplot
  if(plot.type=="2"){
    extra.args <- loa::listUpdate(list(x=ans$speed, y=ans$temp, 
                                  data=NULL, xlab=xlab, 
                                  ylab=ylab, type="n",
                                  scheme=pems.utils::pems.scheme,
                                  key.fun=loa::draw.groupPlotKey),
                             extra.args)
    plt <- rlang::exec(pemsPlot, !!!loa::listHandler(extra.args,
                                                ignore = "breaks"))
    temp <- do.call(loa::listLoad, 
                    loa::listUpdate(extra.args, 
                              list(load="bw")))$bw
    if(is.null(extra.args$breaks)){
      extra.args$breaks <- seq(min.speed, 
                               max(ans$speed, na.rm=TRUE) + 10,
                               by=10)
    }
    temp <- loa::listUpdate(list(lattice.plot=plt, 
                            breaks=extra.args$breaks), temp)
    plt <- do.call(add.XBinYBoxPlot, temp)
  }
  
  plt
##############################
  #do same for plot.type 2
##############################
  
#  extra.args <- listUpdate(list(x=ans$speed, y=ans$temp, 
#                                data=NULL, 
#                                xlab=xlab,ylab=ylab),
#                           extra.args)
#  plt <- rlang::exec(pemsPlot, !!!extra.args)
#  #if(isGood4LOA(extra.args$fit))
#  plt <- add.XYLOESSFit(plt, type="n")
#  plt <- add.XBinYBoxPlot(plt)
#  plt
}



###############################
#unexported functions
###############################

#methods currently not exported 
#better in loa??

add.XBinYBoxPlot <- function(lattice.plot=lattice::trellis.last.object(),
                           preprocess = add.XBinYBoxPlot_prep,
                           model.method = XBinYBoxPlot,
                           panel = panel.XBinYBoxPlot, ...){
  x.args <- list(lattice.plot = lattice.plot, ...,
                 model.method = model.method,
                 preprocess = preprocess, panel=panel)
  x.args$grid <- FALSE
  x.args$type <- "l"
  do.call(loa::add.loaPanel, x.args)
}

add.XBinYBoxPlot_prep<- function(lattice.plot=lattice::trellis.last.object(),
                           model.method=XBinYBoxPlot,
                           ...){
  
  common <- lattice.plot$panel.args.common
  #tidy group/zcase args
  if(!"group.ids" %in% names(common)){
    #might need warning re zcases...
    common$group.ids <- if("groups" %in% common){
      if(is.factor(common$groups)) 
        levels(common$groups) else unique(common$groups)
    } else {
      "default"
    }
    #might need group.args?
  }
  ans <- lapply(lattice.plot$panel.args, 
                function(x){
                  temp <- loa::listUpdate(common, x)
                  if(!"groups" %in% names(temp)){
                    temp$groups <- rep(temp$group.ids[[1]], 
                                       length(temp$x))
                  }
                  out <- lapply(temp$group.ids, function(i){
                    x <- temp$x[temp$groups==i]
                    y <- temp$y[temp$groups==i]
                    model.method(x=x, y=y, ..., 
                                 group.id=as.character(i))
                  })
                  names(out) <- temp$group.ids
                  out
                })
  #this right place?
  lattice.plot$panel.args.common$loa.XBinYBoxPlot <- ans
  #reset x range if need be...
##################################
#might want to work up into function
#might want to expand it so it finds all x terms/y terms
#if not told what to look for... see bin terms previously??
#might want to move to loa...
  lims.bw <- lapply(ans, function(x){
    ans <- lapply(x, function(y){
      if(is.data.frame(y)){
        range(c(y$x1, y$x2, na.rm=TRUE))
      } else {c(NA,NA)}
    })
    ans <- range(do.call(c, ans), na.rm=TRUE)
    temp <- (ans[2]-ans[1])/50
    c(ans[1]-temp, ans[2]+temp)
  })
  if(is.list(lattice.plot$x.limits)){
    for(i in 1:length(lattice.plot$x.limits)){
     lattice.plot$x.limits[[i]] <- range(
       c(lattice.plot$x.limits[[i]], lims.bw[[i]]),
       na.rm=TRUE
     ) 
    }
  } else {
    lattice.plot$x.limits <- range(
      c(lattice.plot$x.limits, lims.bw),
      na.rm=TRUE
    )
  }
  #output
  lattice.plot
}

XBinYBoxPlot <- function(x, y, group.id=NULL, 
                         breaks = NULL, 
                         ...){
  #see handling in loaXYFit_loess
  #cut by x ranges
  
  if(length(x[!is.na(x)]) < 1) {
    return(NULL)
  } else {
    x.range <- if(is.null(breaks)){
      pretty(x)
    } else {
      breaks
    }
    ans <- lapply(1:(length(x.range)-1), function(xx){
      x1 <- x[x >= x.range[xx] & x < x.range[xx+1]]
      y1 <- y[x >= x.range[xx] & x < x.range[xx+1]]
      temp <- grDevices::boxplot.stats(y1)
      #list(df = data.frame(x=x1,y=y1, ref=xx),
      #     box = temp)
      data.frame(x1 = x.range[xx], x2=x.range[xx+1],
                 y1=temp$stats[1], y2=temp$stats[2],
                 y3=temp$stats[3], y4=temp$stats[4],
                 y5=temp$stats[5])
    })
    return(do.call(rbind, ans))
  }
  #nb: returns NULL or ans in last if else
}


panel.XBinYBoxPlot <- function(...){
  #print("panel")
  plot.args <- loa::listUpdate(list(box = TRUE, wisk = TRUE, 
                               out = TRUE, line.col = 1), 
                          list(...))
  #print(names(plot.args))
  if (!"group.ids" %in% names(plot.args)) {
    plot.args$group.ids <- "default"
    if (!"groups" %in% names(plot.args)){ 
      plot.args$groups <- rep("default", length(plot.args$x))
      if(!"col" %in% names(plot.args)) {
        #use default no group colour if no groups...
        plot.args$col <- do.call(loa::colHandler, 
                                 loa::listUpdate(plot.args, list(z = 1)))
      }
    }
  }
  if (!"col" %in% names(plot.args)) {
    plot.args$col <- do.call(loa::colHandler, loa::listUpdate(plot.args, 
                                                    list(ref = 1:length(plot.args$group.ids))))
  }
  #  if (!"lty" %in% names(plot.args)) 
  #    plot.args$lty <- rep(1, length(plot.args$group.ids))
  #  if (!"lwd" %in% names(plot.args)) 
  #    plot.args$lwd <- rep(1, length(plot.args$group.ids))
  all.mods <- if ("loa.XBinYBoxPlot" %in% names(plot.args)){ 
    #print("all.mods")
    plot.args$loa.XBinYBoxPlot[[lattice::panel.number()]]
  } else {
    temp2 <- add.XBinYBoxPlot_prep(list(panel.args.common = plot.args, 
                                        panel.args = list(list())))
    temp2$panel.args.common$loa.mod.fit[[1]]
  }
  
  #testing
  #only run for none missing mods
  test <- names(all.mods)[!sapply(all.mods, is.null)]
  if(length(test)>0){
    for (i in test) {
      #print(test)
      #col/unit
      test.n <- length(test)
      test.this <- which(test==i)
      mod <- all.mods[[i]]
      #print(mod)
      if (!is.null(mod)) { #this stops missing mods being plotted
        if (loa::isGood4LOA(plot.args$box)) {
          m.args <- loa::listUpdate(plot.args, do.call(loa::listLoad, 
                                                  loa::listUpdate(plot.args, list(load = "box")))[["box"]])
          m.args <- loa::listUpdate(list(group.args = "col", 
                                    levels = 3, border = FALSE), 
                               m.args)
          for (j in m.args$group.args) {
            if (j %in% names(m.args)){ 
              m.args[[j]] <- rep(m.args[[j]], 
                                 length(m.args$group.ids))[m.args$group.ids == i]
            }
          }
          
          ###################
          #to do
          #option to turn off/on different bw components
          #user control of y cropping, 1 - scale
          #make if line.col = "col"  
          
          ##########################################
          #this handles groups on different panels 
          #might want to tidy if
          rescale <- (1:test.n/test.n) -(1/test.n)
          nixx <- rescale[test.this]
          mod$x1 <- mod$x1 + nixx
          nixx <- rev(rescale)[test.this]
          mod$x2 <- mod$x2 - nixx
          ##########################################
          
          temp <- mod$x2-mod$x1
          scale <- 0.90  #move to args
          x.mid <- (temp*0.5) + mod$x1 
          x1 <- (temp*(1-scale)*0.5) + mod$x1
          x2 <- mod$x2 - (temp*(1-scale)*0.5) 
          #box
          lattice::lrect(x1, mod$y2, x2, mod$y4,
                border = m.args$line.col,
                col=m.args$col) #, alpha=m.args$alpha)
          lattice::lsegments(x1=x1, x2=x2, 
                    y1=mod$y3, y2=mod$y3, 
                    col=m.args$line.col)
          #wisk
          lattice::lsegments(x1=x1, x2=x2, 
                    y1=mod$y1, y2=mod$y1, 
                    col=m.args$line.col)
          lattice::lsegments(x1=x1, x2=x2, 
                    y1=mod$y5, y2=mod$y5,
                    col=m.args$line.col)
          lattice::lsegments(x1=x.mid, x2=x.mid, 
                    y1=mod$y1, y2=mod$y2,
                    col=m.args$line.col)
          lattice::lsegments(x1=x.mid, x2=x.mid, 
                    y1=mod$y4, y2=mod$y5,
                    col=m.args$line.col)
          #oulaying points
          temp <- loa::listHandler(plot.args, use=c("x", "y"))
          if("groups" %in% names(plot.args)){
            temp$x <- temp$x[plot.args$groups==i]
            temp$y <- temp$y[plot.args$groups==i]
          }
          ref <- rep(FALSE, length(temp$x))
          for(j in 1: nrow(mod)){
            ref[temp$x>=mod$x1[j] & temp$x<mod$x2[j]
                & temp$y<mod$y1[j]] <- TRUE
            ref[temp$x>=mod$x1[j] & temp$x<mod$x2[j]
                & temp$y>mod$y5[j]] <- TRUE
          }
          temp$x <- temp$x[ref]
          temp$y <- temp$y[ref]
          temp$col <- plot.args$col[plot.args$group.ids==i]
          temp$pch <- 20
          do.call(lattice::lpoints, temp)
        }
      }
    }
  }
}









