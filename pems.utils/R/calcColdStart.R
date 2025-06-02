##########################
##########################
##coldstart calculations
##########################
##########################
#need checking re new pems structure
#description

##########################
#functions to calculate and handle cold starts
##########################
#includes 
##########################
#(exported)
#fitColdStart
#coldStartPlot 
#panel.coldStartPlot1
#panel.coldStartPlot2
#(unexported)
#pems_findNearID
#pems_coldStart1 
#pems_coldStart2 

##########################
#to do
##########################
#schedule time to think about this
#other functions
#tidy code
###########################
#comments
##########################
#pems_... are internal functions



##########################
##########################
##fitColdStart
##########################
##########################

#kr 12/02/2021 v 0.1.0
#kr 01/02/2021 v 0.0.3  (shifted to package)

#what it does
##########################
# fits a cold start contribution model to 
# emissions and time data-series 

#urgent
##########################
#

#to do
##########################
#tidy this

#comments
##########################
#

fitColdStart <- function(em, time, engine.on = NULL, 
                         data = NULL, method = 2, ...,
                         fun.name="fitColdStart"){
  
  #get inputs
  time <- getPEMSElement(!!enquo(time), data, if.missing="stop",
                         units="s", fun.name=fun.name)
  em <- getPEMSElement(!!enquo(em), data, if.missing="stop",
                       units="g/s", fun.name=fun.name)
  engine.on <- getPEMSElement(!!enquo(engine.on), data,
                              if.null="return",
                              if.missing="return",
                              fun.name=fun.name)
  
  #engine.on reset is not supplied
  if(is.null(engine.on) || length(engine.on)<1){
    engine.on <- min(time, na.rm=TRUE)
  }
  
  #foreshorten ranges based on engine.on time 
  #doing this here (and add back in) so code does  
  #need to repeated in each pems_coldStart... method
  eng.on.row <- pems_findNearID(time, engine.on)
  time2 <- time[eng.on.row:length(time)]
  time2 <- time2 - min(time2, na.rm=TRUE)
  em2 <- em[eng.on.row:length(time)]
  #calc acc time
  ans <- pems(data.frame(time=time, em=em),
              units=c(units(time), units(em)))
  #switch might not best option
  #also want an option for this to be function
  if(is.numeric(method)){
    switch(method,
           "1" = {method <- pems_coldStart1},
           "2" = {method <- pems_coldStart2}
    )
  }
  #
  ans2 <- method(em2, time2, eng.on.row)
  ans <- align(ans, ans2, n=eng.on.row-1)
  ans$engine.on <- pems.element(rep(engine.on, nrow(ans)),
                                name="engine.on", 
                                units=units(time))
  #output
  ans
}



##########################
##########################
##coldStartPlot
##########################
##########################

#kr 12/02/2021 v 0.1.0
#kr 01/02/2021 v 0.0.8  (shifted to package)

#what it does
##########################
# plots a cold start model for 
#    the supplied em and time 
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

coldStartPlot <- function(time, em = NULL,  
                          ..., data = NULL, engine.on = NULL,
                          plot.type = 1, method = 2,
                          fun.name="coldStartPlot"){
  
  #setup
  extra.args <- list(...)
  settings <- calcChecks(fun.name, ..., data = data)
  
  #get time, etc, locally
  time <- getPEMSElement(!!enquo(time), data,
                         if.missing="stop",
                         units="s", fun.name=fun.name)
  em <- getPEMSElement(!!enquo(em), data, if.missing="stop",
                       units="g/s", fun.name=fun.name)
  engine.on <- getPEMSElement(!!enquo(engine.on), data,
                              if.null="return",
                              fun.name=fun.name)
  #names
  xlab <- if(is.null(attributes(time)$name)) {
    "time"
  } else {
    attributes(time)$name
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
    ans <- fitColdStart(temp, time, engine.on=engine.on,
                        method=method)
    ans$y.ref <- rep(x, nrow(ans))
    ans
  })
  ans <- do.call(rbind, ans)
  #shouldn't need to do these
  units(ans$time) <- "s"
  acc <- ans$acc
  attributes(acc) <- attributes(ans$temp)
  units(acc) <- "g"
  em <- ans$temp
  
  #reset extra.args for data rework here
  #terms going to all plot
  extra.args <- loa::listUpdate(list(x=time, data=ans, bp=ans$bp, 
                                bp.upper=ans$bp.upper, 
                                hot=ans$hot, 
                                hot.upper=ans$hot.upper,
                                hot.lower=ans$hot.lower, 
                                engine.on=ans$engine.on,
                                cold=ans$bkpt, ref.best=ans$bkpt,
                                type="l", xlab=xlab),
                             extra.args)
  
  if(plot.type==1){
    #terms unique to type 1 plot
    #y must be acc em, but ylab, panel can be overwritten 
    
    extra.args <- loa::listUpdate(list(ylab=ylab, multi.y="cond",
                                       panel=pems.utils::panel.coldStartPlot1, 
                                       scheme=pems.utils::pems.scheme),
                                  extra.args)    
    extra.args <- loa::listUpdate(list(y=acc), extra.args)
    plt <- rlang::exec(pems.utils::pemsPlot, !!!extra.args)
    #see https://stackoverflow.com/questions/54593806/do-call-forces-argument-evaluation-before-rlangs-tidy-evaluation
    #re invoke versus do.call for rlang...
    #then docs exec replaces invoke (sort of)
  } else {
    if(plot.type==2){
      #which is better; this or above???
      extra.args <- loa::listUpdate(list(ylab=ylab, multi.y="cond",
                                    panel=pems.utils::panel.coldStartPlot2, 
                                    scheme=pems.utils::pems.scheme),
                                    extra.args)
      extra.args <- loa::listUpdate(list(y=em), extra.args)
      plt <- rlang::exec(pems.utils::pemsPlot, !!!extra.args)
    } else {
      stop("plot type unknown")
    }
  }
  #scale reset?
  plt
}


#associated plot panels

panel.coldStartPlot1 <- function(..., loa.settings = FALSE){
  
  #built on setting from panel.pemsPlot 
  extra.args <- list(...)
  
  if(loa.settings){
    temp <- loa::loaHandler(loa::panel.loa, loa.settings = TRUE)
    temp$common.args <- unique(c(temp$common.args, "units"))
    temp$default.settings <- loa::listUpdate(temp$default.settings, 
                                        list(loa.preprocess = pems.utils::preprocess.pemsPlot, 
                                             type="l", lty=1, lwd=1,
                                             grid = TRUE, cs.model = TRUE,
                                             multi.y = "cond",
                                             group.args = c("col", "type", "lty", "lwd"),
                                             load.lists = c("grid", "cs.model")))
    return(temp)
  }
  #add z if missing
  if(is.null(extra.args$z)) extra.args$z <- 1
  #extra.args$type <- zHandler(extra.args$type, ref=extra.args$x)
  #plot
  do.call(function(...){
    extra.args = list(...)
    #grid
    if(loa::isGood4LOA(extra.args$grid)){
      loa::panel.loaGrid(panel.scales = extra.args$panel.scales,
                    grid = extra.args$grid, 
                    xlim = extra.args$xlim, 
                    ylim = extra.args$ylim)
    }
    extra.args$grid <- FALSE
    #main acc trend
    #using panel.loaPlot2 
    #   - better output for lines if lots of points
    do.call(loa::panel.loaPlot2, extra.args)
    
    #hot contribution model
    #if(loa::isGood4LOA(extra.args$cs.model)){
      if(TRUE){
      cs <- loa::listUpdate(extra.args, 
                       do.call(loa::listLoad, 
                               loa::listUpdate(extra.args, 
                                          list(load="cs.model")))[["cs.model"]])
      if(!"groups" %in% names(cs)){
        cs$groups <- rep("default", 
                         length(cs$x))
      }
      grp.ref <- unique(cs$groups)
      if(!"col" %in% names(cs)){
        cs$col <- do.call(loa::colHandler, 
                          loa::listUpdate(cs,
                                     list(z=1:length(grp.ref),
                                          ref=1:length(grp.ref))))
      }
      for(i in grp.ref){
        temp <- cs
        
        ref <- if("group.ids" %in% names(temp)){
          temp$group.ids
        } else {
          grp.ref
        }
        col <- rep(temp$col, length(ref))
        col <- col[ref==i]
        
        temp$x <- c(temp$x[temp$groups==i],
                    rev(temp$x[temp$groups==i]))
        temp$y <- c(temp$hot.upper[temp$groups==i],
                    rev(temp$hot.lower[temp$groups==i]))
        temp$col <- col
        temp$alpha <- 0.3
        temp$border <- NA
        do.call(lattice::panel.polygon, temp)
        
        #line
        temp <- cs
        temp$x <- temp$x[temp$groups==i]
        temp$y <- temp$hot[temp$groups==i]
        temp$col <- rep(col, length(temp$x))
        temp$lty <- rep(3, length(temp$x))
        do.call(loa::panel.loaPlot2, temp)
        
        #arrow 1
        x <- temp$engine.on[temp$groups==i]
        x1 <- min(x[!is.na(x)])
        y1 <- min(temp$y[!is.na(temp$y)])
        lattice::panel.arrows(x1=x1, x2=x1, y1=y1, y2=0,
                     col=col,
                     length=0.1)
        #line 2
        x <- temp$ref.best[temp$groups==i]
        x2 <- min(x[!is.na(x)])
        x2 <- x1 + x2
        y <- temp$y[temp$x==x2][1]
        lattice::panel.lines(x=c(x2,x2), y=c(y,0),
                    lty=2,
                    col=col,
                    length=0.1)
        
        #arrow 3
        y <- (((max(y, na.rm=TRUE))-0)*0.5) + 0
        lattice::panel.arrows(x1=x1, x2=x2, y1=y, y2=y, 
                     col=col,
                     length=0.08, ends="both")

        #labels 
        lbls <- paste("Cold Start: ", signif(y1, 3),
                      units(temp$y), "; ", x2-x1,
                      "s", sep="")
        lattice::panel.text(x=x2, y=y, labels=lbls, 
                   col=col, pos=4)
        #line name to decide...
        #arguments to tidy
        
      }
    }
    
  }, extra.args)
}


panel.coldStartPlot2 <- function(..., loa.settings = FALSE){
  
  #built on setting from panel.pemsPlot 
  extra.args <- list(...)
  
  if(loa.settings){
    temp <- loa::loaHandler(loa::panel.loa, loa.settings = TRUE)
    temp$common.args <- unique(c(temp$common.args, "units"))
    temp$default.settings <- loa::listUpdate(temp$default.settings, 
                                        list(loa.preprocess = pems.utils::preprocess.pemsPlot, 
                                             type="l", lty=1, lwd=1,
                                             grid = TRUE, cs.model = TRUE,
                                             multi.y = "cond",
                                             group.args = c("col", "type", "lty", "lwd"),
                                             load.lists = c("grid", "cs.model")))
    return(temp)
  }
  #add z if missing
  if(is.null(extra.args$z)) extra.args$z <- 1
  #extra.args$type <- zHandler(extra.args$type, ref=extra.args$x)
  #plot
  do.call(function(...){
    extra.args = list(...)
    #grid
    if(loa::isGood4LOA(extra.args$grid)){
      loa::panel.loaGrid(panel.scales = extra.args$panel.scales,
                    grid = extra.args$grid, 
                    xlim = extra.args$xlim, 
                    ylim = extra.args$ylim)
    }
    extra.args$grid <- FALSE
    
    #hot contribution model
    if(loa::isGood4LOA(extra.args$cs.model)){
      cs <- loa::listUpdate(extra.args, 
                       do.call(loa::listLoad, 
                               loa::listUpdate(extra.args, 
                                          list(load="cs.model")))[["cs.model"]])
      if(!"groups" %in% names(cs)){
        cs$groups <- rep("default", 
                         length(cs$x))
      }
      grp.ref <- unique(cs$groups)
      if(!"col" %in% names(cs)){
        cs$col <- do.call(loa::colHandler, 
                          loa::listUpdate(cs,
                                     list(z=1:length(grp.ref),
                                          ref=1:length(grp.ref))))
      }
      for(i in grp.ref){
        #data source
#not meant for grouping
#but should allow it to handle groups
#for example for multi.y=c("cond", "groups)
        temp <- cs
        x <- temp$engine.on[temp$groups==i]
        ref <- temp$ref.best[temp$groups==i]
        y <- temp$y[temp$groups==i]
        x1 <- min(x[!is.na(ref)])
        y1 <- min(temp$y[!is.na(temp$y)])
        x <- temp$ref.best[temp$groups==i]
        x2 <- min(x[!is.na(x)])
        x2 <- x1 + x2
        x <- temp$ref.best[temp$groups==i]
        x2 <- min(x[!is.na(x)])
        x2 <- x1 + x2
        y <- temp$y[temp$x==x2][1]
        
        #cold start region
        temp$x <- temp$x[temp$groups==i]
        temp$y <- temp$y[temp$groups==i]
        temp$y <- temp$y[temp$x >= x1 & temp$x <= x2]
        temp$x <- temp$x[temp$x >= x1 & temp$x <= x2]
        temp$x <- c(temp$x, temp$x[length(temp$x)], 
                    temp$x[1])
        temp$y <- c(temp$y, 0, 0)
  
  
        ref <- if("group.ids" %in% names(temp)){
          temp$group.ids
        } else {
          grp.ref
        }
        col <- rep(temp$col, length(ref))
        col <- col[ref==i]
  
        lattice::panel.polygon(temp$x, temp$y,
                      col=col,
                      border=FALSE,
                      alpha=0.3)
        
        #arrow labels
        y <- (((max(temp$y, na.rm=TRUE))-0)*0.95) + 0
        lattice::panel.arrows(x1=x1, x2=x2, y1=y, y2=y, 
                     col=col,
                     length=0.08, ends="both")
        lattice::panel.lines(x=c(x2,x2), y=c(y,0),
                    lty=2,
                    col=col[1],
                    length=0.1)
        lattice::panel.lines(x=c(x1,x1), y=c(y,0),
                    lty=2,  
                    col=col[1],
                    length=0.1)
        
        #labels
        #need to look at where units went
        ref <- temp$hot[temp$groups==i]
        ref <- ref[!is.na(ref)][1]
        #temp$hot <- cs$hot[cs$x >= x1 & cs$x <= x2]
        
        lbls <- paste("Cold Start: ", signif(ref, 3),
                       units(ref), "; ", x2-x1,
                       "s", sep="")
        lattice::panel.text(x=x2, y=y, labels=lbls, 
                   col=col[1], pos=4)
        #line name to decide...
        #arguments to tidy
        
      }
    }
    #main acc trend
    #using panel.loaPlot2 
    #   - better output for lines if lots of points
    #     doing this after adding cold start region
    do.call(loa::panel.loaPlot2, extra.args)
    
  }, extra.args)
}








###############################
#unexported functions
###############################

#find ref (or nearest point to ref) in data-series x
pems_findNearID <- function(x, ref){
  temp <- abs(x - ref)
  #giving just first if several at moment
  which(temp==min(temp, na.rm=TRUE))[1]
}

#methods currently not exported 

#coldstart method 1
#break point
pems_coldStart1 <- function(em, time, engine.on, 
                            ...){
  #coldstart model 2
  #em is emissions to accum
  #time is local.time
  ##
  # I am not sure this needs engine.on 
  # but it is currently getting eng.on.row anyway...
  acc <- em
  acc[is.na(acc)] <- 0
  #assuming first point is 1 time unit
  acc <- acc * diff(c(1, time))
  acc <- cumsum(acc)
  #find best break-point
  ref <- rep(NA, length(time))
  for(i in 1:length(time)){
    x2 <- rep(0, length(time))
    x2[1:i] <- i:1
    model <- lm(acc~time + x2)
    ref[i] <-summary(model)$adj.r.squared
  }
  #build best model
  ref.best <- which(ref==max(ref, na.rm=TRUE))
  x2 <- rep(0, length(time))
  x2[1:ref.best] <- ref.best:1
  model <- lm(acc~time + x2)
  #bk model results
  bp <- predict(model, se=TRUE)
  bp.err <- bp$se.fit
  bp <- bp$fit
  bp.upper <- bp + (15*bp.err)
  bp.lower <- bp - (15*bp.err)
  #hot running results
  hot <- predict(model, se=TRUE, 
                 newdata=data.frame(acc=acc, 
                                    x2 = rep(0, length(time)), 
                                    time=time))
  hot.err <- hot$se.fit
  hot <- hot$fit
  hot.upper <- hot + (15*hot.err)
  hot.lower <- hot - (15*hot.err)
  
  ans <- pems(data.frame(acc=acc, 
                         bp=bp, bp.lower=bp.lower, 
                         bp.upper=bp.upper,
                         hot=hot, hot.lower=hot.lower,
                         hot.upper=hot.upper),
              units=c(rep("g", 7)))
  #logged issue 
  #   pems(data.frame(a=pems.element, b=vector))
  #   so adding bkpt after as pems.element
  #this is time after start of engine
  ans$bkpt <- pems.element(rep(time[ref.best], length=nrow(ans)),
                           units=units(time))
  ans
}

#coldstart method 2
#modified break point
pems_coldStart2 <- function(em, time, engine.on, 
                            ...){
  #coldstart model 2
  #em is emissions to accum
  #time is local.time
  #engine.on is time value at which engine is turned on
  #    not the row number of local.time
  acc <- em
  acc[is.na(acc)] <- 0
  #assuming first point is 1 time unit
  acc <- acc * diff(c(1, time))
  acc <- cumsum(acc)
  #find best break-point
  ref <- rep(NA, length(time))
  for(i in 1:length(time)){
    x2 <- rep(0, length(time))
    x2[1:i] <- i:1
    model <- lm(acc~time + x2)
    ref[i] <-summary(model)$adj.r.squared
  }
  #build best model
  ref.best <- which(ref==max(ref, na.rm=TRUE))
  x2 <- rep(0, length(time))
  x2[1:ref.best] <- ref.best:1
  model <- lm(acc~time + x2)
  #bk model results
  bp <- predict(model, se=TRUE)
  bp.err <- bp$se.fit
  bp <- bp$fit
  bp.upper <- bp + (15*bp.err)
  bp.lower <- bp - (15*bp.err)
  #hot running results
  hot <- predict(model, se=TRUE, 
                 newdata=data.frame(acc=acc, 
                                    x2 = rep(0, length(time)), 
                                    time=time))
  hot.err <- hot$se.fit
  hot <- hot$fit
  hot.upper <- hot + (15*hot.err)
  hot.lower <- hot - (15*hot.err)
  
  #push back bkpt to acc/hot.lower intercept
  #this may need more thinking...
  ref.best <- min(which(acc>hot.lower), na.rm=TRUE)
  
  ans <- pems(data.frame(acc=acc, 
                         bp=bp, bp.lower=bp.lower, 
                         bp.upper=bp.upper,
                         hot=hot, hot.lower=hot.lower,
                         hot.upper=hot.upper),
              units=c(rep("g", 7)))
  #logged issue 
  #   see previous method
  ans$bkpt <- pems.element(rep(time[ref.best], length=nrow(ans)),
                           units=units(time))
  ans
}



