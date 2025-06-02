##########################
##########################
##VSP calculations
##########################
##########################
#need checking re new pems structure
#description
##########################
#functions to calculate and handle VSP
##########################
#includes 
##########################
#calcVSP
#calcVSP_JimenezPalacios
#refVSPBin
#refVSPBin_NCSU.14
#refVSPBin_MOVES.24
#VSPPlot
##########################
#to do
##########################
#rethink/remake calcVSP
#   think it wants to be more like 
#remake calcCMEM or equivalent
#tidy refVSPBin
###########################
#comments
##########################
#used calcChecks, checkIfMissing
#binVSP and binVSP_... are current methods


##########################
##########################
##calcVSP
##########################
##########################

#kr 17/06/2018 v 0.0.7

#what it does
##########################
#calcVSP and calcVSP_ calculate VSP based 
##########################
#urgent
##########################
#need to tidy the this.call
#handling give current
#not parent call!!!
##########################
#to do
##########################
#tidy all of this 
#no very rational
#think I am writing same or very 
#similar code in too many places 
#make more like binVSP??

#comments
##########################
#

calcVSP <- function(speed = NULL, accel = NULL, slope = NULL, 
                    time = NULL, distance = NULL, data = NULL,
                    calc.method = calcVSP_JimenezPalacios,
                    ..., fun.name = "calcVSP", this.call = NULL){
  ##############
  #setup
  ##############
  dots <- quos(...)
  this.call <- match.call()
  settings <- calcChecks(fun.name=fun.name, ..., data = data)
  ##############
  #inputs
  ##############
  if(!missing(speed))
    speed <- getPEMSElement(!!enquo(speed), data, if.missing="return")
  if(!missing(accel))
    accel <- getPEMSElement(!!enquo(accel), data, if.missing="return")
  if(!missing(slope))
    slope <- getPEMSElement(!!enquo(slope), data, if.missing="return")
  if(!missing(time))
    time <- getPEMSElement(!!enquo(time), data, if.missing="return")
  if(!missing(distance))
    distance <- getPEMSElement(!!enquo(distance), data, if.missing="return")
  #if missing they are left as NULL
  #so I can see we we can make what we need from what we have...
  #better way to do this???
  if(is.null(speed) & is.null(accel) & is.null(time) & is.null(distance))
    checkIfMissing(if.missing = settings$if.missing, 
                  reply = "want speed and accel but insufficient inputs\n\t can make do with time and distance and work up", 
                  suggest = "add something I can work with to call", if.warning = NULL, 
                  fun.name = fun.name)
  
  if(is.null(speed)){
    if(is.null(time) | is.null(distance)){
      checkIfMissing(if.missing = settings$if.missing, 
                    reply = "want speed but insufficient inputs\n\t can make do with time and distance and work up", 
                    suggest = "add speed or time and distance to call", if.warning = NULL, 
                    fun.name = fun.name)
    } else {
      speed <- calcSpeed(distance = distance, time = time, if.missing = settings$if.missing, 
                        unit.conversions= settings$unit.conversions)
    }
  }
  if(is.null(accel)){
    if(is.null(time) | is.null(speed)){
      checkIfMissing(if.missing = settings$if.missing, 
                    reply = "want accel but insufficient inputs\n\t can make do with time and distance or time and speed", 
                    suggest = "add speed and time or distance and time to call", if.warning = NULL, 
                    fun.name = fun.name)
    } else {
      accel <- calcAccel(speed = speed, time = time, if.missing = settings$if.missing, 
                        unit.conversions= settings$unit.conversions)
    }
  }
  if(is.null(slope)){
    checkIfMissing(if.missing = "warning", 
                   reply = "slope not supplied; assuming 0", 
                   suggest = "add slope to call and rerun if required", if.warning = NULL, 
                   fun.name = fun.name)
    slope <- 0
  }
  #to think about
  #calc.method could be character?
  if(is.function(calc.method)){
    #new version
    #strip output because calcVSP packing this...
    if("output" %in% names(dots))
      dots[[which(names(dots)=="output")]]<-NULL
    vsp <- eval_tidy(quo(calc.method(speed=speed, accel=accel, slope=slope, 
                                     data=data, fun.name=fun.name, 
                                     this.call=this.call, !!!dots)))
    return(pemsOutput(vsp, output = settings$output, data = data,  
                      fun.name = fun.name, this.call = this.call))
  }
  #not good if we get here...
  checkIfMissing(if.missing = settings$if.missing, 
                reply = "could not run calc.method!", 
                suggest = "check ?calcVSP if reason unclear", if.warning = "returning NULL", 
                fun.name = fun.name)
  return(NULL)    
}


##########################
##########################
##calcVSP_JimenezPalacios
##########################
##########################

#kr 16/11/2018 v 0.0.1

#this is part replacement for calcVSP_JimenezPalaciosCMEM
#because I want separate methods... 

#REF needed

calcVSP_JimenezPalacios <- function(speed = NULL, accel = NULL, slope = NULL, 
                                    vehicle.weight = NULL, vsp.a = NULL, 
                                    vsp.b = NULL, vsp.c = NULL, vsp.g = NULL, 
                                    ..., data = NULL,  
                                    fun.name = "calcVSP_JimenezPalacios", 
                                    this.call = NULL){
  #######################  
  #setup
  #######################
  settings <- calcChecks(fun.name, ..., data = data)
  #######################
  #get inputs
  #######################
  if(!missing(speed))
    speed <- getPEMSElement(!!enquo(speed), data, if.missing="return")
  if(!missing(accel))
    accel <- getPEMSElement(!!enquo(accel), data, if.missing="return")
  if(!missing(slope))
    slope <- getPEMSElement(!!enquo(slope), data, if.missing="return")
  
  #######################
  #checks
  ########################
  if(is.null(speed) | is.null(accel))
    checkIfMissing(if.missing = settings$if.missing, 
                   reply = "Need speed and accel", 
                   suggest = "add speed and accel to see or see ?calcVSP", if.warning = NULL, 
                   fun.name = fun.name)

  #############################
  #units
  #############################
  #want specific units
  speed <- convertUnits(speed, to = "m/s", unit.conversions = settings$unit.conversions, 
                        if.missing = settings$if.missing, fun.name = fun.name)  
  accel <- convertUnits(accel, to = "m/s/s", unit.conversions = settings$unit.conversions, 
                        if.missing = settings$if.missing, fun.name = fun.name)  
############################
#to do
############################
#slope units to sort out
############################
  #############################
  #make data always pems
  #############################
  if(!isPEMS(data)) data <- makePEMS(data)
################
#testing
################
#any more needed?
###############################
  ####################
  #get pems.constants
  ####################
  #what if data=NULL or not supplied?
  pems.const <- getPEMSConstants(data)    
  if(is.null(vehicle.weight))        
    if(!is.null(pems.const$vehicle.weight))
      vehicle.weight <- pems.const$vehicle.weight
  if(is.null(vehicle.weight)){
    checkIfMissing(if.missing = "warning", 
                   reply = "vehicle.weight unknown; assuming < 3.885 Tons", 
                   suggest = "confirming vehicle.weight", 
                   if.warning = NULL, fun.name = fun.name)
  } else if(vehicle.weight > 3.885)
    checkIfMissing(if.missing = "warning", 
                   reply = "calcVSP_JimenezPalacios not for vehicles > 3.885 Tons",
                   suggest = "apply different calc.method and rerun", 
                   if.warning = NULL, fun.name = fun.name)
  ######################
  #calc parameters
  ######################
  if(is.null(vsp.a)) 
     vsp.a <- if(is.null(pems.const$vsp.a)) 1.1 else pems.const$vsp.a  
  if(is.null(vsp.b)) 
     vsp.b <- if(is.null(pems.const$vsp.b)) 0.132 else pems.const$vsp.b  
  if(is.null(vsp.c)) 
     vsp.c <- if(is.null(pems.const$vsp.c)) 0.000302 else pems.const$vsp.c  
  if(is.null(vsp.g))        
    vsp.g <- if(is.null(pems.const$vsp.g)) 9.81 else pems.const$vsp.g
  ########################
  #In case every need to check what we are using here 
  ########################
  #might think tidying/renaming this 
  #making diagnostic 
  if(!is.null(list(...)$sneaky))
    print(paste(vehicle.weight, vsp.a, vsp.b, vsp.c, vsp.g, sep="; "))
  ########################
  #calc vsp using this method  
  #########################  
  vsp <- speed * (vsp.a * accel + (vsp.g * slope) + vsp.b) + (vsp.c * speed^3)
  vsp <- pems.element(vsp, name="vsp", units="kW/metric ton")
  vsp <- pemsOutput(vsp, output = settings$output, data = data,  
                    fun.name = fun.name, this.call = this.call)
  vsp
}

########################
#NOT EXPORTING THIS
########################

calcVSP_JimenezPalaciosCMEM <- function(speed = NULL, accel = NULL, 
                    slope = NULL, vehicle.weight = NULL, vsp.a = NULL, 
                    vsp.b = NULL, vsp.c = NULL, vsp.g = NULL, ..., data = NULL,  
                    fun.name = "calcVSP_JimenezPalaciosCMEM", 
                    this.call = NULL){

    #constant g might be messy...
  
    #setup
    settings <- calcChecks(fun.name, ..., data = data)

    if(!missing(speed))
        speed <- getPEMSElement(!!enquo(speed), data, if.missing="return")
    if(!missing(accel))
        accel <- getPEMSElement(!!enquo(accel), data, if.missing="return")
    if(!missing(slope))
        slope <- getPEMSElement(!!enquo(slope), data, if.missing="return")

    if(is.null(speed) | is.null(accel))
            checkIfMissing(if.missing = settings$if.missing, 
                           reply = "Need speed and accel", 
                           suggest = "add speed and accel to see or see ?calcVSP", if.warning = NULL, 
                           fun.name = fun.name)
      
    if(is.null(slope)){
            checkIfMissing(if.missing = "warning", 
                           reply = "slope not supplied\n\t assuming 0", 
                           suggest = "add slope to call and rerun if required", if.warning = NULL, 
                           fun.name = fun.name)
            slope <- 0
    }
    
    #want specific units
    speed <- convertUnits(speed, to = "m/s", unit.conversions = settings$unit.conversions, 
                          if.missing = settings$if.missing, fun.name = fun.name)  
    accel <- convertUnits(accel, to = "m/s/s", unit.conversions = settings$unit.conversions, 
                          if.missing = settings$if.missing, fun.name = fun.name)  

#slope to sort out

    #make data always pems
    if(!isPEMS(data)) data <- makePEMS(data)

################
#testing
################

################
#



    pems.const <- getPEMSConstants(data)    
       
    if(is.null(vehicle.weight)){        
        vehicle.weight <- if(is.null(pems.const$vehicle.weight))
                 1.5 else pems.const$vehicle.weight
    }

#note no special bus handling

    if(is.null(vsp.a)){        
        vsp.a <- if(is.null(pems.const$vsp.a)){
                    if(vehicle.weight < 3.855) 1.1
                       else if(vehicle.weight < 6.35) (0.0996 * vehicle.weight) / 2204.6 
                           else if(vehicle.weight < 14.968) (0.0875 * vehicle.weight) / 2204.6
                               else (0.0661 * vehicle.weight) / 2204.6 
                 } else pems.const$vsp.a
    }
    
    if(is.null(vsp.b)){        
        vsp.b <- if(is.null(pems.const$vsp.b))
                 0.132 else pems.const$vsp.b
    }

    if(is.null(vsp.c)){        
        vsp.c <- if(is.null(pems.const$vsp.c)){
                    if(vehicle.weight < 3.855) 0.000302
                       else if(vehicle.weight < 6.35) (1.47 + (5.2e-05 * vehicle.weight)) / 2205 
                           else if(vehicle.weight < 14.968) (1.93 + (5.90e-5 * vehicle.weight)) / 2205
                               else (2.89 + (4.21e-5 * vehicle.weight)) / 2205
                 } else pems.const$vsp.c
    }

    if(is.null(vsp.g)){        
        vsp.g <- if(is.null(pems.const$vsp.g))
                 9.81 else pems.const$vsp.g
    }

########################
#need to check a, b, c for different wt vehicles 
#might need vsp.a, vsp.b, vsp.c?
#vehicle.weight?
########################
    if(!is.null(list(...)$sneaky))
          print(paste(vehicle.weight, vsp.a, vsp.b, vsp.c, vsp.g, sep="; "))
########################
#might think about a sneaky=TRUE 
#option for diagnostic info
########################


    vsp <- speed * (vsp.a * accel + (vsp.g * slope) + vsp.b) + (vsp.c * speed^3)
    vsp <- pems.element(vsp, name="vsp", units="kW/metric ton")
    vsp <- pemsOutput(vsp, output = settings$output, data = data,  
                      fun.name = fun.name, this.call = this.call)
    vsp
}









###################################
#refVSPBin
###################################

#kr v.0.1.3 19/06/2018
#kr v.0.2.0 13/01/2020 (rename)

#notes refVSPBin_MOVES23 needs more work...
# issue noted in docs 


refVSPBin <- function(..., bin.method="ncsu.14"){
   #NSE method?
   bin.method <- paste("refVSPBin_", toupper(bin.method), sep="")
   bin.method <- try(get(bin.method))
   #error catch#tidy later
   if(class(bin.method)[1]=="try-error" || !is.function(bin.method))
         stop("bin.method not found")     
   #one line of code?
   dots <- quos(...)
   eval_tidy(quo(bin.method(!!!dots)))
}


refVSPBin_NCSU.14 <- function (vsp = NULL, data = NULL, 
                    ..., fun.name="refVSPBin_NSCU.14") {
    #setup
    this.call <- match.call()
    settings <- calcChecks(fun.name=fun.name, ..., data = data)
    #get vsp
    vsp <- getPEMSElement(!!enquo(vsp), data, units="kW/metric ton", 
                          ref.name="vsp")
    ##vsp binning method that just uses vsp
    #ncsu 14 bin method
    #Frey et al 2002/3
    temp <- cut(vsp, right=FALSE,
	        breaks=c(-Inf, -2, 0, 1, 4, 7, 10, 13, 16, 19, 
	                 23, 28, 33, 39, Inf),
	        labels=sprintf("MODE%02d", 1:14),
	        ordered_result=TRUE, exclude=FALSE)
    vsp.bin <- makePEMSElement(temp, units="")
    #output
    pemsOutput(vsp.bin, output = settings$output, data = data,  
                  fun.name = fun.name, this.call = this.call) 
}


refVSPBin_MOVES.23 <- function (vsp = NULL, speed = NULL, data = NULL, 
                    ..., fun.name="refVSPBin_MOVES.23") {

    #setup
    this.call <- match.call()
    settings <- calcChecks(fun.name=fun.name, ..., data = data)
    #get vsp and speed
    vsp <- getPEMSElement(!!enquo(vsp), data, units="kW/metric ton", 
                          ref.name="vsp")
    speed <- getPEMSElement(!!enquo(speed), data=data, units="mph", 
                            ref.name="speed")
    ##modes 23 bin method 
    #Koupal, J., Cumberworth, M. and Beardsley, M., 
    #Introducing MOVES2004, the initial release of EPA's new generation mobile source emission model. Ann Arbor, 1001, p.48105.
    temp <- rep(NA, length(vsp))
#doing this up hand
    temp[vsp < 0 & speed >= 1 & speed < 25] <- "MODE11"
    temp[vsp >= 0 & vsp < 3 & speed >= 1 & speed < 25] <- "MODE12"
    temp[vsp >= 3 & vsp < 6 & speed >= 1 & speed < 25] <- "MODE13"
    temp[vsp >= 6 & vsp < 9 & speed >= 1 & speed < 25] <- "MODE14"
    temp[vsp >= 9 & vsp < 12 & speed >= 1 & speed < 25] <- "MODE15"
    temp[vsp >= 12 & speed >= 1 & speed < 25] <- "MODE16"
    temp[vsp < 0 & speed >= 25 & speed < 50] <- "MODE21"
    temp[vsp >=0 & vsp < 3 & speed >= 25 & speed < 50] <- "MODE22"
    temp[vsp >=3 & vsp < 6 & speed >= 25 & speed < 50] <- "MODE23"
    temp[vsp >=6 & vsp < 9 & speed >= 25 & speed < 50] <- "MODE24"
    temp[vsp >=9 & vsp < 12 & speed >= 25 & speed < 50] <- "MODE25"
    temp[vsp >=12 & vsp < 18 & speed >= 25 & speed < 50] <- "MODE27"
    temp[vsp >=18 & vsp < 24 & speed >= 25 & speed < 50] <- "MODE28"
    temp[vsp >=24 & vsp < 30 & speed >= 25 & speed < 50] <- "MODE29"
    temp[vsp >=30 & speed >= 25 & speed < 50] <- "MODE30"
    temp[vsp < 6 & speed >= 50] <- "MODE33"
    temp[vsp >= 6 & vsp < 12 & speed >= 50] <- "MODE35"
    temp[vsp >= 12 & vsp < 18 & speed >= 50] <- "MODE37"
    temp[vsp >= 18 & vsp < 24 & speed >= 50] <- "MODE38"
    temp[vsp >= 24 & vsp < 30 & speed >= 50] <- "MODE39"
    temp[vsp >= 30 & speed >= 50] <- "MODE40"
    temp[speed < 1] <- "MODE01"
#00 Braking to assign 
    temp1 <- c("MODE00", "MODE01", 
               "MODE11", "MODE12", "MODE13", "MODE14", "MODE15", "MODE16", 
               "MODE21", "MODE22", "MODE23", "MODE24", "MODE25", "MODE27", "MODE28", "MODE29", "MODE30",
               "MODE33", "MODE35", "MODE37", "MODE38", "MODE39", "MODE40")
    temp <- factor(temp, levels=temp1, labels=temp1, ordered=TRUE, 
                   exclude=FALSE)
    vsp.bin <- makePEMSElement(temp, units="")
    pemsOutput(vsp.bin, output = settings$output, data = data,  
              fun.name = fun.name, this.call = this.call) 
}




##########################
##########################
##VSPPlot
##########################
##########################

#kr 14/03/2019 v 0.0.1
#kr 08/03/2021 v 0.1.0  (shifted to package)

#what it does
##########################
# plots vsp visualizations

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

VSPPlot <- function(vsp, em = NULL, 
                        ..., data = NULL,   
                        plot.type = 1, 
                        fun.name="VSPPlot"){
  
  #setup
  extra.args <- list(...)
  settings <- calcChecks(fun.name, ..., data = data)
  
  #get inputs locally
  vsp <- getPEMSElement(!!enquo(vsp), data,
                         if.missing="stop",
                         units="kW/metric ton", fun.name=fun.name)
  em <- getPEMSElement(!!enquo(em), data, if.missing="stop",
                       units="g/s", fun.name=fun.name)
  #these are currently required
  #if.null="return" if we are allowing options without em...?
  
  #names
  xlab <- if(is.null(attributes(vsp)$name)) {
    "vsp"
  } else {
    attributes(vsp)$name
  }
  ylab <- if(is.null(attributes(em)$name)) {
    "em"
  } else {
    attributes(em)$name
  }
  
  #reset extra.args for data rework here
  #terms going to all plot
  
  plot.type <- as.character(plot.type)
  if(!plot.type %in% c("1", "2")){
    stop("In VSPPlot(): plot.type unknown", 
         call. = FALSE)
  }
  
  #plot 1 scatter with loess or break.point
  if(plot.type=="1"){
    extra.args <- loa::listUpdate(list(x=vsp, y=em, 
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
    if(loa::isGood4LOA(extra.args$break.point)){
      ###########################
      #need to think about handling of this
      temp <- do.call(loa::listLoad, loa::listUpdate(extra.args, 
                                           list(load="break.point")))$break.point
      temp <- loa::listUpdate(list(lattice.plot=plt,
                              type="n"), temp)
      plt <- do.call(add.XYBreakPoint, temp)
    }
    
  }
  
  #plot2 boxplot
  if(plot.type=="2"){
    extra.args <- loa::listUpdate(list(x=vsp, y=em, 
                                  data=NULL, xlab=xlab, 
                                  ylab=ylab, type="n",
                                  key.fun=loa::draw.groupPlotKey,
                                  scheme=pems.utils::pems.scheme),
                             extra.args)
#    if(isGood4LOA(extra.args$groups)){
#############################
#think about addNA for groups...       
#############################
#    }
    plt <- rlang::exec(pemsPlot, !!!loa::listHandler(extra.args,
                                                ignore = "breaks"))
    temp <- do.call(loa::listLoad, 
                    loa::listUpdate(extra.args, 
                               list(load="bw")))$bw
    if(is.null(extra.args$stat)){
      extra.args$stat <- if(is.null(extra.args$y)){
        function(x) length(x[!is.na(x)])
      } else {
        function(x) {mean(x, na.rm=TRUE)}
      }
    }
    if(is.null(extra.args$breaks)){
      extra.args$breaks <- seq(min(vsp, na.rm=TRUE), 
                               max(vsp, na.rm=TRUE) + 10,
                               by=10)
    }
    temp <- loa::listUpdate(list(lattice.plot=plt, 
                            breaks=extra.args$breaks), temp)
    plt <- do.call(add.XBinYBarPlot, temp)
  }
  
  plt
}


VSPBinPlot <- function(vspbin, em = NULL,  
                    ..., data = NULL,   
                    plot.type = 1, stat = NULL,
                    fun.name="VSPBinPlot"){
  
  #setup
  extra.args <- list(...)
  settings <- calcChecks(fun.name, ..., data = data)
  
  #get inputs locally
  
  #vspbin 
  #units not defined but needs to be factor
  vspbin <- getPEMSElement(!!enquo(vspbin), data,
                        if.missing="stop",
                        fun.name=fun.name)
  if(!is.factor(vspbin)){
    #is.character?
    #could try to convert then want if 
    stop("In VSPBinPlot(): vspbin not factor...", 
         call. = FALSE)
  }
  
  #em 
  #should we restrict units like in VSPPlot???
  em <- getPEMSElement(!!enquo(em), data, if.missing="stop",
                       if.null="return", fun.name=fun.name)
  if(is.null(em)){
    em <- rep(0, length(vspbin))
    y.in <- FALSE
  } else {
    y.in <- TRUE
  }
  #if.null="return" if we are allowing options without em...?
  #units?
  
  #names
  xlab <- if(is.null(attributes(vspbin)$name)) {
    "vspbin"
  } else {
    attributes(vspbin)$name
  }
  ylab <- if(is.null(attributes(em)$name)) {
    if(y.in) {
      "em"
    } else {
      "count"
    }
  } else {
    attributes(em)$name
  }
  
  #reset extra.args for data rework here
  #terms going to all plot
  plot.type <- as.character(plot.type)
  if(is.null(stat)){
    #if stat not set
    if(y.in){
      #if y available
      stat <- function(x) mean(x, na.rm=TRUE)
    } else {
      if(plot.type!=1){
        warning("In VSPBinPlot(): need y to calculate other stats...",
                "\n\tplotting count",
                call. = FALSE)
        plot.type <- 1
      }
      stat <- function(x) length(x[!is.na(x)])
    }
  } else {
    #if stat set
    if(!y.in){
      #if no y 
      warning("In VSPBinPlot(): need y to calculate other stats...",
              "\n\tplotting count",
              call. = FALSE)
      plot.type <- 1
      stat <- function(x) length(x[!is.na(x)])
    }
  }
  
  if(!plot.type %in% c("1", "2")){
    stop("In VSPBinPlot(): plot.type unknown", 
         call. = FALSE)
  }
  
#if happy with below we could simplify a lot of this... 
  
  #plot1 bar
  #this needs calculating, plotting and resizing
  if(plot.type=="1"){
    extra.args <- loa::listUpdate(list(x=vspbin, y=em, 
                                  data=NULL, xlab=xlab, 
                                  ylab=ylab, type="n", 
                                  scheme=pems.utils::pems.scheme,
                                  key.fun=loa::draw.groupPlotKey),
                             extra.args)
    #    if(isGood4LOA(extra.args$groups)){
    #############################
    #think about addNA for groups...       
    #############################
    #    }
    plt <- rlang::exec(pemsPlot, !!!loa::listHandler(extra.args,
                                                ignore = "breaks"))
    temp <- do.call(loa::listLoad, 
                    loa::listUpdate(extra.args, 
                               list(load="bw")))$bw
    if(is.null(extra.args$breaks)){
      extra.args$breaks <- (1:(length(levels(vspbin))+1))-0.5
    }
    temp <- loa::listUpdate(list(lattice.plot=plt, 
                            stat=stat,
                            breaks=extra.args$breaks), temp)
    plt <- do.call(add.XBinYBarPlot, temp)
  }
  
  #plot2 boxplot
  if(plot.type=="2"){
    extra.args <- loa::listUpdate(list(x=vspbin, y=em, 
                                  data=NULL, xlab=xlab, 
                                  ylab=ylab, type="n",
                                  scheme=pems.utils::pems.scheme,
                                  key.fun=loa::draw.groupPlotKey),
                             extra.args)
    #    if(isGood4LOA(extra.args$groups)){
    #############################
    #think about addNA for groups...       
    #############################
    #    }
    plt <- rlang::exec(pemsPlot, !!!loa::listHandler(extra.args,
                                                ignore = "breaks"))
    temp <- do.call(loa::listLoad, 
                    loa::listUpdate(extra.args, 
                               list(load="bw")))$bw
    if(is.null(extra.args$breaks)){
      extra.args$breaks <- (1:(length(levels(vspbin))+1))-0.5
    }
    temp <- loa::listUpdate(list(lattice.plot=plt, 
                            breaks=extra.args$breaks), temp)
    plt <- do.call(add.XBinYBoxPlot, temp)
  }
  
  plt
}




#####################
#unexported functions
#####################

#should move to loa and tidy this... 

add.XYBreakPoint <- function(lattice.plot=lattice::trellis.last.object(),
                             preprocess = add.XYBreakPoint_prep,
                             model.method = XYBreakPoint,
                             panel = panel.XYBreakPoint, ...){
  x.args <- list(lattice.plot = lattice.plot, ...,
                 model.method = model.method,
                 preprocess = preprocess, panel=panel)
  x.args$grid <- FALSE
  x.args$type <- "l"
  do.call(loa::add.loaPanel, x.args)
}

add.XYBreakPoint_prep<- function(lattice.plot=lattice::trellis.last.object(),
                                 model.method=XYBreakPoint,
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
                    out <- model.method(x=x, y=y, ..., 
                                 group.id=as.character(i))
                    temp <- if(i=="default") 
                      {"all.data"} else {i}
                    out$formula <- paste("brkpnt(", temp, 
                                         ")", sep="")
                    out
                  })
                  names(out) <- temp$group.ids
                  out
                })
  #this right place?
  lattice.plot$panel.args.common$loa.XYBreakPoint <- ans
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

XYBreakPoint <- function(x, y, group.id=NULL, 
                         ...){
  #see handling in loaXYFit_loess
  d <- data.frame(x=as.vector(x), y=as.vector(y))
  d <- d[order(x),]
  d <- na.omit(d)
  #find best break-point
  x <- d$x
  y <- d$y
  ref <- rep(NA, length(x))
  for(i in 1:length(x)){
    x2 <- rep(0, length(x))
    x2[1:i] <- (x[1:i] - x[i]) 
    model <- lm(y~x + x2)
    ref[i] <-summary(model)$adj.r.squared
  }
  #build best model
  ref.best <- which(ref==max(ref, na.rm=TRUE))[1]
  x2 <- rep(0, length(x))
  x2[1:ref.best] <- (x[1:ref.best]-x[ref.best])
  model <- lm(y~x + x2)
  #bk model results
  bp <- predict(model, se=TRUE)
  bp.err <- bp$se.fit
  bp <- bp$fit
  bp.upper <- bp + (3*bp.err)
  bp.lower <- bp - (3*bp.err)
  r2 <- summary(model)$adj.r.squared
  r2 <- paste("(", round(r2, digits=3), ")", 
              sep="")
  ans <- data.frame(y=y, x=x, bp=bp, bp.err=2*bp.err, 
                    bp.lower=bp.lower, 
                    bp.upper=bp.upper,
              r2=r2)
  ans
  
  #nb: returns NULL or ans in last if else
}


panel.XYBreakPoint <- function(...){
  
  plot.args <- loa::listUpdate(list(fit = TRUE, se = TRUE, report = TRUE), 
                                              list(...))
  if (!"group.ids" %in% names(plot.args)) {
    plot.args$group.ids <- "default"
  if (!"groups" %in% names(plot.args)) 
      plot.args$groups <- rep("default", length(plot.args$x))
  }
  if (!"col" %in% names(plot.args)) {
  plot.args$col <- do.call(loa::colHandler, loa::listUpdate(plot.args, 
                                                  list(ref = 1:length(plot.args$group.ids))))
}
if (!"lty" %in% names(plot.args)) 
  plot.args$lty <- rep(1, length(plot.args$group.ids))
if (!"lwd" %in% names(plot.args)) 
  plot.args$lwd <- rep(1, length(plot.args$group.ids))
all.mods <- if ("loa.XYBreakPoint" %in% names(plot.args)) 
  plot.args$loa.XYBreakPoint[[lattice::panel.number()]]
else {
  temp2 <- loa::add.XYFit_prep(list(panel.args.common = plot.args, 
                               panel.args = list(list())))
  temp2$panel.args.common$loa.XYBreakPoint[[1]]
}
for (i in names(all.mods)) {
  mod <- all.mods[[i]]
  if (!is.null(mod$bp)) {
    if (loa::isGood4LOA(plot.args$se)) {
      m.args <- loa::listUpdate(plot.args, do.call(loa::listLoad, 
                                              loa::listUpdate(plot.args, list(load = "se")))[["se"]])
      m.args <- loa::listUpdate(list(group.args = "col", 
                                levels = 3, alpha = 0.75, border = FALSE), 
                           m.args)
      for (j in m.args$group.args) {
        if (j %in% names(m.args)) 
          m.args[[j]] <- m.args[[j]][m.args$group.ids == 
                                       i]
      }
      m.args$alpha <- m.args$alpha/m.args$levels
      for (k in c(m.args$levels:1)) {
        m.args$x <- c(mod$x, rev(mod$x))
        m.args$y <- c(mod$bp + (k * mod$bp.err), 
                      rev(mod$bp - (k * mod$bp.err)))
        do.call(lattice::panel.polygon, m.args)
      }
    }
    if (loa::isGood4LOA(plot.args$fit)) {
      m.args <- loa::listUpdate(plot.args, do.call(loa::listLoad, 
                                              loa::listUpdate(plot.args, list(load = "fit")))[["fit"]])
      m.args <- loa::listUpdate(list(group.args = c("col", 
                                               "lty"), type = "l"), m.args)
      m.args$group.args <- c("col", "lty", 
                             "lwd")
      for (j in m.args$group.args) {
        if (j %in% names(m.args)) 
          m.args[[j]] <- m.args[[j]][m.args$group.ids == 
                                       i]
      }
      m.args$x <- mod$x
      m.args$y <- mod$bp
      m.args$subscripts <- 1:length(m.args$x)
      do.call(lattice::panel.xyplot, m.args)
    }
  }
}
if (loa::isGood4LOA(plot.args$report)) {
  m.args <- loa::listUpdate(plot.args, do.call(loa::listLoad, 
                                               loa::listUpdate(plot.args, 
                                                               list(load = "report")))[["report"]])
  m.args <- loa::listUpdate(list(position = c(0.15, 0.85)), 
                       m.args)
  report.mod.form <- unlist(lapply(all.mods, function(x) x$formula[1]))
  report.mod.r2 <- unlist(lapply(all.mods, function(x) x$r2[1]))
  refs <- plot.args$group.ids %in% unique(names(report.mod.form), 
                                          names(report.mod.r2))
  lines <- list(col = c(NA, plot.args$col[refs]), 
                lty = c(1, plot.args$lty[refs]), 
                lwd = c(1, plot.args$lwd[refs]), 
                size = 0.9)
  formulas <- c("Fit", report.mod.form)
  r2 <- c(NA, report.mod.r2)
  r2[1] <- expression(paste("(", R^2, ")", 
                            sep = ""))
  key.gf <- lattice::draw.key(list(lines = lines, text = list(formulas, 
                          cex = 0.65), text = list(r2, cex = 0.65), between = 0.9, 
                          padding.text = 1, border = 1, background = "white"), 
                     draw = FALSE)
  vp <- grid::viewport(x = grid::unit(m.args$position[1], "npc") + 
                       grid::unit(0.5 - m.args$position[1], "grobwidth", 
                        list(key.gf)), y = grid::unit(m.args$position[2], "npc") + 
                   grid::unit(0.5 - m.args$position[2], "grobheight", 
                        list(key.gf)))
  grid::pushViewport(vp)
  grid::grid.draw(key.gf)
  grid::upViewport()
}
}




#XBinYBar

#this is same as previous
add.XBinYBarPlot <- function(lattice.plot=lattice::trellis.last.object(),
                             preprocess = add.XBinYBarPlot_prep,
                             model.method = XBinYBarPlot,
                             stat = NULL,
                             panel = panel.XBinYBarPlot, ...){
  x.args <- list(lattice.plot = lattice.plot, ...,
                 model.method = model.method, 
                 stat=stat,
                 preprocess = preprocess, panel=panel)
  x.args$grid <- FALSE
  x.args$type <- "l"
  do.call(loa::add.loaPanel, x.args)
}

add.XBinYBarPlot_prep<- function(lattice.plot=lattice::trellis.last.object(),
                                 model.method=XBinYBarPlot,
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
  lattice.plot$panel.args.common$loa.XBinYBarPlot <- ans
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
  
  lims.bw <- lapply(ans, function(x){
    ans <- lapply(x, function(y){
      if(is.data.frame(y)){
        range(c(y$y1, y$y2, na.rm=TRUE))
      } else {c(NA,NA)}
    })
    ans <- range(do.call(c, ans), na.rm=TRUE)
    temp <- (ans[2]-ans[1])/50
    c(ans[1]-temp, ans[2]+temp)
  })
  if(is.list(lattice.plot$y.limits)){
    for(i in 1:length(lattice.plot$y.limits)){
      lattice.plot$y.limits[[i]] <- range(
        lims.bw[[i]],
        #c(lattice.plot$y.limits[[i]], lims.bw[[i]]),
        na.rm=TRUE
      ) 
    }
  } else {
    lattice.plot$y.limits <- range(
      lims.bw,
      #c(lattice.plot$y.limits, lims.bw),
      na.rm=TRUE
    )
  }
  
  #output
  lattice.plot
}

XBinYBarPlot <- function(x, y, group.id=NULL, 
                         breaks = NULL, stat = NULL,
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
    if(is.null(stat)){
      stat <- function(x){ mean(x, na.rm=TRUE)}
    }
    ans <- lapply(1:(length(x.range)-1), function(xx){
      x1 <- x[x >= x.range[xx] & x < x.range[xx+1]]
      y1 <- y[x >= x.range[xx] & x < x.range[xx+1]]
      temp <- stat(y1)
      #list(df = data.frame(x=x1,y=y1, ref=xx),
      #     box = temp)
      data.frame(x1 = x.range[xx], x2=x.range[xx+1],
                 y1=0, y2=temp)
    })
    return(do.call(rbind, ans))
  }
  #nb: returns NULL or ans in last if else
}


panel.XBinYBarPlot <- function(...){
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
    plot.args$col <- do.call(loa::colHandler, 
                             loa::listUpdate(plot.args, 
                                            list(ref = 1:length(plot.args$group.ids))))
  }
  #  if (!"lty" %in% names(plot.args)) 
  #    plot.args$lty <- rep(1, length(plot.args$group.ids))
  #  if (!"lwd" %in% names(plot.args)) 
  #    plot.args$lwd <- rep(1, length(plot.args$group.ids))
  all.mods <- if ("loa.XBinYBarPlot" %in% names(plot.args)){ 
    #print("all.mods")
    plot.args$loa.XBinYBarPlot[[lattice::panel.number()]]
  } else {
    temp2 <- add.XBinYBarPlot_prep(list(panel.args.common = plot.args, 
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
          m.args <- loa::listUpdate(plot.args, 
                                    do.call(loa::listLoad, 
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
          lattice::lrect(x1, mod$y1, x2, mod$y2,
              border = m.args$line.col,
              col=m.args$col) #, alpha=m.args$alpha)
          ##lsegments(x1=x1, x2=x2, 
          ##        y1=mod$y3, y2=mod$y3, 
          ##        col=m.args$line.col)
          #wisk
          ##lsegments(x1=x1, x2=x2, 
          ##        y1=mod$y1, y2=mod$y1, 
          ##        col=m.args$line.col)
          ##lsegments(x1=x1, x2=x2, 
          ##        y1=mod$y5, y2=mod$y5,
          ##        col=m.args$line.col)
          ##lsegments(x1=x.mid, x2=x.mid, 
          ##        y1=mod$y1, y2=mod$y2,
          ##        col=m.args$line.col)
          ##lsegments(x1=x.mid, x2=x.mid, 
          ##        y1=mod$y4, y2=mod$y5,
          ##        col=m.args$line.col)
          #oulaying points
          ##temp <- listHandler(plot.args, use=c("x", "y"))
          ##if("groups" %in% names(plot.args)){
          ##  temp$x <- temp$x[plot.args$groups==i]
          ##  temp$y <- temp$y[plot.args$groups==i]
          ##}
          ##ref <- rep(FALSE, length(temp$x))
          ##for(j in 1: nrow(mod)){
          ##  ref[temp$x>=mod$x1[j] & temp$x<mod$x2[j]
          ##    & temp$y<mod$y1[j]] <- TRUE
          ##  ref[temp$x>=mod$x1[j] & temp$x<mod$x2[j]
          ##    & temp$y>mod$y5[j]] <- TRUE
          ##}
          ##temp$x <- temp$x[ref]
          ##temp$y <- temp$y[ref]
          ##temp$col <- plot.args$col[plot.args$group.ids==i]
          ##temp$pch <- 20
          ##do.call(lpoints, temp)
        }
      }
    }
  }
}


