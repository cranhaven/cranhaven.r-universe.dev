##########################
##########################
##referencing pems objects
##########################
##########################

#these use checkInput and pemsOutput

#description
##########################
#functions to make ref vectors for pems

#includes exported
##########################
#refRow
#refX
#refEngineOn
#refDrivingMode

#includes unexported
##############################
#refTemplate as structure example

#comments
##########################
#dropped ref() because function was not getting used  
#kr 23/01/2012 v 0.0.6 (then cutBy)
#   01/01/2021 v 0.1.0 (discarded)
#what it does
##########################
#wrapper for cutting data

#to do about/think about
#########################
#threshold handling, if units option? internal common function?




################################
#refRow
################################

#kr 23/01/2012 v 0.0.6 (then cutByRow)
#   01/01/2021 v 0.1.0 (renamed when redrafted)
#what it does
#creates ref by row
# n = number of groups
# breaks = rows to make group breaks at


refRow <- function(ref = NULL, n = 4, breaks = NULL, 
                   data = NULL, ..., labels = NULL,
                   fun.name = "refRow"){
  
    #setup
    this.call <- match.call()
    settings <- calcChecks(fun.name, ..., data = data)

    #get supplied data 
    ref <- getPEMSElement(!!enquo(ref), data, fun.name=fun.name,
                            if.missing = settings$if.missing,
                            unit.conversions = settings$unit.conversions)

    #special case 
    #we can work with vector or data.frame here...
    if(is.data.frame(ref)){
      if(is.null(data)){
        data <- ref
      }
      #should we stop this if different data supplied?
      ref <- 1:nrow(ref)
    } 
    #method tracks ref value 
    #if ref supplied directly, convert to vector id...
    #maybe not tidiest fix...
    ref <- 1:length(ref)
    
    #if n and rows are not numeric 
    if(!is.numeric(n) & !is.numeric(breaks)){
        checkIfMissing(if.missing = settings$if.missing, 
             reply = "require at least numeric 'n' or 'breaks' to reference something!", 
             suggest = "check ?ref", if.warning = "returning NULL", 
             fun.name = fun.name)
        return(NULL)  
    }

    if(is.numeric(n) & !is.numeric(breaks)){
        if(n > length(ref)) n <- length(ref)
        breaks <- length(ref)/n[1]
        breaks <- seq(1, length(ref), by = breaks)
    } else {
      breaks <- unique(c(1, breaks))
    }
    #breaks
    breaks <- floor(breaks)
    if(min(breaks,na.rm=TRUE)>min(ref, na.rm=TRUE)){
      breaks <- c(min(ref, na.rm=TRUE), breaks)
    }
    if(max(breaks,na.rm=TRUE)<max(ref, na.rm=TRUE)){
      breaks <- c(breaks, max(ref, na.rm=TRUE))
    }
    breaks <- unique(breaks)
    #cut
    cut <- cut(ref, breaks, 
               include.lowest=TRUE)
    #with cut handling labels after
    cut <- pems_tidyFactorLabels(cut, labels)
    cut <- pems.element(cut, name="refRow", units="refID")
    #output
    pemsOutput(cut, output = settings$output, data = data,  
               fun.name = fun.name, this.call = this.call)
}


###########################
#refX
###########################

#kr 10/01/2021 v 0.0.1
#creates ref by value of x
# n = number of groups
#     divide methods percentile and range
# breaks = start value of new group


refX <- function(ref = NULL, n = 4, breaks = NULL,
                 method = "percentile", 
                 data = NULL, ..., labels = NULL,
                 fun.name = "refX"){
  
  #setup
  this.call <- match.call()
  settings <- calcChecks(fun.name, ..., data = data)
  
  #get supplied data 
  ref <- getPEMSElement(!!enquo(ref), data, fun.name=fun.name,
                        if.missing = settings$if.missing,
                        unit.conversions = settings$unit.conversions)
  
  #if n and rows are not numeric 
  if(!is.numeric(n) & !is.numeric(breaks)){
    checkIfMissing(if.missing = settings$if.missing, 
                   reply = "require at least numeric 'n' or 'breaks' to reference something!", 
                   suggest = "check ?ref", if.warning = "returning NULL", 
                   fun.name = fun.name)
    return(NULL)  
  }
  
  switch(tolower(method),
                 percentile={method <- function(ref,n) 
                   as.numeric(quantile(ref, probs=seq(0, 1,
                                                      length.out=n+1),
                                                      na.rm=TRUE))},
                 range={method <- function(ref,n) 
                   seq(min(ref, na.rm=TRUE), max(ref, na.rm=TRUE),
                       length.out=n+1)}
  )
  if(!is.function(method)){
      stop("method not known")
  }       
           
  #breaks
  if(is.numeric(n) & !is.numeric(breaks)){
    breaks <- method(ref, n)
  }
  if(min(breaks,na.rm=TRUE)>min(ref, na.rm=TRUE)){
    breaks <- c(min(ref, na.rm=TRUE), breaks)
  }
  if(max(breaks,na.rm=TRUE)<max(ref, na.rm=TRUE)){
    breaks <- c(breaks, max(ref, na.rm=TRUE))
  }
  breaks <- unique(breaks)
  #cut
  cut <- cut(ref, breaks,
             include.lowest=TRUE)
  #with cut handling labels after
  cut <- pems_tidyFactorLabels(cut, labels)
  cut <- pems.element(cut, name="refX", units="refID")
  #output
  pemsOutput(cut, output = settings$output, data = data,  
             fun.name = fun.name, this.call = this.call)
}




########################################
#refEngineOn
########################################

#kr 16/01/2021 v 0.0.1
#is engine ON?
#using engine speed > 200 rpm 

#to do
################################
#think about threshold handling 
#error message when rpm is not rpm not informative...
#think about other measures, e.g. conc.co2 or em.co2?
#

refEngineOn <- function(rpm = NULL, data = NULL, 
                        threshold = 200, ..., 
                        labels = NULL, fun.name = "refEngineOn"){
  
  #setup
  this.call <- match.call()
  settings <- calcChecks(fun.name, ..., data = data)
  
  #get supplied data 
  rpm <- getPEMSElement(!!enquo(rpm), data, units="rpm",
                        fun.name=fun.name,
                        if.missing = settings$if.missing,
                        unit.conversions = settings$unit.conversions)
  #build output
  out <- rep(NA, length(rpm))
  out[rpm<=threshold] <- "OFF"
  out[rpm>threshold] <- "ON"
  out <- factor(out)
  out <- pems_tidyFactorLabels(out, labels)
  
  #configure output
  out <- pems.element(out, name="refEngineOn", units="refID")
  #output
  pemsOutput(out, output = settings$output, data = data,  
             fun.name = fun.name, this.call = this.call)
}




########################################
#refDrivingMode (not exported)
########################################

#kr 17/01/2021 v 0.0.1
#reference by driving mode 
#driving mode
#decel, idle, cruise or accel based on 
#speed, accel and thresholds 

#to do
###########################################
#threshold handling needs thinking about
#need to document method citation, one of frey papers
#need to think about engine on/off handling?

refDrivingMode <- function(speed = NULL, accel = NULL,
                        time = NULL, data = NULL, 
                        threshold.speed = 0.1, 
                        threshold.accel = 0.1,
                        ..., 
                        labels = NULL,
                        fun.name = "refDrivingMode"){
  
  #setup
  this.call <- match.call()
  settings <- calcChecks(fun.name, ..., data = data)
  #x.args <-list(...)
  
  #get supplied data 
  speed <- getPEMSElement(!!enquo(speed), data, fun.name=fun.name,
                        units="m/s", if.null = "return",
                        if.missing = settings$if.missing,
                        unit.conversions = settings$unit.conversions)
  accel <- getPEMSElement(!!enquo(accel), data, fun.name=fun.name,
                          units="m/s/s", if.null = "return",
                          if.missing = settings$if.missing,
                          unit.conversions = settings$unit.conversions)
  time <- getPEMSElement(!!enquo(time), data, fun.name=fun.name,
                          units="s", if.null = "return",
                          if.missing = settings$if.missing,
                          unit.conversions = settings$unit.conversions)
  
  #build accel if missing
  if(is.null(accel)){
    if(!is.null(speed) & !is.null(time)){
      accel <- calcAccel(speed, time)
    } else {
      stop("Need speed and accel or enough to build both")
    }
  }
  
#lods need fine tuning
#maybe link levels here and below?
#thresholds currently handling in units I assign
#might be better to make this supplied units
#or accept a pems.element of units whatever  
  
  #build output
  out <- rep(NA, length(speed))
  out[!is.na(speed)] <- "cruise"
  out[speed< threshold.speed] <- "idle"
  #accel and decel before idle?
  #or as else to is.idle
  out[accel> threshold.accel] <- "accel"
  out[accel< -threshold.accel] <- "decel"
  
  #configure output
  out <- factor(out, levels=c("decel", "idle", "cruise", "accel"))
  out <- pems_tidyFactorLabels(out, labels)
  out <- pems.element(out, name="refDriveMode", units="refID")
  #output
  pemsOutput(out, output = settings$output, data = data,  
             fun.name = fun.name, this.call = this.call)
}


########################################
#unexported code
########################################

#refTemplate (not exported)
#kr 10/01/2021 v 0.0.1

#template for other refs
#currently not exporting

refTemplate <- function(ref = NULL, data = NULL, ..., 
                        labels = NULL,
                        fun.name = "ref"){
  
  #setup
  this.call <- match.call()
  settings <- calcChecks(fun.name, ..., data = data)
  
  #get supplied data 
  ref <- getPEMSElement(!!enquo(ref), data, fun.name=fun.name,
                        if.missing = settings$if.missing,
                        unit.conversions = settings$unit.conversions)
  #build output
  
  #configure output
  out <- pems_tidyFactorLabels(out, labels)
  out <- pems.element(out, name="ref", units="refID")
  #output
  pemsOutput(out, output = settings$output, data = data,  
             fun.name = fun.name, this.call = this.call)
}


#pems_tidyFactorLabel

#kr 17/01/2021 v 0.0.1
#common label handling for ref functions.

#to do
#tidy warning message

pems_tidyFactorLabels <- function(fctr, labels){

  if(!is.null(labels)){
    if(length(levels(fctr))==length(labels)){
      levels(fctr) <- labels
    } else {
      if(length(labels)==1){
        levels(fctr) <- paste(labels, levels(fctr), sep="")
      } else {
        warning("ignoring labels...")
      }
    }
  }
  fctr
}

