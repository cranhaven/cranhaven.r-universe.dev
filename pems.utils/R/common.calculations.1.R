##########################
##########################
##common calculations
##########################
##########################

#kr

#description
##########################
#functions to do common calculates


#includes 
##########################
#calcDistance
#calcSpeed
#calcAccel
#calcJerk

#inlcudes (removing)
##########################
#calcChecks
#calcPack

#currently using (exported)
##########################
#calcChecks
#   uses checkOptions 

#includes (using but not exported)
###########################
#pemsOutput

#to do
##########################
#


#comments
##########################
#template to think about

################################
#pemsOutput
################################

pemsOutput <- function(x, ..., track.name=TRUE){
  
     args <- list(...)
     ln <- quo_name(enquo(x))
     if(is.null(args$output)) args$output <- "input"
     if(args$output=="input") return(x) 
#############
#next line not tracking fun.name at moment
     if(args$output %in% c("pems", "data.frame") && is.null(args$data))
         stop("no data to pack with")
     if(!"pems" %in% class(args$data))
         args$data <- pems(args$data)
     #############
     #change track name when packing x
     #n.x <- quo_name(enquo(x))
     #############

     n.x <- if(track.name && "name" %in% names(attributes(x)))
                  attributes(x)$name else ln
     args$data[n.x, force=c("na.pad.target", "na.pad.insert")] <- x
     if(args$output=="data.frame") args$data <- pemsData(args$data)
     args$data
}






##########################
##########################
##calc...
##########################
##########################

#to update fun.name
#          input through out
#          my.output through out

calcTemplate <- function(input=NULL, data = NULL,
                     ..., fun.name = "calcTemplate"){
#setup
    this.call <- match.call()
    settings <- calcChecks(fun.name=fun.name, ..., data = data)

#input (one per input)
    input <- getPEMSElement(!!enquo(input), data, units="units.i.want", 
                            ref.name="what.i.call.it")

#my assumptions

#my operations
    my.output <- input 

#outputs
    my.output <- pems.element(my.output, name="my.output", units="output.units")
    pemsOutput(my.output, output = settings$output, data = data,  
        fun.name = fun.name, this.call = this.call)
}



############################
#calcDistance
############################

#kr v.02 2018/06/15

calcDistance <- function(speed = NULL, time = NULL, data = NULL,
                     ..., fun.name = "calcDistance"){
    
    #setup
    this.call <- match.call()
    settings <- calcChecks(fun.name, ..., data = data)

################
#think I can simplify setup
#maybe merge with pemsin 
#     so we don't rename data? 
################
    speed <- getPEMSElement(!!enquo(speed), data, units="m/s")
    time <- getPEMSElement(!!enquo(time), data, units="s")

    #my assumption
    #first unit resolution is average of rest
    #rest are time.now - time.last

    temp <- diff(time)
    temp <- c(mean(temp, na.rm=TRUE), temp)

    #my calculation
    distance <- speed * temp

    #my structure
################
#look into this 
#    makePEMSElement versus pems.element
#    also does it keep historical data types...
################
    distance <- pems.element(distance, name="distance", units="m")

    #make output
    pemsOutput(distance, output = settings$output, data = data,  
        fun.name = fun.name, this.call = this.call)

}



#############################
##calcSpeed
#############################

#kr v.02 2018/06/17

calcSpeed <- function(distance = NULL, time = NULL, data = NULL,
                     ..., fun.name = "calcSpeed"){
    
    #setup
    this.call <- match.call()
    settings <- calcChecks(fun.name, ..., data = data)

    #get inputs
    distance <- getPEMSElement(!!enquo(distance), data, units="m")
    time <- getPEMSElement(!!enquo(time), data, units="s")

    #my assumption
    #first unit resolution is average of rest
    #rest are time.now - time.last

    temp <- diff(time)
    temp <- c(mean(temp, na.rm=TRUE), temp)

    #my calculation
    speed <- distance / temp

    #my structure
    speed <- pems.element(speed, name="speed", units="m/s")

    pemsOutput(speed, output = settings$output, data = data,  
        fun.name = fun.name, this.call = this.call)
}








###############################
#calcAccel
###############################


#v0.2 kr 17/06/2018

calcAccel <- function(speed = NULL, time = NULL, data = NULL,
                     ..., method = 2, fun.name = "calcAccel"){
    #setup
    this.call <- match.call()
    settings <- calcChecks(fun.name, ..., data = data)

    #get inputs
################
#think I can simplify setup
#maybe merge with pemsin 
#     so we don't rename data? 
################

    speed <- getPEMSElement(!!enquo(speed), data, units="m/s")
    time <- getPEMSElement(!!enquo(time), data, units="s")

    #my assumption
    #first d.speed/d.time is 0
    #rest are ...now - ....last

    #current 4 methods for speed
    #1 speed[t+1]-speed[t]/time[t+1]-time[t]
    #2 speed[t]-speed[t-1]/time[t]-time[t-1]
    #3 speed[t+1]-speed[t-1]/time[t+1]-time[t-1]
    #4 average of 1 and 2 
    # missing start/ends NAs

    ans <- diff(speed)/diff(time)
    if(method==1) accel <- c(ans, NA)
    if(method==2) accel <- c(NA, ans)
    if(method==3) accel <- c(NA, diff(speed,2)/diff(time,2),NA)
#does method 4 need thinking about... If not 1Hz..?
    if(method==4) accel <- (c(ans, NA) + c(NA, ans))/2
    
    #my structure
    accel <- pems.element(accel, name="accel", units="m/s/s")

    #make output
    pemsOutput(accel, output = settings$output, data = data,  
        fun.name = fun.name, this.call = this.call)
}

calcAcceleration <- calcAccel


############################
##calcJerk
############################

#kr v.02 2018/06/17

calcJerk <- function(accel = NULL, time = NULL, data = NULL, 
                     ...,fun.name = "calcJerk"){
    
    #setup
    this.call <- match.call()
    settings <- calcChecks(fun.name, ..., data = data)

    #get inputs
    #accel
    accel <- getPEMSElement(!!enquo(accel), data, units="m/s/s")
    time <- getPEMSElement(!!enquo(time), data, units="s")

    #my assumption
    #first d.accel/d.time is 0
    #rest are ...now - ....last

    d.accel <- diff(accel)
    d.time <- diff(time)

    #my calculation
    jerk <- c(NA, d.accel / d.time)

    #my units
    attr(jerk, "name") <- "jerk"
    attr(jerk, "units") <- "m/s/s/s"
    #my structure
    jerk <- pems.element(jerk, name="jerk", units="m/s/s/s")

    #make output
    pemsOutput(jerk, output = settings$output, data = data,  
        fun.name = fun.name, this.call = this.call)
}






















#############################
#############################
##calcChecks, calcPack
#############################
#############################

#kr 23/01/2012 v 0.0.6

#front end management

#changed data = data to data = null

calcChecks <- function(fun.name = "calcChecks", ..., data = NULL,
                   if.missing = c("stop", "warning", "return"), 
                   output = c("input", "data.frame", "pems", "special"),
                   unit.conversions = NULL, overwrite = FALSE){

    #output handling
    output <- checkOption(output[1], formals(setUnits)$output, 
                       "output", "allowed outputs", 
                       fun.name = fun.name)

    if(output == "special"){
        output <- if(is.null(data))
                      "input" else if(comment(isPEMS(data)) == "other")
                                       "input" else comment(isPEMS(data))
    }

    #if.missing handling
    if.missing <- checkOption(if.missing[1], formals(setUnits)$if.missing, 
                              "if.missing", "allowed if.missings", 
                              fun.name = fun.name)

    list(output = output, if.missing = if.missing, overwrite = overwrite, 
         unit.conversions = unit.conversions)

}



calcPack <- function(output = NULL, data = NULL, settings = NULL, 
               fun.name = "calcPack", this.call = NULL){

    #make output
    output <- checkOutput(input = output, data = data, if.missing = settings$if.missing, 
                          fun.name = fun.name, output = settings$output, 
                          overwrite = settings$overwrite)
#removing history

#    if(isPEMS(output)){
#        old.class <- class(output)
#        class(output) <- "not.pems"
#        output$history[[length(output$history)+1]] <- this.call
#        class(output) <- old.class 
#    }

    output
    
}



