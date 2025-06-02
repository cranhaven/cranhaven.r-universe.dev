##########################
##########################
#various pems imports
##########################
##########################

#kr 10/07/2011 

#includes 
#(functions/code below) 
##########################
#import2PEMS
#  introducing new code (so have .old)
#importTAB2PEMS
#importCSV2PEMS
#importOBS2PEMS
#importOB12PEMS
#

#misc
##########################
# getUnitsFromNames


#to do
##########################
#other importers
##
#LCC GPS
#FTIR
#VBOX
#Driver Behaviour
#others as supplied
#CATI
#
##########################
#importOBS2PEMS
##
#fuel handling
#log.rate correction
#tidy code 

#comments
##########################



##########################
##########################
##import2PEMS
##########################
##########################


#kr 01/02/2012 v 0.1.0
#      mods 2024/12/18

#what it does
##########################
#import simple files to PEMS
#
#expects time.stamp and local.time to identified
#expects units to be assigned
#

#to do
##########################
#tidy code


#comments
##########################

import2PEMS <- function(file.name = file.choose(), ..., 
                            file.reader = read.delim, output="pems"){

#think about this
  
  # with get... functions => rationalisation of import2PEMS v0.1
  file <- file.name
  
  #set defaults
##################
#june updates
# like to tidy some of this...
##################
  args <- loa::listUpdate(list(file = file, file.reader = file.reader, 
                          as.is = TRUE, names=1),
                     list(...))
  if(!"header" %in% names(args)){
    args$header = FALSE
  }
  if(!"data.from" %in% names(args)){
    args$data.from <- if(is.numeric(args$names)) {
      max(args$names, na.rm=TRUE) + 1
    } else {
      1
    }
  }
  if("data.from" %in% names(args) & !"skip" %in% names(args)){
    args$skip = args$data.from-1
  }
  
##################
  
  # to do 
  # file.type reset
  
  #get data
  #this will use any skips, etc in the import then strip them from args
  # could simply but NOT sure if other terms will kill it..?
  temp <- getFormalNames(file.reader, read.table)
  data <- do.call(file.reader, args[names(args) %in% temp])
  args <- args[!names(args) %in% temp]  #strip out the file.reader settings
  
  #if required get names from file
  if("names" %in% names(args) && length(args$names)==1)
    { args$names <- getNamesFromFileRowNum(file, file.reader, args$names) }
  
  #if required get units from file or names
  if("units" %in% names(args) && length(args$units)==1){
    if(is.numeric(args$units))
      args$units <- getNamesFromFileRowNum(file, file.reader, args$units)
    else if(is.character(args$units)){
      if(tolower(args$units)=="get.from.names"){
        temp <- getUnitsFromNames(args$names, args$prefix, 
                                  args$suffix)
        args$names <- temp$names
        args$units <- temp$units
      }
    }
  }
  
  #update names(data) and units 
  if(!is.null(args$names)) data <- renameData(data, args$names)
  units <- args$units
  args <- stripFormals(getNamesFromFileRowNum, getUnitsFromFileRowNum,
                       getUnitsFromNames, args=args)

#from this point forward we could have a pems.list  
  
  #if required get time.stamp info from data and fromat time.stamp
  if(!is.logical(args$time.stamp) || args$time.stamp) {               #don't do if time.stamp=FALSE
     if(any(c("time.stamp", "date", "time") %in% names(args)))
        data$time.stamp <- getTimeStampFromData(args$time.stamp, 
                                                args$date, args$time,
                                                data)
     if("time.stamp" %in% names(data)){
        data <- setDataTimeStamp(data, args$time.format, 
                                 args$tz, output="data")  
        temp <- if(is.null(args$tz)) "Y-M-D H:M:S UTC" else 
        paste("Y-M-D H:M:S", args$tz, sep=" ")
        #check this has worked NOT died..?
        units[which(names(data)=="time.stamp")] <- temp
     }
  }
  args <- stripFormals(getTimeStampFromData, setDataTimeStamp, args=args)

  #if required get local.time info from data
  if(!is.logical(args$local.time) || args$local.time) {               #don't do if local.time=FALSE
     if("local.time" %in% names(args))
        data$local.time <- getLocalTimeFromData(args$local.time, data)
     if("local.time" %in% names(data)){
        #check units handled properly?
        units[which(names(data)=="local.time")] <- "s"
     }
  }
  args <- stripFormals(getLocalTimeFromData, args=args)  
  
  #if required tidy pems list
  #think about this...
  temp <- tidyPEMSList(loa::listUpdate(list(x=data, units=units), args))
                  
  #output
  if(output=="data") return(temp$x)
  if(output=="list") return(temp)
  do.call(pems, temp)
}


import2PEMS.old <- function(file.name = file.choose(), names = NULL, units = NULL,  
                        time.stamp = NULL, local.time = NULL, time.format = NULL, 
                        constants = NULL, history = NULL, 
                        ..., file.type = NULL, file.reader = read.delim){

#could rationalise a lot of this
#listload + formal stripping?

    #setup
    this.call <- match.call()   #for history, might not need this anymore...
    file <- file.name           #in case we need to read the file several time...
    extra.args <- list(...)     #for hidden args, might not need...

    #time.format
    if(is.null(time.format))
        time.format <- "%d/%m/%Y %H:%M:%OS"

    ##################
    #file.type code to add
    ##################
    
    #local tidies
    skip <- 0
    header <- TRUE

    #get units (and names if null) in file 
    if(is.character(units) && length(units)==1){
        temp <- file.reader(file, header=FALSE, as.is=TRUE, nrow=4) #currently assuming into is in first 4 rows
        if(units=="get.from.header"){
            extra.args <- do.call(loa::listLoad, loa::listUpdate(extra.args, list(load="units")))
            temp <- do.call(getUnitsFromNames, loa::listUpdate(list(names=as.character(temp[1,]), output="all"), extra.args$units))
            units <- temp$units
            if(is.null(names)) names <- temp$names
        } else if(units=="get.from.row.2"){
            units <- as.character(temp[2,])
            names <- as.character(temp[1,])
            skip <- if("skip" %in% names(extra.args)) extra.args$skip else 2
            header <- FALSE
        }
        extra.args <- extra.args[names(extra.args)!="units"]
    }

#    if(is.character(units) && length(units)==1 && units=="get.from.header"){
#        extra.args <- do.call(listLoad, extra.args, list(load="units"))
#        temp <- file.reader(file, header=FALSE, as.is=TRUE, nrow=1)
#        temp <- do.call(getUnitsFromNames, listUpdate(list(names=temp, output="all"), extra.args$units))
#        extra.args <- extra.args[names(x=extra.args!="units"]
#        units <- temp$units
#        names <- temp$names
#    }
#    if(is.character(units) && length(units)==1 && units=="get.from.row.2"){
#        temp <- file.reader(file, header=FALSE, as.is=TRUE, nrow=2)
#        units <- as.character(temp[2,])
#        names <- as.character(temp[1,])
#        skip <- if("skip" %in% names(extra.args)) extra.args$skip else 2
#        header <- FALSE
#    }


##################
#this needs better handling
#but can seem to document the above string
###################

    #get data

#############
#get formals of function
#use them to decide what to pass on in import
#then strip those before going on
#############

    data <- file.reader(file, header=header, skip=skip)

###################
#think about this 
#unit and name handling
###################

    if(!is.null(names)) names(data) <- make.names(names, unique=TRUE)
    if(!is.null(units)) units <- units

################
#not always GMT!
################

    #sort time.stamp if there
    if(!is.null(time.stamp)){

        if(is.numeric(time.stamp)){
            data[, time.stamp] <- as.POSIXct(strptime(data[, time.stamp], format = time.format, "GMT"))
            names(data)[time.stamp] <- "time.stamp"
        }

        if(is.character(time.stamp)){
            data[, time.stamp] <- as.POSIXct(strptime(data[, time.stamp], format = time.format, "GMT"))
            names(data)[which(names(data)==time.stamp)] <- "time.stamp"
        }

    }

##################
#sort local.time if there/not there 
##################

#################
#sensible unit handler
##################

    output <- do.call(pems, loa::listUpdate(list(x = data, units = units, constants = constants, 
                       history = history), extra.args)) 
    
    #reset history?
    if(is.null(output[["histroy"]])){
         output[["history"]] <- this.call
    } else {
         temp <- output[["history"]]
         temp[length(temp)] <- this.call
         output[["history"]] <- temp
    }

    output    

}



##########################
##########################
##import2PEMS wrappers
##########################
##########################

#kr 01/02/2012 v 0.1.0

#what it does
##########################
#import a tab delimited or clipboard file to PEMS
#import a comma delimited file to PEMS
#

importTAB2PEMS <- function(..., file.reader = read.delim) import2PEMS(..., file.reader = file.reader)
importCSV2PEMS <- function(..., file.reader = read.csv) import2PEMS(..., file.reader = file.reader)









##########################
##########################
##importOBS2PEMS
##########################
##########################

#kr 10/07/2011 v 0.3.5

#what it does
##########################
#import a standard OBS-1300 tab delimited file
##
#adds time.stamp based on reported start.time/date
#sets units
#


#to do
##########################
#tidy code
#this could be done better now
###################
#foo tidy
##v1 renamed as rename and tidy
##could do a version two and 
##put unnamed functions in sapply calls
###################
#

#comments
##########################


importOBS2PEMS <- function(file.name = file.choose(), pems = "Horiba OBS", 
          constants = NULL, history = NULL, 
          analytes = c("co", "co2", "nox", "hc"),  
          fuel = c("petrol", "diesel", "gasoline"), ...){

################################
#could do this a lot better now
################################

    #setup
    this.call <- match.call()

    #create fuel.constants
    fuel <- checkOption(fuel[1], formals(importOBS2PEMS)$fuel, 
                        "fuel", "known fuel types", 
                        fun.name = "importOBS2PEMS")
    fuel.constants <- list()
    if(fuel == "diesel")
        fuel.constants <- ref.diesel
    if(fuel == "petrol" | fuel == "gasoline")
        fuel.constants <- ref.petrol

##########################
##should this be OS not S?
##########################

    #time and date stamps
    time.stamp <- scan(file.name, nlines = 1, what = character(), quiet=TRUE)
    time.stamp <- paste(time.stamp[2], time.stamp[4], sep=" ")
    time.stamp <- as.POSIXct(strptime(time.stamp, format = "%Y/%m/%d %H:%M:%S", "GMT"))

    #read headers
    data.names <- scan(file.name, skip=1, what = character(), nlines = 1, quiet=TRUE, sep="\t")
    data.names <- gsub(" ", ".", data.names) #replace space with "." in header names
    data.names <- gsub("/", ".", data.names) #replace "/" with "." in n/s and e/w header names
    data.names <- tolower(data.names) #simplify naming
    data.names[1] <- "local.time"

    #rename analytes conc.x
    rename <- function(ans, analyte) #make analyte identifier conc.analyte so emission names are unique 
               {if(ans==analyte) ans <- paste("conc.",ans,sep="") else ans}
    for(i in 1:length(analytes)){ data.names <- sapply(data.names, rename, USE.NAMES=FALSE, analyte=analytes[i]) }

    #read units
    data.units <- scan(file.name, skip=2, what = character(), nlines = 1, quiet=TRUE, sep="\t")
    data.units <- c("Y-M-D H:M:S GMT",data.units)
    tidy<- function(ans) #strip brackets from strings
              {if(!ans=="")  
                  {if(substr(ans,1,1)=="(" & substr(ans,nchar(ans),nchar(ans))==")" ) (substr(ans,2,(nchar(ans)-1))) else ans} 
              else ans}
    data.units <- sapply(data.units, tidy, USE.NAMES=FALSE)

################
#currently gps not handled
################

    #constants
    
#this could be tidier
#move ref.chem out of this 
#then if here use else get from ref.chem?

    temp <- list(log.rate = 1000,
                 delay.co = 3.2, delay.co2 = 3.3, delay.hc = 3.9, delay.nox = 1.6, delay.afr = 1.6,
                 conc.o2 = 20.6, 
                 thc.c6 = 10, pitot.k = 2537.6, pitot.z = 0.015026, setting.gps.port = 0, k.wgec = 3.5, setting.velocity = 1, 
                 setting.720nox = 1, setting.gps = 1, setting.hc = 0, setting.option = "", setting.coco2hc = "", setting.afr = "") 
    temp[names(fuel.constants)] <- fuel.constants
    temp[names(ref.chem)] <- ref.chem
    if(is.list(constants))
        temp[names(constants)] <- constants

    extra.args <- list(...)
    temp[names(extra.args)] <- extra.args

    constants <- temp

    #read data

    data <- read.delim(file.name, header=FALSE, skip=3)

    #fix any mistmatched data and data.names sizes

    if(length(data.names)>ncol(data)){
    
        #add empties
        data[, (ncol(data)+1):length(data.names)] <- NA

    } else if(length(data.names)>ncol(data)){

        #make some missing names
        data.names <- c(data.names, paste("V", (length(data.names)+1):ncol(data), sep=""))

    }

    #add names to main data

    names(data) <- data.names

    #reset for log.rate
    #this is logger interval in ms

#tigthen this for units = seconds?
#probably not needed because it is not right in original

    data$local.time <- data$local.time * (constants$log.rate/1000)

    #pack data

    data<-cbind(time.stamp = (time.stamp + data$local.time), data)

    #gps in d.deg lat, lon
    #set hemisphere

#needs abs? reset for doing signs from scratch
#if used elsewhere

    if("latitude" %in% names(data) | "n.s" %in% names(data)){
        
        #north/south - as lower case 1 character 
        temp <- substr(tolower(as.character(data$n.s)),1,2)
        temp <- ifelse(is.na(temp), "n", temp)

        data$latitude <- ifelse(temp == "n", data$latitude, -data$latitude)
        if(!all(is.na(temp)))
            data.units[which(names(data)=="latitude")] <- "d.degLat"

    }

    if("longitude" %in% names(data) | "w.e" %in% names(data)){
        
        #east/west - as lower case 1 character 
        temp <- substr(tolower(as.character(data$w.e)),1,2)
        temp <- ifelse(is.na(temp), "e", temp)

        data$longitude <- ifelse(temp == "w", -data$longitude, data$longitude)
        if(!all(is.na(temp)))
            data.units[which(names(data)=="longitude")] <- "d.degLon"

    }

    output <- makePEMS(x = data, units = data.units, constants = constants, 
                       history = history, pems = pems, ..., silent = TRUE) 
    
    output

}









##########################
##########################
##importOB12PEMS
##########################
##########################

#kr 12/09/2013 v 0.1.0

#what it does
##########################
#import OB1 files previously created using observer
##
#adds time.stamp based on reported start.time/date
#sets units
#


#to do
##########################
#tidy code
#this could be done better now
###################
#to tidy
###################
#

#comments
##########################


importOB12PEMS <- function(file.name = file.choose(), pems = "Horiba OBS", 
          constants = NULL, history = NULL, 
          analytes = c("co", "co2", "nox", "hc"),  
          fuel = c("petrol", "diesel", "gasoline"), ...){

################################
#could do this a lot better now
################################

    #setup
    this.call <- match.call()

    #create fuel.constants
#not sure fuel matters for OB1
#already set

    fuel <- checkOption(fuel[1], formals(importOBS2PEMS)$fuel, 
                        "fuel", "known fuel types", 
                        fun.name = "importOBS2PEMS")
    fuel.constants <- list()
    if(fuel == "diesel")
        fuel.constants <- ref.diesel
    if(fuel == "petrol" | fuel == "gasoline")
        fuel.constants <- ref.petrol

    #checks
    suspect.tags <- ""

    #read first header
    input <- readLines(file.name, n = 14)
    temp <- strsplit(input[1:6], "\t")

    if(temp[[1]][2]!= "OBServer 1.0.0.")
        suspect.tags <- c(suspect.tags, "file source unrecognised")
    if(temp[[1]][3]!= "OB1 format version 1.1")
           suspect.tags <- c(suspect.tags, "suspect OB1 format")
    
    #date (yyyy/mm/dd) from line 2
    date <- as.character(temp[[2]][2])

    if(length(suspect.tags)>1)
        print(paste(suspect.tags, sep="\n"))
 
    constants <- as.list(as.numeric(temp[[6]]))
    
    #reset names 
    temp <- temp[[5]]
    temp[temp=="Log Rate"] <- "log.rate"
    temp[temp=="CO delay time"] <- "delay.co"
    temp[temp=="CO2 delay time"] <- "delay.co2"
    temp[temp=="HC delay time"] <- "delay.hc"
    temp[temp=="NOx delay time"] <- "delay.nox"
    temp[temp=="AFR delay time"] <- "delay.afr"
    temp[temp=="O2 conc"] <- "conc.o2"

    temp[temp=="CO mol mass"] <- "mm.co"
    temp[temp=="CO2 mol mass"] <- "mm.co2"
    temp[temp=="NOx mol mass"] <- "mm.nox"
    temp[temp=="C mol mass"] <- "mm.c"
    temp[temp=="H mol mass"] <- "mm.h"
    temp[temp=="O mol mass"] <- "mm.0"

    temp[temp=="PITOT k"] <- "pitot.k"
    temp[temp=="PITOT z"] <- "pitot.z"

    temp[temp=="THC/C6"] <- "thc.c6" 
    temp[temp=="Exhaust density"] <- "density.exhaust"
    temp[temp=="Fuel density"] <- "density.fuel"

    temp[temp=="COCO2HC select"] <- "setting.coco2hc"
    temp[temp=="720NOx select"] <- "setting.720nox"
    temp[temp=="K(wgec)"] <- "k.wgec" 
    temp[temp=="H/C"] <- "alpha.hc" 
    temp[temp=="Exh H/C"] <- "alpha.exhaust.hc"
    temp[temp=="GPS port"] <- "setting.gps.port"
    temp[temp=="velocity select"] <- "setting.velocity"
    temp[temp=="O/C"] <- "beta.oc"  
    temp[temp=="GPS select"] <- "setting.gps" 
    temp[temp=="HC select"] <- "setting.hc"
    temp[temp=="OPTION select"] <- "setting.option"
    temp[temp=="AFR select"] <- "setting.afr" 
    temp[temp=="Fuel select"] <- "setting.fuel"
    temp[temp=="Vehicle type"] <- "vehicle.type"

    names(constants) <- temp

    test.fun <- function(x)
                     if(is.na(names(constants)[x])  || names(constants)[x]=="O") 
                         FALSE else TRUE

    constants <- constants[sapply(1:length(constants), test.fun)]

    history <- list(input, this.call)

    units <- read.delim(file.name, skip=14, nrows=1, header=TRUE, stringsAsFactors=FALSE)

    names(units) <- tolower(names(units))
    names(units) <- gsub("real.time.fuel.consumption.by.", "rtfc.", names(units))
    names(units) <- gsub("fuel.consumption.by.", "fc.", names(units))
    names(units) <- gsub("afr.by.", "afr.", names(units))
    names(units) <- gsub("option[.]", "option", names(units))
    names(units) <- gsub(".sensor", "", names(units))
    names(units) <- gsub("pitot.output..retro.calc.", "pitot.output", names(units))

    names(units)[names(units)=="time.1"] <- "time.stamp"
    names(units)[names(units)=="time"] <- "local.time"
    for(i in analytes){
        names(units)[names(units)==i]<- paste("conc.", i, sep="")
        names(units)[names(units)==paste(i, ".1", sep="")]<- paste("em.", i, sep="")
    }
    
    temp <- as.character(units)
    temp <- gsub("[(]", "", temp)
    temp <- gsub("[)]", "", temp)
    temp[temp=="hh:mm:ss"] <- "Y-M-D H:M:S GMT"
    temp[temp=="NA"] <- ""
    temp[temp=="g/sec"] <- "g/s"
    units[] <- temp 

    data <- read.delim(file.name, skip=16, header=FALSE, na.strings = c("NA", "Inf (No Carbon)"))
    names(data) <- names(units)
    data$time.stamp <- paste(date, data$time.stamp, sep=" ")

    #earlier versions of OB1 files did not set lat lon sign
    #later versions did 
    #so cannot assume systematic handling
    #so reset all to positive and redo signs based on n.s and w.e
    
    if("latitude" %in% names(data) & "n.s" %in% names(data)){
        
        #north/south - as lower case 1 character 
        temp <- substr(tolower(as.character(data$n.s)),1,2)
        temp <- ifelse(is.na(temp), "n", temp)

        data$latitude <- abs(data$latitude)
        data$latitude <- ifelse(temp == "n", data$latitude, -data$latitude)

        temp[names(units)=="latitude"] <- "d.degLat"
    }

    if("longitude" %in% names(data) & "w.e" %in% names(data)){
        
        #east/west - as lower case 1 character 
        temp <- substr(tolower(as.character(data$w.e)),1,2)
        temp <- ifelse(is.na(temp), "e", temp)

        data$longitude <- abs(data$longitude)
        data$longitude <- ifelse(temp == "w", -data$longitude, data$longitude)
        
        temp[names(units)=="longitude"] <- "d.degLon"
    }

    output <- makePEMS(x = data, units = units, constants = constants, 
                       history = history, pems = pems, ..., silent=TRUE) 
    
    output
    }






###################################
###################################
##misc
###################################
###################################


###################################
###################################
##getUnitsFromNames
###################################
###################################

#kr 30/12/2017 
#   mod 18/12/2024 
#        note some files with very long headers if different numbers of 
#        rows were killing previous version...
#        did same to units 
#unexported
getNamesFromFileRowNum <- function(file, file.reader, names){
    skip <- if(names<2) { 0 } else { names - 1 }
    names <- file.reader(file, header=FALSE, as.is=TRUE, skip=skip)
    return(as.vector(unlist(names[1,])))
}
#kr 30/12/2017
#unexported
#   mod 18/12/2024
#       just names above with different arg 
#       not sure I am using this any more ???
getUnitsFromFileRowNum <- function(file, file.reader, units){
    skip <- if(names<2) { 1 } else { names - 1 }
    units <- file.reader(file, header=FALSE, as.is=TRUE, skip=skip)
    return(as.vector(unlist(names[1,])))
}

#kr 30/12/2017
#unexported
getUnitsFromNames <- function (names, prefix="(", suffix=")", ..., 
                               output = "all"){

#####################
  #to do
  ###################
  #tighten this up
  #think about handling cases without prefix or suffix? 
  #tidy suffix removal, current assumes it is there and is last character
  # and has 1 character length
  
  if(is.null(prefix)) prefix="("
  if(is.null(suffix)) suffix=")"
  names <- as.character(names) #not sure about this
                               #needed if using read.csv
  test <- lapply(names, function(x) substr(x, nchar(x), nchar(x)) == 
		suffix)
	units <- rep(NA, length(test))
	for (i in 1:length(test)) {
		if (test[[i]]) {
			temp <- gregexpr(paste("[", prefix, "]", sep=""), 
			                 names[[i]])[[1]]
			temp <- temp[length(temp)]
			units[i] <- substr(names[[i]], temp + 1, nchar(names[[i]]) - 
				1)
			names[[i]] <- gsub(" ", "", substr(names[[i]], 1, 
				(temp - 1)))
		}
		else {
			units[i] <- ""
		}
	}
	if (output == "units") 
		return(units)
	if (output == "names") 
		return(names)
	list(names = names, units = units)
}

#kr 30/12/2017
#unexported
#    why does this pass args ???
#    (if goes, would need to change everywhere...) 
getFormalNames <- function(..., args=NULL){
   temp <- lapply(list(...), function(x) names(formals(x)))
   temp <- unique(do.call(c, temp))
   temp[temp!="..."]
} 
#kr 30/12/2017
#unexported
stripFormals <- function(..., args=NULL){
  temp <- getFormalNames(...)
  args[!names(args) %in% temp]
}

#kr 30/12/2017
#unexported
#   mod 18/12/2024 
#       added option to change make.unique rather than make.name 
#       maybe easier in main function??
renameData <- function(data, names, as.names=TRUE){
  #to tidy names 
  #and handle mismatching data cols and names length  
  if(ncol(data)<length(names))
    data[(ncol(data)+1):length(names)]<-NA
  names(data)[1:length(names)] <- if(as.names) 
    {make.names(names, unique=TRUE)} else {make.unique(names)}
  #might need to tidy further if data.cols>names.length ???
  data
}

#kr 30/12/2017
#unexported
setDataTimeStamp <- function(data, time.format="%d/%m/%Y %H:%M:%OS", 
                             tz="UTC", output="time.stamp"){
  #issues
  ######################
  #does not track time.stamp units
  
  if(is.null(time.format)) time.format <- formals(setDataTimeStamp)$time.format
  if(is.null(tz)) tz <- formals(setDataTimeStamp)$tz
  data$time.stamp <- as.POSIXct(strptime(data$time.stamp, format = time.format),
                                tz=tz)
  if(output=="data") return(data)
  data$time.stamp
}

#kr 30/12/2017
#unexported
getTimeStampFromData <- function(time.stamp=NULL, date=NULL, 
                                     time=NULL, data){
  #allows
  #time.stamp = TRUE, 
  #time.stamp = number, 
  #time.stamp = name, 
  #time.stamp = vector of values
  if(!is.null(time.stamp)){
    if(length(time.stamp)==1){
      if(is.character(time.stamp) && time.stamp %in% names(data)) 
        time.stamp <- data[, time.stamp]
      if(is.numeric(time.stamp)) 
        time.stamp <- data[, time.stamp]
      if(is.logical(time.stamp) && time.stamp) 
        time.stamp <- data$time.stamp
    }  
  }
  if(length(time.stamp)==nrow(data)) return(time.stamp)
  if(!is.null(date)){
    if(length(date)==1){
      if(is.character(date) && date %in% names(data)) 
        date <- data[, date]
    } else if(is.numeric(date)) date <- data[, date]
  }
  if(is.null(time)) return(date)
  if(!is.null(time)){
    if(length(time)==1){
      if(is.character(time) && time %in% names(data)) 
        time <- data[, time]
    } else if(is.numeric(time)) time <- data[, time]
  }
  if(is.null(date)) return(time)
  paste(date, time, sep=" ")
}

#kr 30/12/2017
#unexported
getLocalTimeFromData <- function(local.time, data){
  
  local.time <- if(length(local.time)==1){
      if(is.character(local.time)){
        if(local.time=="get.from.time.stamp"){
          if("time.stamp" %in% names(data)) as.numeric(data$time.stamp -
                                                       min(data$time.stamp, 
                                                       na.rm=TRUE))
        } else if(local.time %in% names(data)) data[, local.time]
      } else { 
        if(is.numeric(local.time)) data[, local.time] else 
         if(is.logical(local.time) && local.time) data$local.time
      }
  } 
  #warning if null?
  local.time
}

#kr 30/12/2017
#unexported
tidyPEMSList <- function(pems.ls){
  if("to.lower" %in% names(pems.ls))
    if(is.logical(pems.ls$to.lower) && pems.ls$to.lower)
        names(pems.ls$x) <- tolower(names(pems.ls$x))
  #other tidies?
  pems.ls[!names(pems.ls) %in% c("to.lower")]
}
