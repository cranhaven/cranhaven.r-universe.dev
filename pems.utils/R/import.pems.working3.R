##########################
##########################
#various pems imports
##########################
##########################


#additional (4)

#work in progress


#############################
#############################
##importSEMTECH2PEMS
#############################
#############################

#quick importer for Sensors Inc SEMTECH data
#started 03/03/2016
#kr v0.0.2 (17-02-2017)


#?? functions (see note)
###################
#importSEMTECH2PEMS
#

#currently very crude I have very few examples to work on
#from [client053] and 3DATX [and sent to them by non-client018] 


###################
#to do
###################
#tidy
#


###################
#notes
###################
#

###################
#importSEMTECH2PEMS
###################


importSEMTECH2PEMS <- function (file.name = file.choose(), history = NULL, 
    constants = NULL, pems = "SEMTECH", ...) 
{

    #setup
    this.call <- match.call()
    fun.name <- "importSEMTECH2PEMS"
    extra.args <- list(...)

    #lower case names 
    #current default is not to
########################
#follow *** lead on this
########################
    to.lower <- if ("to.lower" %in% names(extra.args)) 
        extra.args$to.lower
    else FALSE
    extra.args <- extra.args[!names(extra.args) %in% "to.lower"]

    time.format <- if ("time.format" %in% names(extra.args)) 
        extra.args$time.format
    else "%m/%d/%Y %H:%M:%OS"
    extra.args <- extra.args[!names(extra.args) %in% "time.format"]

    #drop.source.date.time 
    #current default is not to
########################
#follow *** lead on this
########################
    drop.source.date.time <- if ("drop.source.date.time" %in% names(extra.args)) 
        extra.args$drop.source.date.time
    else FALSE
    extra.args <- extra.args[!names(extra.args) %in% "drop.source.date.time"] 

    #make.names.unique 
    #current default is not to
########################
#follow *** lead on this
########################
    make.names.unique <- if ("make.names.unique" %in% names(extra.args)) 
        extra.args$make.names.unique
    else TRUE
    extra.args <- extra.args[!names(extra.args) %in% "make.names.unique"] 


    #get headers and units from data source
    extra.args <- loa::listUpdate(list(header = FALSE, stringsAsFactors=FALSE, 
                                       nrow=3), 
                                  extra.args)
    extra.args$file <- file.name
    headers <- do.call(read.csv, extra.args)

    #exatract units
    units <- as.character(headers[3,])
    units <- gsub("n/a", "", units)

    #get data from data source
    extra.args <- extra.args[!names(extra.args) %in% "nrow"]
    extra.args <- loa::listUpdate(list(skip=3, na.strings=c("NA", "", "n/a")), 
                                  extra.args)
    data <- do.call(read.csv, extra.args)

    #name data
    #have to come back to this later
    names(data) <- as.character(headers[2,])    
    
    #add in time.stamp and local.time
    time.stamp <- paste(data$sDATE, data$sTIME, sep = " ")
    temp <- time.stamp[1]
    time.stamp <- as.POSIXct(strptime(time.stamp, format = time.format))
    #check nothing appears to have gone wrong
    if(all(is.na(time.stamp)))
         warning("time.format [", time.format, "] data [", temp, "] mismatch", call. = FALSE)
    local.time <- as.numeric(time.stamp - time.stamp[1])
    units <- c("Y-M-D H:M:S", "s", units)

    #if asked to, remove source date and time record 
    if(drop.source.date.time)
        data <- data[names(data)[!names(data) %in% c("sDATE", "sTIME")]]

    #check for evidence of different time formatting
    if (any(is.na(local.time)) || any(diff(local.time) < 0)) 
        warning("possible clocking issue with time stamp")
    data <- cbind(data.frame(time.stamp=time.stamp, local.time=local.time), data)

    #name and unit resets
    #do these before to.lower, make.names, etc 
    #so names are what we expect, etc.
    
    #name resets
    ##names[names(ans) == "old.name"] <- "new.name"

    #unit resets
    ##units <- rep("", length(names(ans)))
    ##units[names(ans) == "time.stamp"] <- "Y-M-D H:M:S"
    
    #make names uniques 
    #because some replication of names/info...
    names(data) <- make.names(names(data), make.names.unique)    

    #convert names to lower case if required
    if (to.lower) 
        names(data) <- tolower(names(data))

#########################
#could drop empty colums?
#########################

    #output
################
#could be tider
################
#this is tider than previous 
#but still needs work
    output <- makePEMS(x = data, units = units, constants = constants, 
        history = this.call, pems = pems, ...)
#no longer tracking history
#    output[["history"]] <- list(output[["history"]][[1]])
    return(output)
}
