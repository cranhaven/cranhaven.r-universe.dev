########################
########################
##export.pems.data
########################
########################

#in place
#################
#pemsData
#



#TO DO
################
#tidy
#document
#



#questions
###################
#develop these functions as requested? 
###################
#think about date.stamp output save as d-m-yyyy hh:min:sec option? 
###################
#think about a way to save more pems structure?
###################
#think about exportPEMSData, exportPEMS2CSV, etc? 
#   This is maybe not very intuitive?  


#################################
#################################
#working
#not exported
#################################
#################################

########################
########################
##exportPEMS
########################
########################

#version 0.1.0
#karl 31/03/2019


exportPEMS <- function(pems, file = "tempfile", file.writer = write.table, sep = "\t", 
                       ...){

     #this take a pems object, tidies pems, file(name) and time.stamp if requested 
     #and saves it as file(name) 

     args <- list(...)

     #tidies

     #remove special columns
     #not sure of the source of ..count 
     #might want to track down and fix? 
     #currently always applied...
     if("..count" %in% names(pems))
         pems <- pems[names(pems) != "..count"]

     #tidy file extension unless cancelled, tidy.file = FALSE
     file <- addExtension2FileIfMissing(file = file, sep = sep, ...)
     #reset time.stamp unless cancelled
     pems <- resetDataTimeStamp(pems, ...)
     #add unit extensions if requested
     pems <- addUnitsToNames(pems, ...)

     #strip used args 
     args <- stripFormals(addExtension2FileIfMissing, resetDataTimeStamp, 
                          addUnitsToNames, args=args)

     #file.writer call
     args <- loa::listUpdate(list(as.data.frame(pems), file=file, sep=sep), 
                        args)
     do.call(file.writer, args)
} 


########################
########################
##exportPEMS2TAB
########################
########################

#version 0.1.0
#karl 31/03/2019


exportPEMS2TAB <- function(pems, file = "tempfile", file.writer = write.table, sep = "\t",  
                           ...){
     args <- loa::listUpdate(list(pems, file = file, file.writer = file.writer, sep=sep),
                        loa::listUpdate(list(row.names = FALSE, na = "", quote = FALSE), 
                                   list(...)))
     do.call(exportPEMS, args)
}


########################
########################
##exportPEMS2CSV
########################
########################

#version 0.1.0
#karl 31/03/2019

exportPEMS2CSV <- function(pems, file = "tempfile", file.writer = write.table, sep = ",",  
                           ...){
     args <- loa::listUpdate(list(pems, file = file, file.writer = file.writer, sep=sep),
                        loa::listUpdate(list(row.names = FALSE, na = "", quote = FALSE), 
                                   list(...)))
     do.call(exportPEMS, args)
}


###############################
#unexported functions
###############################

#addExtension2FileIfMissing
#version 0.1.0
#karl 31/03/2019

addExtension2FileIfMissing <- function(file="tempfile", sep="\t", tidy.file=TRUE, ...){

   #return if tidy not wanted
   if(!tidy.file) return(file)

   #tidy
   #########################
   #text delimited file tidy
   if(sep=="\t")
     if(grepl("[.]txt$", tolower(file)) != TRUE)
            file <- paste(file, ".txt", sep="")
   #comma delmited file tidy    
   if(sep==",")
     if(grepl("[.]csv$", tolower(file)) != TRUE)
            file <- paste(file, ".csv", sep="")  
   #output
   file 
}

#resetDataTimeStamp
#version 0.1.0
#karl 31/03/2019

resetDataTimeStamp <- function(pems, time.stamp = "time.stamp", time.format = "%d/%m/%Y %H:%M:%OS",
                               tz = "UTC", output = "pems", tidy.time.stamp = TRUE, ...){

   #return if tidy not wanted
   if(!tidy.time.stamp) return(pems)

   #tidy
   ts <- pems[time.stamp]
   tz <- if(is.null(attributes(ts)$tzone)) tz else attributes(ts)$tzone
   ts <- format(ts, time.format, tz)
   time.format <- toupper(gsub("%", "", gsub("%OS", "S", time.format)))
   time.format <- paste(time.format, tz, sep=" ")
   ts <- pems.element(ts, units=time.format, name=time.stamp)

   #output
   if(output=="time.stamp") return(ts) 
   pems[time.stamp] <- ts
   pems  
}

#addUnitsToNames
#version 0.1.0
#karl 31/03/2019

addUnitsToNames <- function(pems, output = "pems", prefix="(", suffix=")", 
                                units = FALSE, ...){

   #return if addition not wanted
   if(is.logical(units) && !units) return(pems)
   if(units != "add.to.names") return(pems)

   #add
   #this might seem long-winded but it stops pems object
   #mucking up if element orders in pems[[data]] and pems[[units]] are different 
   #units <- unlist(c(as.vector(units(pems))))
   col.names <- names(pems)
   for(i in col.names)
        col.names[col.names==i] <- if(!is.na(units(pems)[col.names==i]) && units(pems)[col.names==i]!= "")
                  paste(i, prefix, units(pems)[col.names==i], suffix, sep="") else i
   #output
   if(output=="names") return(col.names) 
   names(pems) <- col.names 
   pems  
}

#stripFormals in import code


