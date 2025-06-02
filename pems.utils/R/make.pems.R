##########################
##########################
##make pems
##########################
##########################

#kr

#description
##########################
#functions for making pems object


#includes 
##########################
#pems (nee makePEMS)
#is.pems (nee isPEMS)
#as.pems 
#pems.element (nee makePEMSElement)


#to do
##########################
#make rebuildPEMS better 


#to think about
##########################
#as.pems...




#comments
##########################





##########################
##########################
##pems   nee makePEMS
##########################
##########################

#kr 18/09/2015 ver 0.0.2

#what it does
##########################
#make pems objects from parts

#to do
##########################
#is.null handling for args
#is.wrong handling for args
#defaults for constants
##########################
#

##########################
#comments
###########
#widely used. 
#think carefully before changing name or argument ordering
###########
#did it anyway...
###########
#

#############################
#to think about 
##########
#currently uses rebuild cheat
###########
#


pems <- function(x, units = NULL, constants = NULL,  
                     history = NULL, ...){

#################
#currently assuming 
# x = data.frame
#################

##################
#testing
#if supply a pems/return
#   unless units supplied 
#   then unpack/repack
#think about this 
#################

                 if(is(x)[1]=="pems" && is.null(units)) return(rebuildPEMS(x))
  
##################
#testing 
#allow x = vector or pems.element
##################

    

    if(is.null(units) && "units" %in% names(attributes(x)))
        units <- attr(x, "units")
    x <- as.data.frame(x)


#reported issue if data.frame[1,n]


    #data has dimension to work with
    if(!is.null(ncol(x)) && ncol(x)>0){

        #units
        if(is.null(units))
            units <- rep(NA, ncol(x))
        if(!is.data.frame(units)){
            units <- as.data.frame(t(units), stringsAsFactors = FALSE)
        }
        #after we know units is data.frame 
        units <- if(ncol(units)<ncol(x))
                    cbind(units, as.data.frame(t(rep(NA, ncol(x)-ncol(units))), 
                                               stringsAsFactors = FALSE)) else
                          units[1:ncol(x)] 
        names(units) <- c(names(x), names(units), rep(NA, ncol(x)))[1:ncol(x)]
    }

#to do
####################
#update constants

#dropping history
    history <- list()
    extra.args <- list(...)

    #update silently?
    test <- if("silent" %in% names(extra.args))
                extra.args$silent else FALSE
    extra.args <- extra.args[names(extra.args)!="silent"]

    #history
    history <- if(test)
                   history else c(history, match.call())    

    #output
    output <- list(data = x, units = units, constants = constants, 
                   history = history)

    #add in ... args
    temp <- extra.args
    output[names(temp)] <- temp

    class(output) <- "pems"

    #restack pems so all columns are pems.elements
    for(i in names(output))
         output[["data"]][, i] <- output[, i]
    rebuildPEMS(output)
}


makePEMS <- function(...) pems(...)








##########################
##########################
##is.pems nee isPEMS
##########################
##########################

#kr 18/09/2015 v 0.0.2

#what it does
##########################
#is.pems -two level tester

#to do
##########################
#make test more robust?

#comments
##########################
#widely used. 
#think carefully before changing name or argument ordering


#this needs thinking about
##############
#

is.pems <- function(x, full.test = TRUE, ...){

   #standard test
   output <- if(is(x)[1]=="pems") TRUE else FALSE
   #full.test
   if(full.test){
     comment(output) <- if(is.null(x)) "NULL" else 
           if(is(x)[1]=="pems") "pems" else
               if(is.data.frame(x)) "data.frame" else "other"
   }
   #output
   output
}

isPEMS <- function(...) is.pems(...)



##########################
##########################
##test.pems
##########################
##########################

#kr 26/09/2019 v 0.0.1

#what it does
##########################
#tests a pems for faulty configuration

#not exported

#not finished 

test.pems <- function(x, verbose = FALSE, ...){
  
  temp <- rebuildPEMS(x)[[]]
  #class(temp) <- "not.pems"
  
  reply <- names(temp$data)
  if(is.null(reply))
    message("\npems object: no named data [suspect]") else 
      message("\npems object: ", ncol(temp$data), 
              " data series (each ", nrow(temp$data), " cases)")
  
  reply <- names(temp)[names(temp) %in% c("units", "constants", "history")]
  if(length(reply) < 1)
    message("\twith no supporting structure [suspect]") else 
      message("\twith supporting structures: ", paste(reply, collapse=", ", sep="")) 
  
  #remember hidden 
  #refine
  
  reply <- names(temp)[!names(temp) %in% c("data", "units", "constants", "history", "dem")]
  if(length(reply) > 0)
    message("\t[and unique tags: ", paste(reply, collapse=", ", sep=""), "]\n")
  
  invisible(x)
}







########################
########################
##pems.element    nee makePEMSElement
########################
########################

pems.element <- function(x, name=NULL, units=NULL, ...){

    attr(x, "class") <- unique(c("pems.element", attr(x, "class")))

#if(is.null(attr(x, "name")) & !is.null(name))
#caused problems because it can't reset attr in calc... functions

    if(!is.null(name))
        attr(x, "name") <- name
    if(!is.null(units))
        attr(x, "units") <- units

    return(x)

}

makePEMSElement <- function(...) pems.element(...)




#######################
#######################
##as.pems....
#######################
#######################

##as.pems @S3 setup 
as.pems <- function(x,...)
                  UseMethod("as.pems")

##as.pems @S3 default
as.pems.default <- function(x,...){

#might need to think about this
    if(class(x)[1]=="pems") return(x)

    stop("no 'as.pems...' method for class ", 
        class(x), call. = FALSE)

}

as.pems.data.frame <- function(x,...) pems(x,...)







#######################
#######################
##rebuildPEMS
#######################
#######################

##rebuild old/new pems object

rebuildPEMS <- function(x, ...){ 

# need to think about a robust version check 
#    or people could be turning olds into olds...
#    which will do weird things...

# need to tidy this when it catches all bad stuff... 
  
# check units and names track in/out pems[[1]] ??
# this could also be in test.pems

    #get arg2 in form rebuildPEMS(pems, new) ..."new", etc...
    #might drop this...

    m.var <- exprs_auto_name(quos(...)) 
    m.var <- gsub("~", "", as.character(m.var))[1]
    m.var <- gsub("\"", "", as.character(m.var))[1]

    if(is.na(m.var)) m.var <- "new"

#    grpd <- "grouped_df" %in% class(x)
#    class(x) <- class(x)[class(x) != "grouped_df"]

    test <- attributes(x)$pems.tags$pems.build

    if (m.var == "new") {

        #quick if new checks
        if (!is.null(test) && test >= 3){
            return(x)
        }
        #assume old rebuild old as new
        class(x) <- "broken"
        out <- x$data
        attributes(out)$units <- x$units
        attributes(out)$pems.tags <- x[names(x)[!names(x) %in% 
            c("data", "units")]]
        attributes(out)$pems.tags$history <- list()
        attributes(out)$pems.tags$pems.build <- 3
        class(out) <- c("pems")
#testing
#    class(.data) <- unique(c(class(.data)[class(.data)!="pems"], c("tbl_df", "tbl", "data.frame"))) 
        class(out) <- c("pems", "tbl_df", "tbl", "data.frame")

        #this assumes grouped object never output as old
        if("grouped_df.tags" %in% names(x)){
              attributes(out)[names(x$grouped_df.tags)] <- x$grouped_df.tags
              class(out) <- c("grouped_df", "pems")
#testing
# 
        class(out) <- c("grouped_df", "pems", "tbl_df", "tbl", "data.frame")       
        }      
        return(out)
    }
    if (m.var == "old") {

        #quick check if is old
        if (!is.null(test) && test < 3) 
            return(x)
        if (is.null(test)) 
            return(x)

        #assume new and rebuild as old
        bare.bones <- attributes(x)[names(attributes(x)) %in% 
            c("units", "pems.tags")]
        attributes(x)$units <- NULL
        attributes(x)$pems.tags <- NULL
        class(x) <- class(x)[class(x) != "pems"]
        if (length(class(x)) == 0) 
            class(x) <- "data.frame"
        #changing class to is in below seems pointless
        ## if (length(class(x)) == 1 && class(x) == "list") 
        #but class(x) create check issue...
        if (length(class(x)) == 1 && is(x) == "list") 
            class(x) <- "data.frame"

        out <- loa::listUpdate(list(data = x, units = bare.bones$units), 
            bare.bones$pems.tags)

        #handling if grouped_df
        if ("grouped_df" %in% class(x)){
             class(out$data)[class(x) == "grouped_df"] <- "data.frame"
             out$grouped_df.tags <- attributes(x)
        }

#test
        class(out$data) <- "data.frame"

        out$pems.build <- 2
        class(out) <- c("pems")
        return(out)
    }

}
