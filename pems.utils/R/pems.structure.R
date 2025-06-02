########################
########################
##pems.structure
########################
########################

#pemsElement (gone 2018/06/30 0.2.25.17)
#pemsElement.old (gone 2018/06/30 0.2.25.17)
#pemsin, pemsin2, pemsin2.old (gone 2018/06/30 0.2.25.17)


#in place
#################
#getPEMSElement
#cpe



#other pems... function under review
########################################
#pemsData
#pemsConstants
#pemsHistory
#pemsDim




#next project
################
#getPEMSData/pemsData


#TO DO
##########################
#pemsConstants
#tidy
#document
#namespace





#questions
###################
#is this better than check...
#do functions need a test that first element is pems, etc?
#




########################
########################
##getPEMSElement
########################
########################

#version 0.3.0
#karl 22/06/2018

#getPEMSElement replaces pemsElement
#pemsElement using rlang/dplyr

#getElement already used...

#########################
#need to think about
#########################
#if.missing warning
#attribute name handling
#attribute units handling

#######################
#possible issue
#######################

#units(a$time.stamp)
#units(a[, "time.stamp"])
#units(a)[1]

#but these not behaving... 
#getPEMSElement(1, a)
#getPEMSElement(time.stamp, a)

#a[[1]][,1] still has original units
#if changed with units(a)[1] <- "har"
#but higher levels are tracking OK... 

# I think you currently need 
# units(a[,"time.stamp"]) <- "new"
# to change it at all levels

#units(a)[1] <- 1
#units(a[["data"]][,"time.stamp"]) == units(a[, "time.stamp"])
##[1] FALSE
#units(a[["data"]][,"time.stamp"]) 
##[1] "ho"
#units(a[, "time.stamp"])
##[1] "1"
#units(a[, "time.stamp"])
##[1] "1"
#units(a$time.stamp)
##[1] "1"
#units(a[[1]]$time.stamp)
##[1] "ho"

## issue with a[[1]][, "time.stamp"]
## but not a[, "time.stamp"]
## wonder if something is using getPemsElement

## units(a$time.stamp) <- "www" works but 
## units(a)[1] <- "ick" does not 
## units(a)[...] needs to be units(a$name) <- for each...
 
getPEMSElement <- function (x, pems = NULL, units = NULL, ..., 
                         fun.name = "getPEMSElement", 
                         if.missing = "stop",
                         if.null = if.missing,
                         track.name = TRUE,
                         .x = enquo(x)){

#################################################
#I guess there must be a better way of doing this
#but I am not seeing it
#################################################

    #################################################
    #I die if arg is missing
    #################################################
    #element.name <- quo_name(.x)
    #test...
    #NB if quo_is_null(.x) that means x is null because nothing to do???

#return(.x)
    
    
    ref.name <- if(is.null(list(...)$ref.name)) "element" else list(...)$ref.name
    ##element.name <- try(quo_name(.x), silent=TRUE)
    if(quo_is_null(.x)){
         checkIfMissing(if.missing = if.null,
                       reply = paste("required ", ref.name, " NULL", sep=""),
                       suggest = "checking call arguments", 
                       if.warning = NULL, 
                       fun.name = fun.name)
         return(NULL)
    }
    
    element.name <- try(quo_name(.x), silent = TRUE)   
    if(class(element.name)[1]=="try-error"){
        element.name <- ""
    }

    ans <- if (is.null(pems)) NULL else 
                try(as.data.frame(pems)[[element.name]], silent = TRUE)
###############
#testing cond eval
    if (is.null(ans) | class(ans)[1] == "try-error") {
        ans <- try(eval_tidy(enquo(x), pemsData(pems)), silent=TRUE)
    }
#################

    if (is.null(ans) | class(ans)[1] == "try-error") {
             ans <- try(eval_tidy(.x), silent = TRUE)
    }
    if (is.null(ans) | class(ans)[1] == "try-error") {
             ans <- try(eval_tidy(x), silent = TRUE)
    }
    if (class(ans)[1] == "try-error") 
             ans <- NULL
    if(is.null(ans))
         checkIfMissing(if.missing = if.missing,
                       reply = paste(ref.name, " '", element.name[1], "' not found or NULL", sep=""),
                       suggest = "checking call arguments", 
                       if.warning = NULL, 
                       fun.name = fun.name) 

    ##name management
    if(track.name) {
        attributes(ans)$name <- element.name
    }
    
    #pass ref to convertUnits?
#no option if units are set but ans does not have units...

    if (!is.null(units) & !is.null(ans)){ 
        temp <- attributes(ans)$name
        ans <- convertUnits(ans, to = units)
        attributes(ans)$name <- temp
    }
    ans
}

##################################
#cpe
##################################

#c.pems.element alternative
#################################
#currently have a problem  with c.pems.element
#so using this as work around

cpe <- function(...){
    
    ref <- quos(..., .named=TRUE)
    ans <- list(...)
    #get attributes
    temp <- lapply(ans, function(x) attributes(x))
    b <- temp[[1]]
    if(length(temp)>1){
        for(i in 2:length(temp)){
            for(j in names(b)){
                if(j %in% names(temp[[i]])){
                    b[[j]] <- c(b[[j]], temp[[i]][[j]])
                } 
                nms <- names(temp[[i]])[!names(temp[[i]]) %in% names(b)] 
                if(length(nms)>0){
                    for(j in nms){
                        b[[j]] <- temp[[i]][[j]]
                    }
                }
            }
        }
    }
    
    ######################################
    #this might not be best way to handle 
    #cpe() same repeat elements, cpe(a,a)
    ######################################
    local.names <- make.unique(names(ref))

    #update name because they might have modified
    b$name <- paste(local.names, collapse=", ")
    temp <- names(b)[names(b)!="name"]
    for(i in temp){
        if(i=="units"){
            if(length(unique(b[[i]]))>1){
                b$sub.un <- b$units
                b$units <- paste(b$units, collapse = ", ")
            } else {
                b$units <- unique(b$units)
                b$sub.un <- NULL
            }
        } else {
            b[[i]] <- unique(b[[i]])
        }
    }
    

    refs <- unlist(lapply(ans, function(x) length(x)))
    
    if(any(refs==0)){
        stop("unknown element requested")
    }
    
    if(length(refs)>1){
        b$sub.id <- refs
        b$sub.nm <- local.names 
    } else {
        b$sub.id <- NULL
        b$sub.nm <- NULL
    }
    
    if(any(b$class=="factor")){
        warning("dropping factor structure")
        b$class <- b$class[b$class!="factor"]
        b$levels <- NULL
        b$labels <- NULL
    }
    
    b$class <- unique("pems.element", b$class)
    
    ans <- lapply(ans, as.vector)
    ans <- do.call(c, ans)
    attributes(ans) <- b
    return(ans)

}


#########################
#########################
##getPEMSSetUp
#########################
#########################

#version 0.0.1
#karl 24/06/2018

#this is a general test for several of the getPEMS... functions
#not exporting it....

#this kills function if missing and if.missing="stop"
#returns NULL, FALSE or TRUE

getPEMSSetUp <- function(pems=NULL, fun.name = "getPEMSSetUp", 
                         if.missing = "stop", 
                         .pems = enquo(pems), ...){

    if(quo_is_null(.pems)){
         checkIfMissing(if.missing = if.missing,
                       reply = paste("required pems NULL", sep=""),
                       suggest = "checking call arguments", 
                       if.warning = NULL, 
                       fun.name = fun.name)
         return(NULL)
    }
    pems.name <- quo_name(.pems)
    if(is.null(pems)){
          checkIfMissing(if.missing = if.missing,
               reply = paste("pems '", pems.name[1], "' not found", sep=""),
               suggest = "checking call arguments", 
               if.warning = NULL, 
               fun.name = fun.name)
         return(NULL)
    }
    if(!"pems" %in% class(pems)) {
          checkIfMissing(if.missing = if.missing,
               reply = paste("pems '", pems.name[1], "' not a pems", sep=""),
               suggest = "checking pems", 
               if.warning = NULL, 
               fun.name = fun.name)
        return(FALSE)
    }
    TRUE
}



##############################
##############################
##getPEMSConstants
##############################
##############################

#replacing pemsConstants

getPEMSConstants <- function(pems=NULL, ..., 
         fun.name = "getPEMSConstants", if.missing = "stop",
         .pems = enquo(pems)){

    test <- getPEMSSetUp(pems, fun.name=fun.name, if.missing=if.missing, 
             .pems=.pems)
    if(is.logical(test) && test) pems[["constants"]] else NULL
}



##############################
##############################
##getPEMSData
##############################
##############################

#replacing with pemsData

getPEMSData <- function(pems=NULL, ..., 
         fun.name = "getPEMSData", if.missing = "stop",
         .pems = enquo(pems)){

#################
#might want to include option for 
#more aggressive conversions
#################

    test <- getPEMSSetUp(pems, fun.name=fun.name, if.missing=if.missing, 
             .pems=.pems)
    if(is.logical(test) && test) pems[["data"]] else NULL
}




























##############################
##############################
##pemsData
##############################
##############################


pemsData <- function(pems=NULL, ..., 
         fun.name = "pemsData", if.missing = "stop",
         pems.name = deparse(substitute(pems))){

    if(is.null(pems)){
          checkIfMissing(if.missing = if.missing,
               reply = paste("pems '", pems.name[1], "' not found", sep=""),
               suggest = "checking call arguments", 
               if.warning = NULL, 
               fun.name = fun.name)
    }
    #class(pems) <- "not.pems"
    #pems$data

#new build
#might want to strip out units, etc...?

    pems <- rebuildPEMS(pems)
    as.data.frame(pems)

}




##############################
##############################
##pemsConstants
##############################
##############################

#replacing with getPEMSContansts


pemsConstants <- function(pems=NULL, ..., 
         fun.name = "pemsConstants", if.missing = "stop",
         pems.name = deparse(substitute(pems))){

    if(is.null(pems)){
          checkIfMissing(if.missing = if.missing,
               reply = paste("pems '", pems.name[1], "' not found", sep=""),
               suggest = "checking call arguments", 
               if.warning = NULL, 
               fun.name = fun.name)
    }
    pems <- rebuildPEMS(pems, "old")
    class(pems) <- "not.pems"
    pems$constants

}










##############################
##############################
##pemsHistory
##############################
##############################


pemsHistory <- function(pems=NULL, ..., 
         fun.name = "pemsHistory", if.missing = "stop",
         pems.name = deparse(substitute(pems))){

    if(is.null(pems)){
          checkIfMissing(if.missing = if.missing,
               reply = paste("pems '", pems.name[1], "' not found", sep=""),
               suggest = "checking call arguments", 
               if.warning = NULL, 
               fun.name = fun.name)
    }
    pems <- rebuildPEMS(pems, "old")
    class(pems) <- "not.pems"
    pems$history

}








##############################
##############################
##pemsDim   
##############################
##############################


pemsDim <- function(pems=NULL, ..., 
         fun.name = "pemsDim", if.missing = "stop",
         pems.name = deparse(substitute(pems))){

    if(is.null(pems)){
          checkIfMissing(if.missing = if.missing,
               reply = paste("pems '", pems.name[1], "' not found", sep=""),
               suggest = "checking call arguments", 
               if.warning = NULL, 
               fun.name = fun.name)
    }
    pems <- rebuildPEMS(pems, "old")
    class(pems) <- "not.pems"
    dim(pems$data)

}





