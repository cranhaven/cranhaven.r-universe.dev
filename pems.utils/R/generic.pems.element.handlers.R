##########################
##########################
##generic pems.element.handlers
##########################
##########################

#kr 01/02/2012 v 0.3.1

#kr rebuild 11/2017 
##print.pems.element reactivated


#includes 
#(functions/code below) 
##########################
#as.data.frame.pems.element
#as.pems.pems.element
#print.pems.element
#plot.pems.element
#units.pems.element
#<-.units.pems.element
#summary.pems.element
#round.pems.element
#[.pems.element
#[<-.pems.element


###############################
#to do
#############

#############
#export [<-.pems.element
#############
#need to look at plot and print etc.
#Warning message:
#In class(ans) <- class(ans)[class(ans) != "pems.element"] :
#  NAs introduced by coercion
#compare with other pems.element[] and pems.element[]<-
#############
#


###############################
#to think about
################
#print.pems could have earlier classes
#so don't dump attributes when
#pems.utils not installed
#####################
#cat or message???
####################
# pems.element attribute must be name not names 




as.data.frame.pems.element <- function(x, ...){

#####################
#translation to data.frame changed
#    temp <- data.frame(as.vector(x))
#####################
#not sure why I cannot simplify this!!!
#####################
    temp <- as.data.frame(as.vector(x), drop=FALSE)
    names(temp) <- if(is.null(attr(x, "name")))
                       "x" else 
                           as.character(attr(x, "name")[1])
    names(temp) <- make.names(names(temp))
    #class(temp[,1]) <- class(x)
#################
#this attribute handling needs to be checked
#################
    #this replaces attributes(temp[,1]) <- attributes(x)
    temp[[1]] <- x
    temp    
}


as.pems.pems.element <- function(x, ...) pems(x, ...)


print.pems.element <- function (x, ..., n = NULL, rows = NULL, width = NULL){

#to do
###################
#tidy code
#

#to think about
###################
#do print and cat statements need to be merged
#as single output?
###################
#include width?

    # 'pems (n=n)'
    out.0 <- paste(class(x)[1], " [n=", length(x), "]", sep="")

    out.2 <- ""

    if(!is.null(n)){
        #this assumes n is null or numeric
        if(n<0) n <- length(x) #this assume any minus value means show all
        if(n<length(x)) out.2 <- paste(" not showing: ", length(x)-n, " (of ", length(x), ") elements", sep="")
        x <- x[1:n]
    }
    if(is.null(width)) width <- getOption("width") * 0.9

    #data 
    #print with default method and attributes stripped
    ans <- x

    if(length(class(ans))>1) class(ans) <- class(ans)[-1] else
         class(ans)[1] <- if("levels" %in% names(attributes(ans)))
                                "factor" else mode(ans)
    attributes(ans) <- attributes(ans)[!names(attributes(ans))%in% c("name", "units")]

    out.3 <- ""

    if(length(class(ans))>0) 
          out.3 <- paste(out.3, " <", paste(class(ans), collapse=","), ">", sep="")
    if("name" %in% names(attributes(x)))
          out.3 <- paste(out.3, " ", attributes(x)$name, sep="")
    if("units" %in% names(attributes(x)))
         if(!is.na(attributes(x)$units) && attributes(x)$units != "")
             out.3 <- paste(out.3, " [", attributes(x)$units, "]", sep="")

    out.1 <- utils::capture.output(print(ans, ...))
    
#testing this to strip old factor labels
    if("levels" %in% names(attributes(ans)))
        out.1 <- out.1[1:(length(out.1)-1)]

#this one makes nice ...
    temp <- out.1[1] #all have same spacing
    indent <- gregexpr("[]]", temp)[[1]]-3
    if(length(indent)>1) indent <- indent[1]
#print(indent)
    indent <- if(indent>0) paste(rep(" ", indent), collapse="") else ""
    indent <- paste(indent, "...", sep="")

    if(is.null(n) & is.null(rows)){
         rows <- 3
    }
    if(!is.null(rows)){
         if(rows<0) rows <- length(out.1) #assumes any minus means show all
         if(rows<length(out.1)){
######################
#this one gets next row name
#              temp <- out.1[rows+1]
#              temp <- substr(temp, gregexpr("[[]", temp)[[1]]+1,
#                                   gregexpr("[]]", temp)[[1]]-1)
######################              
              out.2 <- paste(" not showing: ", length(out.1)-rows, " rows", sep="")
              out.1 <- if(rows==0) "" else out.1[1:rows]
         }
    }
    
    if(!is.null(n) && n==0) out.1=""

    if(length(out.1)>1 || out.1!="") out.1 <- paste(out.1, collapse="\n")
    out.1 <- if(out.1 != "" & out.2 !="") paste(out.1, "\n", indent, out.2, "\n", sep="") else 
                 if(out.1 != "") paste(out.1, "\n", sep="") else  paste(indent, out.2, "\n", sep="")

    out.1 <- paste(out.0, "\n", out.1, sep="")
    out.1 <- paste(out.1, indent, out.3, "\n", sep="")


#######################################
#need to update this to handling 
#long level lists...
#testing
# strwrap(paste(levels, collapse=""), width=width)
#######################################

    if("levels" %in% names(attributes(ans))){
        temp <- paste(indent, " levels: ", sep="")
        out.2 <- paste(temp,
                       strwrap(paste(levels(ans), collapse="; ", sep=""),
                         width=width*0.85), sep="")
        if(length(out.2)>1){
                 out.2[2:length(out.2)] <- gsub(temp, 
                               paste("\n", paste(rep(" ", nchar(temp)), collapse="", sep=""),
                               sep=""), out.2[2:length(out.2)])
                 out.2 <- paste(out.2, collapse="")
        }
        out.1 <- paste(paste(out.1, out.2, sep=""), "\n", sep="")
    }

    #output 
    cat(out.1)
}


print.pems.element.old <- function (x, ...){

#to do
###################
#tidy code
#

#to think about
###################
#do print and cat statements need to be merged
#as single output?
###################
#

    #data 
    #print with default method and attributes stripped
    ans <- x

#############
#test
#############
#    class(ans) <- "default"
#    attributes(ans) <- NULL
#    print.default(ans, ...)

####################
#update to class handling for factors etc
#might be able to replace with an inherits???
#if I understood them
#    class(ans) <- class(ans)[class(ans)!="pems.element"]
####################
    if(length(class(ans))>1) class(ans) <- class(ans)[-1] else
         class(ans)[1] <- if("levels" %in% names(attributes(ans)))
                                "factor" else mode(ans)
    attributes(ans) <- attributes(ans)[!names(attributes(ans))%in% c("name", "units")]

#allows element to print as prior class
#############

    print(ans, ...)


#see note below in plot.pems.element about
#units = "[]" plotting

    #attr
    #local report
    temp2 <- if(is.null(attributes(x)$name))
                 " [unnamed]" else paste(" ", attributes(x)$name, sep = "")
    #old line
    #         temp2 <- paste(temp2, " [", attributes(x)$units, "]", sep = "")
    if(!is.null(attributes(x)$units)){
         temp <- paste(" [", attributes(x)$units, "]", sep="")
         if(temp != " []") temp2 <- paste(temp2, temp, sep = "")
    }


    temp2 <- paste(temp2, " [n = ", length(x), "]", sep = "")
    
    cat("pems.element;", temp2, "\n", sep="")

}





##################
#plot.pems.element
##################

plot.pems.element <- function (x, y = NULL, xlab = NULL, ylab = NULL, ...){


#to this is lattice?

#rethink order
#think about error handling for mismatching cases
#bad plots, etc
#output styles?

    #x reset
#############
#previous
#    class(x) <- "default"
#############
#recent added 
#lx0 as early creation of deparse(substitute(x))
#limited deparse(substitute(x)) and deparse(substitute(y)) to ...[1]
#may need to go to 
#############

    lx0 <- deparse(substitute(x))[1]
    if(length(class(x))>1) class(x) <- class(x)[-1] else
        class(x)[1] <- if("levels" %in% names(attributes(x)))
                             "factor" else mode(x)

##could not use && attributes(x)$units!=""
##in condition term for adding [unit] to labs
##(to stop []) in plots
##must be a better way of doing this
##current is retrospective

    #get x name
    if(is.null(y)){ 
        if(is.null(ylab)){
            ylab <- if ("name" %in% names(attributes(x))) 
                attributes(x)$name else lx0
        
            if(!is.null(attributes(x)$units)){
                temp <- paste(" [", attributes(x)$units, "]", sep="")
                if(!temp %in% c(" []", " [NA]")) ylab <- paste(ylab, temp, sep = "")
            }
        }
    } else {
        if(is.null(xlab)){
            xlab <- if ("name" %in% names(attributes(x))) 
                attributes(x)$name else lx0
        
            if(!is.null(attributes(x)$units)){
                temp <- paste(" [", attributes(x)$units, "]", sep="")
                if(!temp %in% c(" []", " [NA]"))xlab <- paste(xlab, temp, sep = "")
            }
        }
        if(is.null(ylab)){
            ylab <- if ("name" %in% names(attributes(y))) 
                attributes(y)$name else deparse(substitute(y))[1]

            if(!is.null(attributes(y)$units)){
                temp <- paste(" [", attributes(y)$units, "]", sep="")
                if(!temp %in% c(" []", " [NA]")) ylab <- paste(ylab, temp, sep = "")
            }
        }
    if(!is.null(y)){
            #might also need to strip units from attributes?
            class(y) <- class(y)[class(y) != "pems.element"]
        }
    }

    plot(x = x, y = y, xlab = xlab, ylab = ylab,...)    

}




##################
#units.pems.element
##################

#need to think about this some more
units.pems.element <- function(x) attr(x, "units")

`units<-.pems.element` <- function(x, value) { 

    #could add padding to prevent bad inserts being tried
    attr(x, "units") <- value 
    x
}



####################
#summary.pems.element
####################


#summary 


summary.pems.element <- function(object, ...){

    attr(object, "class") <- attr(object, "class")[attr(object, "class") != "pems.element"]
    summary(object)

}


###################
#round.pems.element
###################

#round 
#needed because time.stamp rounding is specially handled

round.pems.element <- function(x,...){
   att <- attributes(x)
   class(x) <- class(x)[class(x) != "pems.element"]
   pems.element(round(x,...), units=att$units, name=att$name)
}





##########################
##########################
##[.pems.element
##########################
##########################

#kr 31/04/2014 v 0.2.1

#what it does
##########################
#handles pems.element[] calls 
#etc
#

#to do
##########################
#tidy
#think about force, simplify

`[.pems.element` <- function(x, i, ..., force=TRUE, wrap=FALSE){

    #pems.element handling
    #x[1] element 1 of x, etc 
    
    #output 
    #x[i] with pems.element attributes retained

#    att <- attributes(x)
#    class(x) <- class(x)[class(x)!="pems.element"]

############
#new
    att <- attributes(x)
    old.class <- class(x)
    class(x)[1] <- "not.pems.element"
############

    if(!force){
        i <- if(is.character(i)) 
                 i[i %in% names(x)] else i[i %in% 1:length(x)]
    }
    if(wrap && is.numeric(i)){
        if(length(x) < max(i, na.rm=TRUE)) x <- rep(x, length.out=max(i, na.rm=TRUE))
        
    }
     
    x <- try(x[i], silent = TRUE)
    if(is(x)[1] == "try-error") 
      stop("In pems.element[i] 'i' unknown/unfound", 
          call. = FALSE)

#################
#new 
#this handles pems.element if attribute()$names have been added
#happens in summary(lm())

    if("names" %in% names(att))
          att$names <- att$names[i]

#might need to refine this
#################

    attributes(x) <- att
################
#new
    class(x) <- old.class
################    
    x

}






##########################
##########################
##[<-.pems.element
##########################
##########################

#kr 31/04/2014 v 0.2.1

#what it does
##########################
#handles pems.element[] <- calls 
#etc
#

#to do
##########################
#tidy
#think about force, simplify

`[<-.pems.element` <- function(x, i, ..., force=TRUE, wrap=FALSE, value){

    #pems.element handling
    #x[1] <- 2 replace first case of x with 2, etc 
    #output 
    #x with pems.element attributes retained and requested insert
    #if allowed

#####################
#think about this bit here
#and in above function
#####################

#    att <- attributes(x)
#    class(x) <- class(x)[class(x)!="pems.element"]

############
#new
    att <- attributes(x)
    old.class <- class(x)
    class(x) <- if(length(class(x))<2)
                    mode(x) else class(x)[-1]
############

    if(!force){
        if(length(i) != length(value))
                  stop("In pems.element[i]<-value: i/value dimensions mismatch", 
                        call. = FALSE)
    } 
     

    test <- try(x[i]<- value, silent = TRUE)
    if(is(test)[1]=="try-error")
       stop("In pems.element[i]<- value: cannot coerce value into x[i]", 
          call. = FALSE)
#    x <- test

###############
#we currently generate a warning message here
#if attributes don't match but be coerced...
###############

    #attributes(x) <- att
################
#new
    class(x)[1] <- "pems.element"
################   
    x

}


##########################
##########################
##c.pems.element
##########################
##########################

#kr 03/01/2021 v 0.0.2

#what it does
##########################
#combines pems.elements 
#but merge attributes
#

#to do
##########################
#tidy
#think about force, simplify

#currently using cpm as local c 
#because code does not work as method...










