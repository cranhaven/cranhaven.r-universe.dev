##########################
##########################
##generic pems handlers
##########################
##########################

#check re pems.element but I don't think it contains any

#might to archive a copy of this and then tidy
#because it is a real mess at the moment

##rebuild 11/2017

#exporting

#print.pems
#names.pems, names<-.pems
#as.data.frame.pems
#dim.pems

#cheated old code
#[.pems, [<-.pems using cheat code
#$.pems, $<- works if above works
#summary, head, tail, 

###############################
#to fix 
#########
#pems[ breaking on example(pems)
#########
#tail not giving actual numbers...
#  tail(pems) should be row 995 not 1
#########
#all needs tidying
#########
#



###############################
#to think about
##########
#




##########################
##########################
##print.pems
##########################
##########################

#kr 06/06/2013 v 0.3.0
#kr 14/11/2017 v 0.4.0

#what it does
##########################
#handles pems console appearance 
#

###################################
#too fix
###################################
#

##################################
#to watch 
##################################
#print(pems, width [smaller than first column width])
#print(pems, cols [less than 1])
#


#to think about
##########################
# group labelling that includes number of cases
# paste(names(attributes(x)$labels), "[", lapply(attributes(x)$labels, function(x) length(unique(x))),"]", sep="", collapse="; ")
#####
#hide width?
#####
#option to show class 
# instead of units?
#####
#col=-1, rows=-1 as a 
# show full data range?
#####
#

print.pems <- function(x,..., rows=NULL, cols=NULL, width=NULL){

    ##################################
    #new print.pems for new structure
    #based 

#this could be simplified/tidied...

    x <- rebuildPEMS(x) 
    #test structure
    has.units <- !is.null(attributes(x)$units)
    grpd <- "grouped_df" %in% class(x)

#plot setup
#to tidy using rows not n....    

    n <- if(is.null(rows)) 6 else rows
    if(n > nrow(x)) n <- nrow(x)
    if(is.null(width)) width <- getOption("width") * 0.9

###
#new bit
#foreshortening shown other columns
######################
    extra.args <- list(...)
    max.other.cols <- if("max.other.cols" %in% names(extra.args))
                          extra.args$max.other.cols else 6
########################

    #make all columns characters
########################
#a lot of this can go when 
#pems[works again...
#for example here
    b <- x
    class(b) <- class(b)[class(b)!="pems"]
    if(length(class(b))==1) class(b) <- "data.frame"
#this can go if 2nd b 
#in next line becomes x
########################
    b <- data.frame(lapply(b[1:n,], as.character), stringsAsFactors=FALSE, 
                                                   check.names=FALSE)      
                                                   #or it corrects names like velocity<5
    if(n==0) b[] <- "<empty>"   #to catch empty frames
    b[is.na(b)] <- "NA"   #to catch NAs
    
    #unit labels extensions
    if(has.units){
         #if data and units do not have same dimensions or names...
         if(length(names(b))!=length(names(attributes(x)$units)) || any(names(b)!=names(attributes(x)$units))){
              cat("suspect pems [data/units conflict]; halted print\n")
              return(invisible(x))
         }
         #note the leading space on temp
         attributes(x)$units[] <- gsub("[[][]]", "", paste("[", attributes(x)$units, "]", sep=""))
         b <- rbind(attributes(x)$units, b)
    }
    temp <- data.frame(t(names(b)))
    names(temp) <- names(b)
    b <- rbind(temp, b)
######################
#replacing
##    b <- if(has.units) cbind(data.frame(..rows=c("", "", 1:n), b)) else
##               cbind(data.frame(..rows=c("", 1:n), b))
#to catch empty data frames
#also to use row.names if there...
    temp <- if (has.units) c("", "") else c("")
    temp <- if(n>0) c(temp, if(!is.null(row.names(x))) row.names(x)[1:n] else 1:n) else 
                    c(temp, " ")
    b <- cbind(data.frame(..rows = temp, b))
######################
    b.n <- apply(b, 2, function(x) nchar(x))
    b.max <- apply(b.n, 2, function(x) max(x))+1  
                   #NAs might be an issue
    b.n <- data.frame(t(b.max-t(b.n)))
    b.n <- apply(b.n, c(1,2), function(x) paste(rep(" ", x), collapse=""))
    for(i in 1:ncol(b))
         b[,i] <- paste(b.n[,i], b[,i], sep=" ")
    test <- if(is.null(cols)) max(which(cumsum(b.max)<width)) else
                 cols + 1 #because we add ..row column
    if(test>ncol(b)) test <- ncol(b)
    b <- b[,1:test]

    #############################
    #the header

#test removed \n from start of header
    header <- paste("pems (", nrow(x), "x", ncol(x), ")", sep="")
    if(grpd) {
      gg <- names(attributes(x)$groups)
      gg <- gg[gg != ".rows"]
      header <- paste(header, "\n<grp>: ", paste(gg, collapse="; "), sep="", collapse="")
    }
    #############################
    #the shown data grid

    #############################
    #test == 1 does not show data....
    #############################
    #replaced code
    ##out <- apply(b, 1, function(x) paste(x, collapse=""))          
    ##out <- paste(paste(out, collaspe="\n", sep=""), collapse="")
    if(test>1){
       out <- apply(b, 1, function(x) paste(x, collapse=""))          
       out <- paste(paste(out, collaspe="\n", sep=""), collapse="")
    } else {
       out <- ""
    }
    ##############################
    out <- paste(header, out, sep="\n", collapse="\n")
 
    #############################
    #with above replaced code
    ##h.row <- nrow(x)-nrow(b)+2 #+2 for header
    ##h.col <- ncol(x)-ncol(b)+1 #+1 for row.number
    h.row <- if(is.null(nrow(b)))  
                  nrow(x) else nrow(x)-nrow(b) +2      ##+2 for header
    h.col <- if(is.null(ncol(b))) 
                  ncol(x) else ncol(x)-ncol(b) +1       #+1 for row.number
    ################################

    #################################
    #first footer rows and rows not plotted
    footer <-""
    if(h.row > 0 | h.col>0){
        temp <- " ... not showing: "
        if(h.row>0) temp <- paste(temp, h.row, " rows", sep="")
        if(h.row>0 & h.col>0) temp <- paste(temp, "; ", sep="")
        if(h.col>0) temp <- paste(temp, h.col, " cols (elements)", sep="")
        footer <- paste(temp, footer, collapse="\n")
        footer <- paste(footer, "\n", sep="")
    } 

#this needs tidying
#header and foooter widths not controlled by forced width.....

    ################################
    #second footer other columns 
    #if range reduced
    footer2 <- ""
    if(h.col>0){
         #note: not test+1 because I added column to b
         footer2 <- names(x)[(test):ncol(x)]
         if(has.units){
             footer2 <- paste(footer2, attributes(x)$units[(test):ncol(x)], sep="")
         }
         footer2[1:(length(footer2)-1)] <- paste(footer2[1:(length(footer2)-1)], "; ", sep="")
         footer2[1] <- paste("other cols: ", footer2[1], sep="")
         footer2 <- strwrap(paste(footer2, collapse=""), width=width)
         footer2[1] <- paste(" ... ", footer2[1], sep="")
         if(length(footer2)>1)
               footer2[2:length(footer2)] <- paste("      ", footer2[2:length(footer2)], sep="")
         footer2 <- paste(footer2, "\n", collapse="", sep="")

         test <- gregexpr("\n", footer2)[[1]]

###########################################
#new bit foreshortening other cols
#might change
         if(attributes(test)$match.length[1]>0){
             if(length(test)>max.other.cols){
                 test2 <- length(gregexpr(";", substr(footer2, test[max.other.cols], nchar(footer2)))[[1]])+1 
                 footer2 <- substr(footer2, 1, test[max.other.cols])
                 footer2 <- paste(footer2, "      ... and ", test2, " other unreported columns", sep="")
             }
         }
############################################
         #footer2 <- paste(footer2, "\n", sep="")
    } 
            
    #out
    cat(out, footer, footer2, "\n", collapse="", sep="")
    invisible(x)

}







##########################
##########################
##names.pems
##########################
##########################

#kr 07/12/2011 v 0.2.0
#kr 14/11/2017 v 0.3.0

#what it does
##########################
#names()
#returns data series names from pems
############
#names()<- 
#as above 

#these do names()[] and names()[]<- 
#by passing lot....

###########################
#to fix
##########
#

###########################
#to think about
###########
#


##' @S3method print pems

names.pems <- function(x, ...) {

    x <- rebuildPEMS(x)
    class(x) <- class(x)[class(x)!="pems"]
    if(length(class(x))==1) class(x) <- "data.frame"

    if(is.null(attributes(x)$pems.tags)){
       message("\npems object [suspect]")
       #not of enough of a reason not to try
       ##return(invisible(NULL))
    }
    names(x)
}


## @S3method names<-.pems

`names<-.pems` <- function(x, ..., value) {

    #variation on units<-
    #very crude handling of names$ and names[]

    x <- rebuildPEMS(x)
    old.class <- class(x)

    #############################
    #x <- as.data.frame(x) #need data.frame to get at 
    #                      #element attributes 
    #############################
    #since as.data.frame strips units
    #############################
    class(x) <- "data.frame"

    #check for duplication
    #currently corrects names without saying anything....
    if(any(duplicated(value))){
         value <- make.names(value, unique=TRUE)
    }

#    if(***************){
#       warning("In units(pems): [suspect units]", call.=FALSE)
#       units <- data.frame(matrix(NA, nrow = 1, ncol = ncol(x$data)))
#       names(units) <- names(x$data)
#    }

#might also think about adding units if not there
#see above
#error check on state of value?

#this sets all of x and all names even for names[]<-

    names(x) <- value
    names(attributes(x)$units) <- value

    for(i in value)
         attributes(x[,i])$name <- i

    class(x) <- old.class
    return(x)

}




##########################
##########################
##as.data.frame.pems
##########################
##########################

#############################
#what it does
#############
#pull data.frame out of pems
#for lattice, lm, etc.

############################
#to fix
############
#

############################
#to think about
############
#could make a more aggressive
#  clean clean-up like 
#  fortify 


as.data.frame.pems <- function(x, ...){

#in whatever, makes/uses new, exports df
 
     x <- rebuildPEMS(x, "new")
     class(x) <- "data.frame"
     #########################
     #added unit reset because  
     #      df <- as.data.frame(pems)); modify df; pems(df)
     #      can cause units/data conflict if  
     attributes(x) <- attributes(x)[names(attributes(x))!="units"]
     x

}




##########################
##########################
##dim
##ncol
##nrow
##########################
##########################

#############################
#what it does
#############
#get pems dimensions
#

############################
#to fix
############
#

############################
#to think about
############
#don't need ncol and nrow 
#  because the use dim...
#  and only need dim to 
#  catch any older pems
#  objects...
############
#

################################
#doh
#############
#using dim(rebuildPEMS(x)) causes issue
# because it tries to dim.pems inside 
# dim.pems... 
#############
#

dim.pems <- function(x, ...) dim(as.data.frame(x))












##########################################################
#older code
##########################################################


#any of that not deactivated as ...pems.old..., 
#   works in form 
#      rebuildPEMS(x, "old")
#      old code
#      rebuildPEMS(x)

#[, [<- (these enable $ and $<-)


#kr 01/02/2012 v 0.3.1

#includes 
#(functions/code below) 
##########################
#as.data.frame.pems
#dim.pems
#############->nrow.pems
#############->ncol.pems
#[.pems
#[<-.pems
#[[.pems
#[[.<-pems
#$.pems
#$<-.pems
#with.pems
#subset.pems
#print.pems
#plot.pems
#names.pems
#names<-.pems
#summary.pems
#head.pems
#tail.pems
#units.pems
#units<-.pems
#










##########################
##########################
##[[.pems
##########################
##########################

#kr 18/08/2015 v 0.1.0

#what it does
##########################
#handles pems[[]] calls 
#for access to data, units and other tags
#

#to do
##########################
#tidy
#think about this

##need pems[[]]<- operator


`[[.pems` <- function(x, k, ...){

  
    x <- rebuildPEMS(x, "old")
    #break pems
    class(x) <- "list"
    
    
    #special operators
    #####################
    #testing
    #####################
    if("extra.pems.tags" %in% as.character(match.call())){
      #out
      ###########################
      #cat(out, footer, footer2, "\n", collapse="", sep="")
      #invisible(x)
      return(x[!tolower(names(x)) %in% 
                 c("data", "units", "constants", "history", "pems.build")])
    }

    #return as list if nothing declared
    if(missing(k)) return(x)

    #select structural elements
    temp.fun <- function(k){
        ans <- try(x[[k]], silent=TRUE)
        if(class(ans)[1] == "try-error") NULL else ans
    }
    if(length(k)==1) return(temp.fun(k))
    x <- lapply(k, temp.fun)
    rebuildPEMS(x)

}




#########################
#########################
##[[<-.pems
#########################
#########################

`[[<-.pems` <- function(x, k, ..., value){

    x <-rebuildPEMS(x, "old")
    #break pems
    old.class <- class(x)
    class(x) <- "list"

    #return as list is nothing declared
    if(missing(k) | missing(value)) return(x)

    #add in if it exists or not
    #might want to think about this?

    x[[k]] <- value 
    class(x) <- old.class
    rebuildPEMS(x)

}





##########################
##########################
##[.pems
##########################
##########################

#kr 06/06/2013 v 0.3.0

#what it does
##########################
#handles pems[] calls 
#etc
#

#to do
##########################
#tidy
#think about force, simplify




`[.pems` <- function(x, i, j, ..., force = FALSE, simplify = TRUE){


    #quick cheat
    x <- rebuildPEMS(x, "old")



    ########################
    #generic pems handling
    ########################

    #x[1] element 1 as 1 col data.frame
    #x[1,] row 1
    #x[,1] element 1 
    #x[1,1] row, element 1,1 
    
    #output options
    #force=FALSE fail call if any requested elements/rows invalid
    #force=TRUE return valid element/row requests, discard invalid 
    #simplify=TRUE as pems.element if possible else pems
    #simplify=FALSE as pems 

    #force = FALSE, simplify = TRUE gives the response most like a 
    #conventional data frame
    #currently handles negs like data.frame (I think)...

    #negs are discarded if a mixed (neg and pos) vector supplied
    #and forced. Possibly a better way of handling this

    call1 <- sys.call()
    call2 <- match.call()
    check.i <- !missing(i)
    check.j <- !missing(j)

    #request pems[], pems[,], etc
    #do nothing
    if(!check.i & !check.j) return(x)

#in progress 
#current na.pad.output is functional but in testing
#might still be issues

    #force special handling
    #options omit.err.cases (default forcing)
    #        no.forcing
    #        na.pad.output
    if(is.logical(force))
        force <- if(force) "omit.err.cases" else "no.forcing"

    #make 
    old.class <- class(x)
    class(x) <- "not.pems"

    #request pems[i], etc
    #columns/elements, etc
    if(check.i && !check.j && length(as.character(call1))==length(as.character(call2))){
       j <- i
       i <- 1:nrow(x$data)
    }

    #request pems[i,], etc
    #rows, etc
    if(check.i && !check.j && length(as.character(call1))!=length(as.character(call2)))
        j <- 1:ncol(x$data)

    #request pems[,j], etc
    #columns/elements
    if(!check.i && check.j)
        i <- 1:nrow(x$data)
    
    #otherwise it is pems[i,j], etc
    
#####################
#new fix/testing
#####################

    #fix for logicals
    if(is.logical(i))
        i <- c(1:length(i))[i]
    if(is.logical(j)) 
        j <- c(1:length(j))[j]


    #negative handling 

    #at this stage we err out on negs
    test <- character()
    if(is.numeric(j) && any(j<=0)) test <- c(test, "elements")
    if(is.numeric(i) && any(i<=0)) test <- c(test, "rows")
    if(length(test)>0)
        stop(paste("In pems[i,j]<-: neg ",
                   paste(test, sep=" and ", collapse=" and "),
                   " requested \n       [not currently allowed]",
                   sep=""), call. = FALSE)


#data.frame like neg handling

#note/known issue
#currently regards zero as negative
 
#    if(is.numeric(i) && any(i<=0))
#        if(all(i<=0))
#            i <- c(1:nrow(x$data))[!1:nrow(x$data) %in% abs(i)] else
#                 if(force=="omit.err.cases") 
#                     i <- i[i>0] else 
#                         stop("In pems[i,j]: mixed (pos & neg) elements and/or rows not allowed", 
#                              call. = FALSE)
#    if(is.numeric(j) && any(j<=0))
#        if(all(j<=0))
#            j <- c(1:ncol(x$data))[!1:ncol(x$data) %in% abs(j)] else
#                if(force=="omit.err.cases") 
#                     j <- j[j>0] else 
#                         stop("In pems[i,j]: mixed (pos & neg) elements and/or rows not allowed", 
#                              call. = FALSE)

    #make dummy data.frame for force == na.pad.output
    if(force=="na.pad.output"){
        dummy <- as.data.frame(matrix(nrow=length(i), ncol=length(j)), row.names = NULL, optional = TRUE)
        if(is.character(j)) names(dummy) <- j 
        if(is.numeric(j)) names(dummy) <- names(x$data)[j]
        row.names(dummy) <- i
    }

#try allows pems[does.not.exist,] 
#generates as data.frame of NAs
#so using another check

#note 
#might have fixed this later

#this need thinking about 
#could be rationalised


    index.i <- if(is.character(i))
                   !i %in% row.names(x$data) else !i %in% 1:nrow(x$data)
    check.i <- i[index.i]
    index.i <- c(1:length(index.i))[!index.i]

    index.j <- if(is.character(j))
                   !j %in% names(x$data) else !j %in% 1:ncol(x$data)
    check.j <- j[index.j]
    index.j <- c(1:length(index.j))[!index.j]

#old version -with no index.j and .j

##    check.i <- if(is.character(i))
##                   i[!i %in% row.names(x$data)] else i[!i %in% 1:nrow(x$data)]
##    check.j <- if(is.character(j))
##                   j[!j %in% names(x$data)] else j[!j %in% 1:ncol(x$data)]
  
    if(length(check.i)>0 || length(check.j)>0){

        #previously told user what was missing 
        #but that was messy if lots missing

        if("no.forcing" %in% force){
            temp <- paste(c("elements", "rows")[c(length(check.j)>0, length(check.i)>0)], sep="", collapse=" and ")
            stop(paste("In pems[i,j]: unknown ", temp, " called", sep="", collapse=""), 
                 call. = FALSE)
        }

        #these are know known i and j terms
        i <- i[!i %in% check.i]
        j <- j[!j %in% check.j]

        if("omit.err.cases" %in% force | "na.pad.output" %in% force){
            if(length(i)<1 || length(j)<1){
                temp <- paste(c("elements", "rows")[c(length(j)<1, length(i)<1)], sep="", collapse=" or ")
                if("omit.err.cases" %in% force)
                    stop(paste("In pems[i,j]: no known ", temp, " even after forcing", sep="", collapse=""), 
                         call. = FALSE)
             }
         }
        
    }

    #try to get x$data[i,j]
    ans <- try(x$data[i,j,..., drop=F], silent=TRUE)
    if(class(ans)[1]=="try-error" || (is.data.frame(ans) && nrow(ans)==0))
        if(force=="na.pad.output") 
            ans <- dummy else
                   stop("In pems[i,j], unexpected issue [please contact package admin.]", 
                        call. = FALSE)

#this is still in progress
#put everything that is there and then
#transfer the attributes?

    if("na.pad.output" %in% force){
         dummy[index.i,index.j] <- ans

    

##this works (I think...)
##but is well messy

##also suspect the following might die
##if units are present but does not contain
##an expected entry

        if(length(j)>0)
            for(jj in 1:length(j))
                attributes(dummy[,index.j[jj]]) <- attributes(ans[,jj])

        ans <- dummy
        dummy <- dummy[1,,drop = FALSE]
        dummy[index.j] <- x$units[j, drop=FALSE]
        names(ans) <- make.names(names(ans), unique = TRUE)
        names(dummy) <- names(ans)
        j <- names(ans)
        x$units <- dummy

#known issue
#this makes no unit cases NA
#elsewhere units "" if not set
#does this matter 
#which to do, if doing only one?
#if doing both, are both handled elsewhere?
    }


    #if simplify can be done return pems.element
    if(simplify & ncol(ans)==1){

############
#testing as.data.frame because I think tbl_df is stopping this
        out <- as.data.frame(ans)[,1]  
############
        attr(out, "row.names") <- NULL
        attr(out, "name") <- names(ans)
        attr(out, "units") <- as.character(x$units[1,j])
        class(out) <- unique(c("pems.element", class(out)))
################
#think about makePEMSElement
################
        return(out)
    }
    
    #otherwise return rebuilt pems
    x$data <- ans
    #######################
    #this update to stop pems.1[1, simplify=FALSE] killing print.pems
    ##x$units <- x$units[1,j]
    x$units <- x$units[j]
    if("history" %in% names(x))
         x$history <- c(x$history, call2)

#    class(x) <- old.class
    class(x) <- "pems"
    rebuildPEMS(x)

}




#########################
#########################
##[<-.pems
#########################
#########################

#########################
#to think about
#########################
#with pems...
#> a[["data"]][1,1:5] <- rep(NA,5) 
#works but...
#> a[1,1:5] <- rep(NA,5)
#Error: In pems[i,j]<-value: questionable request
#       elements and rows pems[i,j] and insert[i,j] dimension mismatch
#       [check force setting if insertion required]
#should you be able to change multiple rows at a go with a pems...

`[<-.pems` <- function(x, i, j, ..., force = FALSE, value){

    #cheat
    x <- rebuildPEMS(x, "old")

    ########################
    #generic pems handling
    ########################

#overwrite not currently doing anything

#?option for pems[1,1] <- 1:10 to force insert as pems[1:10,1]
#even if pems[1:10,1] exists 
#this would be the overwrite???

#?option for pems[1:2] <- vector shorter than pems rows
#to write as element replicated as vector+NAs
#note: this is na.pad.target for elements then fill.target/insert for rows. 

    #x[1] element 1 as 1 col data.frame
    #x[1,] row 1
    #x[,1] element 1 
    #x[1,1] row, element 1,1

    #x[,] <- value insert value into  
    
    #output options
    #force=FALSE fail call if any requested elements/rows dimensions 
    #            do not fit exactly or is any missing 
    #force=TRUE return valid element/row requests, discard invalid 
    #            trim dimensions to trim smallest
    # 
#???    #simplify=TRUE as pems.element if possible else pems
#???    #simplify=FALSE as pems 

    #currently handles negs like data.frame (I think)...

    #negs are discarded if a mixed (neg and pos) vector supplied
    #and forced. Possibly a better way of handling this

#????    #overwrite = TRUE, if pems info/attributes in value 
    #                  write them over what is pems else use 
    #                  pems attributes if there
#????    #overwrite = FALSE, if pems info/attributes in pems 
    #                  write retain it. If nothing and 
    #                  something in value should be use it?
    #                  or does this make third case

#????    #attribute.source = "x", x only "x.value" x then value, etc, ...
    #                   "value", "value.x" 


    #######################
    #check what I was supplied
    #######################

    call1 <- sys.call()
    call2 <- match.call()
    check.i <- !missing(i)
    check.j <- !missing(j)
    check.op <- FALSE

    ###########################
    #force special handling
    ###########################

    #options omit.err.cases (default forcing)
    if(is.logical(force))
        force <- if(force) "omit.err.cases" else "no.forcing"
    if(!is.character(force))
         stop("In pems[i,j]<-: unknown force option", 
                              call. = FALSE)

# this would restrict forcing to known types

#    if(!all(force %in% c("no.forcing", "omit.err.cases", "na.pad.target", "na.pad.insert", "fill.target", "crop.insert")))
#         stop("In pems[i,j]<-: unknown force option", 
#                              call. = FALSE)

    ##############################
    #crack open pems
    ##############################
    
    old.class <- class(x)
    class(x) <- "not.pems"

    #########################
    #make it a standard pems[i,j]
    #########################

    #request pems[], pems[,], etc
    #might think about this 
    if(!check.i & !check.j) {
        i <- 1:nrow(x$data)
        j <- 1:ncol(x$data)
    }

    
    #request pems[i], etc
    #columns/elements, etc
    if(check.i && !check.j && length(as.character(call1))==length(as.character(call2))){
       j <- i
       i <- 1:nrow(x$data)
       check.op <- TRUE
    }

    #request pems[i,], etc
    #rows, etc
    if(check.i && !check.j && length(as.character(call1))!=length(as.character(call2)))
        j <- 1:ncol(x$data)

    #request pems[,j], etc
    #columns/elements
    if(!check.i && check.j)
        i <- 1:nrow(x$data)
    
    #at this point the request must be in form pems[i,j]

    ##############################
    #special case - negatives
    ##############################
 
    #current not going to accept them
    test <- character()
    if(is.numeric(j) && any(j<=0)) test <- c(test, "elements")
    if(is.numeric(i) && any(i<=0)) test <- c(test, "rows")
    if(length(test)>0)
        stop(paste("In pems[i,j]<-: neg ",
                   paste(test, sep=" and ", collapse=" and "),
                   " requested \n       [not currently allowed]",
                   sep=""), call. = FALSE)

    ################################
    #identify allowed cases
    ################################
    #T/F - is/isnt there 
    check.i <- if(is.character(i))
                   i %in% row.names(x$data) else i %in% 1:nrow(x$data)
    check.j <- if(is.character(j))
                   j %in% names(x$data) else j %in% 1:ncol(x$data)

    ###################
    #get value dimensions
    ###################

    #previous attempts to standardise value did not work
    check.value <- NULL
#testing grepl("POSIX*", is(value)[1])
#to insert time stamp into pems object
    if(is.vector(value) | is(value)[1]=="pems.element" | is.factor(value) | grepl("POSIX*", is(value)[1])){
        check.value <- "vector"
        value.dim <- c(length(value),1)
    }
    if(is.data.frame(value)){
        check.value <- "data.frame"
        value.dim <- dim(value)
    }
    if(is(value)[1]=="pems"){
        check.value <- "pems"
        class(value) <- "not.pems"
        pems.units <- value$units
        value <- value$data
        value.dim <- dim(value)
    }
    if(is.null(check.value)){
        stop("In pems[i,j]<-: can't insert insert of that class!", call.=FALSE)
    }

    

#    if(is.vector(value) | is(value)[1]=="pems.element"){
#        temp <- attributes(value)
#        value <- as.data.frame(value, drop=F, stringsAsFactors=FALSE)
#        if(!is.null(temp)) attributes(value[,1]) <- temp 
#    }
#    if(is.data.frame(value)){
#replace with makePEMS???
#need to make sure 1x1 data.frame 
#does not revert to vector
#        value <- list(data=as.data.frame(value, drop=F, stringsAsFactors =FALSE),
#                      units=data.frame())
#        class(value)<- "not.pems"
#    } 
#    if(class(value)[1]=="pems"){
#        class(value)<- "not.pems"
#    }
#    if(class(value)[1]!="not.pems"){
#        stop("In pems[i,j]<-: can't insert insert of that class!", call.=FALSE)
#    }

#    #here value must have structure value$data
    
    #######################
    #get dim of value 
    #######################
#    value.dim <- dim(value$data)

    #value.dim = value[rows, elements]

#################
#fixed for update
#################

    #######################
    #force by omit.err.cases
    #######################

    if("omit.err.cases" %in% force){
        i <- i[check.i]
        j <- j[check.j]
        check.i <- check.i[check.i]
        check.j <- check.i[check.j]
    }


#################
#fixed in update
#################

    #########################
    #force by na.pad.target (1)
    ######################### 

    #second part to this after crop.value

    if("na.pad.target" %in% force){
        if(any(!check.j)){
            if(is.character(j)){
                temp.j <- j[!check.j]
                j <- j[check.j]
                temp <- names(x$data)
                x$data[,temp.j] <- NA
                temp.j <- names(x$data)[!names(x$data) %in% temp]
#this could still fall over
#if data and unit names mismatched before
                x$units[,temp.j] <- NA
                j <- c(j, temp.j)
                check.j <- rep(TRUE, length(j))
            } else {
                #assuming it is numeric
                temp.j <- (ncol(x$data)+1) : max(j[!check.j], na.rm=TRUE)
                temp <- names(x$data)
                x$data[,temp.j] <- NA
                temp.j <- names(x$data)[!names(x$data) %in% temp]
#as above but remember 
#,10 would create 10 
#plus missing before it
                x$units[,temp.j] <- NA
                check.j <- rep(TRUE, length(j))
            }
        }
        #like above but for row.name, nrow, i, etc...
        #no unit update
        if(any(!check.i)){
            if(is.character(i)){
                temp.i <- i[!check.i]
                i <- i[check.i]
                temp <- row.names(x$data)
                x$data[temp.i,] <- NA
                temp.i <- row.names(x$data)[!row.names(x$data) %in% temp]
                i <- c(i, temp.i)
                check.i <- rep(TRUE, length(i))
            } else {
                #assuming it is numeric
                temp.i <- (nrow(x$data)+1) : max(i[!check.i], na.rm=TRUE)
                temp <- row.names(x$data)
                x$data[temp.i,] <- NA
                check.i <- rep(TRUE, length(i))
            }
        }        
    }

###################
#working for update
###################

    #########################
    #force by crop.insert
    ######################### 

    if("crop.insert" %in% force){
       if(value.dim[1] > length(i)){
           if(check.value=="vector")
               value <- rep(value, length.out=value.dim[1])
           if(check.value=="data.frame" | check.value=="pems")
               value <- value[1:length(i),]
           value.dim[1]<- length(i)
       } 
       if(value.dim[2] > length(j)){
           #should not happen?? with vector
           if(check.value=="data.frame")
               value <- value[,1:length(j)]
           if(check.value=="pems"){
               value$data <- value$data[,1:length(i)]
               pems.units <- pems.units[,1:length(i)]
           }
       }
    }

#######################
#fixed in update
#######################

    #########################
    #force by na.pad.target part 2
    #########################

    #this must be after crop or we pad for something in value
    #we then remove...

    #just do these as numerics for now
    #could look into assigning names from value if there later???

    if("na.pad.target" %in% force){
       if(length(j) < value.dim[2]){ 
           #assuming it is numeric
           temp.j <- (ncol(x$data)+1) : (ncol(x$data)+(value.dim[2]-length(j)))
           temp <- names(x$data)
           x$data[,temp.j] <- NA
           temp.j <- names(x$data)[!names(x$data) %in% temp]
#as na.pad.target 1 
           x$units[,temp.j] <- NA
           j <- c(j, temp.j)
           check.j <- rep(TRUE, length(j))
       }
       if(length(i) < value.dim[1]){ 
           temp.i <- (nrow(x$data)+1) : (nrow(x$data)+(value.dim[1]-length(i)))
           temp <- row.names(x$data)
           x$data[temp.i,] <- NA
           temp.i <- row.names(x$data)[!row.names(x$data) %in% temp]
           i <- c(i, temp.i)
           check.i <- rep(TRUE, length(i)) 
       }
    }

#####################
#fixed for update
#####################

    #############################
    #na.pad.insert
    #############################
    if("na.pad.insert" %in% force){
        if(length(j) > value.dim[2]){
            #if value is a vector don't need to do anything
            #except tell it that it is OK
            if(check.value=="vector"){
                value.dim[2] <- length(j) 
            } else {
                temp.j <- (ncol(value)+1) : (ncol(value)+(length(j)-value.dim[2]))
                value[,temp.j] <- NA
                value.dim <- dim(value)
            }
        }
        if(length(i) > value.dim[1]){
            if(check.value=="vector"){
                value <- c(value, rep(NA, length.out=length(i) - value.dim[1]))
                value.dim[1] <- length(i)
            } else {
                temp.i <- (nrow(value)+1) : (nrow(value)+(length(i)-value.dim[1]))
                value[temp.i,] <- NA            
                value.dim <- dim(value)
            }
        }
    }


#####################
#fixed for update
#####################

    #############################
    #fill.insert
    #############################
    if("fill.insert" %in% force){
        if(length(i) > value.dim[1]){
            if(check.value=="vector"){
                value <- c(value, rep(value, length.out=length(i) - value.dim[1]))
                value.dim[1] <- length(i)
            } else {
                temp.i <- (nrow(value)+1) : (nrow(value)+(length(i)-value.dim[1]))
                value[temp.i,] <- value[rep(1:nrow(value), length.out=length(temp.i)),]            
                value.dim <- dim(value)
            }
        }
        if(length(j) > value.dim[2]){
            #if value is a vector don't need to do anything
            #except tell it that it is OK
            if(check.value=="vector"){
                value.dim[2] <- length(j) 
            } else {
                temp.j <- (ncol(value)+1) : (ncol(value)+(length(j)-value.dim[2]))
                value[,temp.j] <- value[,rep(1:ncol(value), length.out=length(temp.j))]
                value.dim <- dim(value)
            }
        }
    }

    ###############################
    #check dimensions
    ###############################

#could move the first of these message to just after 
#omit.err.cases
#might fall over otherwise

    #error out if it does not fit.
    fault.message <- "In pems[i,j]<-value: questionable request"
    if(length(i)<1 | length(j)<1){
        temp <- paste(c("elements", "rows")[c(length(j)<1, length(i)<1)], sep="", collapse=" and ")
        fault.message <- paste(fault.message, "\n       no valid ", temp, " set in pems[i,j]", sep="", collapse="")
        fault.message <- paste(fault.message, "\n       [check force setting if insertion required]", sep="", collapse="")
        stop(fault.message, call.=FALSE)
    }
    if(any(!check.i) | any(!check.j)){
        temp <- paste(c("elements", "rows")[c(any(!check.j), any(!check.i))], sep="", collapse=" and ")
        fault.message <- paste(fault.message, "\n       unknown ", temp, " set in pems[i,j]", sep="", collapse="")
        fault.message <- paste(fault.message, "\n       [check force setting if insertion required]", sep="", collapse="")
        stop(fault.message, call.=FALSE)
    }
    if(length(i) != value.dim[1] | length(j) != value.dim[2]){
        temp <- paste(c("elements", "rows")[c(length(j)!=value.dim[2], length(i)!=value.dim[1])], sep="", collapse=" and ")
        fault.message <- paste(fault.message, "\n       ", temp, " pems[i,j] and insert[i,j] dimension mismatch", sep="", collapse="")
        fault.message <- paste(fault.message, "\n       [check force setting if insertion required]", sep="", collapse="")
        stop(fault.message, call.=FALSE)
    }

    #################################
    #try insert
    #################################

    test <- if(check.op)
                try(x$data[j] <- value, silent=T) else 
                try(x$data[i,j] <- value, silent=T)
    if(class(test)[1]=="try-error")
        stop("In pems[i,j]<-value: bad insertion \n       significant pems[i,j]/value class mismatch",
              call.=FALSE)



#error messaging could be more informative?
#could like this into addition force/overwrite options

    ##################################
    #update units
    ##################################
    
    #currently
    #only copying attributes if units not there
    #options could be possible, overwrite or force options
    #could also do a method for data.frames or pems without units
    #would read attributes of each column

    if(check.value=="vector"){
        if("units" %in% names(attributes(value))){
            for(jj in j){
######################
#update units of input 
#replaces units of pems
#######################
##               if(check.op || is.null(x$units[1,jj]) || is.na(x$units[1,jj]) || x$units[1,jj]=="")
                  x$units[1,jj] <- attributes(value)$units
##                  attributes(x$data[,jj]) <- attributes(value)
               }
        }
    }    
    if(check.value=="pems"){
#might need an error catcher for 
#not pems.units
         for(jj in 1:length(j))
######################
#update units of input 
#replaces units of pems
#######################
##             if(is.null(x$units[1,j[jj]]) || is.na(x$units[1,j[jj]]) || x$units[1,j[jj]]==""){
                 x$units[1,j[jj]] <- pems.units[1,jj]
##                 attributes(x$data[,j[jj]]) <- attributes(value[,jj])
##             }
    }    

    #####################
    #history update
    #####################

    #might want a silence history logging option?

    if ("history" %in% names(x)) 
        x$history <- c(x$history, call2)

################################
#class conflicts not currently handled
#next jobs....
#think about simplify
#does it have meaning here?
################################

    #check for broken time.stamps
    test <- names(x$data)[sapply(x$data, function(x) any(grep("POSIX", class(x))))]
    if(length(test)>0)
        for(t in test)
             class(x$data[,t]) <- unique(c(class(x$data[,t]), "POSIXct", "POSIXt"))

    ####################################
    # send back data
    ####################################

#    class(x) <- old.class
    class(x) <- "pems"
    return(rebuildPEMS(x))

}











##########################
#########################
##$.pems
#########################
#########################

`$.pems` <- function(x, name, ...){


#####################
#old version
#####################
#    class(x) <- "not.pems"
#    ans <- try(x$data[, i], silent = TRUE)
#    if(class(ans)[1] == "try-error"){
#        warning("Element '", i, "' not found in pems", call. = FALSE)
#        return(NULL)
#    }
#    if (!is.null(ans)) 
#        attr(ans, "name") <- i
#    if (!is.null(ans) && !is.null(units)) 
#        if (is.null(attributes(ans)$units)) 
#            attr(ans, "units") <- x$units[1,i]
#        class(ans) <- "pems.element"
#    ans
#think about x[,i, simplify=TRUE]
#might not be need because

#######################
#another old version
#######################
#    ans <- try(x[, i], silent = TRUE)
#    if(class(ans)[1] == "try-error"){
#        warning("Element '", i, "' not found in pems", call. = FALSE)
#        return(NULL)
#    }
#    if (!is.null(ans)) 
#        attr(ans, "name") <- i
##########################
#this looks a bit screwy
#########################
#    if (!is.null(ans) && !is.null(units)) 
#        if (is.null(attributes(ans)$units)) 
#            attr(ans, "units") <- x$units[1,i]
#    ans

########################
#another old version
########################
#    ans <- try(x[, name, simplify = TRUE, force = FALSE], silent = TRUE)
#make silent like data.frame$does.not.exist
#    if(class(ans)[1] == "try-error"){
#        warning("Element '", i, "' not found in pems", call. = FALSE)
#        return(NULL)
#    } else return(ans)

    ans <- try(x[, name, simplify = TRUE, force = FALSE], silent = TRUE)
    if(class(ans)[1] == "try-error") NULL else ans

}



#########################
#########################
##$<-.pems
#########################
#########################

`$<-.pems`<- function(x, name, ..., value){

      x[name, force=c("na.pad.insert", "na.pad.target")] <- value
      x

}





#########################
#########################
##with.pems
#########################
#########################

#version 0.1.0 kr 2015-08-02

#test run parsync/carb data analysis

#note currently discards units
#not sure there is a way around this...

with.pems <- function(data, expr, ...) {

   eval(substitute(expr), pemsData(data), enclos = parent.frame())

}





#########################
#########################
##subset.pems
#########################
#########################

#version 0.1.0 kr 2015-09-07

#test run parsync/ucr data analysis

subset.pems <- function(x,...){

    x <- rebuildPEMS(x, "old")
    x[["data"]] <- subset(x[["data"]], ...)
    rebuildPEMS(x)

}









##########################
##########################
##plot.pems
##########################
##########################

#kr 07/12/2011 v 0.2.0

#what it does
##########################
#generates simple plot
#does not keep units
#

#to do
##############################
#remove temp


##' @S3method plot pems
plot.pems <- function(x, id = NULL, ignore = "time.stamp", n = 3, ...) {

   temp <- rebuildPEMS(x, "old")
   class(temp) <- "no.class"

   reply <- temp$data

   if(is.null(reply)){
      message("\npems object [suspect]")
      return(invisible(NULL))
   }

   if(is.null(id)){
       id <- 1:ncol(reply)
       if(length(ignore)>0)
           id <- id[!names(reply) %in% ignore]
       if(n>0 && length(id)>n)
           id <- id[1:n]
   }

   reply <- reply[id]      

   plot(reply, ...)

}








##########################
##########################
##summary.pems
##########################
##########################

#kr 07/12/2011 v 0.2.0

#what it does
##########################
#generates summary reports 
#

#to do
##########################
#make dedicated summary 
#and option for as current as alternative
#


#comments
##########################
#to tidy


##' @S3method print pems
summary.pems <- function(object, ...) {

   object <- rebuildPEMS(object, "old")
   class(object) <- "no.class"

   object <- object$data

   if(is.null(object)){
      message("\npems object [suspect]")
      return(invisible(NULL))
   }

   return(summary(object))
                       
}





##########################
##########################
##units.pems
##########################
##########################

# kr 2024-12-2024 
# maybe look at 
# https://cran.r-project.org/web/packages/units/vignettes/measurement_units_in_R.html#related-work-in-r

#kr 07/12/2011 v 0.2.0

#what it does
##########################
#extracts units from pems 
#

#to do
##########################
#make dedicated summary 
#and option for as current as alternative
#


#comments
##########################
#to tidy

#units and units<- both now use the new structure 

##' @S3method units.pems
units.pems <- function(x) {

    x <- attributes(rebuildPEMS(x))$units

    if(is.null(x)){
       warning("In units(pems): pems unitless [suspect]", call.=FALSE)
       return(invisible(NULL))
    }

    return(x)

}



## @S3method units<-.pems

`units<-.pems` <- function(x, value) {
  
  #like names<-
  #very crude handling units$ and units[]
  
  x <- rebuildPEMS(x)
  old.class <- class(x)
  class(x) <- "data.frame"

  units <- attributes(x)$units 
  if(is.null(units)){
    warning("In units(pems): [suspect units]", call.=FALSE)
    units <- data.frame(matrix(NA, nrow = 1, ncol = ncol(x$data)))
    names(units) <- names(x$data)
  }
  
  if(is.data.frame(value)){
    units[1, 1:ncol(value)] <- value[1,]
    if(!is.null(names(units)))
      names(units)[1:ncol(value)] <- names(value)
  } else {
    units[1,1:length(value)] <- as.character(value)
  }
  attributes(x)$units <- units
  #tidy layers
  for(i in names(units)){
    attributes(x[,i])$units <- units[i]
  }

  class(x) <- old.class
  return(x)
  
}






##########################
##########################
##head.pems
##tail.pems
##########################
##########################

#kr 31/04/2014 v 0.2.4

#if number of columns less than n?
#need to catch this


head.pems <- function(x, n=6, ...){
    x <- rebuildPEMS(x)
    out <- x[1:n,,force=T,simplify=F]
    print(out, rows=n)
    invisible(out)
}


tail.pems <- function(x, n=6, ...) {
    x <- rebuildPEMS(x)
    out <- dim(as.data.frame(x))[1]
    out <- (out-n+1):(out)
    out <- out[out>0]
    out <- x[out,,force=TRUE,simplify=F]
    print(out, rows=n)
    invisible(out)
}



######################################
######################################
##na.omit.pems
######################################
######################################

#kr 0.0.1 2018/07/06

#not sure why I now need this...

#needs more work
#not sure why pems[complete.cases(pems),] dies 
#  when all rows contain NAs 
#  might be down to pems[i,] dying on pems[FALSE,] etc... 

#might be able to reduce code if I fix this...

na.omit.pems <- function(object, ...) {
    object[["data"]] <- object[["data"]][complete.cases(object[["data"]]),]
    object
}


############################################################
############################################################








###############################
#working
###############################



###########################
#i do not think you can do anymore than
#units(pems) <- value
##########################
#error out if it does not work
#not even sure we can do this
##########################



