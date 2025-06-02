##########################
##########################
##merge pems
##########################
##########################

#kr

#testing 

#cAlign_ylagxCOR <- function(x, y) {
#  .Call('C_ylagxCOR', PACKAGE = 'pems.utils', x, y)
#}


#description
##########################
#functions for merging data and pems

#includes 
##########################
#align
#cAlign
#cAlign.old (not exported)
#C_ylagxCOR (c code)
#tAlign
#findLinearOffset
#stackPEMS

#removed
############################
#bindPEMS

#to do
##########################
#check vector issue with align
#

#comments
##########################
#




##########################
##########################
##align
##########################
##########################

#kr v.0.4 31/11/2015

#align
#plyr, loa
#from sleeper.service

#aligns data in two objects
##uses 
##full_join in dplyr
##pems.utils structure


#comments
##this is quick version of alignment

align <- function(data1, data2, n=0, ...){

#data alignment function
#this currently returns a pems object
#could return it in simplest of supplied cases 
#or most complex???

#to look into 
#this does not track names of vectors
#starting with example align(x,y,-15)
#makes frame with names x and x.1...

   extra.args <- list(...)
   this.call <- if("this.call" %in% names(extra.args))
                     extra.args$this.call else match.call()
   data1 <- rebuildPEMS(makePEMS(data1), "new")
   data2 <- rebuildPEMS(makePEMS(data2), "new")

   att <- loa::listUpdate(attributes(data1)$pems.tags,
                     attributes(data1)$pems.tags, 
                     ignore.b="history")
   #att <- attributes(data1)
#print(names(att))
   #att <- loa::listUpdate(att, attributes(data2))
#print(names(att$pems.tags))
#print(names(data1[["constants"]])) 

  data1 <- rebuildPEMS(makePEMS(data1), "old")
  data2 <- rebuildPEMS(makePEMS(data2), "old")

   new.names <- make.names(c(names(data1), names(data2)), unique=TRUE)
   names(data1) <- new.names[1:ncol(data1)]
   names(data2) <- new.names[(ncol(data1)+1):length(new.names)] 

   #calculate refs including offset
   data1$align.temp.ref <- 1:nrow(data1)
   data2$align.temp.ref <- (1:nrow(data2)) + n

   #merge data 
   new.data <- dplyr::full_join(pemsData(data1), pemsData(data2), by="align.temp.ref")
   new.data <- new.data[order(new.data$align.temp.ref),]
   row.names(new.data) <- 1:nrow(new.data)

   #use this to order elsewhere
   ##if(order)
   ##   temp <- arrange(temp, order(temp[,t1]))

   #remove the align.temp.ref
   new.data <- new.data[names(new.data)[names(new.data) %in% new.names]]

   #get associated units
   new.units <- cbind(units(data1), units(data2))
   new.units <- new.units[new.names[new.names %in% names(new.units)]]

   #get rest of structure
   #class(data1) <- class(data1)[class(data1)!="pems"]
   #class(data2) <- class(data2)[class(data2)!="pems"]
#print(data1)
   out <- pems(x=new.data, units=new.units)
   attributes(out)$pems.tags <- loa::listUpdate(att, attributes(out)$pems.tags)
#   data1 <- listUpdate(data2, data1, ignore=c("data", "units"))
#   data1 <- listUpdate(list(data=new.data, units=new.units), data1)

#this should keep all tags
#but needs testing
   out[["history"]] <- this.call
   #class(data1) <- "pems" 

   return(rebuildPEMS(out))
}





######################################
######################################
##cAlign
######################################
######################################

#currently not exported

#kr 06/01/2018 v 0.2.0
#version update from SGS L&L

#replaced previous cAlign  
#add a method option

#this needs tidying


cAlign.old <- function(form, data1=NULL, data2 = NULL, ...){

#function for the time alignment of data in two datasets
#using a common time-series or similar time-series.

#form formula 
#data1 first data source
#data2 optional second data source

#uses 
#ccf in  base package?
#align in sleeper.service

#changes relative to previous
#added lag.start = "middle"
#added default for lag.max
#added min.overlap

#to do
#tidy error messages
#need to get this working with no data
#cAlign(x~y)
#need to sort out replacement for find offset command

    #set up
    data1 <- makePEMS(data1)
    vars <- as.character(form)[-1]
    #vars[1] is the "~"

#tidy the next bit later
#but don't want it as formal
#output = all needed...

    extra.args <- list(...)
    output <- if(is.null(extra.args$output)) c("plot", "pems") else extra.args$output
    if("plot" %in% names(extra.args))
         output <- if(extra.args$plot) 
             unique(c(output, "plot")) else output[output != "plot"] 
    if("pems" %in% names(extra.args))
         output <- if(extra.args$pems) 
             unique(c(output, "pems")) else output[output != "pems"]
    if("offset" %in% names(extra.args))
         output <- if(extra.args$offset) 
             unique(c(output, "offset")) else output[output != "offset"] 
    extra.args <- extra.args[!names(extra.args) %in% c("plot", "pems", "offset", "output")]

#not tested
    if(is.null(data2)){
        if(length(vars)<2) stop("need two cases if only one data set provided")
        if(nrow(data1)<1){
            data2 <- data1
        } else {
            data2 <- makePEMS(data1[all.vars(form)[2]])
            data1 <- data1[names(data1)[names(data1) != all.vars(form)[2]]]
        }
    }

    #this first variable with be vars[1]
    #note: x and y remain data.frames here
    #(see below about making this work with no data)
    #(backward compat...)
    x <- try(model.frame(as.formula(paste("~", vars[1], sep="")), data1, na.action = na.pass),
             silent = TRUE)
    if(class(x)[1]=="try-error") stop("cAlign() conflict, '", vars[1], "' not found where expected", 
                                      call. = FALSE)
    
    #get next term
    temp <- if(length(vars)>1) vars[2] else vars[1]    
    y <- try(model.frame(as.formula(paste("~", temp, sep="")), data2, na.action = na.pass),
             silent = TRUE)
    if(class(y)[1]=="try-error") stop("cAlign() conflict, '", temp, "' not found where expected", 
                                      call. = FALSE)

    #to make above work with no data sources
    if(nrow(data1)<1) data1 <- makePEMS(x)
    if(nrow(data2)<1) data2 <- makePEMS(y)
    x <- x[,1]
    y <- y[,1]

#to think about
#################################################
#would add mask here
#could use something like mask.1=1:30 to hide first 30 points of x, etc...

    #######################
    #for lag.start
    len.x <- length(x)
    len.y <- length(y)
    lag.tidy <- FALSE
    if(!"lag.start" %in% names(extra.args) || extra.args$lag.start=="middle"){
           lag.tidy <- TRUE
           extra.args$lag.start <- floor((0.5* len.x)-(0.5*len.y))
    }

    if(extra.args$lag.start>0) y <- c(rep(NA,extra.args$lag.start), y)
    if(extra.args$lag.start<0) x <- c(rep(NA,-extra.args$lag.start), x)

#lag.tidy<-TRUE 

#print(length(x))
#print(length(y))
    if(lag.tidy){
       if(length(x)>length(y)) y <- c(y, rep(NA, length(x)-length(y)))
       if(length(y)>length(x)) x <- c(x, rep(NA, length(y)-length(x)))
    }

##################################


#might rethink error messages
#so only message if both args are missing

    #align using ccf
#might still need to do more work on what we pass
#and make it more robust? ccf formals only???


###################################
#new bits

#need to sort out min.overlap
#        lag.max
#        min.overlap
#        method control

#need to think about best 10, etc...

    if(!"min.overlap" %in% names(extra.args))
         extra.args$min.overlap <- 0

#think about limiting
#    if(extra.args$min.overlap>min(c(len.x, len.y))) extra.args$min.overlap <- min(c(len.x, len.y))

    if(!"lag.max" %in% names(extra.args)) 
        extra.args$lag.max <- if(lag.tidy) ceiling((0.5*len.x)+(0.5*len.y)- extra.args$min.overlap) else 
                                           min(c(length(x), length(y))) 

    if(length(x) < extra.args$lag.max) x <- c(x, rep(NA, extra.args$lag.max -length(x)))
    if(length(y) < extra.args$lag.max) y <- c(y, rep(NA, extra.args$lag.max -length(y)))

#default lag.max to 30 for old case...

    ans <- do.call(stats::ccf, loa::listUpdate(list(x=x, y=y, na.action=na.pass, plot=FALSE), 
                                   extra.args))
    ans$lag <- ans$lag + extra.args$lag.start

##########################

    ##ans <- ccf(x, y, na.action=na.pass, plot=FALSE, ...)
    fit <- ans$lag[which(ans$acf==max(ans$acf, na.rm=T))]

#might want to do a prettier plot
#this is before offset check 

    if("plot" %in% output){
       plot(ans, main="cAlign ACF")
       abline(v=0, col="pink", lty=3)
       abline(v=fit, col="red", lty=3)
       if(fit!=0)
           arrows(0, max(ans$acf, na.rm=T) ,fit, max(ans$acf, na.rm=T), 
                  col="red", 0.1)
    }

    if("offset" %in% output)
       if(!"pems" %in% output) return(fit) else 
           print(paste("offset = ", fit, sep=""))

    return(align(data1, data2, fit))

}


#kr 15/11/2018 v 0.5.0
#version update from RDE work...

cAlign <- function(form, data1=NULL, data2 = NULL, ...){

#function for the time alignment of data in two datasets
#using a common time-series or similar time-series.

#form formula 
#data1 first data source
#data2 optional second data source
  
#uses
#align 
#C_ylagxCORR

#changes relative to previous
#removed lag.start
#removed lag.max
#reinstated min.overlap

#to do
#######################
#tidy error messages
#input handling 
  
#need to get this working with no data
#cAlign(x~y)
#need to sort out replacement for find offset command
#     maybe just cAlign(x, y, output="offset", ...)
  
#to think about 
#######################
##would add mask here
#could use something like mask.1=1:30 to hide first 30 points of x, etc...


  ######################
  #set up
  ######################
  data1 <- makePEMS(data1)
  vars <- as.character(form)[-1]  #vars[1] is the "~"
#tidy the next bit later
#but don't want it as formal
  extra.args <- list(...)
  output <- if(is.null(extra.args$output)) 
    c("plot", "pems") else extra.args$output
  if("all" %in% output) output <- c("pems", "plot", "offset")
  if("plot" %in% names(extra.args))
     output <- if(extra.args$plot) 
                  unique(c(output, "plot")) else output[output != "plot"] 
  if("pems" %in% names(extra.args))
    output <- if(extra.args$pems) 
                unique(c(output, "pems")) else output[output != "pems"]
  if("offset" %in% names(extra.args)) 
    output <- if(extra.args$offset) 
                unique(c(output, "offset")) else output[output != "offset"] 
  extra.args <- extra.args[!names(extra.args) %in% 
                  c("plot", "pems", "offset", "output")]
  if(is.null(data2)){
    if(length(vars)<2) 
      stop("need two elements if only one data set provided")
    if(nrow(data1)<1){
      data2 <- data1
    } else {
      #data2 <- makePEMS(data1[all.vars(form)[2]])
      #data2 <- makePEMS(data2)
      data2 <- makePEMS(data1[all.vars(form)[2]])
      data1 <- data1[names(data1)[names(data1) != all.vars(form)[2]]]
    }
  }

  
  
  #this first variable with be vars[1]
  #note: x and y remain data.frames here
  #(see below about making this work with no data)
  #(backward compat...)
  x <- try(model.frame(as.formula(paste("~", vars[1], sep="")), 
              data1, na.action = na.pass), silent = TRUE)
  if(class(x)[1]=="try-error") 
    stop("cAlign() conflict, '", vars[1], "' not found where expected", 
         call. = FALSE)
  #get next term
  temp <- if(length(vars)>1)  vars[2] else vars[1]    
  y <- try(model.frame(as.formula(paste("~", temp, sep="")), data2, 
         na.action = na.pass), silent = TRUE)
  if(class(y)[1]=="try-error") 
    stop("cAlign() conflict, '", temp, "' not found where expected", 
         call. = FALSE)
  #to make above work with data sources
  
  if(nrow(data1)<1) data1 <- makePEMS(x)
  if(nrow(data2)<1) data2 <- makePEMS(y)

  x <- x[,1]
  y <- y[,1]

  #############################
  #main routine
  #############################
  #this is faster than version 3 but
  #this is still slower than older cAlign (1-2) that 
  #used stat ccf function
  
  #fit smallest to biggest
  if(length(y)>length(x)){
    temp <- y
    y <- x
    x <- temp
    reversed=TRUE
  } else {
    reversed=FALSE
  }
  
  #set min.overlap if not in call
  if(!"min.overlap" %in% names(extra.args))
    extra.args$min.overlap <- min(c(floor(min(length(x), 
                                              length(y))*0.2), 2000))
  pad <- length(x) - extra.args$min.overlap
  y <- c(rep(NA, pad), y, rep(NA, pad))
  ans <- sapply(1:(length(y)-pad), function (j){
    .test <- y[j:(length(x)+j-1)]
    .tt <- !is.na(x) & !is.na(.test)
    if(length(.tt[.tt]) < 10){
      NA
    } else {
    suppressWarnings(cor(x, .test, use= "pairwise.complete.obs"))
    }
  })
  
  #return(ans)
  #use C_ylagxCOR to solve this
#return (list(x=x, y=y))
  
  #ans <- .Call("_pems_utils_C_ylagxCOR", x, y)
  index <- (1:length(ans)) - length(x) + extra.args$min.overlap - 1
  if(!reversed) index <- -index
  ans2 <- index[which(ans==max(ans, na.rm=TRUE))[1]]   #[1] in case tie!!

##########################
#plot input tidy
##########################
  
  #this is before offset check 
  if("plot" %in% output){
    plot(index, ans, main="cAlign ACF", type="h")
    abline(v=0, col="pink", lty=3)
    abline(v=ans2, col="red", lty=3)
    if(ans2!=0)
      arrows(0, max(ans, na.rm=T) ,ans2, max(ans, na.rm=T), 
             col="red", 0.1)
  }

  #####################
  #offset to sort out
  ####################
#this should be better handled
  if("offset" %in% output)
     if(!"pems" %in% output) return(ans2) else 
         print(paste("offset = ", ans2, sep=""))
  return(align(data1, data2, ans2))

}








##########################
##########################
##findLinearOffset
##########################
##########################

#kr 15/11/2018 v 0.3.0
#update based on cAlign 15/11/2018 update

#what it does
##########################
#Finds the linear offset between
#two vectors 

#to do
##########################
#make test more robust?
#make it handle data.frames, etc

#comments
##########################
#
#might not be keeping this 

findLinearOffset <- function(x = NULL, y = NULL, ...) cAlign(x~y, output="offset", ...)










######################################
######################################
##tAlign
######################################
######################################


#kr 26/12/2015 v 0.0.3
#using sleeper.service version

tAlign <- function(form, data1, data2 = NULL, order = TRUE, ...){

#function for the time alignment of data in two datasets
#using a common time.stamp, local.time, etc.

#form formula 
#data1 first data source
#data2 optional second data source

#not sure this makes sense

#uses 
#full_join in dplyr

#urgent to do
#order does not seem to be working

#to do
#think about data2 being optional
#tidy error messages
#think about information lost on merging
#(look at how align handles this)

    #set up
    data1 <- makePEMS(data1)
    vars <- as.character(form)[-1]
    #as.character(form)[1] is the "~"

#not sure this is wanted/needed
#would time aligning a single case be useful
    if(is.null(data2)){
        if(length(vars)<2) stop("need two cases if only one data set provided")
        data2 <- makePEMS(data1[all.vars(form)[2]])
        data1 <- data1[names(data1)[names(data1) != all.vars(form)[2]]]
    }

#if a ~ b then b needs to be renamed as a
    if(length(vars)>1)
        names(data2)[names(data2)==vars[2]] <- vars[1]

     t1 <- vars[1]

#make names unique 
#merging does not drop cases
    temp1 <- names(data1)
    temp2 <- names(data2)
    temp <- make.names(c(temp1, temp2), unique=TRUE)
    names(data1) <- temp[1:length(temp1)]
    names(data2) <- temp[(length(temp1)+1):length(temp)] 
    names(data2)[temp2 == t1] <- t1

#pass data to join
    temp1 <- pemsData(data1)
    temp2 <- pemsData(data2)
    temp <- full_join(temp1, temp2, by=t1)

    #names in new case
    temp.names <- names(temp)

    #set up units
    units <- units(data1)[names(units(data1)) %in% temp.names]
    temp.names <- temp.names[!temp.names %in% names(units(data1))]
    units <- cbind(units, units(data2)[names(units(data2)) %in% temp.names])

    #reorder based on time case
#arrange seems a little unreliable???
#could replace with temp[order(...),]
#could be something else going on here???

    if(order)
        temp <- temp[order(temp[,t1]),]

    #tidy
    row.names(temp) <- 1:nrow(temp)

#this does not seem to work with POSIX classes
#    if(order)
#        temp <- arrange(temp, order(temp[,t1]))

    #might rethink this to get more info out of data 1, 2
    #makePEMS() & pems() return invisible()
    out <- makePEMS(temp, units)
    out

}







####################################
####################################
##stackPEMS
####################################
####################################

#kr v.0.2.1 2018/07/04

stackPEMS <- stack <- function(..., key=key, ordered=TRUE){
  
  #####################
  #notes
  #####################
  #exporting from rlang: get_expr, exprs
  #####################
  #key = source identifier 
  #  where source is the name used for the column 
  #     indicating what is stacked....
  #     so stackPEMS(pems.1, d2=pems.2) 
  #     should identify sources as pems.1 and d2

  #####################
  #to do
  #####################
  #sort name

  #####################
  #to think about
  #####################
  #think about specifying rlang for exprs, etc?
  #think about stripping list(...) of not-pems..?
  #think about order of elements 
  #     should the key always be last
  #think about key handling 
  #     should it overwrite an existing case
  # 
  
  temp <- exprs(...)
  refs <- names(temp)
  refs[refs==""] <- as.character(temp[refs==""])
  #added make.unique() refs below in case same pems sent twice...
  #   (have to do this to refs after temp/ref update)
  #   (not sure why anyone doing this...)
  refs <- make.unique(refs)
  key <- as.character(get_expr(enquo(key)))
  
  dots <- list(...) #rlang this later?
  d1 <- rebuildPEMS(pems(dots[[1]]))
  d1[, key, force=c("na.pad.target", "fill.insert")] <- refs[1]
  
  for(i in 2:length(dots)){
    
    #need to get this here but not d1     
    d2 <- rebuildPEMS(pems(dots[[i]]))
    d2[, key, force=c("na.pad.target", "fill.insert")] <- refs[i]
     
    #compare names 
    ref <- dplyr::intersect(names(d1), names(d2))
    for(i in ref){
      test <- units(d1[i])==units(d2[i])
      if(!is.na(test) && !test){
        d2[i] <- convertUnits(d2[i], to=units(d1[i]))
      }
    }
    d1[["data"]] <- dplyr::bind_rows(fortify(d1), fortify(d2))

    ##################################
    #fix for names with spaces in    
    #temp <- as.data.frame(loa::listUpdate(as.list(units(d1)), 
    #                                 as.list(units(d2))),
    #                      stringsAsFactors=FALSE)
    ##################################
    temp <- loa::listUpdate(as.list(units(d1)), 
                       as.list(units(d2)))
    test <- names(temp)
    temp <- as.data.frame(temp, stringsAsFactors=FALSE)
    names(temp) <- test
    ##################################
    #better way?	
    ##################################

    d1[["units"]] <- temp[names(d1[["data"]])]  
    att.1 <- attributes(d1)$pems.tags
    att.2 <- attributes(d2)$pems.tags
    attributes(d1)$pems.tags <- loa::listUpdate(att.1, att.2)
  }
  d1[key] <- factor(d1[key], levels=refs, ordered=ordered)
  #make the vectors pems.elements
  for(i in names(d1))
       d1[["data"]][, i] <- d1[, i]
  d1
}
