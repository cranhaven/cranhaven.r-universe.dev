
##########################
##########################
##common.corrections
##########################
##########################

#########################
#updating 
#########################
#this still uses calcCheck
# but checkInput replaced with getPEMSElement 
# and calcPack2 replaced with pemsOutput

#no longer used calcPack2 includes an overwrite...



#kr

#description
##########################
#functions to apply corrections
#so take an input, modify and 
#(unless told not to) 
#save it as the same thing


#includes 
##########################
#correctInput
#correctPitotDrift
#


#
#most urgent
######################################
#

#urgent
##########################
#

#to do
##########################
#

#comments
##########################
#



##########################
##########################
##correctInput
##########################
##########################

#kr 23/11/2013 v 0.0.1

#what it does
##########################
#takes an input
#applies a correction
#function
#return that as input 
#assuming overwrite = TRUE



###############################
###############################
##calcPack2
###############################
###############################

calcPack2 <- function(input, ..., settings = NULL, data = NULL){

    #run checks
    extra.args <- loa::listUpdate(list(this.call=match.call(), fun.name = "calcPack2", 
                                  overwrite=TRUE), 
                             list(...))
    att <- attributes(input)

    if(settings$output=="input")
        return(input)

    data <- checkPEMS(data, output="pems")

    if(settings$overwrite==FALSE){
        temp <- make.names(c(names(data), attributes(input)$name), unique=TRUE)
        attributes(input)$name <- temp[length(temp)]
    } else {
        attributes(input)$name <- make.names(attributes(input)$name)
    }
    att$name <- attributes(input)$name

    old.class <- class(data)
    class(data) <- "not.pems"

    if(length(input)<nrow(data$data)){
        input <- c(input, rep(NA, nrow(data$data)-length(input)))
        attributes(input) <- att
    }

    data$data[1:length(input), attributes(input)$name]<-input

    data$units[1, attributes(input)$name] <- if("units" %in% names(attributes(input)))
                                                attributes(input)$units else NA
   
    if("history"  %in% names(data))
        data$history <- c(data$history, extra.args$this.call)
    
    class(data) <- old.class

    if(settings$output=="data.frame")
        return(pemsData(data)) else return(data)
    
}


################################
################################
##correctInput
################################
################################


#this uses calcChecks, checkInput, calcPack2

#replaced checkInput with getPEMSElement
#         calcPack2 with pemsOutput (not export)

correctInput <- function(input = NULL, ..., data = NULL,
         correction = NULL){

    #run checks
    extra.args <- loa::listUpdate(list(this.call=match.call(), fun.name = "correctInput", 
                                  overwrite=TRUE), 
                             list(...))
    settings <- do.call(calcChecks, loa::listUpdate(list(data=data), extra.args))

#need to look at what this does...

    input <- getPEMSElement(!!enquo(input), data)
    att <- attributes(input)
    temp <- try(names(formals(correction)), silent=TRUE)
    if(class(temp)[1]=="try-error") 
        stop("In ", extra.args$fun.name, ": problem with correction", 
             call. = FALSE)
    ignore <- if("..." %in% temp)
                  NULL else names(extra.args)[!names(extra.args) %in% temp]
    temp2 <- list(input=input)
    if(!"input" %in% temp)
        names(temp2)[1] <- temp[1] 
    ans <- try(do.call(correction, 
                       loa::listUpdate(temp2, extra.args, ignore=ignore)), 
               silent=TRUE)
    if(class(ans)[1]=="try-error")
        stop("In ", extra.args$fun.name, ": problem with correction", 
             call. = FALSE)
    attributes(ans) <- att

    pemsOutput(x=ans, data=data, output=settings$output,
              this.call=extra.args$this.call, fun.name=extra.args$fun.name)
 
}





################################
################################
##zeroNetagive
################################
################################



zeroNegatives <- function(input = NULL, ..., data = NULL, screen = FALSE){

    #run checks
    extra.args <- loa::listUpdate(list(this.call=match.call(), fun.name = "zeroNegatives", 
                                  overwrite=TRUE), 
                             list(...))
    settings <- do.call(calcChecks, loa::listUpdate(list(data=data), extra.args))
    input <- getPEMSElement(!!enquo(input), data, if.missing = "stop", 
                            fun.name = extra.args$fun.name)

    #get current attributes
    att <- attributes(input)

    #main calculation
    ans <- ifelse(input<0, 0, input)

    if(screen){

#proposed plot

#loaPlot(a~1:1000*pems.1$conc.co, panel=panel.compareZcases, 
#scheme="kr.blues", col.regions="Reds", line.col="darkblue")

#



        index <- 1:length(input)
        temp.panel <- function(x=x, y=y, z=z,...){
                            lattice::panel.xyplot(x=x, y=y, col="black", type="l", ...)
                            lattice::panel.xyplot(x=x, y=z, col="red", type="l", ...)
                            lattice::panel.xyplot(x=x, y=y, col=ifelse(y==z, NA, "red"),...)
                      }
        plot.list <- list(x=ans~index*input, grid=TRUE, key=FALSE,
                          panel=temp.panel)
        plot.list <- loa::listUpdate(plot.list, list(...))
        print(do.call(loa::loaPlot, plot.list))

        #accept, discard or rework plot option
        #accept send you on
        #discard stops here
        #rework allows you to redo

    }



    #to use other function to do something similar
    #ans <- correctInput(input = input, correction = function(x) x <- ifelse(x<0,0,x), 
    #                    this.call=extra.args$this.call, 
    #                    fun.name=extra.args$fun.name)
    
    #note: we pass on fun.name, this.call and hijack

    #transfer attributes
    attributes(ans) <- att

    pemsOutput(x=ans, data=data, output=settings$output,
              this.call=extra.args$this.call, fun.name=extra.args$fun.name)
 
}



###############################
###############################
##correctBaseline
###############################
###############################



#kr v.0.3
#04/01/2018

#correct signal baseline

#this is currently very messy but I wanted access to various output

#this was previously in sleeper.service

#transfer should not affect perfromance
#because sleeper.service loads pems.utils

#previsional version 
#need to rationalise this and ofter pems corrections...

#rlang input + data handling
#pemsOutput handling of outputs?

correctBaseline <- function(x, ..., data = NULL, output = "ans"){

    #correct baseline for sleeper.service
    #v 0.2 based on UCR method from 2015/16 work 
    #v 0.3 update to extend output options 01/2018

############################################
#uses baseline package
#if running locally
#    require(baseline)
############################################

############################################
#needs to use
#    input <- getPEMSElement(!!enquo(input), data, units="units.i.want", 
#                            ref.name="what.i.call.it")
#    and include data in formals
#############################################


    #ans <- baseline(..., method='modpolyfit', deg=6)
    #                     method='rollingBall', wm=50, ws=50)

#notes
#like this to work in form
#ans.pems.element <- function(pems.element,...)
#ans <- correctBaseline(x, "rollingball")   #use rolling ball and other defaults (wm and ws)
#ans <- correctBaseline(x, 50, 25)          #use default (rolling ball) and settings wm =50, ws=25
#ans <- correctBaseline(x, ws=75)           #use rolling ball and settings wm =50, ws=25


    #set up method
    x.args <- list(...)
    x <- getPEMSElement(!!enquo(x), data, fun.name=x.args$fun.name, 
                        if.missing="stop", ref.name="x")

#think about this

    #output="plot" is same as output="diagnostic"
    if("plot" %in% output) output[output=="plot"] <- "diagnostic"

#na.handling
    na.ref <- NULL
    if(any(is.na(x))){
        na.ref <- is.na(x)
        x <- x[!na.ref]
    } 

###############################
#simplify this?

    #handle unnames args
    if(length(x.args)<1) x.args <- list(method="rollingBall") else if(is.null(names(x.args))) names(x.args) <- rep(NA, length(x.args))
    if(!"method" %in% names(x.args)){
        test <- sapply(x.args, is.character) & is.na(names(x.args))
        if(length(test)>0 && length(test[test])>0)
            names(x.args)[which(test)[1]] <- "method" else
                 x.args$method <- "rollingBall"
    }

    #defaults for rollingball
    if(tolower(x.args$method) == "rollingball"){
        x.args$method <- "rollingBall"
        if(!"wm" %in% names(x.args)){
           test <- sapply(x.args, is.numeric) & is.na(names(x.args))
           if(length(test)>0 && length(test[test])>0)
                names(x.args)[which(test)[1]] <- "wm" else
                     x.args$wm <- 50
           }
        if(!"ws" %in% names(x.args)){
           test <- sapply(x.args, is.numeric) & is.na(names(x.args))
           if(length(test)>0 && length(test[test])>0)
                names(x.args)[which(test)[1]] <- "ws" else
                     x.args$ws <- 50
           }
    }

#
###############################


###test###
#   print(x.args)

    #save x attributes
    x.attr <- attributes(x)

    #do baseline subtraction

    bc <- do.call(baseline::baseline, loa::listUpdate(list(spectra=as.matrix(t(as.vector(x)))), x.args))

    ans <- as.vector(baseline::getCorrected(bc))

#temp diagnostic
#update for nicer plot
#based on code from NIESL project

    if (any(output %in% c("diagnostic", "all", "baseline", "pems", "df"))) {
         bl <- as.vector(baseline::getBaseline(bc))
         index <- 1:length(ans)
         df <- data.frame(index = index, x, baseline = bl, case = "input")
         names(df)[2] <- "x"
         df <- rbind(df, data.frame(index = index, x = ans, baseline = NA, 
                     case = "output"))
         cols <- loa::colHandler(1:6, col.regions = "Greens")[c(6, 3)]
         plt <- (lattice::xyplot(x + baseline ~ index | case, data = df, 
                 grid = TRUE, scales = list(y = list(relation = "free")), 
                 layout = c(1, 2), type = "l", key = list(text = list(c("x", 
                 "baseline")), lines = list(col = cols)), between = list(y = 0.5), 
                 as.table = TRUE, col = cols))
         if("name" %in% names(attributes(x))){
              plt$ylab <- paste(attributes(x)$name, " + baseline", sep="")
              plt$legend$top$args$key$text[[1]][1] <- attributes(x)$name
         }
    plt$legend$top$args$key$lines$lwd <- c(1,10)
    plt$legend$top$args$key$lines$col <- loa::colHandler(1:6, col.regions="Greens")[c(5,2)]
    plt$panel <- function(x=x, y=y, groups=groups, col=col, subscripts=subscripts, ...){
        groups <- groups[subscripts]
        #if(panel.number()==2) y[y< -0.005] <- -0.005
        lattice::panel.grid(-1,-1)
        xx <- x[groups==(levels(groups)[2])]
        yy <- y[groups==(levels(groups)[2])]
        ccol <- col[2]
        xx <- c(xx, rev(xx))
        yy <- c(yy, rep(min(yy), length.out=length(yy)))
        lattice::panel.polygon(x=xx, y=yy, col=ccol, border=NA, alpha=0.5)
        xx <- x[groups==(levels(groups)[1])]
        yy <- y[groups==(levels(groups)[1])]
        ccol <- col[1]
        lattice::panel.xyplot(x=xx, y=yy, col=ccol, type="l")
    }
    
if(length(output)==1 && output=="diagnostic") return(plt) else 
                        if("all" %in% output)print(plt)
}

#other option to make a little pems: x, base.line, ans

#should tidy this when I am happy with output
    
    if("diagnostic" %in% output) output <- output[output!="diagnostic"]

    if("df" %in% output) return(df)
    if(length(output)==1 && output == "df") return(df)

#na.handling if na.ref is not null
    if(!is.null(na.ref)){
         temp <- rep(NA, length(na.ref))
         temp[!na.ref] <- ans
         ans <- temp
         if(any(output %in% c("pems", "baseline"))){
             temp <- rep(NA, length(na.ref)) #needed?
             temp[!na.ref] <- bl
             bl <- temp
         }
    }

    #reset output so like x
    attributes(ans) <- x.attr
    if(length(output)==1 && output == "pems") {
              attributes(bl) <- x.attr
              attributes(bl)$name <- "baseline"
              out <- pems(data.frame(x=ans, baseline=bl), units=c(units(ans), units(bl)))
              if("name" %in% names(attributes(ans))) names(out)[1] <- attributes(x)$name
              return(out)
    } 

    if(length(output)==1 && output == "baseline") {
        attributes(bl) <- x.attr
        attributes(bl)$name <- "baseline"
        return(bl)
    }

    #output
    ans

}


