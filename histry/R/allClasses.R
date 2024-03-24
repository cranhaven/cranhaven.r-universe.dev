#' @import methods evaluate
#' @importFrom utils tail head



#' @importFrom fastdigest fastdigest
ht_callback = function(expr, value, success, printed, tracker) {
    if(!success)
        return(TRUE)
    tracker$addInfo(expr = expr, class = utils::head(class(value), 1), hash = fastdigest(value))
    TRUE
}


ignorepattern = "^.ess"


##' @name HistoryData
##' @description A (non-reference) class representing the known history state
##' @title HistoryData
##' @docType methods
##' @exportClass HistoryData
##' @aliases HistoryData-class
setClass("HistoryData", representation = list(exprs = "ANY", classes = "character", hashes = "character"))


#' @name HistoryTracker
#' @description These classes implement history tracking in various contexts
#' @title A reference class for tracking code history
#' @docType methods
#' @exportClass VirtHistoryTracker
#' @rdname HistoryTracker
#' @aliases VirtHistoryTracker-class

vh_tracker = setRefClass("VirtHistoryTracker",
                         fields = c(hdata = "HistoryData",
                                   tracking = "logical"),
                        methods = list(
                            addInfo = function(expr, class, hash, envir = .GlobalEnv) {


                        
                            if(is.character(expr) && length(expr) > 1)
                                expr = paste(expr, collapse="\n")

                            if(any(grepl(ignorepattern,expr)))
                                return(NULL)
                            newdat= new("HistoryData", exprs = expr, classes = class, hashes = hash)
                            .self$hdata = combineHistry(hData(.self), newdat)
                        },
                        toggleTracking = function() stop("Not implemented on virtual class"),
                        clear = function() {
                            .self$hdata = new("HistoryData")
                        },
                        filter = function(syms = ls(ns, all.names=TRUE), ns = emptyenv()) {
                            

                            
                        },
                        importHistory = function(impdata, before = TRUE) {
                            if(is.character(impdata) && length(impdata) == 1 && file.exists(impdata))
                                impdata = readRDS(impdata)
                            if(!is(impdata, "HistoryData"))
                                stop("Only able to import HistoryData objects")
                            .self$hdata = combineHistry(x = .self$hdata, y = impdata, before = before)
                        }))


#' @rdname HistoryTracker
#' @docType methods
#' @exportClass HistoryTracker
#' @aliases HistoryTracker-class
h_tracker = setRefClass("HistoryTracker",
                        contains = "VirtHistoryTracker",
                        fields = c(
                            id = "character"
                            ),
                        methods = list(
                            initialize = function(id = "hist_tracker", ...) {
                            exstcbs = getTaskCallbackNames()
                            origid = id
                            id2 = id
                            while(id2 %in% exstcbs)
                                id2 = paste0(origid, fastdigest(id2))
                            
                            obj = callSuper(...)
                            obj$id = id2
                            obj$tracking = FALSE
                            obj$toggleTracking()
                            obj
                        },
                        toggleTracking = function() {
                            if(.self$tracking) {
                                removeTaskCallback(.self$id)
                                .self$tracking = FALSE
                            } else {
                                addTaskCallback(ht_callback, data = force(.self), name = id)
                                .self$tracking = TRUE
                            }
                            .self$tracking
                        })
                        )


##' @title knitr history tracking
##'
##' These functions are exported due to the vagaries of how tracing
##' functions works in R. Knitr history support is now turned on when
##' the histry package is loaded; They should never be called directly
##' by an end user.
##' 
##'
##' @param on logical. Should tracking be turned on (TRUE) or off (FALSE)
##' @param record logical. Should visibly printed results within the weaving
##' process be recorded (if trackr is available).
##' @rdname tracers
##' @export
knitrtracer = function(on, record = FALSE) {
    if(!requireNamespace("evaluate", quietly=TRUE) || !requireNamespace("knitr", quietly=TRUE))
        return(NULL)
    
    if(on) {
        ## evaltracer is always on now. Only reason to ever turn it off is to turn it back on immediately with different record value.
        ## The tracer for evaluate:::evaluate_call now checks if we're in knitr, so no need to make sure it's off
        ## in when we aren't.
        suppressMessages(trace(knitr::knit,
                               where = asNamespace("knitr"),
                               tracer = parse(text = 'if(!opts_knit$get("child")) histropts()$knitrHistory$clear(); histry_setinknitr(TRUE)'),
                               exit = quote(if(!opts_knit$get("child")) histry_setinknitr(FALSE)),
                               print=FALSE))
    } else {
        suppressMessages(untrace(knitr::knit,
                where = asNamespace("knitr")))
    }
}



## currently we grab the first (top level) S3 "class" here.
## to get the lowest level replace class(ev$value)[1] with
## tail(class(ev$value), 1)
##' evaltracer
##' 
##' @rdname tracers
##' @export
evaltracer = function(on=TRUE, record = FALSE) {
    if(on) {
        if(record) {
            if(!requireNamespace("trackr"))
                stop("Can't have record=TRUE without the trackr package installed")
        }
        evc = get("evaluate_call", asNamespace("evaluate"))
        if(!inherits(evc, "functionWithTrace")) {
            ev_call_len = length(as.list(body(get("evaluate_call", asNamespace("evaluate")))))
            ## the things I do to make CRAN/R CMD check happy...
            
            if(record) {
                suppressMessages(trace("evaluate_call",
                                       where = asNamespace("evaluate"),
                                       at = ev_call_len,##list(c(28, 4,4)), ## FRAGILE!!!!!!!!!!!
                                       tracer = quote(if( histropts()$inKnitr && !is(ev$value, "try-error")) {
                                                          expr2 = deparse(expr)
                                                          histry_addinfo(expr = expr2,
                                                                     class = utils::head(class(ev$value), 1), hash = fastdigest::fastdigest(ev$value))
                                                          if(ev$visible)
                                                              record(ev$value, symorpos = length(histry()))
                                                      }),
                                       print = FALSE
                                       )
                                 )
            } else {
                suppressMessages(trace("evaluate_call", 
                                       where = asNamespace("evaluate"),
                                       at = ev_call_len,
                                       tracer = quote(if(histropts()$inKnitr && !is(ev$value, "try-error")) {
                                                          expr2 = deparse(expr)
                                                          histry_addinfo(expr = expr2,
                                                                         class = utils::head(class(ev$value), 1), hash = fastdigest::fastdigest(ev$value))
                                                      }),
                                       print = FALSE
                                       )
                                 )
            }
        }
    } else {
        suppressMessages(untrace("evaluate_call", where = asNamespace("evaluate")))
        ##suppressMessages(untrace(ev_call_expr))# obfu_colons("evaluate", "evaluate_call"))) #ev_call))##evaluate:::evaluate_call))
    }
}



#' @name KnitrHistoryTracker
#' @docType methods
#' @exportClass KnitrHistoryTracker
#' @rdname HistoryTracker
#' @aliases KnitrHistoryTracker-class

kh_tracker = setRefClass("KnitrHistoryTracker",
                         contains = "VirtHistoryTracker",
                         methods = list(
                             initialize = function(...) {
                             if(!requireNamespace("knitr", quietly=TRUE))
                                 stop("Can't use KnitrHistoryTracker without knitr, which failed to load")
                             obj = callSuper(...)
                             obj$tracking=TRUE
                             obj
                         }
                         )
                         )


setClassUnion("KnitrHistoryTrackerOrNULL", c("KnitrHistoryTracker", "NULL"))
state = setRefClass("HistryState",
                  fields = list(evalHistory = "HistoryTracker",
                                knitrHistory = "KnitrHistoryTrackerOrNULL",
                                inKnitr = "logical",
                                history = function(val) {
                      if(length(.self$inKnitr) == 0 || is.na(.self$inKnitr))
                          .self$inKnitr = getOption("knitr.in.progress", FALSE)
                      if(missing(val)) {
                          if(.self$inKnitr)
                              .self$knitrHistory
                          else
                              .self$evalHistory
                      } else {
                          stop("Can't set history tracker this way, set evalHistory or knitrHistory directly")
                      }
                  }))



##' @rdname accessors
##' @description Getters and setters for histry-related objects
##' @title Histry object accessors
##' @param obj The object to access components of.
##' @export
setGeneric("exprs", function(obj) standardGeneric("exprs"))
##' @rdname accessors
##' @export
##' @aliases exprs,HistoryData
setMethod("exprs", "HistoryData", function(obj) obj@exprs)
##' @rdname accessors
##' @export
##' @aliases exprs,VirtHistoryTracker
setMethod("exprs", "VirtHistoryTracker", function(obj) exprs(hData(obj)))

##' @rdname accessors
##' @param value The new value for the field.
##' @export
##' @aliases exprs<-
setGeneric("exprs<-", function(obj, value) standardGeneric("exprs<-"))
##' @rdname accessors
##' @export
##' @aliases exprs<-,HistoryData
setMethod("exprs<-", "HistoryData", function(obj, value) {
    obj@exprs = value
    obj
})
##' @rdname accessors
##' @export
##' @aliases exprs<-,VirtHistoryTracker
setMethod("exprs<-", "VirtHistoryTracker", function(obj, value) {
    
    tmp = hData(obj)
    exprs(tmp) <- value
    hData(obj) = tmp
    obj

})

## I didn't like having an accessor named classes, so added the ret_ here
##' @rdname accessors
##' @export
##' @aliases ret_classes
setGeneric("ret_classes", function(obj) standardGeneric("ret_classes"))
##' @rdname accessors
##' @export
##' @aliases ret_classes,HistoryData
setMethod("ret_classes", "HistoryData", function(obj) obj@classes)
##' @rdname accessors
##' @export
##' @aliases ret_classes,VirtHistoryTracker
setMethod("ret_classes", "VirtHistoryTracker", function(obj) ret_classes(hData(obj)))

##' @rdname accessors
##' @export
##' @aliases ret_classes<-

setGeneric("ret_classes<-", function(obj, value) standardGeneric("ret_classes<-"))

##' @rdname accessors
##' @export
##' @aliases ret_classes<-,HistoryData
setMethod("ret_classes<-", "HistoryData", function(obj, value) {
    obj@classes = value
    obj
})
##' @rdname accessors
##' @export
##' @aliases ret_classes<-,VirtHistoryTracker

setMethod("ret_classes<-", "VirtHistoryTracker", function(obj, value) {
    tmp = hData(obj)
    ret_classes(tmp) <- value
    hData(obj) = tmp
    obj
})


##' @rdname accessors
##' @export
##' @aliases hashes
setGeneric("hashes", function(obj) standardGeneric("hashes"))

##' @rdname accessors
##' @export
##' @aliases hashes,HistoryData
setMethod("hashes", "HistoryData", function(obj) obj@hashes)

##' @rdname accessors
##' @export
##' @aliases hashes,VirtHistoryTracker
setMethod("hashes", "VirtHistoryTracker", function(obj) hashes(hData(obj)))

##' @rdname accessors
##' @export
##' @aliases hashes<-
setGeneric("hashes<-", function(obj, value) standardGeneric("hashes<-"))
##' @rdname accessors
##' @export
##' @aliases hashes<-,HistoryData
setMethod("hashes<-", "HistoryData", function(obj, value) {
    obj@hashes = value
    obj
})
##' @rdname accessors
##' @export
##' @aliases hashes<-,VirtHistoryTracker
setMethod("hashes<-", "VirtHistoryTracker", function(obj, value) {
    tmp = hData(obj)
    hashes(tmp) <- value
    hData(obj) = tmp
    obj
})

##' @rdname accessors
##' @export
##' @aliases hData
setGeneric("hData", function(obj) standardGeneric("hData"))
##' @rdname accessors
##' @export
##' @aliases hData,HistoryData
setMethod("hData", "HistoryData", function(obj) obj)
##' @rdname accessors
##' @export
##' @aliases hData,VirtHistoryTracker
setMethod("hData", "VirtHistoryTracker", function(obj) obj$hdata)

##' @rdname accessors
##' @export
##' @aliases hData<-
setGeneric("hData<-", function(obj, value) standardGeneric("hData<-"))
##' @rdname accessors
##' @export
##' @aliases hData<-,HistoryData
setMethod("hData<-", "HistoryData", function(obj, value) value)
##' @rdname accessors
##' @export
##' @aliases hData<-,VirtHistoryTracker
setMethod("hData<-", "VirtHistoryTracker", function(obj, value) {
    obj$hdata = value
    obj
})



