#######################################################################
#                                                                     #
# Package: lcc                                                        #
#                                                                     #
# File: init.R                                                        #
# Contains: init function                                             #
#                                                                     #
# Written by Thiago de Paula Oliveira                                 #
# copyright (c) 2017-18, Thiago P. Oliveira                           #
#                                                                     #
# First version: 11/10/2017                                           #
# Last update: 29/07/2019                                             #
# License: GNU General Public License version 2 (June, 1991) or later #
#                                                                     #
#######################################################################

##' @title Internal Function to Prepare \code{\link[lcc]{lccModel}}
##'   Function
##'
##' @description This is an internally called function used to verify
##'   the specification of variance-covariance matrices and likelihood
##'   based method.
##'
##' @usage NULL
##' @return No return value, called for side effects
##' @importFrom nlme lme
##'
##' @author Thiago de Paula Oliveira, \email{thiago.paula.oliveira@@alumni.usp.br}
##'
##' @keywords internal
init <- function(var.class, weights.form, REML, qf, qr, pdmat, dataset,
               resp, subject, method, time, gs, numCore){
  resp <- resp
  subject <- subject
  method <- method
  time <- time
  Data <- data.frame(dataset)
  Data <- try(rename.vars(Data, from = c(resp, subject, method, time),
    to = c("y", "ind", "FacA", "time"),
    info = FALSE), TRUE)
  if (inherits(Data,"try-error")) {
    stop("Please, verify the name of 'resp', 'subject', 'method', and 'time' variables",
         call.=FALSE)
  }
  if(is.factor(Data$ind)==FALSE) stop("Please, 'subject' variable should be factor",
                                      call.=FALSE)
  if(is.factor(Data$FacA)==FALSE) stop("Please, 'method' variable should be factor",
                                       call.=FALSE)
  if(is.numeric(Data$time)==FALSE) stop("Please, 'time' variable should be numeric",
                                        call.=FALSE)
  if(is.numeric(Data$y)==FALSE) stop("Please, 'resp' variable should be numeric",
                                     call.=FALSE)
    if(!is.function(pdmat)) {
      if(is.character(pdmat)) {
        if(substr(pdmat, nchar(pdmat) - 1, nchar(pdmat)) == "()") {
          pdmat <- substr(pdmat, 1, nchar(pdmat) - 2)
        }
        pdmat <- get(pdmat)
      } else {
        stop("Do not include brackets after the pdmat function, e.g. pdSymm()")
      }
    }
  #---------------------------------------------------------------------
  # Test for var.class
  #---------------------------------------------------------------------
  if(is.null(var.class)==FALSE){
    if(!is.function(var.class)) {
    if(is.character(var.class)) {
      if(substr(var.class, nchar(var.class) - 1, nchar(var.class)) == "()") {
        var.class <- substr(var.class, 1, nchar(var.class) - 2)
      }
      var.class <- get(var.class)
    } else {
      stop("Do not include brackets after the var.class function, e.g. varExp()")
      }
    }
  }
  #---------------------------------------------------------------------
  # Test for var.class and weigth form
  #---------------------------------------------------------------------
    if(is.null(var.class)==FALSE){
      vc <- class(var.class())[1]
      if(is.null(weights.form)) stop("Please specify the 'weights.form' argument.",
                                     call.=FALSE)
      if(weights.form=="time.ident" || weights.form == "method") {
        vc<-class(var.class())[1]
        if(vc != "varIdent") stop("Please specify the 'weight.form' correctly for varExp class",
                                  call.=FALSE)
    }
    if(weights.form=="time" || weights.form == "both") {
      vc<-class(var.class())[1]
      if(vc != "varExp") stop("Please specify the 'weight.form' correctly for varIdent class",
                              call.=FALSE)
    }

    if(weights.form != "time" && weights.form != "method" &&
         weights.form != "time.ident" && weights.form != "both") stop("The weights.form argument are \"time\", \"method\", \"time.ident\", or \"both\".",
                                                                      call. = FALSE)
     if(vc != "varIdent" && vc != "varExp") {
       stop("Method only implemented for classes varIdent and varExp",
            call.=FALSE)
      }
    }
  #---------------------------------------------------------------------
  # Test for REML
  #---------------------------------------------------------------------
  if(REML==TRUE){
    MethodREML<-"REML"
  }else{
    if(REML==FALSE){
      MethodREML<-"ML"
    }else{
      stop("logical argument: TRUE for REML or FALSE for ML estimates.", call. = FALSE)
    }
  }
  #---------------------------------------------------------------------
  # Test for qr and qf
  #---------------------------------------------------------------------
  if(qf<=0) stop("polynomial degree should be greater or equal to 1 in the fixed part.",
                 call.=FALSE)
  if(qr<0)  stop("polynomial degree should be greater or equal to 0 in the random part.",
                 call.=FALSE)
  if(qf<qr) stop("'qr' should be less or equal 'qf'", call. = FALSE)
  if(is.numeric(qf)==FALSE) stop("'qf' should be integer.", call.=FALSE)
  if(is.numeric(qr)==FALSE) stop("'qr' should be integer.", call.=FALSE)
  initList<-list("MethodREML" = MethodREML, "pdmat" = pdmat,
                 "var.class" = var.class)
  #---------------------------------------------------------------------
  # Test for gs
  #---------------------------------------------------------------------
  if (is.null(gs) == FALSE) {
    if (!inherits(gs, "character"))  {
      stop("Please specify the 'gs' as character string.
          Example: gs = 'Scanner'", call.= FALSE)
    }
    if (sum(levels(dataset[, method]) == gs) == 0) {
      stop(paste0("There is no method level called: '", gs, "'"),
           call.= FALSE)
    }
  }
  #---------------------------------------------------------------------
  # Number of cores
  #---------------------------------------------------------------------
  if(numCore < 1) stop("Please, 'numCore' argument must be 1 or higher",
                                      call.=FALSE)
  return((list2env(initList, .GlobalEnv)))
}

#-----------------------------------------------------------------------
# Rename vars
#-----------------------------------------------------------------------
##' @title Internal Function to Prepare \code{\link[lcc]{lccModel}}
##'   Function
##'
##' @description This is an internally called function used to verify
##'   the specification of variance-covariance matrices and likelihood
##'   based method.
##'
##' @usage NULL
##'
##' @author Code by Don MacQueen 
##'
##' @keywords internal

rename.vars <- function(data,from='',to='',info=TRUE) {

   dsn <- deparse(substitute(data))
   dfn <- names(data)

   if ( length(from) != length(to)) {
     message('--------- from and to not same length ---------\n')
     stop()
   }

   if (length(dfn) < length(to)) {
     message('--------- too many new names ---------\n')
     stop()
   }

   chng <- match(from,dfn)

   frm.in <- from %in% dfn
   if (!all(frm.in) ) {
     message('---------- some of the from names not found in',dsn,'\n')
     stop()
   }

   if (length(to) != length(unique(to))) {
     message('---------- New names not unique\n')
     stop()
   }

   dfn.new <- dfn
   dfn.new[chng] <- to
   if (info) {
     message('\nChanging in',dsn)
     }
   tmp <- rbind(from,to)
   dimnames(tmp)[[1]] <- c('From:','To:')
   dimnames(tmp)[[2]] <- rep('',length(from))
   if (info)
     {
       print(tmp,quote=FALSE)
       message("\n")
     }
   names(data) <- dfn.new
   data
}
