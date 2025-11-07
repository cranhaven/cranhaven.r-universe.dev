#######################################################################
#                                                                     #
# Package: lcc                                                        #
#                                                                     #
# File: lccModel.R                                                    #
# Contains: lccModel function                                         #
#                                                                     #
# Written by Thiago de Paula Oliveira                                 #
# copyright (c) 2017-18, Thiago P. Oliveira                           #
#                                                                     #
# First version: 11/10/2017                                           #
# Last update: 29/07/2019                                             #
# License: GNU General Public License version 2 (June, 1991) or later #
#                                                                     #
#######################################################################
##' @title Internal Function to Fits a Linear Mixed-Effects Model in the
##'   Formulation Described in Laird and Ware (1982).
##'
##' @description This is an internally called function used to fits a
##'   linear mixed-effects model; see \code{\link[nlme]{lme}}.
##'
##' @usage NULL
##' @return No return value, called for side effects
##' @author Thiago de Paula Oliveira,
##'   \email{thiago.paula.oliveira@@alumni.usp.br}
##'
##' @importFrom nlme lmeControl pdSymm
##'
##' @importFrom stats model.matrix
##'
##' @importFrom utils capture.output
##'
##' @references Laird, N.M. and Ware, J.H. (1982) Random-Effects Models
##'   for Longitudinal Data, \emph{Biometrics}, 38, 963–974.
##'
##'Pinheiro, J.C. and Bates., D.M. (1996) Unconstrained Parametrizations
##' for Variance-Covariance Matrices, \emph{Statistics and Computing},
##' 6, 289–296.
##'
##'Pinheiro, J.C., and Bates, D.M. (2000) Mixed-Effects Models in S and
##' S-PLUS, \emph{Springer}.
##'
##' @keywords internal
lccModel <- function(dataset, resp, subject, method, time, qf, qr,
                     interaction, covar,  gs = NULL, var.class = NULL,
                     weights.form, lme.control = NULL, method.init,
                     pdmat) {
  if(is.null(lme.control)) lme.control <- lmeControl()

  Data <- dataBuilder(dataset = dataset, resp = resp, subject = subject,
                      method = method, time = time, gs = gs)
  Poly<-with(Data,poly(time, degree = qf, raw = TRUE))
  if(interaction == TRUE){
    fixed <- model.matrix( ~ method * Poly, Data)
    }else{
    fixed <- model.matrix( ~ method + Poly, Data)
    }
  Data$fixed <- fixed
  if (length(covar) > 0) {
    pos <- pmatch(covar, names(Data))
    if (any(nap <- is.na(pos))) {
      stop(
        sprintf(ngettext(length(nap),
                         "unrecognized 'covar' variable named %s ignored",
                         "unrecognized 'covar' variable named %s ignored"),
                paste(sQuote(covar[nap]), collapse = ", ")), call. = FALSE)
      pos <- pos[!nap]
      covar <- covar[!nap]
    }

    COVAR<-list()
    for(i in seq_len(length(pos))){
    COVAR[[i]]<-model.matrix(~Data[,pos[i]])[,-1]
    colnames(COVAR[[i]])<- paste(covar[[1]][i], levels(Data[,pos[i]])[-1])
    }
    Data_covar<-do.call(cbind.data.frame, COVAR)
    Data_covar<-as.matrix(Data_covar)
    if(interaction == TRUE){
      fixed <- model.matrix( ~ method * Poly, Data)
    }else{
      fixed <- model.matrix( ~ method + Poly, Data)
    }
    fixed <- cbind(fixed, Data_covar)
    Data$fixed <- fixed
    }

  if(is.null(var.class)) {
    if(qr == 0) {
      model.lme <-
        try(lme(resp ~ fixed - 1, Data,
                random = list(subject = pdSymm(form = ~ 1)),
                control = lme.control,
                method = method.init), silent = TRUE)
    } else {
      fmla.rand <- model.matrix( ~ poly(time, degree = qr, raw = TRUE), Data)
      Data$fmla.rand <- fmla.rand
      if(is.function(pdmat)){
        model.lme <-
          try(lme(resp ~ fixed - 1, Data,
                  random = list(subject = pdmat(form = ~ fmla.rand - 1)),
                  control = lme.control, method = method.init),
              silent = TRUE)
      }else{
        stop("Available only for pdSymm, pdLogChol, pdDiag, pdIdent, pdCompSymm, and pdNatural.",
             call.=FALSE)
      }
    }
  } else {
     .form <- switch(weights.form,
                    "time"        = ~ time,
                    "method"      = ~ 1 | method,
                    "time.ident"  = ~ 1 | time,
                    "both"        = ~ time | method)
    if(qr == 0) {
      model.lme <- try(lme(resp ~ fixed - 1, Data, random = list(subject = pdSymm(form = ~ 1)),
                           weights = var.class(form = .form),
                           control = lme.control,
                           method = method.init), silent = TRUE)
    } else {
      fmla.rand <- model.matrix( ~ poly(time, degree = qr, raw = TRUE), Data)
      Data$fmla.rand <- fmla.rand
      if(is.function(pdmat)){
        model.lme <-
          try(lme(resp ~ fixed - 1, Data,
                  random = list(subject = pdmat(form = ~ fmla.rand - 1)),
                  weights = var.class(form = .form),
                  control = lme.control,
                  method = method.init), silent = TRUE)
      }else{
        stop("Available only for pdSymm, pdLogChol, pdDiag, pdIdent, pdCompSymm, and pdNatural.",
             call.=FALSE)
      }
    }
  }
  warning.count <- 0
  if (inherits(model.lme, "try-error")) {
    warning.count <- 1
    mes <- paste(capture.output(cat(model.lme[1])), collapse = " ")
  } else if(is.character(model.lme$apVar) == TRUE) {
    warning.count <- 1
    mes <- model.lme$apVar
  } else {mes = NULL}
  lcc.fit <- list("model" = model.lme, "summary" = summary(model.lme),
                  "q_f" = qf, "data" = Data, "wcount" = warning.count,
                  "lme.control" = lme.control,  "message" = mes)
  class(lcc.fit) <- "lcc.fit"
  return(lcc.fit)
}
