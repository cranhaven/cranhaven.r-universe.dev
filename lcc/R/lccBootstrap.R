########################################################################
# Package: lcc                                                         #
#                                                                      #
# File: lccBootstrap.R                                                 #
#                                                                      #
# Contains: dataBootstrap, bootstrapSamples, lccBootstrap,             #
# lpcBootstrap, laBootstrap                                            #
#                                                                      #
# Written by Thiago de Paula Oliveira                                  #
# copyright (c) 2017-18, Thiago P. Oliveira                            #
#                                                                      #
# First version: 11/10/2017                                            #
# Last update: 29/07/2019                                              #
# License: GNU General Public License version 2 (June, 1991) or later  #
#                                                                      #
########################################################################

##' @title Internal Functions to Generate Bootstrap Samples Based on
##'   Dataset.
##'
##' @description This is an internally called functions used to generate
##'   bootstrap samples.
##'
##' @usage NULL
##' @return No return value, called for side effects
##' @author Thiago de Paula Oliveira,
##'   \email{thiago.paula.oliveira@@alumni.usp.br}
##'
##' @importFrom utils txtProgressBar setTxtProgressBar capture.output
##'
##' @importFrom foreach foreach %dopar%
##'
##' @importFrom doRNG %dorng%
##'
##' @importFrom doSNOW registerDoSNOW
##'
##' @importFrom parallel makeCluster stopCluster
##'
##' @keywords internal

dataBootstrap<-function(model){
N <- length(levels(model$data$subject))
Dataset <- model$data
subject <- NULL
sample_data <- sample(as.character(unique(model$data$subject)),
                    N,replace=TRUE)
Frame <- list(NA)
for (i in 1:N) {
  Frame[[i]] <- subset(Dataset, subject==sample_data[i])
  Frame[[i]]$subject <- c(rep(i,length(Frame[[i]][,1])))
}
Boot_Dataset <- do.call(rbind.data.frame, Frame)
return(Boot_Dataset)
}

##' @title Internal functions to estimate fixed effects and variance
##'   components.
##'
##' @description This is an internally called functions used to estimate
##'   fixed effects and variance components for each bootstrap sample.
##'
##' @usage NULL
##' @return No return value, called for side effects
##' @author Thiago de Paula Oliveira,
##'   \email{thiago.paula.oliveira@@alumni.usp.br}
##'
##' @importFrom nlme fixef
##'
##' @keywords internal
bootstrapSamples<-function(nboot, model, q_f, q_r, interaction, covar,
                           var.class, pdmat, weights.form,
                           show.warnings, lme.control, method.init,
                           numCore){
  Dataset_boot<-list(NA)
  Boot_model<-list(NA)
  Diff<-list(NA)
  warnings <- 0
  #---------------------------------------------------------------------
  # Without parallelization
  #---------------------------------------------------------------------
  if (numCore == 1){
    pb <- txtProgressBar(
      title = "Processing the bootstrap confidence intervals",
      style = 3, min = 0, max = nboot)
    for(i in 1:nboot){
      lccModel.fit <- lccModel(dataset=dataBootstrap(model=model), resp="resp",
                               subject="subject", covar = covar,
                               method="method", time="time",
                               qf=q_f, qr=q_r,
                               interaction = interaction, pdmat = pdmat,
                               var.class = var.class,
                               weights.form = weights.form,
                               lme.control = lme.control,
                               method.init = method.init)
      x<-NULL
      y<-NULL
      if(lccModel.fit$wcount == 1) {
        Boot_model[[i]] <- model
        tk <- sort(unique(Boot_model[[i]]$data$time))
        lev.lab <- levels(Boot_model[[i]]$data$method)
        lev.facA <- length(lev.lab)
        lev.lab<-unique(merge(rep("method",q_f),lev.lab))
        lev.lab<-transform(lev.lab,newcol=paste(x,y, sep = ""))
        fx <- fixef(Boot_model[[i]])
        if(show.warnings) cat("\n", "  Estimation problem on bootstrap sample", i, "\n")
      } else {
        Boot_model[[i]] <- lccModel.fit$model
        tk <- sort(unique(Boot_model[[i]]$data$time))
        lev.lab <- levels(Boot_model[[i]]$data$method)
        lev.facA <- length(lev.lab)
        lev.lab<-unique(merge(rep("method",q_f),lev.lab))
        lev.lab<-transform(lev.lab,newcol=paste(x,y, sep = ""))
        fx <- fixef(Boot_model[[i]])
      }
      warnings <- warnings + lccModel.fit$wcount
      pat <- list()
      for(j in 2:lev.facA) pat[[j-1]] <- grep(lev.lab$newcol[j], names(fx))
      beta1 <- fx[-unlist(pat)]
      betas <- list()
      for(j in 2:lev.facA) betas[[j-1]] <- - fx[pat[[j-1]]]
      Diff[[i]]<-betas
      #-----------------------------------------------------------------
      # print
      #-----------------------------------------------------------------
      #cat("Sample number: ", i, "\n")
      setTxtProgressBar(pb, i, label=paste( round(i/nboot*100, 0),
                                           "% done"))
    }
  }else {
    #===================================================================
    # With parallelizarion
    #===================================================================
    # Sampling data
    cl <- makeCluster(numCore, type = "SOCK")
    registerDoSNOW(cl)
    pb <- txtProgressBar(max=nboot, style=3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress=progress)
    #-------------------------------------------------------------------
    lccModel.fit <- foreach(i = 1:nboot, .options.snow = opts) %dorng% {
      lccModel(dataset = dataBootstrap(model=model), resp = "resp",
               subject = "subject", covar = covar,
               method = "method", time="time",
               qf = q_f, qr = q_r,
               interaction = interaction, pdmat = pdmat,
               var.class = var.class,
               weights.form = weights.form,
               lme.control = lme.control,
               method.init = method.init)
    }
    stopCluster(cl)
    #-------------------------------------------------------------------
    for(i in 1:nboot){
      x<-NULL
      y<-NULL
      if(lccModel.fit[[i]]$wcount == 1) {
        Boot_model[[i]] <- model
        tk <- sort(unique(Boot_model[[i]]$data$time))
        lev.lab <- levels(Boot_model[[i]]$data$method)
        lev.facA <- length(lev.lab)
        lev.lab<-unique(merge(rep("method",q_f),lev.lab))
        lev.lab<-transform(lev.lab,newcol=paste(x,y, sep = ""))
        fx <- fixef(Boot_model[[i]])
        if(show.warnings) cat("\n", "  Estimation problem on bootstrap sample", i, "\n")
      } else {
        Boot_model[[i]] <- lccModel.fit[[i]]$model
        tk <- sort(unique(Boot_model[[i]]$data$time))
        lev.lab <- levels(Boot_model[[i]]$data$method)
        lev.facA <- length(lev.lab)
        lev.lab<-unique(merge(rep("method",q_f),lev.lab))
        lev.lab<-transform(lev.lab,newcol=paste(x,y, sep = ""))
        fx <- fixef(Boot_model[[i]])
      }
      warnings <- warnings + lccModel.fit[[i]]$wcount
      pat <- list()
      for(j in 2:lev.facA) pat[[j-1]] <- grep(lev.lab$newcol[j], names(fx))
      beta1 <- fx[-unlist(pat)]
      betas <- list()
      for(j in 2:lev.facA) betas[[j-1]] <- - fx[pat[[j-1]]]
      Diff[[i]]<-betas
    }
  }
  message("\n", "  Convergence error in", warnings, "out of",
                             nboot, "bootstrap samples.", "\n")
  lcc.bootstrap <- list("Boot_Model" = Boot_model, "Diffbetas"=Diff)
  class(lcc.bootstrap) <- "lcc.bootstrap"
  return(lcc.bootstrap)
}

##' @title Internal functions to generate longitudinal concordance
##'   correlation samples.
##'
##' @description This is an internally called functions used to generate
##'   longitudinal concordance correlation samples.
##'
##' @usage NULL
##' @return No return value, called for side effects
##' @author Thiago de Paula Oliveira,
##'   \email{thiago.paula.oliveira@@alumni.usp.br}
##'
##' @keywords internal
lccBootstrap<-function(model_boot, diff_boot, ldb, nboot, tk, q_f){
  CCC_Boot<-list(NA)
  if(ldb == 1) {
    for(i in 1:nboot){
      CCC_Boot[[i]] <-
        lccWrapper(model = model_boot[[i]], q_f = q_f,
                   n.delta = 1, tk = tk,
                   diffbeta = as.numeric(diff_boot[[i]][[1]]))
    }
    return(CCC_Boot)
  } else {
    nd<-length(summary(model_boot[[1]])$modelStruct$varStruct)
    if(nd<=1){
      CCC_l <- list()
      for(i in 1:nboot){
        for(j in 1:ldb)  {
          CCC_l[[j]] <-
            lccWrapper(model = model_boot[[i]], q_f = q_f, n.delta = 1,
                       tk = tk,
                       diffbeta = as.numeric(diff_boot[[i]][[j]]))
        }
        CCC_Boot[[i]]<-CCC_l
      }
    } else{
      CCC_l <- list()
      for(i in 1:nboot){
        for(j in 1:ldb)  {
          CCC_l[[j]] <-
            lccWrapper(model = model_boot[[i]], q_f = q_f, n.delta = j,
                       tk = tk,
                       diffbeta = as.numeric(diff_boot[[i]][[j]]))
        }
        CCC_Boot[[i]]<-CCC_l
      }
    }
    return(CCC_Boot)
  }
}

##' @title Internal functions to generate longitudinal Pearson
##'   correlation samples.
##'
##' @description This is an internally called functions used to generate
##'   longitudinal Pearson correlation samples.
##'
##' @usage NULL
##' @return No return value, called for side effects
##' @author Thiago de Paula Oliveira,
##'   \email{thiago.paula.oliveira@@alumni.usp.br}
##'
##' @keywords internal
lpcBootstrap<-function(model_boot, ldb, nboot, tk, q_f){
  LPC_Boot<-list(NA)
  if(ldb == 1) {
    for(i in 1:nboot){
      LPC_Boot[[i]] <-
        lpcWrapper(model = model_boot[[i]], q_f = q_f, n.delta = 1,
                   tk = tk)
    }
    return(LPC_Boot)
  } else {
    nd<-length(summary(model_boot[[1]])$modelStruct$varStruct)
    if(nd<=1){
      LPC_l <- list()
      for(i in 1:nboot){
        for(j in 1:ldb)  {
          LPC_l[[j]] <-
            lpcWrapper(model = model_boot[[i]], q_f = q_f, n.delta = 1,
                       tk = tk)
        }
        LPC_Boot[[i]]<-LPC_l
      }
    } else{
      LPC_l <- list()
      for(i in 1:nboot){
        for(j in 1:ldb)  {
          LPC_l[[j]] <-
            lpcWrapper(model = model_boot[[i]], q_f = q_f, n.delta = j,
                       tk = tk)
        }
        LPC_Boot[[i]]<-LPC_l
      }
    }
    return(LPC_Boot)
  }
}

##' @title Internal functions to generate longitudinal accuracy samples.
##'
##' @description This is an internally called functions used to generate
##'   longitudinal caccuracy samples.
##'
##' @usage NULL
##' @return No return value, called for side effects
##' @author Thiago de Paula Oliveira,
##'   \email{thiago.paula.oliveira@@alumni.usp.br}
##'
##' @keywords internal
laBootstrap<-function(model_boot, diff_boot, ldb, nboot, tk, q_f){
  Cb_Boot<-list(NA)
  if(ldb == 1) {
    for(i in 1:nboot){
      Cb_Boot[[i]] <-
        laWrapper(model = model_boot[[i]], q_f = q_f, n.delta = 1,
                  tk = tk, diffbeta = as.numeric(diff_boot[[i]][[1]]))
    }
    return(Cb_Boot)
  } else {
    nd<-length(summary(model_boot[[1]])$modelStruct$varStruct)
    if(nd<=1){
      Cb_l <- list()
      for(i in 1:nboot){
        for(j in 1:ldb)  {
          Cb_l[[j]] <-
            laWrapper(model = model_boot[[i]], q_f = q_f, n.delta = 1,
                      tk = tk,
                      diffbeta = as.numeric(diff_boot[[i]][[j]]))
        }
        Cb_Boot[[i]]<-Cb_l
      }
    } else{
      Cb_l <- list()
      for(i in 1:nboot){
        for(j in 1:ldb)  {
          Cb_l[[j]] <-
            laWrapper(model = model_boot[[i]], q_f = q_f, n.delta = j,
                      tk = tk,
                      diffbeta = as.numeric(diff_boot[[i]][[j]]))
        }
        Cb_Boot[[i]]<-Cb_l
      }
    }
    return(Cb_Boot)
  }
}
