#######################################################################
#                                                                     #
# Package: lcc                                                        #
#                                                                     #
# File: ciBuilder.R                                                   #
# Contains: ciBuilder function                                        #
#                                                                     #
# Written by Thiago de Paula Oliveira                                 #
# copyright (c) 2017-18, Thiago P. Oliveira                           #
#                                                                     #
# First version: 11/10/2017                                           #
# Last update: 29/07/2019                                             #
# License: GNU General Public License version 2 (June, 1991) or later #
#                                                                     #
#######################################################################

##' @title Internal Function to Prepare the \code{ciCompute} Function.
##'
##' @description This is an internally called function used to prepare
##'   the \code{\link[lcc]{ciCompute}} function.
##'
##' @usage NULL
##' @return No return value, called for side effects
##' @author Thiago de Paula Oliveira,
##'   \email{thiago.paula.oliveira@@alumni.usp.br}
##'
##' @keywords internal
ciBuilder<-function(model, nboot, q_f, q_r, interaction, covar, pdmat,
                    var.class, weights.form, show.warnings, tk,
                    diffbeta, ldb, tk.plot, tk.plot2, ci, percentileMet,
                    alpha, components,lme.control, method.init,
                    numCore){
  #---------------------------------------------------------------------
  # Computing the bootstrap samples for calculate the confidence
  # intervals
  #---------------------------------------------------------------------
  Models <- bootstrapSamples(nboot = nboot, model = model, q_f = q_f,
                             q_r = q_r, interaction = interaction,
                             covar = covar, pdmat = pdmat,
                             var.class = var.class,
                             weights.form = weights.form,
                             show.warnings = show.warnings,
                             lme.control = lme.control,
                             method.init = method.init,
                             numCore = numCore)
  #---------------------------------------------------------------------
  # Computing the lcc or lcc, lpc and la bootstrap confidence intervals
  #---------------------------------------------------------------------
  if(components==FALSE){
    LCC_Boot <-
      lccBootstrap(model_boot = Models$Boot_Model,
                   diff_boot = Models$Diffbetas,
                   ldb=ldb, nboot = nboot, tk = tk,
                   q_f = q_f)
    if(ldb == 1) {
      rho <- lccWrapper(model = model, q_f = q_f, n.delta = 1,
                         tk = tk, diffbeta = as.numeric(diffbeta[[1]]))
      CI<-lcc_intervals(rho = rho, tk.plot = tk.plot, tk.plot2 =
                                                        tk.plot2,
                        ldb = ldb, model = model, ci = ci,
                        percentileMet = percentileMet,
                        LCC_Boot = LCC_Boot, alpha = alpha)
      CI.LCC<-CI$CI.LCC
    } else{
      #-----------------------------------------------------------------
      # If components = TRUE and ldb = 1 (two methods)
      #-----------------------------------------------------------------
      rho <- list()
      for(i in 1:ldb)  rho[[i]] <- lccWrapper(model = model, q_f = q_f,
                                              n.delta = 1, tk = tk,
                                              diffbeta =

  as.numeric(diffbeta[[i]]))
      rho.ret <- data.frame(do.call(cbind.data.frame, rho))
      CI<-lcc_intervals(rho = rho.ret, tk.plot = tk.plot,
                        tk.plot2 = tk.plot2, ldb = ldb,
                        model = model, ci = ci,
                        percentileMet = percentileMet,
                        LCC_Boot = LCC_Boot, alpha = alpha)
    }
  }else{
    #-------------------------------------------------------------------
    # If components = TRUE
    #-------------------------------------------------------------------
    LCC_Boot<-
      lccBootstrap(model_boot = Models$Boot_Model,
                   diff_boot = Models$Diffbetas, ldb=ldb,
                   nboot = nboot, tk=tk, q_f=q_f)
    LPC_Boot<-
      lpcBootstrap(model_boot = Models$Boot_Model, ldb=ldb,
                   nboot = nboot, tk=tk, q_f=q_f)
    Cb_Boot<-
      laBootstrap(model_boot = Models$Boot_Model,
                  diff_boot = Models$Diffbetas, ldb=ldb,
                  nboot = nboot, tk=tk, q_f=q_f)
    if(ldb == 1) {
      rho <-
        lccWrapper(model = model, q_f = q_f, n.delta = 1,
                   tk = tk,
                   diffbeta = as.numeric(diffbeta[[1]]))
      rho.pearson<-
        lpcWrapper(model = model, q_f = q_f, tk = tk, n.delta = 1)
      Cb<-
        laWrapper(model = model, q_f = q_f, n.delta = 1,
                      tk = tk, diffbeta = as.numeric(diffbeta[[1]]))
      CI<-
        ciCompute(rho = rho, rho.pearson = rho.pearson, Cb = Cb,
              tk.plot = tk.plot, tk.plot2 = tk.plot2, alpha = alpha,
              ldb = ldb, model = model, ci = ci, percentileMet,
              LCC_Boot = LCC_Boot,
              LPC_Boot = LPC_Boot, Cb_Boot = Cb_Boot)
    } else{
      #-----------------------------------------------------------------
      # If ldb > 1 (more than two methods)
      #-----------------------------------------------------------------
      rho <- list()
      rho.pearson <- list()
      Cb<-list()
      for(i in 1:ldb){
        rho[[i]] <- lccWrapper(model = model, q_f = q_f, n.delta = 1,
                               tk = tk,
                               diffbeta = as.numeric(diffbeta[[i]]))
        rho.pearson[[i]] <- lpcWrapper(model = model, q_f = q_f,
                                       n.delta = 1, tk = tk)
        Cb[[i]]<-laWrapper(model = model, q_f = q_f, n.delta = 1,
                           tk = tk,
                           diffbeta = as.numeric(diffbeta[[i]]))
      }
      rho.ret <- data.frame(do.call(cbind.data.frame, rho))
      rho.pearson.ret <- data.frame(do.call(cbind.data.frame,
                                            rho.pearson))
      Cb.ret <- data.frame(do.call(cbind.data.frame, Cb))
      CI<-ciCompute(rho = rho.ret, rho.pearson = rho.pearson.ret,
                    Cb = Cb.ret, tk.plot = tk.plot,
                    tk.plot2 = tk.plot2, alpha = alpha,
                    ldb = ldb, model = model, ci = ci,
                    percentileMet, LCC_Boot = LCC_Boot,
                    LPC_Boot = LPC_Boot, Cb_Boot = Cb_Boot)
    }
  }
  return(invisible(CI))
}
