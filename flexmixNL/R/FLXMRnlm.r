setClass("FLXMRnlm",
         representation(start = "list",
                        family = "character",
                        refit = "function"),
         contains = "FLXMR")

utils::globalVariables(c("w"))

FLXMRnlm <- function(formula = .~.,
                     family = c("gaussian", "Gamma"),
                     start = list(), offset = NULL)
{
  formula <- as.formula(formula)
  family <- match.arg(family)
  
  z <- new("FLXMRnlm", weighted = TRUE, formula = formula, start = start,
           name = paste("FLXMRnlm", family, sep=":"), offset = offset,
           family = family, refit = refit)
  
  if(family=="gaussian"){ 
    z@defineComponent <- function(para){
      predict <- function(x, ...){
        data0 <- data.frame(x)
        startEnv <- new.env(hash = FALSE, parent = environment(formula))
        for (i in names(para$start)) assign(i, para$coef[[i]], envir = startEnv)
        p <- eval(formula[[3L]], data0, startEnv)
        p
      }
      logLik <- function(x, y, ...) dnorm(y, mean = predict(x, ...), 
                                          sd = para$sigma, log = TRUE)
      
      new("FLXcomponent",
          parameters=list(coef = para$coef, sigma = para$sigma),
          logLik = logLik, predict = predict,
          df = para$df)
    }
    z@fit <- function(formula, start, x, y, w)
    {
      fit <- nls.wfit(formula = formula, start = start, 
                      data = data.frame(x,y,w))
      z@defineComponent(para = list(coef = coef(fit),
                                    start = as.list(fit$start),
                                    df = length(fit$start)+1,
                                    sigma = sqrt(sum(fit$weights * 
                                                       fit$residuals^2 /
                                                       mean(fit$weights))/ 
                                                   (fit$df.residuals))))
    }
  }else if(family=="Gamma"){
    z@defineComponent <- function(para){
      predict <- function(x, ...){
        dotarg <- list(...)
        if("offset" %in% names(dotarg)) offset <- dotarg$offset
        p <- sapply(seq_len(nrow(x)),
                    function(i) {
                      eval(parse(text = as.formula(formula[[3L]][[3L]])$term(
                        unlist(para$coef),x[i,])))
                    })
        p <- as.matrix(p)
      }
      
      logLik <- function(x, y, ...) {
        dgamma(y, shape = para$shape, scale = predict(x, ...)/para$shape,
               log = TRUE)}
      
      new("FLXcomponent", parameters = list(coef = para$coef, 
                                            shape = para$shape),
          predict = predict, logLik = logLik, df = para$df)
    }
    
    z@fit <- function(formula, start, x, y, w)
    { 
      fit <- gnm.wfit(formula = formula, start = start, data= data.frame(x,y,w),
                      family = Gamma(link="identity"))
      z@defineComponent(para = list(fit = fit, coef = fit$coefficients,
                                    df = length(start)+1,
                                    shape = sum(fit$prior.weights)/
                                      fit$deviance))
    }
  }
  else stop(paste("Unknown family", family))
  z
}

setMethod("FLXgetModelmatrix", signature(model = "FLXMRnlm"),
          function(model, data, formula, start = list(),...)
          {
            if(is.null(model@formula))  model@formula <- formula
            model@fullformula <- update.formula(formula, model@formula)
            mt <- terms(formula, data = data)
            varNamesRHS <- all.vars(formula[[3L]])
            prednames <- varNamesRHS[varNamesRHS %in% names(data)]
            model@x <- as.matrix(data[prednames])
            response <- all.vars(update(formula, . ~ 1))
            model@y <- as.matrix(data[response])
            model
          })

setMethod("FLXmstep", signature(model = "FLXMRnlm"),
          function(model, weights, components,...) {
            sapply(seq_len(ncol(weights)),
                   function(k) {
                     if(length(names(components[[k]]@parameters))==0)
                       model@fit(model@formula, model@start[[k]], model@x, 
                                 model@y, weights[,k])
                     else
                       model@fit(model@formula, 
                                 as.list(components[[k]]@parameters$coef), 
                                 model@x, model@y, weights[,k])
                   })
          })

setMethod("FLXdeterminePostunscaled", signature(model = "FLXMRnlm"),
          function(model, components, ...) {
            sapply(components, function(x) x@logLik(model@x, model@y))
          })

nls.wfit <- function(formula, start, data = list())
{
  w <- data$w
  fit <- nls(formula = formula, start = start, data = data, 
             weights = as.vector(w))
  startEnv <- new.env(hash = FALSE, parent = environment(formula))
  for (i in names(start)) assign(i, coef(fit)[[i]], envir = startEnv)
  fit$fitted.values <- eval(formula[[3L]], data, startEnv)
  response <- all.vars(update(formula, . ~ 1))
  fit$residuals <- as.vector(residuals(fit))
  fit$df.residuals <- df.residual(fit)
  fit$weights <- weights(fit)
  fit$formula <- formula
  fit$start <- coef(fit)
  fit
}

gnm.wfit <- function(formula, start, data = list(), family = list())
{
  w <- data$w
  fit <- gnm(formula = formula, family = Gamma(link = "identity"),
             data = data, start = unlist(start), weights = as.vector(w), 
             verbose = FALSE, trace = FALSE, checkLinear = TRUE)
  fit$df.residuals <- df.residual(fit)
  fit$coefficients <- coef(fit)
  fit$start <- as.list(unlist(coef(fit)))
  fit$rank <- fit$rank[1]
  fit
}