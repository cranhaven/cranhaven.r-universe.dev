#***********************************************************************************
#
# Fit a Cox Additive Model with a Two-Dimensional Smooth
# Copyright (C) 2016, The University of California, Irvine
#
# This library is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this library??? if not, <http://www.gnu.org/licenses/>.
#
#*******************************************************************************
gamcox.fit <- function(Y, X, smooth.frame, weights=NULL, span=0.5, I.span = 0.2, degree=1,
                       loess.trace="exact", Maxiter=40, tol=1e-7){
  N <- length(Y$time)
  if(is.null(weights)) weights <- rep(1,N)
  fit <- coxph(Surv(Y$time,Y$event) ~ X, weights=weights)
  eta <- fit$linear.predictors
  which = grep("lo\\([[:print:]]+\\)", colnames(X))
  ls <- dls(Y, X, which, eta, I.span, TRUE)
  w <- ls$w*(ls$w>=0)
  deltaeta <- pmin(pmax(ls$deltaeta,-3),3)
  dev.old <- -ls$l*2
  ## Update eta iteratively
  for(i in 1:Maxiter){
    Z <- eta + deltaeta
    residuals <- Z
    fit <- list(fitted.values=rep(0,N), coefficients=0)
    s <- rep(0, N)
    ### Backfitting starts
    for (j in 1:Maxiter){
      R <- residuals + fit$fitted.values
      fit <- lm(R ~ X, weights=w)
      residuals <- fit$residuals
      old <- s
      R <- residuals + s
      smooth.fit <- loess(R ~ smooth.frame, weights=w, span=span, degree=degree,
                          normalize=FALSE, control=loess.control(trace.hat=loess.trace))
      s <- smooth.fit$fitted - mean(smooth.fit$fitted)
      residuals <- R - s
      deltaf <- weighted.mean((s - old)^2, w)
      RATIO <- sqrt(deltaf/sum(s^2))
      if (RATIO <= tol) break;
    }  
    ### Backfitting ends
    eta <- Z - residuals
    if(any(abs(eta)==Inf)) stop("infinite values in linear predictor")
    ls <- dls(Y, X, which, eta, I.span, TRUE)
    dev.new <- -2*ls$l
    if(i==1 | dev.new<=dev.old){
      rslt.smooth.fit=smooth.fit
      rslt <- list()
      rslt$cov <- vcov(fit)[-1,-1]
      rslt$coefficients <- fit$coefficients[-1]
      rslt$weights <- w
      rslt$additive.predictors <- eta
      rslt$smooth <- s
      dev.out <- dev.new
      rslt$residuals <- residuals
      deltaeta <- pmin(pmax(ls$deltaeta,-3),3)
      w <- ls$w*(ls$w>=0)
    }
    ## check convergence
    if(dev.new>dev.old | (abs(dev.old - dev.new)/(dev.old + 0.1) <=tol)) break;
    dev.old <- dev.new
  }
  if(i == Maxiter) warning(paste("The process does not converge in",Maxiter,"iterations"))
  if (i == 1 & dev.new > dev.old) warning(paste("Deviance is better (",round(dev.old,1),
  	") for separate linear predictors for X and Y coordinates, without any smoothing!"))
  rslt$deviance <- dev.out
  dfs = predict(rslt.smooth.fit, se=TRUE)
  rslt$var = dfs$se^2
  rslt$df.residual = dfs$df-if(!is.null(rslt$coefficients))length(rslt$coefficients) else 0
  rslt$aic = rslt$deviance + 2*(N-rslt$df.residual)
  if(!is.null(rslt$coefficients))
    names(rslt$coefficients) <- colnames(X)
  rslt
}