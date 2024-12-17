ols.predict = function(mod, data = list(), xnew, antilog = FALSE, details = FALSE){

  if (inherits(mod, "formula")) { # Wenn Formel übergeben ...
    mod = ols(mod, data = data)
  }

  if (missing(xnew)){xnew = as.matrix(mod$data[,-1])}
  xnew = as.matrix(xnew)

  if(dim(xnew)[1] > 1 & dim(xnew)[2] == 1) xnew = t(xnew) # convert column vector to row vector

  co = coef(mod) # Übergebene Koeffizienten
      if(mod$ncoef - sum(ols.has.const(mod)) == dim(xnew)[2]){
        if(ols.has.const(mod)){
          xnew = cbind(rep(1,dim(xnew)[1]),xnew) # Füge Vektor von Einsen ein
        }
        val = as.vector(xnew %*% co) # Fitted values
        if (antilog){# Korrekturfaktor bei log(y)
          cf = exp(mod$sig.squ/2)
          val = exp(val) * cf
          message(paste("Predicted value corrected by factor",cf))
        }

        sig2 = mod$sig.squ # Sigma^2
        varpe = diag(xnew %*% vcov(mod) %*%t (xnew)) + sig2 # Standardabw. Prognosefehler

        out = list(pred.val = val)
        if(ols.has.const(mod)){xnew = xnew[,-1]}
        out$xnew = xnew
        out$var.pe = varpe
        out$sig.squ = sig2
        out$smpl.err = varpe - sig2
        out$mod = mod


        attr(out, "title") = NULL
        attr(out, "details") = if (details) {T} else {F}
        attr(out, "type") = "pred"
        class(out) = c("desk")

      } else {
        stop("xnew must be (T x K) where T is the  number of predictions,
                and K is the number of exogenous predictors", call. = F)
      }
  return(out)
}
