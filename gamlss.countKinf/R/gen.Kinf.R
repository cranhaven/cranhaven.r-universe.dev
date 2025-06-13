gen.Kinf=function(family = "NO", kinf=1)
{
  fam <- as.gamlss.family(family)
  fname <- fam$family[[1]]
  dfun <- paste("d", "inf", kinf,fname, sep = "")
  pfun <- paste("p", "inf", kinf,fname, sep = "")
  qfun <- paste("q", "inf", kinf,fname, sep = "")
  rfun <- paste("r", "inf", kinf,fname, sep = "")
  fun <- paste("inf", kinf, fname, sep = "")
  alldislist <- c(dfun, pfun, qfun, rfun, fun)

  inf.dfun=paste("dKI", fname, sep = "")
  inf.pfun=paste("pKI", fname, sep = "")
  inf.qfun=paste("qKI", fname, sep = "")
  inf.rfun=paste("rKI", fname, sep = "")
  inf.fun=paste("KI", fname, sep = "")

  inf.dfun=eval(parse(text = inf.dfun))
  inf.pfun=eval(parse(text = inf.pfun))
  inf.qfun=eval(parse(text = inf.qfun))
  inf.rfun=eval(parse(text = inf.rfun))
  inf.fun=eval(parse(text = inf.fun))

  rep_vars <- function(expr, repValue) {
    if (!length(expr)) return()
    for (i in seq_along(expr)) {
      if (is.call(expr[[i]])) expr[[i]][-1L] <- Recall(expr[[i]][-1L], repValue)
      if (is.name(expr[[i]]) && deparse(expr[[i]]) %in% names(repValue))
        expr[[i]] <- repValue[[deparse(expr[[i]])]]
    }
    return( expr )
  }

  newValue <- c('kinf'=kinf)

  body(inf.dfun) <- rep_vars(body(inf.dfun), newValue)
  body(inf.pfun) <- rep_vars(body(inf.pfun), newValue)
  body(inf.qfun) <- rep_vars(body(inf.qfun), newValue)
  body(inf.rfun) <- rep_vars(body(inf.rfun), newValue)
  body(inf.fun) <- rep_vars(body(inf.fun), newValue)

  eval(call("<-", as.name(dfun), inf.dfun), envir = parent.frame(n = 1))
  eval(call("<-", as.name(pfun), inf.pfun), envir = parent.frame(n = 1))
  eval(call("<-", as.name(qfun), inf.qfun), envir = parent.frame(n = 1))
  eval(call("<-", as.name(rfun), inf.rfun), envir = parent.frame(n = 1))
  eval(call("<-", as.name(fun), inf.fun), envir = parent.frame(n = 1))

  cat("inflated at", kinf ," distribution is generated from", fname,
      "and saved under the names: ", "\n", paste(alldislist,
                                                 sep = ","), "\n")
}

