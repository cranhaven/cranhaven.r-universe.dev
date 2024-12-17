jb.test = function(x, data = list(), sig.level = 0.05, details = FALSE, hyp = TRUE){

  if (inherits(x, "lm")){ # if x is a fitted lm object ...
    x = x$residuals
  } else if (inherits(x, "formula")){ # if x is a formula...
    X = model.matrix(x, data = data)
    y = model.response(model.frame(x, data = data))
    x = lm.fit(X,y)$residuals
  } else if (is.vector(x) & is.numeric(x)){ # if x is a numeric vector...
    x = x
  } else stop("Argument not recognized!", call. = F)

  asym = function (x, na.rm = FALSE) {
    n = length(x)
    (sum((x - mean(x))^3)/n)/(sum((x - mean(x))^2)/n)^(3/2)
  }

  kur = function (x) {
    n = length(x)
    n * sum((x - mean(x))^4)/(sum((x - mean(x))^2)^2)
  }

  n = length(x)
  K = kur(x)
  A = asym(x)
  JB = (n/6) * (A^2 + 0.25 * ((K - 3)^2))

  if (hyp){
    H = c("skew = 0 and kur = 3 (norm.)", "skew <> 0 or kur <> 3 (non-norm.)")
    names(H) = c("H0:", "H1:")
    H = t(H)
  } else {
    H = NULL
  }

  ## Generate other data
  chi2.crit = qchisq(1 - sig.level, df = 2)
  p.val = 1 - pchisq(JB, df = 2)

  test.result = if (p.val < sig.level) "rejected" else "not rejected"
  results = data.frame(JB = JB,
                       crit.value = chi2.crit,
                       p.value = p.val,
                       sig.level = sig.level,
                       H0 = test.result,
                       row.names = "")

  out = list()
  attr(out, "title") = "Jarque-Bera test for normality"
  out$hyp = H # Null and alternative hypothesis
  out$results = results # Basic test results
  out$skew = A
  out$kur = K
  out$nobs = n
  out$nulldist = list(type = "chisq", df = 2)

  attr(out, "direction") = "right"
  attr(out, "details") = if (details) {T} else {F}
  attr(out, "type") = "htest"
  attr(out, "test.type") = "jbtest"
  class(out) = c("desk")

  return(out)
  }
