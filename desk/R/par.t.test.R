par.t.test = function(mod,
                       data = list(),
                       nh,
                       q = 0,
                       dir = c("both", "left", "right"),
                       sig.level = 0.05,
                       details = FALSE,
                       hyp = TRUE){

  if (inherits(mod, "formula")) { # Wenn Formel übergeben ...
    mod = ols(mod, data = data)
  }

  dir = match.arg(dir) # Take the first default argument

  df = mod$df.residual # T-K-1
  sd = sqrt(nh %*% vcov(mod) %*% nh)
  bhat = coef(mod)
  coefnames = names(bhat)

  # Calculate t-Value
  t.val = (bhat %*% nh - q)/sd

  ## Critical and p-value
  if (dir == "both"){
    t.crit = sign(t.val) * qt(1 - sig.level/2, df)
    p.val = pt(-abs(t.val), df)*2
    H0sgn = " = "
    H1sgn = " <> "
  }
  if (dir == "right"){
    t.crit = qt(1-sig.level, df)
    p.val = 1 - pt(t.val, df)
    H0sgn = " <= "
    H1sgn = " > "
  }
  if (dir == "left"){
    t.crit = qt(sig.level, df)
    p.val = pt(t.val, df)
    H0sgn = " >= "
    H1sgn = " < "
  }

  ## Generate Hypothesis
    H = matrix(NA, 1, 2L)
      h = ""
      for (i in 1:length(coefnames)){
        tmp = coefnames[i]
        if (nh[i] != 0){ # If coef not zero...
          tmp = paste(as.character(abs(nh[i])),"*",tmp, sep = "")
          tmp = paste(if ((nh[i]>0) & (h != "")) " + " else if (nh[i]<0) " - ", tmp, sep = "")
        } else {tmp = ""}# If coef zero, then no name
        h = paste(h, tmp, sep = "")
      } # end of inner for
  if (hyp) {
    H = c(paste(h, H0sgn , q, sep = ""), paste(h, H1sgn, q, sep = ""))
    names(H) = c("H0:", "H1:")
    H = t(H)
  } else {
    H = NULL
  }

  test.result = if (p.val < sig.level) "rejected" else "not rejected"
  results = data.frame(t.value = t.val,
                       crit.value = t.crit,
                       p.value = p.val,
                       sig.level = sig.level,
                       H0 = test.result,
                       row.names = "")

  out = list()
  attr(out, "title") = "t-test on one linear combination of parameters"
  out$hyp = H # Null and alternative hypothesis
  out$nh = nh # Null and alternative hypothesis (Vector form)
  out$lcomb = h
  out$results = results # Basic test results
  out$std.err = sd
  out$nulldist = list(type = "t", df = df)

  attr(out, "direction") = dir
  attr(out, "details") = if (details) {T} else {F}
  attr(out, "type") = "htest"
  attr(out, "test.type") = "ttest"
  class(out) = c("desk")

  return(out)
}
