gq.test = function (mod,
                    data = list(),
                    split = 0.5,
                    omit.obs = 0,
                    ah = c("increasing", "unequal", "decreasing"),
                    order.by = NULL,
                    sig.level = 0.05,
                    details = FALSE,
                    hyp = TRUE
                    ){

  # Function of a custom regression
  my.reg = function(X, y, name){
    mod = lm.fit(X,y)
    ssr = sum(mod$residuals^2)
    sig.hat = ssr/mod$df.residual
    VC = sig.hat * chol2inv(chol(t(X) %*% X))
    misc = matrix(NA, 1, 4)
    misc[1] = length(y)
    misc[2] = mod$df.residual
    misc[3] = ssr
    misc[4] = sig.hat
    colnames(misc) = c("nobs", "df", "ssr", "sigma.hat")
    rownames(misc) = name

    hreg = matrix(NA, k, 4)
    hreg[,1] = mod$coefficients
    hreg[,2] = sqrt(diag(VC))
    hreg[,3] = mod$coefficients/sqrt(diag(VC))
    hreg[,4] = 2*pt(-abs(hreg[,3]), df = mod$df.residual)
    colnames(hreg) = c("coef", "std.err", "t.value", "p.value")
    rownames(hreg) = names(mod$coefficients)
    return(list(misc = misc, hreg = hreg))
  }

  ah = match.arg(ah)
  if (!inherits(mod, "formula")){ # if mod is a fitted lm object ...
    X = model.matrix(terms(mod), model.frame(mod))
    y = model.response(model.frame(mod))
  } else { # if mod is a formula...
    X = model.matrix(mod, data = data)
    y = model.response(model.frame(mod, data = data))
  }

  k = ncol(X) # Number of coefs in the model
  n = nrow(X) # Number of observations


  if (split > 1) { # If partitioning point is absolute ...
    if (omit.obs < 1){
      omit.obs = floor(omit.obs * n)
    }
    point1 = split - ceiling(omit.obs/2)
    point2 = split + ceiling(omit.obs/2 + 0.01)
  }
  else { # If partitioning point is percentage of data ...
    if (omit.obs >= 1) {
      omit.obs = omit.obs/n
    }
    point1 = floor((split - omit.obs/2) * n)
    point2 = ceiling((split + omit.obs/2) * n + 0.01)
  }
  if (point2 > n - k + 1 | point1 < k)
    stop("Inadmissable breakpoint or too many central observations omitted!")
  if (!is.null(order.by)) { # If variable to be ordered is specified ...
    if (inherits(order.by, "formula")) {
      z = model.matrix(order.by, data = data)
      z = as.vector(z[, ncol(z)])
    }
    else {
      z = order.by
    }
    X = as.matrix(X[order(z), ])
    y = y[order(z)]
  }

  # Regression 1
  df1 = point1 - k
  hreg1 = my.reg(as.matrix(X[1:point1, ]), y[1:point1], name = "Group I    ")

  # Regression 2
  df2 = n - point2 + 1 - k
  hreg2 = my.reg(as.matrix(X[point2:n, ]), y[point2:n], name = "Group II   ")

  # Formulate Hypotheses
  if (hyp) {
  H1 = switch(ah,
              unequal = "sigma I <> sigma II",
              decreasing = "sigma I > sigma II",
              increasing = "sigma I < sigma II"
  )
  H0 = switch(ah,
              unequal = "sigma I = sigma II",
              decreasing = "sigma I <= sigma II",
              increasing = "sigma I >= sigma II"
  )
  H = matrix(c(H0, H1), 1L, 2L)
  dimnames(H) = list("",c("H0:", "H1:"))
  } else {
    H = NULL
  }

  # Goldfeld-Quandt F-value
  gq = switch(ah,
              unequal = max(hreg1[[1]][4]/hreg2[[1]][4], hreg2[[1]][4]/hreg1[[1]][4]),
              decreasing = hreg1[[1]][4]/hreg2[[1]][4],
              increasing = hreg2[[1]][4]/hreg1[[1]][4]
  )

  max.idx = which.max(c(hreg1[[1]][4],hreg2[[1]][4]))

  # critical- and p-values
  f.crit = switch(ah,
              unequal = qf(1-sig.level/2, c(df1,df2)[max.idx], c(df1,df2)[-max.idx]),
              decreasing = qf(1-sig.level, df1, df2),
              increasing = qf(1-sig.level, df2, df1)
  )

  p.val = switch(ah,
              unequal = pf(gq, c(df1,df2)[max.idx], c(df1,df2)[-max.idx]),
              decreasing = 1 - pf(gq, df1, df2),
              increasing = 1 - pf(gq, df2, df1)
  )

  test.result = if (p.val < sig.level) "rejected" else "not rejected"
  results = data.frame(f.value = gq,
                       crit.value = f.crit,
                       p.value = p.val,
                       sig.level = sig.level,
                       H0 = test.result,
                       row.names = "")

  # Generate return list
  out = list()
  attr(out, "title") = "Goldfeld-Quandt test for heteroskedastic errors in a linear model"
  out$hyp = H # Null and alternative hypothesis
  out$results = results # Basic test results
  out$hreg1 = hreg1[[2]]
  out$stats1 = hreg1[[1]]
  out$hreg2 = hreg2[[2]]
  out$stats2 = hreg2[[1]]
  out$nulldist = list(type = "f", df = c(df1,df2))

  attr(out, "direction") = "right"
  attr(out, "details") = if (details) {T} else {F}
  attr(out, "type") = "htest"
  attr(out, "test.type") = "gqtest"
  class(out) = c("desk")
  return(out)
}
