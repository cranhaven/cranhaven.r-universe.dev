dw.test = function (mod, data = list(),
                    dir = c("left", "right", "both"),
                    method = c("pan1", "pan2", "paol", "spa"),
                    crit.val = TRUE,
                    sig.level = 0.05,
                    details = FALSE,
                    hyp = TRUE
                    ){

  method = match.arg(method)
  dir = match.arg(dir)

  if (!inherits(mod, "formula")) { # Wenn Modell übergeben ...
    mf = model.frame(mod)
    X = model.matrix(terms(mod), model.frame(mod))
    y = model.response(model.frame(mod))
  }
  else { # Wenn Formel übergeben ...
    mf = model.frame(mod, data = data)
    y = model.response(mf)
    X = model.matrix(mod, data = data)
    mod = lm.fit(X,y)
  }

  coefnames = names(mod$coefficients)
  y = as.matrix(y)
  colnames(y) = colnames(mf)[1]

  k = ncol(X) # Number of coefs
  n = nrow(X) # Number of observations

  ##################################################
  # Pan's Algorithm
  # translated and modified from Statlib: AS 153
  # Farebrother (1980), Appl. Stat. Vol. 29, No.2
  ##################################################
  pan1 = function(lambda, x, n){
    m = length(lambda)
    nu = which(lambda >= x)[1]
    if(is.na(nu)) {out = 0}
    nu = nu - 1
    h = m - nu
    if (h > nu){
      d = 2; h = nu; k = -1; j1 = 0; j2 = 2; j3 = 3; j4 = 1
    } else {
      d = -2; nu = nu + 1; k = 1; j1 = m - 2; j2 = m - 1; j3 = m + 1; j4 = m
    }
    out = (k + 1)/2
    sgn = k/n
    for  (i in seq(from = h - 2*floor(h/2), to = 0, by = -1)){
      # first integrals
      for (l2 in seq(from = j2, to = nu, by = d)){
        sum1 = lambda[j4]
        if (l2 == 0){
          prod = x
        } else {
          prod = lambda[l2]
        }
        U = 0.5 * (sum1 + prod)
        V = 0.5 * (sum1 - prod)
        sum1 = 0
        for (I in seq(from = 1, to = 2*n-1, by = 2)){
          Y = U - V * cos(I*pi/n)
          if (j1 > 0) a = lambda[1:j1] else a = NULL
          if (m > j3) b = lambda[j3:m] else b = NULL
          prod = prod((Y - x) / (Y - c(a,b)))
          sum1 = sum1 + sqrt(abs(prod))
        }
        sgn = -sgn
        out = out + sgn * sum1
        j1 = j1 + d; j3 = j3 + d; j4 = j4 + d
      }
      # second integral
      if (d == 2) {
        j3 = j3 - 1
      } else {
        j1 = j1 + 1
      }
      j2 = 0
      nu = 0
    }
    return(out)
  }

  ##################################################
  # Paolella's implementation of Imhof's (1961)
  # Algorithm
  # see ch. 10 in Paolella (2007) "Intermediate
  # Probability: A computational approach", Wiley
  ##################################################
  paolella = function(lambda, x, df = rep(1, length(lambda)), nc = rep(0, length(lambda)), uplim = -1){
    tol = 1e-8
    cdf = numeric()
    for(i in 1:length(x)){
      x = x[i]
      if(uplim<0){
        cdf[i] = 0.5 - (1/pi)*integrate(ff, tol, 1-tol,
                                        x = x, lambda = lambda,
                                        df = df, nc = nc,
                                        uplim = uplim)$value}
      else {
         cdf[i] = 0.5 - (1/pi)*integrate(ff, tol, uplim,
                                         x = x, lambda = lambda,
                                         df = df, nc = nc,
                                         uplim = uplim)$value
        }
    }
    return(cdf)
  }
  ff = function(tvec, x, lambda, df, nc, uplim){
    I = numeric()
    usetransform = (uplim < 0)

    for(ii in 1:length(tvec)){
      t = tvec[ii]
      if(usetransform) {u = (1-t)/t} else {u = t}
      p = u*lambda
      b = p^2
      c = 1+b
      ss0 = sum( df*atan(p) + nc*p/c )
      beta = (ss0 - x*u)/2
      ss1 = sum( nc*b/c )
      ss2 = sum( df*log(c) )
      gam = exp( 0.5*ss1 + 0.25*ss2 )
      I[ii] = sin(beta)/(u*gam)
      if(usetransform) {I[ii] = I[ii]/t^2}
    }
    return(I)
  }

  ##################################################
  # Paolella's implementation of a saddlepoint
  # approximation
  # see ch. 10 in Paolella (2007) "Intermediate
  # Probability: A computational approach", Wiley
  ##################################################
  spa = function(daniels, xvec, lambda, df = rep(1, length(lambda)), q = rep(0, length(lambda)), s = 0){

    alo = 2*min(lambda)
    ahi = 2*max(lambda)
    if(alo>0) {lower = -Inf; upper = 1/ahi; boundtype = 1} else if(ahi<0)
    {lower = 1/alo; upper = Inf; boundtype = 2;} else
    {lower = 1/alo; upper = 1/ahi; boundtype = 3}

    sstart = s
    pdf = cdf = svec = numeric()

    for(i in 1:length(xvec)){
      x = xvec[i]
      result = sparoot(sstart,x,lambda,df,q,lower,upper)

      #try once again, using a grid of s values

      if((result[[2]]>0) & (boundtype==3)){
        ss = seq(lower, upper, by=(upper-lower)/200)
        #x
        for(j in seq(2,length(ss)-1)){
          result = sparoot(ss[j],x,lambda,df,q,lower,upper)
          if(result[[2]]==0) break
        }
      }

      svec[i] = result[[1]]
      #sstart=s; % turn on to use shat from xvec(i-1) as starting value

      if((result[[2]]==0) | (result[[2]]==5)){
        v = 1/(1-2*result[[1]]*lambda)
        K = 0.5 * sum(df * log(v)) + result[[1]]*sum(lambda * q * v)
        Kpp  = 2 * sum(lambda^2 * v^2 * (df + 2 * q * v))
        pdf[i] = exp(K - x*result[[1]]) / sqrt(2*pi*Kpp)
        fac = 2*result[[1]]*x-2*K
        ww = sign(result[[1]])*sqrt(fac)
        u = result[[1]]*sqrt(Kpp)
        if((result[[2]]==5) | (abs(ww)<1e-7)){
          Kpp0  = 2 * sum(lambda^2 * (df + 2 * q))
          Kppp0 = 8 * sum(lambda^3 * (df + 3 * q))
          cdf[i] = 0.5+Kppp0/6/sqrt(2*pi)/Kpp0^(3/2)

          K3 = 8*sum(lambda^3 * v^3 * (df + 3*q*v))
          K4 = 48*sum(lambda^4 * v^4 * (df+4*q*v))
          kap3 = K3/(Kpp)^(3/2)
          kap4 = K4/(Kpp)^(4/2)
          forboth = (kap4/8 - 5*kap3^2/24)
        } else {
          npdf = dnorm(ww)
          tempcdf = pnorm(ww)+npdf*(1/ww - 1/u)
          if(daniels==2){
            K3 = 8*sum(lambda^3 * v^3 * (df + 3*q*v))
            K4 = 48*sum(lambda^4 * v^4 * (df+4*q*v))
            kap3 = K3/(Kpp)^(3/2)
            kap4 = K4/(Kpp)^(4/2)
            forboth = (kap4/8 - 5*kap3^2/24)

            #the cdf correction can be problematic!
            bigterm = forboth/u - 1/u^3 - kap3/2/u^2 + 1/ww^3
            correction = npdf * bigterm
            if(abs(correction)/tempcdf > 0.1){
              #The 2nd order CDF correction term is being ditched!
              correction = 0
            }
            cdf[i] = tempcdf-correction
            if((cdf[i]<0) | (cdf[i]>1)){
              #The 2nd order CDF correction term is being ditched!
              cdf[i] = tempcdf
            }
          } else {
            cdf[i] = tempcdf
          }
        }
        if(daniels==2) {pdf[i] = pdf[i] * (1+forboth)}
      } else {
        pdf[i] = 0
        cdf[i] = 0
      }
    }
    return(list(pdf = round(pdf,5), cdf = round(cdf,5)))
  }

  sparoot = function(s0,x,lambda,df,q,lower,upper){
    tol1 = 1e-8  # how close to the edge of the support of X'AX
    tol2 = 1e-12 # how close to zero Kpp can be in the Newton step
    tol3 = 1e-12 # when to stop the root search
    tol4 = 1e-8  # how close to zero shat may be

    MAXIT = 200
    s = 0
    iter = 0
    derivOK = 1
    disc = 10
    report = 0

    while((iter <= MAXIT) & (derivOK == 1) & (disc >= tol3)){
      iter = iter + 1
      bot = 1 - 2*s0*lambda
      v = 1/bot
      kp = sum(lambda * v * (df + q*v))
      kpp = 2*sum(lambda^2 * v^2 * (df + 2*q*v))
      derivOK = (kpp > tol2)
      if(derivOK){
        s = s0 - (kp - x)/kpp
        disc = abs(s - s0)
        s0 = s
      } else {
        report =6
        break
      }
    }
    if(report == 0){
      if(s < lower + tol1) {report = 11} else if(s > upper - tol1)
      {report = 12} else if(iter == MAXIT)
      {report = 3}  else if(abs(s) < tol4)
      {report = 5}
    }
    list(s=s, report=report)
  }

  #############################
  pan2 = function(x, lambda, n){
    if(any(x < 0)) stop("Calculating CDF, so: Check that any(x)>=0")
    F = numeric()
    yin = cos((2*seq(1, n) - 1)*pi/(2*n))
    for(loop in 1:length(x)){
      x = x[loop]
      if(all(lambda < 0)) {F[loop] = 1 - AS_R52(-x, -lambda, yin)} else {
        F[loop] = AS_R52(x, lambda, yin)
      }
    }
    return(F)
  }
  AS_R52 = function(c, lambda, yin){
    lambda = -sort(-lambda)
    m = length(lambda)
    n = length(yin)
    if(any(lambda <= 0) == FALSE) {v = m} else {v = min(which(lambda <= 0))-1}
    deltav = 0.5*(1-(-1)^v)
    deltamv = 0.5*(1-(-1)^(m-v))
    F = 0
    if(c >= 0 | (c == 0 & v <= m-v)){
      for(j in seq(1, floor(0.5*v))){
        F = F + ((-1)^j)*mean(panintegrand(yin, c, lambda, c(2*j, 2*j-1)))
      }
      F = 1 + F
      if(deltav > 0) {
        F = F + (deltav)*(-1)^(0.5*(v+1))*mean(panintegrand(yin,c,lambda,v))
      }
    } else {
      for(j in seq(1, floor(0.5*(m-v)))){
        F = F + ((-1)^j)*mean( panintegrand(yin,c,lambda,c(m - 2*j + 1, m - 2*j + 2)) )
      }
      if(deltamv>0){
        F = F + (deltamv)*(-1)^(0.5*(m - v + 1))*mean(panintegrand(yin, c, lambda, v + 1))
      }
      F = -F
    }
    F
  }
  panintegrand = function(tvec, c, lambda, outind){
    m = length(lambda)
    inind = 1:m
    outnumber = length(outind)
    for(i in 1:outnumber){
      inind = inind[which(inind != outind[i])]
    }
    sv = 1
    if(outnumber==2) {sv=c(1,-1)}
    I = numeric()
    for(tloop in 1:length(tvec)){
      t = tvec[tloop]
      yv = 0.5*(sum(lambda[outind])) - 0.5*(sum(lambda[outind]*sv))*t
      if(yv==0){helper = 0} else {helper = (exp(-c/(2*(yv))))}
      I[tloop] = helper*sqrt(abs(((yv)^(m - outnumber))/prod(yv - lambda[inind])))
    }
    return(I)
  }

  #############################################################

  r = lm.fit(X, y)$residuals # Get residuals
  dw = sum(diff(r)^2)/sum(r^2) # Durbin Watson statistic,
  Id = diag(rep(1, n))
  Q1 = chol2inv(qr.R(qr(X)))

  # generate first differencing matrix A
  A = diag(c(1, rep(2, n - 2), 1))
  A[abs(row(A) - col(A)) == 1] = -1

  # calculate M = I - X*inv(X'X)*X'
  M = Id - X %*% Q1 %*% t(X)

#
# Note: P((u'Au/u'Bu) < x) = P(u'(A-xB)u < 0).
#
  MAM = M %*% A %*% M  # see Ali (1984)
  evls = sort(eigen((MAM - M*dw))$values)
  pv = switch(method,
               pan1 = (pan1(lambda = evls, x = 0, n = n)),
               pan2 = (pan2(lambda = evls, x = 0, n = n)),
               paol = (paolella(lambda = evls, x = 0, df = rep(1, length(evls)))),
               spa = (spa(daniels = 2, x = 0, lambda = evls)$cdf)
          )

a = switch(dir,
            left = sig.level,
            right = 1 - sig.level,
            both = if (dw < 2) sig.level/2 else 1 - sig.level/2)

# Binary search for the critical value
  if(crit.val){
    tol = 0.00001
    val = dw # start value
    cdf_val = pv
    delta = 0.1
    sgn = sign(cdf_val - a) # direction of search
    sgn_curr = sgn

    while (abs(cdf_val - a) > tol){
      while(sgn == sgn_curr){
        val = val - sgn*delta
        cdf_val = spa(daniels = 2, x = 0, lambda = sort(eigen((MAM - M*val))$values))$cdf
        sgn_curr = sign(cdf_val - a) # direction of search
      }
    sgn = sgn_curr
    delta = 0.5 * delta # reduce delta
    }
  }

  ## CompQuadForm Implementation
  # cat(1 - davies(q = 0, lambda = evls)$Qq, "Davies (CompQuadForm)", "\n")
  # cat(1 - imhof(q = 0, lambda = evls)$Qq, "Imhof (CompQuadForm)", "\n")

  ## Only for testing purposes: compare results to original algorithm
  ## Fortran compiler (e.g. rtools) has to be installed
  #     p.gradsol = function(dw) .Fortran("gradsol",
  #                                 as.double(c(dw, ev)), # A
  #                                 as.integer(length(ev)), # M
  #                                 as.double(0), # C
  #                                 as.integer(n), # Iterations
  #                                 ret.val = double(1), # RETURN
  #                                 PACKAGE = "desk")$ret.val

  ## Useful if critical values of dL and dU should be calculated
  ## See Farebrother (2002) Handbook article
  # eiga = 2*(1-cos(pi*(1:n-1)/n)) # or: 4*sin(pi/(2*n)*(1:n-1))^2
  # eiga = sort(eiga[eiga > tol])
  # eigL = eiga[1:(n-k)]
  # eigU = eiga[k:(n-1)]
  # print(pan(eigL, 1.13295, n)) # has to return 5% siglevel

  p.value = switch(dir,
                both = (2 * min(pv,1 - pv) ),
                right = (1 - pv),
                left = pv)

  if (is.na(p.value) || ((p.value > 1) | (p.value < 0))) {
    stop("Exact p value not found, please try another method.")
  }

  # Formulate Hypotheses
  if (hyp) {
    H1 = switch(dir,
                left = "d < 2 (rho > 0, pos. a.c.)",
                right = "d > 2 (rho < 0, neg. a.c.)",
                both = "d <> 2 (rho <> 0, a.c.)"
    )
    H0 = switch(dir,
                left = "d >= 2 (rho <= 0, no pos. a.c.)",
                right = "d <= 2 (rho >= 0, no neg. a.c.)",
                both = "d = 2 (rho = 0, no a.c.)"
    )
    H = matrix(c(H0, H1), 1L, 2L)
    dimnames(H) = list("",c("H0:", "H1:"))
  } else {
    H = NULL
  }

test.result = if (p.value < sig.level) "rejected" else "not rejected"
results = data.frame(dw = dw,
                     crit.value = if(crit.val) val else NA,
                     p.value = p.value,
                     sig.level = sig.level,
                     H0 = test.result,
                     row.names = "")

out = list()
attr(out, "title") = "Durbin-Watson Test on AR(1) autocorrelation"
out$hyp = H # Null and alternative hypothesis
out$results = results # Basic test results
out$nulldist = list(type = "dw", par = X)

attr(out, "direction") = dir
attr(out, "details") = if (details) {T} else {F}
attr(out, "modmat") = X
attr(out, "type") = "htest"
attr(out, "test.type") = "dwtest"
class(out) = c("desk")

return(out)
}
