pdw = function(x, mod, data = list()){

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

    while((iter<=MAXIT) & (derivOK==1) & (disc>=tol3)){
      iter = iter + 1
      bot = 1 - 2*s0*lambda
      v = 1/bot
      kp = sum(lambda*v*(df + q*v))
      kpp = 2*sum(lambda^2*v^2*(df + 2*q*v))
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
  #####################################################################

  if (inherits(mod, "formula")) { # If formula ...
    X = model.matrix(mod, data = data)
  }
  if (inherits(mod, "lm")) { # If model ...
    X = model.matrix(terms(mod), model.frame(mod))
  }
  if (inherits(mod, "matrix")) { # If model matrix ...
    X = mod
  }

  k = ncol(X) # Number of coefs
  n = nrow(X) # Number of observations

  Q1 = chol2inv(qr.R(qr(X)))

  # generate first differencing matrix A
  A = diag(c(1, rep(2, n - 2), 1))
  A[abs(row(A) - col(A)) == 1] = -1

  # calculate M = I - X*inv(X'X)*X'
  M = diag(rep(1, n)) - X %*% Q1 %*% t(X)

  dist = rep(NA, length(x))
  for (i in 1:length(x)){
    Z = (M %*% A %*% M - M*x[i]) # see Ali (1984)
    evls = sort(eigen(Z)$values)
    dist[i] = spa(daniels = 2, x = 0, lambda = evls)$cdf
  }
  return(dist)
}
