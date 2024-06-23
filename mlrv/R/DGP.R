####DGP#######
#' @useDynLib mlrv,.registration = TRUE

fdiff_sint<-function(x, burnin = 200)
{
  n = length(x)
  t = (1:n)/n
  d = 0.35 + 0.1*cos(2*pi*t)
  xd = Ctvfdiff(x, d, burnin)
  xd = as.vector(xd)
  return(xd)
}
fdiff<-function (x, d)
{
  iT <- length(x)
  np2 <- nextn(2 * iT - 1, 2)
  k <- 1:(iT - 1)
  b <- c(1, cumprod((k - d - 1)/k))
  dx <- fft(fft(c(b, rep(0, np2 - iT))) * fft(c(x, rep(0, np2 -
                                                         iT))), inverse = T)/np2
  return(Re(dx[1:iT]))
}

#### compare with fdiff#####
# x = rnorm(1000)
# n = length(x)
# t = (1:n)/n
# d = rep(0.4,n)
# xd = Ctvfdiff(x,d)
# xd = as.vector(xd)
# dx = fdiff(x,-0.4)
# microbenchmark::microbenchmark(fdiff(x,-0.4),fdiff_sint(x))

################################################################

Qtvarma<-function(T_n, white_noise = NULL, tw = 1, rate = 1, center = 0.2, ma_rate = 0,
                  burnin = 200, full = FALSE)
{
  G1 = rep(0, T_n + burnin)
  if(is.null(white_noise))
    white_noise = rnorm(burnin + T_n, mean = 0, sd = tw)
  for(i in (-burnin + 2) : T_n)
  {
    x = 0
    for(j in (-burnin + 2) :i){
      t = i/T_n
      x = (center - rate * 4 * (t - 0.5)^2) * x  + white_noise[j + burnin] - ma_rate * sin(pi * t) * white_noise[j + burnin - 1]
    }
    G1[i + burnin] = x
  }
  if(full)
     return(G1)
  else
    return(G1[(burnin+1):(burnin+T_n)])
}


Qtvtrend<-function(T_n,cur = 1){
  t = 1 : T_n / T_n
  mt = (-cur * 8 * (t-0.5)^2)
  return(mt)
}

#' @title Simulate data from time-varying trend model
#' @param T_n integer, sample size
#' @param param a list of parameters
#' \itemize{
#' \item tw double, squared root of  variance  of the innovations
#' \item rate double, magnitude of non-stationarity
#' \item center double, the center of the ar coefficient
#' \item ma_rate double, ma coefficient
#' }
#' @return a vector of non-stationary time series
#' @examples
#' param = list(d = -0.2,  tvd = 0, tw = 0.8, rate = 0.1, center = 0.3, ma_rate =  0, cur = 1)
#' data = Qt_data(300, param)
#' @export
Qt_data<-function(T_n,param)
{
  t = 1 : T_n / T_n
  G1 = Qtvarma(T_n, tw = param$tw, rate = param$rate, center = param$center, ma_rate = param$ma_rate)

  if(param$tvd){
    G1 = fdiff_sint(G1)
  }else if(param$d)
    G1 = fdiff(G1, param$d) # print(paste("d =",d))

  trend = Qtvtrend(T_n, cur = param$cur)
  v = G1 + trend
  return(v)
}

Ctvarma<-function(T_n, tw = 1, rate = 1,center = -0.2,
                  burnin = 200, full = FALSE)
{
  ut = rep(0, T_n + burnin)
  white_noise = rnorm(T_n + burnin, mean = 0, sd = tw)
  for(i in (-burnin + 1) : T_n)
  {
    x = 0
    for(j in (-burnin + 1): i){
      t = i/T_n
      x = (center + rate * cos(2 * pi * t)) * x + white_noise[j + burnin]
    }
    ut[i + burnin] = x
  }
  if(full)
    return(ut)
  else
    return(ut[(burnin+1):(burnin+T_n)])
}

tvgarch11<-function (T_n, a , b, rnd = rnorm, ntrans = 200, ...){
  # alpha p*n
  # beta p*n
  n = T_n
  total.n = n + ntrans
  alpha = matrix(c((a[2]-a[1])/2*cos(pi/3+ 2*pi*seq(0, 1, length.out =  total.n))
                   +(a[2]+a[1])/2,
                   seq(a[3], a[4], length.out =  total.n)),
                 byrow = T, nrow=2)
  beta = matrix(seq(b[1], b[2], length.out = n + ntrans), nrow=1)
  if (!missing(beta))
    p = nrow(beta)
  else p = 0
  if (!missing(alpha))
    q = nrow(alpha) - 1
  else stop("beta is missing!")
  if (q == 0)
    stop("Check model: q=0!")
  if(ncol(alpha) == ncol(beta))
    n = ncol(alpha)
  else
    stop("Check alpha and beta!")
  e = rnd(total.n, ...)            # generate innovations
  x = double(total.n)              # create double precision vector
  sigt = x
  d = max(p, q)

  for(i in 1:d){
    sigma2 = sum(alpha[-1,i])   #?
    if (p > 0)
      sigma2 = sigma2 + sum(beta[,i])
    if (sigma2 > 1)
      stop("Check model: it does not have finite variance")
    sigma2 = alpha[1,i]/(1 - sigma2)
    if (sigma2 <= 0)
      stop("Check model: it does not have positive variance")
    x[i] = rnd(d, sd = sqrt(sigma2))
    sigt[i] = sigma2
  }
  xtmp = x

  for (i in (d + 1):total.n) {
    for(j in (d + 1):i){
      sigt[j] = sum(alpha[,i] * c(1, xtmp[j - (1:q)]^2)) + sum(beta[,i] * sigt[j - (1:p)])
      xtmp[j] = e[j] * sqrt(sigt[j])
    }
    x[i] = e[i] * sqrt(sigt[i])
  }

  return(x[(ntrans + 1):total.n])
}


#' @title Simulate data from time-varying time series regression model
#' @param T_n int, sample size
#' @param param list, a list of parameters
#' @param type type = 1 means the long memory expansion begins from its infinite past, type = 2 means the long memory expansion begins from t = 0
#' @return list, a list of data, covariates, response and errors.(before and after fractional difference)
#' @examples
#' param = list(d = -0.2, heter = 2, tvd = 0,
#' tw = 0.8, rate = 0.1, cur = 1, center = 0.3,
#' ma_rate =  0, cov_tw =  0.2, cov_rate = 0.1,
#' cov_center = 0.1, all_tw  = 1, cov_trend = 0.7)
#' n = 500
#' data = Qct_reg(n, param)
#' @export
Qct_reg<-function(T_n, param, type = 1){
  # type = 1 means the long memory expansion begins from its infinte past
  # type = 2 means the long memory expansion begins from t = 0
    t = (1:T_n)/T_n
    beta1 = sin(pi*t) * 4 * param$cur #similar smoothness
    if("curbeta" %in% names(param)){
      beta2 = exp(- (t - 1/2)^2*2) * param$curbeta #similar smoothness
    }else{
      beta2 = exp(- (t - 1/2)^2*2) * 4
    }
    if("burnin" %in% names(param)){
      burnin = param$burnin
    }else{
      burnin = 200
    }
    white_noise = NULL
    if("garch" %in% names(param)){
      if(param$garch == T)
        white_noise = tvgarch11(T_n + burnin,
                                c(0.8, 1, 0.1, 0.2),
                                c(0.1, 0.2),
                                ntrans = 100)
    }else if("heavy" %in% names(param)){
      if(param$heavy == "t8")
        white_noise = rt(T_n + burnin, 8)
      else if(param$heavy == "t6")
        white_noise = rt(T_n + burnin, 6)
      else if(param$heavy == "t5")
        white_noise = rt(T_n + burnin, 5)
      else if(param$heavy == "t4")
        white_noise = rt(T_n + burnin, 4)
      else if(param$heavy == "chi2"){
        white_noise = (rchisq(T_n + burnin, 5)-5)/sqrt(10)
      }
    }

    if((param$d == 0) && (param$tvd == 0))
    {
      #the Covariate with zero mean
      x = Ctvarma(T_n, tw = param$cov_tw,
                  rate = param$cov_rate,
                  center = param$cov_center,
                  burnin = burnin) #return length=n
      #the Covariate with time varying trend
      if("cov_trend" %in% names(param))
      {
        if(param$cov_trend)
        {
           x = x +  (t - 0.5)^2 * param$cov_trend
          # x = x + cos(pi* t) * param$cov_trend
        }
      }

      X = cbind(rep(1, T_n), x)
      trend = beta1 + beta2 * x
      #Innovation
      error = Qtvarma(T_n, white_noise, tw = param$tw,
                      rate = param$rate,
                      center = param$center,
                      ma_rate = param$ma_rate,
                      burnin = burnin)
      #Heterscedastic
      if(param$heter == 1){
        error = error * x * param$all_tw
      }else if(param$heter == 2){
        error = error * sqrt(1 + x^2) * param$all_tw
      }
      error0 = NA
    }else{
      if(type == 1)
      {
        #the Covariate with zero mean
        x = Ctvarma(T_n, tw = param$cov_tw,
                    rate = param$cov_rate,
                    center = param$cov_center,
                    burnin = burnin, full=TRUE)
        #the Covariate with time varying trend
        if("cov_trend" %in% names(param))
        {
          if(param$cov_trend)
          {
            t1 = (1:(T_n + burnin)) / T_n - burnin / T_n
            x = x +  (t1 - 0.5)^2 * param$cov_trend
            #x = x +  cos(pi*t1) * param$cov_trend
          }
        }
        X = cbind(rep(1, T_n), x[(burnin + 1) : (burnin + T_n)])
        trend = beta1 + beta2 * x[(burnin + 1) : (burnin + T_n)]
        #Innovation
        error = Qtvarma(T_n, white_noise, tw = param$tw,
                        rate = param$rate,
                        center = param$center,
                        ma_rate = param$ma_rate,
                        burnin = burnin, full=TRUE)

        #Heterscedastic
        if(param$heter == 1){
          error = error * x * param$all_tw
        }else if(param$heter == 2){
          error = error * sqrt(1 + x^2) * param$all_tw
        }
        if(param$tvd){
          error0 = error
          error = fdiff_sint(error, burnin)
        }else if(param$d){
          error0 = error
          error = fdiff(error, param$d)
        }
        error0 = error0[(burnin + 1) : (burnin + T_n)]
        error = error[(burnin + 1) : (burnin + T_n)]
      }
    }
    y = trend + error
    return(list(x = X, y = y, e = error, e0 = error0))

}



xx12 <- function(n, type = "norm"){
  burnin = n/2

  if(type == "norm"){
    ee0=rnorm(n + burnin)
    ee2=rnorm(n + burnin)
  }else if(type == "t4"){
    ee0=rt(n + burnin, 4)
    ee2=rt(n + burnin, 4)
  }else if(type == "t5"){
    ee0=rt(n + burnin, 5)
    ee2=rt(n + burnin, 5)
  }else if(type == "t6"){
    ee0=rt(n + burnin, 6)
    ee2=rt(n + burnin, 6)
  }else{
    warning("not supported!")
  }

  ee1=(ee0+ee2)/sqrt(2)
  ex1=rep(1,n)
  ex2=ex1

  for (i in 1:n){
    tmp1 = 0
    tmp2 = 0
    t = i/n
    c1 = 1/2 - t/2
    c2 = 1/4+(t-1/2)^2/2
    for(j in (-burnin + 1):i){
      tmp1 =  c1*tmp1 + ee1[j+burnin]
      tmp2 =  c2*tmp2 + ee2[j+burnin]
    }
    ex1[i] = tmp1
    ex2[i] = tmp2
  }
  return(list(xx1=ex1,xx2=ex2))
}


lserror <-function(nn, type = "norm"){

  burnin = nn/2
  if(type == "norm"){
    innov=rnorm(burnin+ nn)
  }else if(type == "t4"){
    innov=rt(burnin + nn, 4)
  }else if(type == "t5"){
    innov=rt(burnin + nn, 5)
  }else if(type == "t6"){
    innov=rt(burnin + nn, 6)
  }else{
    warning("not supported!")
  }

  e1=rep(1,nn)

  for (i in 1:nn){
    tmp1 = 0
    t = i/nn
    c = 0.65*cos(2*pi*t)
    for(j in (-burnin + 1):i){
      tmp1 =  c*tmp1 + innov[j+burnin]
    }
    e1[i] = tmp1
  }
  return(e1)
}


#' @title Simulate data from time-varying time series regression model with change points
#' @param nn sample size
#' @param cp number of change points. If cp is between 0 and 1, it specifies the location of the single change point
#' @param delta double, magnitude of the jump
#' @param type type of distributions of the innovations, default normal. It can also be "t4", "t5" and "t6".
#' @return a list of data, x covariates, y response and e error.
#' n = 300
#' data = bregress2(n, 2, 1) # time series regression model with 2 changes points
#' @export
bregress2<-function(nn, cp = 0, delta = 0, type="norm"){

  ar.sim3=(lserror(nn, type))/2
  zz=xx12(nn, type)
  xx1=zz$xx1
  xx2=zz$xx2
  gamma = 0.1
  t = (1:nn)/nn
  # x = rchisq(n=nn,df=5)/5

  if(cp>0 & cp<=1){
    mm1= 2*sin(2*pi*t)*xx1
    mm1[1:round(nn*cp)] = 0
    yy= 1+xx1+xx2+(1+gamma*xx1)*ar.sim3 + delta*mm1
    ee = (1+gamma*xx1)*ar.sim3
    xx = cbind(rep(1, nn), xx1, xx2)
  }else if(cp == 2){
    mm1= xx1
    mm1[1:round(nn*0.7)] = 0
    mm2 = 2*sin(2*pi*t)
    mm2[round(nn*0.4):nn] = 0
    yy= 1+xx1+xx2+(1+gamma*xx1)*ar.sim3 + delta/2*mm1 +  delta/2*mm2
    ee = (1+gamma*xx1)*ar.sim3
    xx = cbind(rep(1, nn), xx1, xx2)
  }else if(cp == 4){
    mm2 = 1.5*sin(2*pi*t)
    mm2[round(nn*0.2):round(nn*0.4)] = 0
    mm2[round(nn*0.6):round(nn*0.8)] = 0
    yy = 1+xx1+xx2+(1+gamma*xx1)*ar.sim3 +  delta*mm2
    ee = (1+gamma*xx1)*ar.sim3
    xx = cbind(rep(1, nn), xx1, xx2)
  }else{
    xx = cbind(rep(1, nn), xx1, xx2)
    yy = apply(xx, 1, sum) + sqrt((1+xx1^2+xx2^2)/4)*ar.sim3
    ee = sqrt((1+xx1^2+xx2^2)/4)*ar.sim3
  }

  return(list(x = xx, y=yy, e = ee))
}


#' This is data to be included in my package
#'
#' @name hk_data
#' @docType data
#' @author T. S. Lau
#' @references Fan, J., and Zhang, W. (1999). Statistical estimation in varying coefficient models. The annals of Statistics, 27(5), 1491-1518.
#' @keywords data
NULL

