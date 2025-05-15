#' To calculate the stratum sample sizes (nh) for a fixed sample size (n) based
#' on the hypothetical distribution of the data
#'
#' This function is called towards the final stages of the stratification process
#' after OSB have been determined. It uses the boundaries to calculate the stratum
#' sample allocations using Neyman allocation for all individual strata using the
#' underlying distribution of the population.
#'
#' @param my_env The environment my_env which has various constants and outputs stored
#' from earlier operations through various other functions
#'
#' @import stats
#'
#' @return \code{} calculates and stores quantities such as nh, Nh, Vh, etc.
#' in the my_env to be accessed and printed as outputs
#'
#' @author Karuna Reddy <karuna.reddy@usp.ac.fj>\cr MGM Khan <khan_mg@usp.ac.fj>
#'
distr.alloc <- function(my_env)
{
  h <- my_env$h
  initval <- (my_env$initval)*(my_env$maxval) # for real data
  x <- c(initval, ((my_env$df)$x)*(my_env$maxval)) #append OSB to initval
  n <- my_env$n
  N <- my_env$N

  ch <- my_env$ch #a vector of stratum sample costs

  distr <- my_env$obj["distr"] #extract distr

  Wh <- double(h)
  AWh <- double(h) #adj weight
  Nh <- double(h)
  Vh <- double(h)
  nume <- double(h)
  deno <- 0
  nh <- double(h)
  fh <- double(h)

  #adjust extreme values form uncensored distributions like wei, gam, exp, lnorm, norm, cauchy and also adjust
  #doesnt apply to norm and uniform distribs
  #idea is to output adj weights that sum up to one while Vh, WhVh etc are calculated using the original weights
  #adjust the initial and final values of x to adjust stratum weight

  y <- x # create a copy of osb given in x as y
  d = y[length(y)]-y[1]

  #for distribs like wei, gam, exp, lnorm, etc. which are defined on x>= 0
  if(d < 10) {y[1] <- 0; y[length(y)] <- y[length(y)]+10}
  if(d >= 10 & d < 100) {y[1] <- 0; y[length(y)] <- y[length(y)]+100}
  if(d >= 100 & d < 1000) {y[1] <- 0; y[length(y)] <- y[length(y)]+1000}
  if(d >= 1000 & d < 10000) {y[1] <- 0; y[length(y)] <- y[length(y)]+10000}
  if(d >= 10000) {y[1] <- 0; y[length(y)] <- y[length(y)]+100000}

  #-------------------------------------------------------------------------
  # weibull distribution
  if(distr == "weibull")
  {
    #get params input by user
    r <- my_env$obj[["params"]]["shape"] #shape
    theta <- my_env$obj[["params"]]["scale"] #scale
    g <- 0  #2 parameters only

    # Wh for weibull
    f1 <- function(x) {
      return((r/theta)*(((x-g)/theta)^(r-1))*exp(-1*(((x-g)/theta)^r)))
    }
    WhWeibull <- function(a,b) {
      rs <- integrate(f1,lower=a, upper=b)$value
      return(rs)
    }

    # function xf(x) for weibull
    f2 <- function(x) {
      return(x*((r/theta)*(((x-g)/theta)^(r-1))*exp(-1*(((x-g)/theta)^r))))
    }
    xfxWeibull <- function(a,b) {
      rs <- integrate(f2,lower=a, upper=b)$value
      return(rs)
    }

    # function (x^2)f(x) for weibull
    f3 <- function(x) {
      return((x^2)*((r/theta)*(((x-g)/theta)^(r-1))*exp(-1*(((x-g)/theta)^r))))
    }
    x2fxWeibull <- function(a,b) {
      rs <- integrate(f3,lower=a, upper=b)$value
      return(rs)
    }

    for(i in 1:(length(x)-1))
    {
      AWh[i] <- WhWeibull(y[i], y[i+1]) #stratum weight

      Nh[i] <- N*AWh[i]  #stratum population totals

      Wh[i] <- WhWeibull(x[i], x[i+1]) #stratum weight
      Vh[i] <- (x2fxWeibull(x[i], x[i+1])/WhWeibull(x[i], x[i+1]))-
        ((xfxWeibull(x[i], x[i+1])/WhWeibull(x[i], x[i+1]))^2)  # Stratum Var
      nume[i] <- (Wh[i]*sqrt(Vh[i]))*sqrt(ch[i])
      deno <- deno + nume[i]

      my_env$output <- data.frame("Wh" = round(AWh, digits=2),
                                  "Vh" = round(Vh, digits=2),
                                  "WhSh" = round(nume, digits=3))
    }
  }
  #-------------------------------------------------------------------------
  #3P gamma distribution from here
  if(distr == "gamma")
  {
    r <- my_env$obj[["params"]]["shape"] #shape
    rate <- my_env$obj[["params"]]["rate"] #rate param is used to calculate scale, which is used for calcs.
    theta <- 1/rate  #scale
    g <- 0

    # Wh for Gamma dist
    f7 <- function(x) {
      return((((x-g)^(r-1))/((theta^r)*gamma(r)))*exp(-1*((x-g)/theta)))
    }
    WhGamma <- function(a,b) {
      rs <- integrate(f7,lower=a, upper=b)$value
      return(rs)
    }
    # function xf(x) for Gamma
    f8 <- function(x) {
      return(x*((((x-g)^(r-1))/((theta^r)*gamma(r)))*exp(-1*((x-g)/theta))))
    }
    xfxGamma <- function(a,b) {
      rs <- integrate(f8,lower=a, upper=b)$value
      return(rs)
    }
    # function (x^2)f(x) for Gamma
    f9 <- function(x) {
      return((x^2)*((((x-g)^(r-1))/((theta^r)*gamma(r)))*exp(-1*((x-g)/theta))))
    }
    x2fxGamma <- function(a,b) {
      rs <- integrate(f9,lower=a, upper=b)$value
      return(rs)
    }

    for(i in 1:(length(x)-1))
    {
      AWh[i] <- WhGamma(y[i], y[i+1]) #stratum weight
      Nh[i] <- N*AWh[i]  #stratum population totals

      Wh[i] <- WhGamma(x[i], x[i+1]) #stratum weight
      Vh[i] <- (x2fxGamma(x[i], x[i+1])/WhGamma(x[i], x[i+1]))-
        ((xfxGamma(x[i], x[i+1])/WhGamma(x[i], x[i+1]))^2)  # Stratum Var
      nume[i] <- (Wh[i]*sqrt(Vh[i]))*sqrt(ch[i])
      deno <- deno + nume[i]

      my_env$output <- data.frame("Wh" = round(AWh, digits=2),
                                  "Vh" = round(Vh, digits=2),
                                  "WhSh" = round(nume, digits=3))
    }
  }
  #-------------------------------------------------------------------------
  #Exponential distribution from here
  if(distr == "exp")
  {
    lambda <- my_env$obj[["params"]]["rate"] #rate is lambda

    # Wh for exp dist
    f10 <- function(x) {
      return(lambda*exp(-1*lambda*x))
    }
    WhExponential <- function(a,b) {
      rs <- integrate(f10,lower=a, upper=b)$value
      return(rs)
    }
    # function xf(x) for exp
    f11 <- function(x) {
      return(x*(lambda*exp(-1*lambda*x)))
    }
    xfxExponential <- function(a,b) {
      rs <- integrate(f11,lower=a, upper=b)$value
      return(rs)
    }
    # function (x^2)f(x) for exp
    f12 <- function(x) {
      return((x^2)*(lambda*exp(-1*lambda*x)))
    }
    x2fxExponential <- function(a,b) {
      rs <- integrate(f12,lower=a, upper=b)$value
      return(rs)
    }

    for(i in 1:(length(x)-1))
    {
      AWh[i] <- WhExponential(y[i], y[i+1]) #stratum weight
      Nh[i] <- N*AWh[i]  #stratum population totals

      Wh[i] <- WhExponential(x[i], x[i+1]) #stratum weight
      Vh[i] <- (x2fxExponential(x[i], x[i+1])/WhExponential(x[i], x[i+1]))-
        ((xfxExponential(x[i], x[i+1])/WhExponential(x[i], x[i+1]))^2)  # Stratum Var
      nume[i] <- (Wh[i]*sqrt(Vh[i]))*sqrt(ch[i])
      deno <- deno + nume[i]

      my_env$output <- data.frame("Wh" = round(AWh, digits=2),
                                  "Vh" = round(Vh, digits=2),
                                  "WhSh" = round(nume, digits=3))
    }
  }
  #-------------------------------------------------------------------------
  #2P Normal distribution from here
  if(distr == "norm")
  {
    mu <- my_env$obj[["params"]]["mean"] #mu
    sig <- my_env$obj[["params"]]["sd"] #sigma
    gam <- 0

    # Wh for Normal
    f13 <- function(x) {
      return((1/(sig*sqrt(2*pi)))*exp(-0.5*(((x-mu)/sig)^2)))
      #return((1/((x-gam)*sigma*sqrt(2*pi)))*exp(-0.5*((((x-gam)-mu)/sigma)^2)))
    }
    WhNormal <- function(a,b) {
      rs <- integrate(f13,lower=a, upper=b)$value
      return(rs)
    }
    # function xf(x) for Normal
    f14 <- function(x) {
      return(x*((1/(sig*sqrt(2*pi)))*exp(-0.5*(((x-mu)/sig)^2))))
    }
    xfxNormal <- function(a,b) {
      rs <- integrate(f14,lower=a, upper=b)$value
      return(rs)
    }
    # function (x^2)f(x) for Normal
    f15 <- function(x) {
      return((x^2)*((1/(sig*sqrt(2*pi)))*exp(-0.5*(((x-mu)/sig)^2))))
    }
    x2fxNormal <- function(a,b) {
      rs <- integrate(f15,lower=a, upper=b)$value
      return(rs)
    }

    #adjust Wh since values are negatives as well
    if(d < 10) {y[1] <- y[1]-10; y[length(y)] <- y[length(y)]+10}
    if(d >= 10 & d < 100) {y[1] <- y[1]-100; y[length(y)] <- y[length(y)]+100}
    if(d >= 100 & d < 1000) {y[1] <- y[1]-1000; y[length(y)] <- y[length(y)]+1000}
    if(d >= 1000 & d < 10000) {y[1] <- y[1]-10000; y[length(y)] <- y[length(y)]+10000}
    if(d >= 10000 & d < 100000) {y[1] <- y[1]-100000; y[length(y)] <- y[length(y)]+100000}
    if(d >= 100000) {y[1] <- y[1]-100000; y[length(y)] <- y[length(y)]+100000}

    for(i in 1:(length(x)-1))
    {
      AWh[i] <- WhNormal(y[i], y[i+1]) #stratum weight
      Nh[i] <- N*AWh[i]  #stratum population totals

      Wh[i] <- WhNormal(x[i], x[i+1]) #stratum weight
      Vh[i] <- (x2fxNormal(x[i], x[i+1])/WhNormal(x[i], x[i+1]))-
        ((xfxNormal(x[i], x[i+1])/WhNormal(x[i], x[i+1]))^2)  # Stratum Var
      nume[i] <- (Wh[i]*sqrt(Vh[i]))*sqrt(ch[i])
      deno <- deno + nume[i]

      my_env$output <- data.frame("Wh" = round(AWh, digits=2),
                                  "Vh" = round(Vh, digits=2),
                                  "WhSh" = round(nume, digits=3))
    }
  }
  #-------------------------------------------------------------------------
  #3P Lognormal distribution from here
  if(distr == "lnorm")
  {
    mu <- my_env$obj[["params"]]["meanlog"] #meanlog
    sig <- my_env$obj[["params"]]["sdlog"] #sdlog
    gam <- 0 #for 2P lnorm distr

    # Wh for Lognormal
    f4 <- function(x) {
      return((1/((x-gam)*sig*sqrt(2*pi)))*exp(-0.5*(((log(x-gam)-mu)/sig)^2)))
    }
    WhLognormal <- function(a,b) {
      rs <- integrate(f4,lower=a, upper=b)$value
      return(rs)
    }
    # function xf(x) for Lognormal
    f5 <- function(x) {
      return(x*((1/((x-gam)*sig*sqrt(2*pi)))*exp(-0.5*(((log(x-gam)-mu)/sig)^2))))
    }
    xfxLognormal <- function(a,b) {
      rs <- integrate(f5,lower=a, upper=b)$value
      return(rs)
    }
    # function (x^2)f(x) for Lognormal
    f6 <- function(x) {
      return((x^2)*((1/((x-gam)*sig*sqrt(2*pi)))*exp(-0.5*(((log(x-gam)-mu)/sig)^2))))
    }
    x2fxLognormal <- function(a,b) {
      rs <- integrate(f6,lower=a, upper=b)$value
      return(rs)
    }

    for(i in 1:(length(x)-1))
    {
      AWh[i] <- WhLognormal(y[i], y[i+1]) #stratum weight
      Nh[i] <- N*AWh[i]  #stratum population totals

      Wh[i] <- WhLognormal(x[i], x[i+1]) #stratum weight
      Vh[i] <- (x2fxLognormal(x[i], x[i+1])/WhLognormal(x[i], x[i+1]))-
        ((xfxLognormal(x[i], x[i+1])/WhLognormal(x[i], x[i+1]))^2)  #Var
      nume[i] <- (Wh[i]*sqrt(Vh[i]))*sqrt(ch[i])
      deno <- deno + nume[i]

      my_env$output <- data.frame("Wh" = round(AWh, digits=2),
                                  "Vh" = round(Vh, digits=2),
                                  "WhSh" = round(nume, digits=3))
    }
  }
  #-------------------------------------------------------------------------
  # Cauchy distribution from here
  if(distr == "cauchy")
  {
    mu <- my_env$obj[["params"]]["location"] #location
    sig <- my_env$obj[["params"]]["scale"] #scale

    # Wh for Cauchy
    f16 <- function(x){
      return(1/((pi*sig)*(1+((x-mu)/sig)^2)))
    }
    WhCauchy <- function(a,b) {
      rs <- integrate(f16,lower=a, upper=b)$value
      return(rs)
    }
    # function xf(x) for Cauchy
    f17 <- function(x) {
      return(x*(1/((pi*sig)*(1+((x-mu)/sig)^2))))
    }
    xfxCauchy <- function(a,b){
      rs <- integrate(f17,lower=a, upper=b)$value
      return(rs)
    }
    # function (x^2)f(x) for Cauchy
    f18 <- function(x){
      return((x^2)*(1/((pi*sig)*(1+((x-mu)/sig)^2))))
    }
    x2fxCauchy <- function(a,b) {
      rs <- integrate(f18,lower=a, upper=b)$value
      return(rs)
    }

    #adjust Wh since values are negatives as well
    if(d < 10) {y[1] <- y[1]-10000; y[length(y)] <- y[length(y)]+10000}
    if(d >= 10 & d < 100) {y[1] <- y[1]-10000; y[length(y)] <- y[length(y)]+10000}
    if(d >= 100 & d < 1000) {y[1] <- y[1]-100000; y[length(y)] <- y[length(y)]+100000}
    if(d >= 1000 & d < 10000) {y[1] <- y[1]-100000; y[length(y)] <- y[length(y)]+100000}
    if(d >= 10000 & d < 100000) {y[1] <- y[1]-1000000; y[length(y)] <- y[length(y)]+1000000}
    if(d >= 100000) {y[1] <- y[1]-1000000; y[length(y)] <- y[length(y)]+1000000}

    for(i in 1:(length(x)-1))
    {
      AWh[i] <- WhCauchy(y[i], y[i+1]) #adj stratum weight
      Nh[i] <- N*AWh[i]

      Wh[i] <- WhCauchy(x[i], x[i+1]) #stratum weight
      Vh[i] <- (x2fxCauchy(x[i], x[i+1])/Wh[i])-
        ((xfxCauchy(x[i], x[i+1])/Wh[i])^2)  #Var
      nume[i] <- (Wh[i]*sqrt(Vh[i]))*sqrt(ch[i])
      deno <- deno + nume[i]

      my_env$output <- data.frame("Wh" = round(AWh, digits=2),
                                  "Vh" = round(Vh, digits=2),
                                  "WhSh" = round(nume, digits=3))
    }
  }
  #-------------------------------------------------------------------------
  # Uniform distribution from here
  if(distr == "unif")
  {
    minn <- my_env$obj[["params"]]["min"] #min
    maxx <- my_env$obj[["params"]]["max"] #max

    # Wh for Uniform
    f19 <- function(x){
      return(1/(maxx-minn))
    }

    f19 <- Vectorize(f19, "x")

    WhUniform <- function(a,b){
      rs <- integrate(f19,lower=a, upper=b)$value
      return(rs)
    }
    # function xf(x) for Uniform
    f20 <- function(x){
      return(x*(1/(maxx-minn)))
    }
    xfxUniform <- function(a,b){
      rs <- integrate(f20,lower=a, upper=b)$value
      return(rs)
    }
    # function (x^2)f(x) for Uniform
    f21 <- function(x){
      return((x^2)*(1/(maxx-minn)))
    }
    x2fxUniform <- function(a,b){
      rs <- integrate(f21,lower=a, upper=b)$value
      return(rs)
    }

    for(i in 1:(length(x)-1))
    {
      Wh[i] <- WhUniform(x[i], x[i+1]) #stratum weight
      Nh[i] <- N*Wh[i]
      Vh[i] <- (x2fxUniform(x[i], x[i+1])/Wh[i])-
        ((xfxUniform(x[i], x[i+1])/Wh[i])^2)  #Var
      nume[i] <- (Wh[i]*sqrt(Vh[i]))*sqrt(ch[i])
      deno <- deno + nume[i]

      my_env$output <- data.frame("Wh" = round(AWh, digits=2),
                                  "Vh" = round(Vh, digits=2),
                                  "WhSh" = round(nume, digits=3))
    }
  }
  #-------------------------------------------------------------------------
  #Triangle distribution piecewise fxn1 1 & 2
  if(distr == "triangle")
  {
    a <- my_env$obj[["params"]]["min"]  #lower limit
    b <- my_env$obj[["params"]]["max"]  #upper limit
    m <- my_env$obj[["params"]]["mode"] #mode

    # Wh for Triangle1
    f25 <- function(x){
      return((2*(x-a))/((b-a)*(m-a)))
    }
    WhTriangle1 <- function(a,b){
      rs <- integrate(f25,lower=a, upper=b)$value
      return(rs)
    }
    # function xf(x) for Triangle1
    f26 <- function(x){
      return(x*((2*(x-a))/((b-a)*(m-a))))
    }
    xfxTriangle1 <- function(a,b){
      rs <- integrate(f26,lower=a, upper=b)$value
      return(rs)
    }
    # function (x^2)f(x) for Triangle1
    f27 <- function(x){
      return((x^2)*((2*(x-a))/((b-a)*(m-a))))
    }
    x2fxTriangle1 <- function(a,b){
      rs <- integrate(f27,lower=a, upper=b)$value
      return(rs)
    }
    # Wh for Triangle2
    f28 <- function(x){
      return((2*(b-x))/((b-a)*(b-m)))
    }
    WhTriangle2 <- function(a,b){
      rs <- integrate(f28,lower=a, upper=b)$value
      return(rs)
    }
    # function xf(x) for Triangle2
    f29 <- function(x){
      return(x*((2*(b-x))/((b-a)*(b-m))))
    }
    xfxTriangle2 <- function(a,b){
      rs <- integrate(f29,lower=a, upper=b)$value
      return(rs)
    }
    # function (x^2)f(x) for Triangle2
    f30 <- function(x){
      return((x^2)*((2*(b-x))/((b-a)*(b-m))))
    }
    x2fxTriangle2 <- function(a,b){
      rs <- integrate(f30,lower=a, upper=b)$value
      return(rs)
    }

    y[1] <- a; y[length(y)] <- b

    for(i in 1:(length(x)-1))
    {
      if(x[i+1] <= m) #when osb < m, use WhTriangle1()
      {
        AWh[i] <- WhTriangle1(y[i], y[i+1]) #stratum weight
        Nh[i] <- N*AWh[i]

        Wh[i] <- WhTriangle1(x[i], x[i+1]) #stratum weight
        Vh[i] <- (x2fxTriangle1(x[i], x[i+1])/Wh[i])-
          ((xfxTriangle1(x[i], x[i+1])/Wh[i])^2)  #Var
        nume[i] <- (Wh[i]*sqrt(Vh[i]))*sqrt(ch[i])
        deno <- deno + nume[i]
      }
      else # osb > m, i.e., x[i+1] > m
      {
        if(x[i] <= m) #if previous osb < m, if < m then use WhT1 & Wh2
        {
          AWh[i] <- WhTriangle1(y[i], m) + WhTriangle2(m, y[i+1]) #stratum weight
          Nh[i] <- N*AWh[i]

          Wh[i] <- WhTriangle1(x[i], m) + WhTriangle2(m, x[i+1]) #stratum weight
          Vh[i] <- ((x2fxTriangle1(x[i], m)+x2fxTriangle2(m, x[i+1]))/Wh[i])-
            (((xfxTriangle1(x[i], m)+xfxTriangle2(m, x[i+1]))/Wh[i])^2)  #Var
          nume[i] <- (Wh[i]*sqrt(Vh[i]))*sqrt(ch[i])
          deno <- deno + nume[i]
        }
        else #if > m, use WhT2 only
        {
          AWh[i] <- WhTriangle2(y[i], y[i+1])
          Nh[i] <- N*AWh[i]

          Wh[i] <- WhTriangle2(x[i], x[i+1])
          Vh[i] <- (x2fxTriangle2(x[i], x[i+1])/Wh[i])-
            ((xfxTriangle2(x[i], x[i+1])/Wh[i])^2)  #Var
          nume[i] <- (Wh[i]*sqrt(Vh[i]))*sqrt(ch[i])
          deno <- deno + nume[i]
        }
      }

      my_env$output <- data.frame("Wh" = round(AWh, digits=2),
                                  "Vh" = round(Vh, digits=2),
                                  "WhSh" = round(nume, digits=3))
    }
  }
  #-------------------------------------------------------------------------
  #Right-triangle distribution from here
  if(distr == "rtriangle")
  {
    a <- my_env$obj[["params"]]["min"] #lower limit
    b <- my_env$obj[["params"]]["max"] #upper limit

    f22 <- function(x){
      return((2*(b-x))/((b-a)^2))
    }
    WhRTriangle <- function(a,b){
      rs <- integrate(f22,lower=a, upper=b)$value
      return(rs)
    }
    # function xf(x) for Right Triangle
    f23 <- function(x){
      return(x*((2*(b-x))/((b-a)^2)))
    }
    xfxRTriangle <- function(a,b){
      rs <- integrate(f23,lower=a, upper=b)$value
      return(rs)
    }
    # function (x^2)f(x) for Right Triangle
    f24 <- function(x){
      return((x^2)*((2*(b-x))/((b-a)^2)))
    }
    x2fxRTriangle <- function(a,b){
      rs <- integrate(f24,lower=a, upper=b)$value
      return(rs)
    }

    y[1] <- a; y[length(y)] <- b

    for(i in 1:(length(x)-1))
    {
      AWh[i] <- WhRTriangle(y[i], y[i+1]) #stratum weight
      Nh[i] <- N*AWh[i]

      Wh[i] <- WhRTriangle(x[i], x[i+1])
      Vh[i] <- (x2fxRTriangle(x[i], x[i+1])/Wh[i])-
        ((xfxRTriangle(x[i], x[i+1])/Wh[i])^2)  #Var
      nume[i] <- (Wh[i]*sqrt(Vh[i]))*sqrt(ch[i])
      deno <- deno + nume[i]

      my_env$output <- data.frame("Wh" = round(AWh, digits=2),
                                  "Vh" = round(Vh, digits=2),
                                  "WhSh" = round(nume, digits=3))
    }
  }
  #-------------------------------------------------------------------------
  #Pareto distribution from here
  if(distr == "pareto")
  {
    if(!requireNamespace("actuar", quietly = TRUE)) {
      stop("Package 'actuar' needed, please install it!", call. = FALSE)}

    a <- my_env$obj[["params"]]["shape"] #shape
    s <- my_env$obj[["params"]]["scale"] #scale

    # Wh for Pareto
    f31 <- function(x){
      return((a*(s^a))/((x+s)^(a+1)))
    }
    WhPareto <- function(a,b){
      rs <- integrate(f31,lower=a, upper=b)$value
      return(rs)
    }
    # function xf(x) for Pareto
    f32 <- function(x){
      return(x*((a*(s^a))/((x+s)^(a+1))))
    }
    xfxPareto <- function(a,b){
      rs <- integrate(f32,lower=a, upper=b)$value
      return(rs)
    }
    # function (x^2)f(x) for Pareto
    f33 <- function(x){
      return((x^2)*((a*(s^a))/((x+s)^(a+1))))
    }
    x2fxPareto <- function(a,b){
      rs <- integrate(f33,lower=a, upper=b)$value
      return(rs)
    }

    for(i in 1:(length(x)-1))
    {
      AWh[i] <- WhPareto(y[i], y[i+1]) #stratum weight
      Nh[i] <- N*AWh[i]  #stratum population totals

      Wh[i] <- WhPareto(x[i], x[i+1]) #stratum weight
      Vh[i] <- (x2fxPareto(x[i], x[i+1])/Wh[i])-
        ((xfxPareto(x[i], x[i+1])/Wh[i])^2)  #Var
      nume[i] <- (Wh[i]*sqrt(Vh[i]))*sqrt(ch[i])
      deno <- deno + nume[i]

      my_env$output <- data.frame("Wh" = round(AWh, digits=2),
                                  "Vh" = round(Vh, digits=2),
                                  "WhSh" = round(nume, digits=3))
    }
  }
  #-------------------------------------------------------------------------
  #common segment for all distributions
  for(i in 1:(length(x)-1))
  {
    nh[i] <- n*nume[i]/deno #initial samples via Neyman allocation
  }

  #handle oversampling
  realloc(h, x, nh, Nh, nume, my_env)
  nh <- my_env$nh #get re-allocated samples

  #check again recursively
  for(i in 1:(length(x)-1)){
    if(nh[i] > Nh[i]){
      realloc(h, x, nh, Nh, nume, my_env)
      nh <- my_env$nh
    }
    else{ #<= case
      nh <- my_env$nh}
  }
  fh <- round((nh/Nh), digits=2) #stratum sampling fractions
  my_env$out <- data.frame("nh"=round(nh), "Nh"=round(Nh), "fh"=fh) #passed to data.res()

  #get some totals
  my_env$deno <- round(deno, digits=3)

  # Wh for all distribs adjusted except uniform
  if(distr=="unif"){
    my_env$WhTot <- round(sum(Wh), digits=2)
  }
  else {my_env$WhTot <- round(sum(AWh), digits=2)}

  my_env$NhTot <- round(sum(Nh))
  my_env$nhTot <- round(sum(nh))
  my_env$VhTot <- round(sum(Vh), digits=2)
  my_env$fhTot <- round((my_env$nhTot/my_env$NhTot), digits=2)
}
#-----------------------------------------------------------------------
