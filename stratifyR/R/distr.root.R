#' Calculate the objective function values
#'
#' This function is called within other important functions in the package
#' to calculate the objective function values at systematic incremental
#' progressions of stratum width and range of the data
#'
#' @param y A numeric: stratum width
#' @param d A numeric: distance or range of data
#' @param c A numeric: stratum cost
#' @param my_env My environment my_env contains the constants and outputs
#' from various calculations carried out by other key functions
#'
#' @import zipfR
#'
#' @return \code{} returns the value of the objective function
#'
#' @author Karuna Reddy <karuna.reddy@usp.ac.fj>\cr
#' MGM Khan <khan_mg@usp.ac.fj>
#'
distr.root <- function(d, y, c, my_env)
{
  #access these scaled quantities from my_env
  initval <- my_env$initval # min val of scaled data given in strata.distr()
  maxval <- my_env$maxval #max val of the original distr giiven by user

  distr <- my_env$obj["distr"] #extract distr given by user

  #if(!requireNamespace("zipfR", quietly = TRUE)) {
    #stop("Package 'zipfR' needed, please install it!", call. = FALSE)}

  calc <- 0

  #Now deal with all the distributions
  #-----------------------------------------------------------------------
  if(distr == "weibull") ##ALREADY SET!
    {
    r <- my_env$obj[["params"]]["shape"] #shape param doesn't change for scaled data
    t <- my_env$obj[["params"]]["scale"]/maxval #scale param changes for scaled data - div by maxval
    g <- 0  #2 parameters only

    g1r <- zipfR::Cgamma(1+1/r)
    g2r <- zipfR::Cgamma(1+2/r)

    A <- (t^2)*g2r*(exp(-1*((d-y+initval-g)/t)^r) - exp(-1*((d+initval-g)/t)^r))
    B <- zipfR::Rgamma(((2/r)+1), ((d-y+initval-g)/t)^r, .Machine$double.xmin) -
                zipfR::Rgamma(((2/r)+1), ((d+initval-g)/t)^r, .Machine$double.xmin)
    C <- t*g1r*(zipfR::Rgamma(((1/r)+1), ((d-y+initval-g)/t)^r, .Machine$double.xmin) -
                zipfR::Rgamma(((1/r)+1), ((d+initval-g)/t)^r, .Machine$double.xmin))

    calc <- (A*B-(C^2))*(c)
    }
  #-----------------------------------------------------------------------
  if(distr == "gamma") #ALREADY SET!
  {
    r <- my_env$obj[["params"]]["shape"] #shape remains same in scaled data
    f <- my_env$obj[["params"]]["rate"]*maxval #rate param chjanges for scaled data is * with maxval
    t <- 1/f  #scale
    g <- 0

    A <- (t^2)*r*(r+1)*(zipfR::Rgamma(r,(d-y+initval-g)/t)-zipfR::Rgamma(r,(d+initval-g)/t))
    B <- zipfR::Rgamma((r+2),(d-y+initval-g)/t)-zipfR::Rgamma((r+2),(d+initval-g)/t)
    C <- t*r*(zipfR::Rgamma((r+1),(d-y+initval-g)/t)-zipfR::Rgamma((r+1),(d+initval-g)/t))

    calc <- (A*B-(C^2))*(c)
  }
  #-----------------------------------------------------------------------
  if(distr == "exp")
    {
    lambda <- my_env$obj[["params"]]["rate"]*maxval #rate is lambda, changes in scaled data

    A <- exp(-1*lambda*(d-y+initval))
    B <- (1/(lambda^2))*((1-exp(-1*lambda*y))^2)
    C <- (y^2)*exp(-1*lambda*y)

    calc <- ((A^2)*(B-C))*c
    }
  #-----------------------------------------------------------------------
  if(distr == "norm")
    {
    mu <- my_env$obj[["params"]]["mean"]/maxval #mu changes
    sigma <- my_env$obj[["params"]]["sd"]/maxval #sigma changes
    gam <- 0

    A <- erf((d+initval-mu)/(sigma*sqrt(2)))-erf((d-y+initval-mu)/(sigma*sqrt(2)))
    B <- ((d-y+initval-mu)/sigma)*exp(-1*(((d-y+initval-mu)/(sigma*sqrt(2)))^2)) -
      ((d+initval-mu)/sigma)*exp(-1*(((d+initval-mu)/(sigma*sqrt(2)))^2))
    C <- exp(-1*(((d-y+initval-mu)/(sigma*sqrt(2)))^2)) - exp(-1*(((d+initval-mu)/(sigma*sqrt(2)))^2))

    calc <- (((sigma^2)/(2*sqrt(2*pi)))*A*B + 0.25*(sigma^2)*(A^2) - ((sigma^2)/(2*pi))*(C^2))*(c)
  }
  #-----------------------------------------------------------------------
  if(distr == "lnorm")
  {
    mu <- my_env$obj[["params"]]["meanlog"]-log(maxval) #meanlog changes
    sigma <- my_env$obj[["params"]]["sdlog"] #sdlog doesn't change
    gam <- 0 #for 2P lnorm distr

    A = (erf((log(d+initval-gam)-mu-2*(sigma^2))/(sigma*sqrt(2))) - erf((log(d-y+initval-gam)-mu-2*(sigma^2))/(sigma*sqrt(2))))
    B = (erf((log(d+initval-gam)-mu)/(sigma*sqrt(2))) - erf((log(d-y+initval-gam)-mu)/(sigma*sqrt(2))))
    C = (erf((log(d+initval-gam)-mu-(sigma^2))/(sigma*sqrt(2))) - erf((log(d-y+initval-gam)-mu-(sigma^2))/(sigma*sqrt(2))))

    calc = (0.25*exp(2*mu+2*(sigma^2))*A*B - 0.25*exp(2*mu + (sigma^2))*(C^2))*(c)
  }
  #-----------------------------------------------------------------------
  if(distr == "unif")
  {
    minn <- my_env$obj[["params"]]["min"]/maxval #location change
    maxx <- my_env$obj[["params"]]["max"]/maxval #scale changes

    xh = d+initval
    xh1 = d-y+initval #xh-1

    wh = y/(maxx-minn)
    muh = (y+2*xh1)/2
    sig2h = (y^2)/12

    calc = ((wh^2)*(sig2h))*(c)
  }
  #-----------------------------------------------------------------------
  if(distr == "cauchy") #for standard cauchy scale=1, location=0
  {
    #these params are not used!
    l <- my_env$obj[["params"]]["location"]/maxval #location
    s <- my_env$obj[["params"]]["scale"]/maxval #scale

    #non-standard cauchy
    xh = d+initval
    xh1 = d-y+initval #xh-1

    wh = (1/pi)*(atan((xh1+y-l)/s) - atan((xh1-l)/s))

    muh = (1/(2*pi*wh))*(s*log((xh1+y-l)^2+s^2) + 2*l*atan((xh1+y-l)/s)
                         - s*log((xh1-l)^2+s^2) - 2*l*atan((xh1-l)/s))
    sig2h = (1/(pi*wh))*(l*s*log((xh1+y-l)^2+s^2) + (l^2-s^2)*atan((xh1+y-l)/s)
                          + s*(xh1+y) - l*s*log((xh1-l)^2+s^2)
                          - (l^2-s^2)*atan((xh1-l)/s) - s*xh1) - muh^2

    calc = ((wh/pi)*(l*s*log((xh1+y-l)^2+s^2) + (l^2-s^2)*atan((xh1+y-l)/s) +
                  s*(xh1+y) - l*s*log((xh1-l)^2+s^2) -
                  (l^2-s^2)*atan((xh1-l)/s) - s*xh1) -
                  ((1/(4*pi^2))*((s*log((xh1+y-l)^2+s^2) + 2*l*atan((xh1+y-l)/s) -
                  s*log((xh1-l)^2+s^2) - 2*l*atan((xh1-l)/s)))^2))*(c)
  }
  #-----------------------------------------------------------------------
  if(distr == "triangle") #this is when the range is b-a with mode c also given
  {
    a <- my_env$obj[["params"]]["min"]/maxval  #location changes (min)
    b <- my_env$obj[["params"]]["max"]/maxval  #scale changes (max)
    c <- my_env$obj[["params"]]["mode"]/maxval #shape changes (mode)

    #special case: if max==mode, triangle is actually rtriangle
    if(round(b, digits=1) == round(c, digits=1))
      {
       stop("The distribution appears to be right-triangular")
      }

    eps = 1e-10 #if initial value or min param are 0, add a small epsilon
    if(initval==0)
    {
      initval <- initval+eps
    }
    else{initval <- initval}

    xh = d+initval
    xh1 = d-y+initval #xh-1

    #for first piecewise fxn (b-a<=c)
    wh1 = (y*(y+2*(xh1-a)))/((b-a)*(c-a))
    muh1 = ((2/3)*(y^2)+2*y*xh1-a*y+2*(xh1-a)*xh1)/(y+2*(xh1-a))
    sig2h1 = ((y^2)*((y^2)+6*y*(xh1-a)+6*(xh1-a)^2))/(18*(y+2*(xh1-a))^2)

    #for second piecewise fxn (b-a>c)
    wh2 = (y*(2*(b-xh1)-y))/((b-a)*(b-c))
    muh2 = (3*(b-xh1)*y-3*y*xh1+6*(b-xh1)*xh1-2*y^2)/(3*(2*(b-xh1)-y))
    sig2h2 = ((y^2)*(6*(b-xh1)^2-6*y*(b-xh1)+y^2))/((18*(2*(b-xh1)-y)^2))

    #for two piece-wise functions
    if(d <= c)
    {
     calc = ((wh1^2)*sig2h1)*(c)
    }

    if(d > c)
    {
     calc = ((wh2^2)*sig2h2)*(c)
    }
  }
  #-----------------------------------------------------------------------
  if(distr == "rtriangle")
    {
      #if min=node, Triangle distr == RTriangle distr
      a <- my_env$obj[["params"]]["min"]/maxval #location changes
      b <- my_env$obj[["params"]]["max"]/maxval #scale changes

      eps = 1e-10 #if initial value or min param are 0, add a small epsilon
      if(initval==0)
        {
        initval <- initval+eps
        }
      else{initval <- initval}

      xh = d+initval
      xh1 = d-y+initval #xh-1

      #as per Dr Khan's papers
      wh = (y*(2*(b-xh1)-y))/((b-a)^2)
      muh = (3*b*(y+2*xh1) - 2*(y^2+3*y*xh1) + 3*(xh1^2))/(3*(2*(b-xh1)-y))
      sig2h = ((y^2)*(y^2-6*(b-xh1)*y+6*((b-xh1)^2)))/(18*((2*(b-xh1)-y)^2))

      calc = ((wh^2)*(sig2h))*(c)
    }
  #-----------------------------------------------------------------------
  if(distr == "pareto") #pareto type II
  {
    a <- my_env$obj[["params"]]["shape"] #shape - alpha
    s <- my_env$obj[["params"]]["scale"]/maxval #scale - beta

    xh <- d+initval #xh-xh1 = y, thus, xh=(y+xh1)
    xh1 <- d-y+initval

    Wh <- (s/(xh1+s))^a - (s/((y+xh1)+s))^a
    A = ((y+xh1+s)^(2-a))/(2-a)
    B = (2*s*(y+xh1+s)^(1-a))/(1-a)
    C = ((s^2)*(y+xh1+s)^(-a))/a
    D = ((xh1+s)^(2-a))/(2-a)
    E = (2*s*(xh1+s)^(1-a))/(1-a)
    G = ((s^2)*(xh1+s)^(-a))/a
    H = (a*(y+xh1)+s)/((y+xh1+s)^a)
    I = (a*xh1+s)/((xh1+s)^a)

    calc = ((a*s^a)*Wh*(A-B-C-D+E+G) - ((s^(2*a))/((1-a)^2))*(H-I)^2)*(c)

    }
  #-----------------------------------------------------------------------
  if(calc < 0 || is.nan(calc) || is.na(calc))
  {
    rtval <- -1
  }
  else
  {
    rtval <- sqrt(calc)
  }
  return(rtval)
}
##########################################################################
