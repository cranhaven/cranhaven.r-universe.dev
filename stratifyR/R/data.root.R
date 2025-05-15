#' To calculate the objective function values
#'
#' This function is called within other important functions in the stratifyR
#' package to calculate the objective function values at systematic incremental
#' progressions of stratum width and range of the data
#'
#' @param y A numeric: stratum width
#' @param d A numeric: distance or range of data    
#' @param c A numeric: stratum cost                                                                 
#' @param my_env The environment my_env contains the constants and outputs
#' from various calculations carried out by other key functions
#'
#' @import zipfR
#'
#' @return \code{} returns the value of the objective function
#'
#' @author Karuna Reddy <karuna.reddy@usp.ac.fj>\cr MGM Khan <khan_mg@usp.ac.fj>
#'
data.root <- function(d, y, c, my_env)
{
  #access these from my_env
  initval <- my_env$initval #this is for scaled data
  
  ch <- my_env$ch #a vector of stratum sample costs
  
  #distr <- my_env$distr
  distr <- my_env$obj["distr"] #extract distr from list given by GetDist() in StratifyData()
  #params <- my_env$obj["params"] #extract the params type from list above

  #if(!requireNamespace("zipfR", quietly = TRUE)) {
  #  stop("Package 'zipfR' needed, please install it!", call. = FALSE)}

  #Now deal with all the distributions
  #--------------------------------------------------------------------------------
  if(distr == "weibull") ##ALREADY SET!
    {
    r <- my_env$obj[["params"]]["shape"] #shape
    t <- my_env$obj[["params"]]["scale"] #scale
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
  #--------------------------------------------------------------------------------
  if(distr == "gamma") #ALREADY SET!
  {
    r <- my_env$obj[["params"]]["shape"] #shape
    f <- my_env$obj[["params"]]["rate"] #scale is calculated from rate param
    t <- 1/f  #scale
    g <- 0

    A <- (t^2)*r*(r+1)*(zipfR::Rgamma(r,(d-y+initval-g)/t)-
                  zipfR::Rgamma(r,(d+initval-g)/t))
    B <- zipfR::Rgamma((r+2),(d-y+initval-g)/t)-
                  zipfR::Rgamma((r+2),(d+initval-g)/t)
    C <- t*r*(zipfR::Rgamma((r+1),(d-y+initval-g)/t)-
                  zipfR::Rgamma((r+1),(d+initval-g)/t))

    calc <- (A*B-(C^2))*(c)
  }
  #------------------------------------------------------------------------------------------
  if(distr == "exp")
    {
    lambda <- my_env$obj[["params"]]["rate"] #rate is lambda

    A <- exp(-1*lambda*(d-y+initval))
    B <- (1/(lambda^2))*((1-exp(-1*lambda*y))^2)
    C <- (y^2)*exp(-1*lambda*y)

    calc <- ((A^2)*(B-C))*(c)
    }
  #-------------------------------------------------------------------------------
  if(distr == "norm")
    {
    mu <- my_env$obj[["params"]]["mean"] #mu
    sigma <- my_env$obj[["params"]]["sd"] #sigma

    A <- erf((d+initval-mu)/(sigma*sqrt(2)))-erf((d-y+initval-mu)/(sigma*sqrt(2)))
    B <- ((d-y+initval-mu)/sigma)*exp(-1*(((d-y+initval-mu)/(sigma*sqrt(2)))^2)) -
      ((d+initval-mu)/sigma)*exp(-1*(((d+initval-mu)/(sigma*sqrt(2)))^2))
    C <- exp(-1*(((d-y+initval-mu)/(sigma*sqrt(2)))^2)) -
      exp(-1*(((d+initval-mu)/(sigma*sqrt(2)))^2))

    calc <- (((sigma^2)/(2*sqrt(2*pi)))*A*B + 0.25*(sigma^2)*(A^2) -
       ((sigma^2)/(2*pi))*(C^2))*(c)
  }
  #--------------------------------------------------------------------------------
  if(distr == "lnorm")
  {
    mu <- my_env$obj[["params"]]["meanlog"] #meanlog
    sigma <- my_env$obj[["params"]]["sdlog"] #sdlog

    A = (erf((log(d+initval)-mu-2*(sigma^2))/(sigma*sqrt(2))) -
           erf((log(d-y+initval)-mu-2*(sigma^2))/(sigma*sqrt(2))))
    B = (erf((log(d+initval)-mu)/(sigma*sqrt(2))) -
           erf((log(d-y+initval)-mu)/(sigma*sqrt(2))))
    C = (erf((log(d+initval)-mu-(sigma^2))/(sigma*sqrt(2))) -
           erf((log(d-y+initval)-mu-(sigma^2))/(sigma*sqrt(2))))

    calc = (0.25*exp(2*mu+2*(sigma^2))*A*B - 0.25*exp(2*mu + 
            (sigma^2))*(C^2))*(c)
  }
  #-------------------------------------------------------------------------------
  if(distr == "cauchy") #for standard cauchy scale=1, location=0
  {
    #these params are not used!
    mu <- my_env$obj[["params"]]["location"] #location
    sig <- my_env$obj[["params"]]["scale"] #scale

    #non-standard cauchy
    xh = (d+initval)
    xh1 = (d-y+initval) #xh-1

    wh = (1/pi)*(atan((xh1+y-mu)/sig) - atan((xh1-mu)/sig))

    muh = (1/(2*(atan((xh1+y-mu)/sig) -
                    atan((xh1-mu)/sig))))*(sig*log((xh1+y-mu)^2+sig^2) +
                    2*mu*atan((xh1+y-mu)/sig) - sig*log((xh1-mu)^2+sig^2) -
                    2*mu*atan((xh1-mu)/sig))

    sig2h = (1/(atan((xh1+y-mu)/sig) -
                   atan((xh1-mu)/sig)))*(mu*sig*log((xh1+y-mu)^2+sig^2) +
                   (mu^2-sig^2)*atan((xh1+y-mu)/sig) + sig*(xh1+y) -
                   mu*sig*log((xh1-mu)^2+sig^2) -
                   (mu^2-sig^2)*atan((xh1-mu)/sig) - sig*xh1) - muh^2

   calc = ((1/(pi^2))*(atan((xh1+y-mu)/sig) -
          atan((xh1-mu)/sig))*(mu*sig*log((xh1+y-mu)^2+sig^2) +
          (mu^2-sig^2)*atan((xh1+y-mu)/sig) +
          sig*(xh1+y) - mu*sig*log((xh1-mu)^2+sig^2) -
          (mu^2-sig^2)*atan((xh1-mu)/sig) - sig*xh1) -
          ((1/(4*pi^2))*((sig*log((xh1+y-mu)^2+sig^2) +
          2*mu*atan((xh1+y-mu)/sig) -
          sig*log((xh1-mu)^2+sig^2) -
          2*mu*atan((xh1-mu)/sig)))^2))*(c)
    }
  #------------------------------------------------------------------------------
  if(distr == "triangle") #this is when the range is b-a with mode c also given
  {
    a <- my_env$obj[["params"]]["min"]  #location (min)
    b <- my_env$obj[["params"]]["max"]  #scale (max)
    c <- my_env$obj[["params"]]["mode"] #shape (mode)

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

    #for the two piece-wise functions
    if(d <= c)
    {
      calc = ((wh1^2)*sig2h1)*(c)
    }

    if(d > c)
    {
      calc = ((wh2^2)*sig2h2)*(c)
    }
  }
  #-----------------------------------------------------------------------------
  if(distr == "rtriangle")
  {
    #if min=node, Triangle distr == RTriangle distr
    a <- my_env$obj[["params"]]["min"] #location
    b <- my_env$obj[["params"]]["max"] #scale

    eps = 1e-10 #if initial value or min param are 0, add a small epsilon
    if(initval==0)
    {
      initval <- initval+eps
    }
    else{initval <- initval}

    xh = d+initval
    xh1 = d-y+initval #xh-1

    #as per Khan et al paper
    wh = (y*(2*(b-xh1)-y))/((b-a)^2)
    muh = (3*b*(y+2*xh1) - 2*(y^2+3*y*xh1) + 3*(xh1^2))/(3*(2*(b-xh1)-y))
    sig2h = ((y^2)*(y^2-6*(b-xh1)*y+6*((b-xh1)^2)))/(18*((2*(b-xh1)-y)^2))

    calc = ((wh^2)*(sig2h))*(c)
  }
  #-------------------------------------------------------------------------------
  if(distr == "unif")
  {
     minn <- my_env$obj[["params"]]["min"] #location
     maxx <- my_env$obj[["params"]]["max"] #scale

     xh = d+initval
     xh1 = d-y+initval #xh-1

     wh = y/(maxx-minn)
     muh = (y+2*xh1)/2
     sig2h = (y^2)/12

     calc = ((wh^2)*(sig2h))*(c)
  }
  #------------------------------------------------------------------------------
  if(distr == "pareto") #pareto type II
  {
     a <- my_env$obj[["params"]]["shape"] #shape - alpha
     s <- my_env$obj[["params"]]["scale"] #scale - beta

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
  #-------------------------------------------------------------------------------
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
##################################################################################