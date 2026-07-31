#' Gaussian Hypergeometric Generalized Beta Distribution
#'
#' These functions provide the ability for generating probability density values,
#' cumulative probability density values and moment about zero values for the
#' Gaussian Hypergeometric Generalized Beta distribution bounded between [0,1].
#'
#' @usage
#' dGHGBeta(p,n,a,b,c)
#'
#' @param p              vector of probabilities.
#' @param n              single value for no of binomial trials.
#' @param a              single value for shape parameter alpha representing as a.
#' @param b              single value for shape parameter beta representing as b.
#' @param c              single value for shape parameter lambda representing as c.
#'
#' @details
#' The probability density function and cumulative density function of a unit bounded
#' Gaussian Hypergeometric Generalized Beta Distribution with random variable P are given by
#'
#' \deqn{g_{P}(p)= \frac{1}{B(a,b)}\frac{2F1(-n,a;-b-n+1;1)}{2F1(-n,a;-b-n+1;c)} p^{a-1}(1-p)^{b-1} \frac{c^{b+n}}{(c+(1-c)p)^{a+b+n}} };
#' \eqn{0 \le p \le 1}
#' \deqn{G_{P}(p)= \int^p_0 \frac{1}{B(a,b)}\frac{2F1(-n,a;-b-n+1;1)}{2F1(-n,a;-b-n+1;c)} t^{a-1}(1-t)^{b-1}\frac{c^{b+n}}{(c+(1-c)t)^{a+b+n}} \,dt } ;
#' \eqn{0 \le p \le 1}
#' \deqn{a,b,c > 0}
#' \deqn{n = 1,2,3,...}
#'
#' The mean and the variance are denoted by
#' \deqn{E[P]= \int^1_0 \frac{p}{B(a,b)}\frac{2F1(-n,a;-b-n+1;1)}{2F1(-n,a;-b-n+1;c)} p^{a-1}(1-p)^{b-1}\frac{c^{b+n}}{(c+(1-c)p)^{a+b+n}} \,dp }
#' \deqn{var[P]= \int^1_0 \frac{p^2}{B(a,b)}\frac{2F1(-n,a;-b-n+1;1)}{2F1(-n,a;-b-n+1;c)} p^{a-1}(1-p)^{b-1}\frac{c^{b+n}}{(c+(1-c)p)^{a+b+n}} \,dp - (E[p])^2}
#'
#' The moments about zero is denoted as
#' \deqn{E[P^r]= \int^1_0 \frac{p^r}{B(a,b)}\frac{2F1(-n,a;-b-n+1;1)}{2F1(-n,a;-b-n+1;c)} p^{a-1}(1-p)^{b-1}\frac{c^{b+n}}{(c+(1-c)p)^{a+b+n}} \,dp}
#' \eqn{r = 1,2,3,...}
#'
#' Defined as \eqn{B(a,b)} as the beta function.
#' Defined as \eqn{2F1(a,b;c;d)} as the Gaussian Hypergeometric function.
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#' The output of \code{dGHGBeta} gives a list format consisting
#'
#' \code{pdf}           probability density values in vector form.
#'
#' \code{mean}          mean of the Gaussian Hypergeometric Generalized Beta Distribution.
#'
#' \code{var}           variance of the Gaussian Hypergeometric Generalized Beta Distribution.
#'
#' @references
#' \insertRef{rodriguez2007generalization}{fitODBOD}
#' \insertRef{pearson2009computation}{fitODBOD}
#'
#' @seealso
#' \code{\link[hypergeo]{hypergeo_powerseries}}
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(5)
#' a <- c(.1,.2,.3,1.5,2.15)
#' plot(0,0,main="Probability density graph",xlab="Random variable",ylab="Probability density values",
#' xlim = c(0,1),ylim = c(0,10))
#' for (i in 1:5)
#' {
#' lines(seq(0,1,by=0.001),dGHGBeta(seq(0,1,by=0.001),7,1+a[i],0.3,1+a[i])$pdf,col = col[i])
#' }
#'
#' dGHGBeta(seq(0,1,by=0.01),7,1.6312,0.3913,0.6659)$pdf   #extracting the pdf values
#' dGHGBeta(seq(0,1,by=0.01),7,1.6312,0.3913,0.6659)$mean  #extracting the mean
#' dGHGBeta(seq(0,1,by=0.01),7,1.6312,0.3913,0.6659)$var   #extracting the variance
#'
#' #plotting the random variables and cumulative probability values
#' col <- rainbow(6)
#' a <- c(.1,.2,.3,1.5,2.1,3)
#' plot(0,0,main="Cumulative density graph",xlab="Random variable",ylab="Cumulative density values",
#' xlim = c(0,1),ylim = c(0,1))
#' for (i in 1:6)
#' {
#' lines(seq(0.01,1,by=0.001),pGHGBeta(seq(0.01,1,by=0.001),7,1+a[i],0.3,1+a[i]),col=col[i])
#' }
#'
#' pGHGBeta(seq(0,1,by=0.01),7,1.6312,0.3913,0.6659) #acquiring the cumulative probability values
#' mazGHGBeta(1.4,7,1.6312,0.3913,0.6659)            #acquiring the moment about zero values
#'
#' #acquiring the variance for a=1.6312,b=0.3913,c=0.6659
#' mazGHGBeta(2,7,1.6312,0.3913,0.6659)-mazGHGBeta(1,7,1.6312,0.3913,0.6659)^2
#'
#' #only the integer value of moments is taken here because moments cannot be decimal
#' mazGHGBeta(1.9,15,5,6,1)
#'
#' @export
 dGHGBeta<-function(p,n,a,b,c)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(p,n,a,b,c))) | any(is.infinite(c(p,n,a,b,c))) | any(is.nan(c(p,n,a,b,c))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    #checking if shape parameters are greater than zero, if not providing an error message and
    #stopping the function progress
    if(a <= 0 | b <= 0 | c <= 0)
    {
      stop("Shape parameters cannot be less than or equal to zero")
    }
    else
    {
      #checking if binomial trial value is less than zero, if not providing an error message and
      #stopping the function progress
      if(n < 0)
      {
        stop("Binomial trial value cannot be less than zero")
      }
      else
      {
        forward1<-Re(hypergeo::hypergeo_powerseries(-n,a,-b-n+1,1))
        forward2<-Re(hypergeo::hypergeo_powerseries(-n,a,-b-n+1,c))
        #checking if the hypergeometric function value is NA(not assigned)values,
        #infinite values or NAN(not a number)values if so providing an error message and
        #stopping the function progress
        if(forward1<=0 | is.infinite(forward1) | is.nan(forward1) |
           forward2<=0 | is.infinite(forward2) | is.nan(forward2) |
           (forward1/forward2) <= 0 | is.infinite(forward1/forward2) |
           is.nan(forward1/forward2)  )
        {
          stop("Given parameter values generate error values for Gaussian Hypergeometric function")
        }
        else
        {
          #for each input values in the vector necessary calculations and conditions are applied
          if(any(p<0) | any(p>1)){
            stop("Invalid values in the input")
          }
          ans<-sapply(1:length(p),function(i) (1/beta(a,b))*(forward1/forward2)*(p[i]^(a-1))*((1-p[i])^(b-1))*
                        ((c^(b+n))/((c+(1-c)*p[i])^(a+b+n))))
        }
        # generating an output in list format consisting pdf,mean and variance
        return(list("pdf"=ans,"mean"=mazGHGBeta(1,n,a,b,c),
                    "var"=mazGHGBeta(2,n,a,b,c)-mazGHGBeta(1,n,a,b,c)^2))
      }
    }
  }
}

#' Gaussian Hypergeometric Generalized Beta Distribution
#'
#' These functions provide the ability for generating probability density values,
#' cumulative probability density values and moment about zero values for the
#' Gaussian Hypergeometric Generalized Beta distribution bounded between [0,1].
#'
#' @usage
#' pGHGBeta(p,n,a,b,c)
#'
#' @param p              vector of probabilities.
#' @param n              single value for no of binomial trials.
#' @param a              single value for shape parameter alpha representing as a.
#' @param b              single value for shape parameter beta representing as b.
#' @param c              single value for shape parameter lambda representing as c.
#'
#' @details
#' The probability density function and cumulative density function of a unit bounded
#' Gaussian Hypergeometric Generalized Beta Distribution with random variable P are given by
#'
#' \deqn{g_{P}(p)= \frac{1}{B(a,b)}\frac{2F1(-n,a;-b-n+1;1)}{2F1(-n,a;-b-n+1;c)} p^{a-1}(1-p)^{b-1} \frac{c^{b+n}}{(c+(1-c)p)^{a+b+n}} };
#' \eqn{0 \le p \le 1}
#' \deqn{G_{P}(p)= \int^p_0 \frac{1}{B(a,b)}\frac{2F1(-n,a;-b-n+1;1)}{2F1(-n,a;-b-n+1;c)} t^{a-1}(1-t)^{b-1}\frac{c^{b+n}}{(c+(1-c)t)^{a+b+n}} \,dt } ;
#' \eqn{0 \le p \le 1}
#' \deqn{a,b,c > 0}
#' \deqn{n = 1,2,3,...}
#'
#' The mean and the variance are denoted by
#' \deqn{E[P]= \int^1_0 \frac{p}{B(a,b)}\frac{2F1(-n,a;-b-n+1;1)}{2F1(-n,a;-b-n+1;c)} p^{a-1}(1-p)^{b-1}\frac{c^{b+n}}{(c+(1-c)p)^{a+b+n}} \,dp }
#' \deqn{var[P]= \int^1_0 \frac{p^2}{B(a,b)}\frac{2F1(-n,a;-b-n+1;1)}{2F1(-n,a;-b-n+1;c)} p^{a-1}(1-p)^{b-1}\frac{c^{b+n}}{(c+(1-c)p)^{a+b+n}} \,dp - (E[p])^2}
#'
#' The moments about zero is denoted as
#' \deqn{E[P^r]= \int^1_0 \frac{p^r}{B(a,b)}\frac{2F1(-n,a;-b-n+1;1)}{2F1(-n,a;-b-n+1;c)} p^{a-1}(1-p)^{b-1}\frac{c^{b+n}}{(c+(1-c)p)^{a+b+n}} \,dp}
#' \eqn{r = 1,2,3,...}
#'
#' Defined as \eqn{B(a,b)} as the beta function.
#' Defined as \eqn{2F1(a,b;c;d)} as the Gaussian Hypergeometric function.
#'
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#' The output of \code{pGHGBeta} gives the cumulative density values in vector form.
#'
#'
#' @references
#' \insertRef{rodriguez2007generalization}{fitODBOD}
#' \insertRef{pearson2009computation}{fitODBOD}
#'
#' @seealso
#' \code{\link[hypergeo]{hypergeo_powerseries}}
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(5)
#' a <- c(.1,.2,.3,1.5,2.15)
#' plot(0,0,main="Probability density graph",xlab="Random variable",ylab="Probability density values",
#' xlim = c(0,1),ylim = c(0,10))
#' for (i in 1:5)
#' {
#' lines(seq(0,1,by=0.001),dGHGBeta(seq(0,1,by=0.001),7,1+a[i],0.3,1+a[i])$pdf,col = col[i])
#' }
#'
#' dGHGBeta(seq(0,1,by=0.01),7,1.6312,0.3913,0.6659)$pdf   #extracting the pdf values
#' dGHGBeta(seq(0,1,by=0.01),7,1.6312,0.3913,0.6659)$mean  #extracting the mean
#' dGHGBeta(seq(0,1,by=0.01),7,1.6312,0.3913,0.6659)$var   #extracting the variance
#'
#' #plotting the random variables and cumulative probability values
#' col <- rainbow(6)
#' a <- c(.1,.2,.3,1.5,2.1,3)
#' plot(0,0,main="Cumulative density graph",xlab="Random variable",ylab="Cumulative density values",
#' xlim = c(0,1),ylim = c(0,1))
#' for (i in 1:6)
#' {
#' lines(seq(0.01,1,by=0.001),pGHGBeta(seq(0.01,1,by=0.001),7,1+a[i],0.3,1+a[i]),col=col[i])
#' }
#'
#' pGHGBeta(seq(0,1,by=0.01),7,1.6312,0.3913,0.6659) #acquiring the cumulative probability values
#' mazGHGBeta(1.4,7,1.6312,0.3913,0.6659)            #acquiring the moment about zero values
#'
#' #acquiring the variance for a=1.6312,b=0.3913,c=0.6659
#' mazGHGBeta(2,7,1.6312,0.3913,0.6659)-mazGHGBeta(1,7,1.6312,0.3913,0.6659)^2
#'
#' #only the integer value of moments is taken here because moments cannot be decimal
#' mazGHGBeta(1.9,15,5,6,1)
#'
#' @export
pGHGBeta<-function(p,n,a,b,c)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(p,n,a,b,c))) | any(is.infinite(c(p,n,a,b,c))) | any(is.nan(c(p,n,a,b,c))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    #checking if shape parameters are greater than zero, if not providing an error message and
    #stopping the function progress
    if(a <= 0 | b <= 0 | c <= 0)
    {
      stop("Shape parameters cannot be less than or equal to zero")
    }
    else
    {
      #checking if binomial trial value is less than zero, if not providing an error message and
      #stopping the function progress
      if(n < 0)
      {
        stop("Binomial trial value cannot be less than zero")
      }
      else
      {
        forward1<-Re(hypergeo::hypergeo_powerseries(-n,a,-b-n+1,1))
        forward2<-Re(hypergeo::hypergeo_powerseries(-n,a,-b-n+1,c))
        #checking if the hypergeometric function value is NA(not assigned)values,
        #infinite values or NAN(not a number)values if so providing an error message and
        #stopping the function progress
        if(forward1<=0 | is.infinite(forward1) | is.nan(forward1) |
           forward2<=0 | is.infinite(forward2) | is.nan(forward2) |
           (forward1/forward2) <= 0 | is.infinite(forward1/forward2) |
           is.nan(forward1/forward2))
        {
          stop("Given parameter values generate error values for Gaussian Hypergeometric Function")
        }
        else
        {
          #for each input values in the vector necessary calculations and conditions are applied
          if(any(p<0) | any(p>1)){
            stop("Invalid values in the input")
          }
          ans<-sapply(1:length(p),function(i) stats::integrate(function(p){(1/beta(a,b))*(forward1/forward2)*
              (p^(a-1))*((1-p)^(b-1))*((c^(b+n))/((c+(1-c)*p)^(a+b+n)))},
              lower=0,upper=p[i])$value)
        }
        #generating an ouput vector of cumulative probability values
        return(ans)
      }
    }
  }
}

#' Gaussian Hypergeometric Generalized Beta Distribution
#'
#' These functions provide the ability for generating probability density values,
#' cumulative probability density values and moment about zero values for the
#' Gaussian Hypergeometric Generalized Beta distribution bounded between [0,1].
#'
#' @usage
#' mazGHGBeta(r,n,a,b,c)
#'
#' @param n              single value for no of binomial trials.
#' @param a              single value for shape parameter alpha representing as a.
#' @param b              single value for shape parameter beta representing as b.
#' @param c              single value for shape parameter lambda representing as c.
#' @param r              vector of moments.
#'
#' @details
#' The probability density function and cumulative density function of a unit bounded
#' Gaussian Hypergeometric Generalized Beta Distribution with random variable P are given by
#'
#' \deqn{g_{P}(p)= \frac{1}{B(a,b)}\frac{2F1(-n,a;-b-n+1;1)}{2F1(-n,a;-b-n+1;c)} p^{a-1}(1-p)^{b-1} \frac{c^{b+n}}{(c+(1-c)p)^{a+b+n}} };
#' \eqn{0 \le p \le 1}
#' \deqn{G_{P}(p)= \int^p_0 \frac{1}{B(a,b)}\frac{2F1(-n,a;-b-n+1;1)}{2F1(-n,a;-b-n+1;c)} t^{a-1}(1-t)^{b-1}\frac{c^{b+n}}{(c+(1-c)t)^{a+b+n}} \,dt } ;
#' \eqn{0 \le p \le 1}
#' \deqn{a,b,c > 0}
#' \deqn{n = 1,2,3,...}
#'
#' The mean and the variance are denoted by
#' \deqn{E[P]= \int^1_0 \frac{p}{B(a,b)}\frac{2F1(-n,a;-b-n+1;1)}{2F1(-n,a;-b-n+1;c)} p^{a-1}(1-p)^{b-1}\frac{c^{b+n}}{(c+(1-c)p)^{a+b+n}} \,dp }
#' \deqn{var[P]= \int^1_0 \frac{p^2}{B(a,b)}\frac{2F1(-n,a;-b-n+1;1)}{2F1(-n,a;-b-n+1;c)} p^{a-1}(1-p)^{b-1}\frac{c^{b+n}}{(c+(1-c)p)^{a+b+n}} \,dp - (E[p])^2}
#'
#' The moments about zero is denoted as
#' \deqn{E[P^r]= \int^1_0 \frac{p^r}{B(a,b)}\frac{2F1(-n,a;-b-n+1;1)}{2F1(-n,a;-b-n+1;c)} p^{a-1}(1-p)^{b-1}\frac{c^{b+n}}{(c+(1-c)p)^{a+b+n}} \,dp}
#' \eqn{r = 1,2,3,...}
#'
#' Defined as \eqn{B(a,b)} as the beta function.
#' Defined as \eqn{2F1(a,b;c;d)} as the Gaussian Hypergeometric function.
#'
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#'
#' The output of \code{mazGHGBeta} give the moments about zero in vector form.
#'
#' @references
#' \insertRef{rodriguez2007generalization}{fitODBOD}
#' \insertRef{pearson2009computation}{fitODBOD}
#'
#' @seealso
#' \code{\link[hypergeo]{hypergeo_powerseries}}
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(5)
#' a <- c(.1,.2,.3,1.5,2.15)
#' plot(0,0,main="Probability density graph",xlab="Random variable",ylab="Probability density values",
#' xlim = c(0,1),ylim = c(0,10))
#' for (i in 1:5)
#' {
#' lines(seq(0,1,by=0.001),dGHGBeta(seq(0,1,by=0.001),7,1+a[i],0.3,1+a[i])$pdf,col = col[i])
#' }
#'
#' dGHGBeta(seq(0,1,by=0.01),7,1.6312,0.3913,0.6659)$pdf   #extracting the pdf values
#' dGHGBeta(seq(0,1,by=0.01),7,1.6312,0.3913,0.6659)$mean  #extracting the mean
#' dGHGBeta(seq(0,1,by=0.01),7,1.6312,0.3913,0.6659)$var   #extracting the variance
#'
#' #plotting the random variables and cumulative probability values
#' col <- rainbow(6)
#' a <- c(.1,.2,.3,1.5,2.1,3)
#' plot(0,0,main="Cumulative density graph",xlab="Random variable",ylab="Cumulative density values",
#' xlim = c(0,1),ylim = c(0,1))
#' for (i in 1:6)
#' {
#' lines(seq(0.01,1,by=0.001),pGHGBeta(seq(0.01,1,by=0.001),7,1+a[i],0.3,1+a[i]),col=col[i])
#' }
#'
#' pGHGBeta(seq(0,1,by=0.01),7,1.6312,0.3913,0.6659) #acquiring the cumulative probability values
#' mazGHGBeta(1.4,7,1.6312,0.3913,0.6659)            #acquiring the moment about zero values
#'
#' #acquiring the variance for a=1.6312,b=0.3913,c=0.6659
#' mazGHGBeta(2,7,1.6312,0.3913,0.6659)-mazGHGBeta(1,7,1.6312,0.3913,0.6659)^2
#'
#' #only the integer value of moments is taken here because moments cannot be decimal
#' mazGHGBeta(1.9,15,5,6,1)
#'
#' @export
mazGHGBeta<-function(r,n,a,b,c)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(r,n,a,b,c))) | any(is.infinite(c(r,n,a,b,c))) | any(is.nan(c(r,n,a,b,c))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    #checking if shape parameters are greater than zero, if not providing an error message and
    #stopping the function progress
    if(a <= 0 | b <= 0 | c <= 0)
    {
      stop("Shape parameters cannot be less than or equal to zero")
    }
    else
    {
      #checking if binomial trial value is less than zero, if not providing an error message and
      #stopping the function progress
      if(n < 0)
      {
        stop("Binomial trial value cannot be less than zero")
      }
      else
      {
        forward1<-Re(hypergeo::hypergeo_powerseries(-n,a,-b-n+1,1))
        forward2<-Re(hypergeo::hypergeo_powerseries(-n,a,-b-n+1,c))
        #checking if the hypergeometric function value is NA(not assigned)values,
        #infinite values or NAN(not a number)values if so providing an error message and
        #stopping the function progress
        if(forward1<=0 | is.infinite(forward1) | is.nan(forward1) |
           forward2<=0 | is.infinite(forward2) | is.nan(forward2) |
           (forward1/forward2) <= 0 | is.infinite(forward1/forward2) |
           is.nan(forward1/forward2) )
        {
          stop("Given parameter values generate error values for Gaussian Hypergeometric Function")
        }
        else
        {
          #the moments cannot be a decimal value therefore converting it into an integer
          r<-as.integer(r)
          #for each input values in the vector necessary calculations and conditions are applied
          if(any(r<=0)){
            #checking if moment values are less than or equal to zero and creating
            # an error message as well as stopping the function progress
            stop("Moments cannot be less than or equal to zero")
          }
          #integral function to calculate the moment about zero values
          ans<-sapply(1:length(r),function(i) stats::integrate(function(p,r){(p^r)*(1/beta(a,b))*(forward1/forward2)*
              (p^(a-1))*((1-p)^(b-1))*((c^(b+n))/((c+(1-c)*p)^(a+b+n)))},
              lower=0,upper=1,r=r[i])$value)
        }
      }
      #generating an ouput vector of moment about zero values
      return(ans)
    }
  }
}

#' Gaussian Hypergeometric Generalized Beta  Binomial Distribution
#'
#' These functions provide the ability for generating probability function values and
#' cumulative probability function values for the Gaussian Hypergeometric Generalized
#' Beta  Binomial distribution.
#'
#' @usage
#' dGHGBB(x,n,a,b,c)
#'
#' @param x        vector of binomial random variables.
#' @param n        single value for no of binomial trials.
#' @param a        single value for shape parameter alpha value representing a.
#' @param b        single value for shape parameter beta value representing b.
#' @param c        single value for shape parameter lambda value representing c.
#'
#' @details
#' Mixing Gaussian Hypergeometric Generalized Beta distribution with Binomial distribution will
#' create the Gaussian Hypergeometric Generalized Beta Binomial distribution.
#' The probability function and cumulative probability function can be constructed
#' and are denoted below.
#'
#' The cumulative probability function is the summation of probability function values.
#'
#' \deqn{P_{GHGBB}(x)=\frac{1}{2F1(-n,a;-b-n+1;c)} {n \choose x} \frac{B(x+a,n-x+b)}{B(a,b+n)}(c^x) }
#' \deqn{a,b,c > 0}
#' \deqn{x = 0,1,2,...n}
#' \deqn{n = 1,2,3,...}
#'
#' The mean, variance and over dispersion are denoted as
#' \deqn{E_{GHGBB}[x]= nE_{GHGBeta} }
#' \deqn{Var_{GHGBB}[x]= nE_{GHGBeta}(1-E_{GHGBeta})+ n(n-1)Var_{GHGBeta} }
#' \deqn{over dispersion= \frac{var_{GHGBeta}}{E_{GHGBeta}(1-E_{GHGBeta})} }
#'
#' Defined as \eqn{B(a,b)} is the beta function.
#' Defined as \eqn{2F1(a,b;c;d)} is the Gaussian Hypergeometric function
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions necessary error
#' messages will be provided to go further.
#'
#' @return
#' The output of \code{dGHGBB} gives a list format consisting
#'
#' \code{pdf}             probability function values in vector form.
#'
#' \code{mean}            mean of Gaussian Hypergeometric Generalized Beta Binomial Distribution.
#'
#' \code{var}             variance of Gaussian Hypergeometric Generalized Beta Binomial Distribution.
#'
#' \code{over.dis.para}   over dispersion value of Gaussian Hypergeometric Generalized Beta
#'                        Binomial Distribution.
#'
#' @references
#' \insertRef{rodriguez2007generalization}{fitODBOD}
#' \insertRef{pearson2009computation}{fitODBOD}
#'
#' @seealso
#' \code{\link[hypergeo]{hypergeo_powerseries}}
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(6)
#' a <- c(.1,.2,.3,1.5,2.1,3)
#' plot(0,0,main="GHGBB probability function graph",xlab="Binomial random variable",
#' ylab="Probability function values",xlim = c(0,7),ylim = c(0,0.9))
#' for (i in 1:6)
#' {
#' lines(0:7,dGHGBB(0:7,7,1+a[i],0.3,1+a[i])$pdf,col = col[i],lwd=2.85)
#' points(0:7,dGHGBB(0:7,7,1+a[i],0.3,1+a[i])$pdf,col = col[i],pch=16)
#' }
#'
#' dGHGBB(0:7,7,1.3,0.3,1.3)$pdf      #extracting the pdf values
#' dGHGBB(0:7,7,1.3,0.3,1.3)$mean     #extracting the mean
#' dGHGBB(0:7,7,1.3,0.3,1.3)$var      #extracting the variance
#' dGHGBB(0:7,7,1.3,0.3,1.3)$over.dis.par  #extracting the over dispersion value
#'
#' #plotting the random variables and cumulative probability values
#' col <- rainbow(4)
#' a <- c(1,2,5,10)
#' plot(0,0,main="Cumulative probability function graph",xlab="Binomial random variable",
#' ylab="Cumulative probability function values",xlim = c(0,7),ylim = c(0,1))
#' for (i in 1:4)
#' {
#' lines(0:7,pGHGBB(0:7,7,1+a[i],0.3,1+a[i]),col = col[i])
#' points(0:7,pGHGBB(0:7,7,1+a[i],0.3,1+a[i]),col = col[i])
#' }
#'
#' pGHGBB(0:7,7,1.3,0.3,1.3)     #acquiring the cumulative probability values
#'
#' @export
dGHGBB<-function(x,n,a,b,c)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,n,a,b,c))) | any(is.infinite(c(x,n,a,b,c))) | any(is.nan(c(x,n,a,b,c))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    #checking if shape parameters are less than or equal zero ,
    #if so providing an error message and stopping the function progress
    if(a <= 0 | b <= 0 |c <= 0)
    {
      stop("Shape parameters cannot be less than or equal to zero")
    }
    else
    {
      #checking if at any chance the binomial random variable is greater than binomial trial value
      #if so providing an error message and stopping the function progress
      if(max(x)>n)
      {
        stop("Binomial random variable cannot be greater than binomial trial value")
      }
      #checking if any random variable or trial value is negative if so providig an error message
      #and stopping the function progress
      else if(any(x<0) | n<0)
      {
        stop("Binomial random variable or binomial trial value cannot be negative")
      }
      else
      {
        forward<-Re(hypergeo::hypergeo_powerseries(-n,a,-b-n+1,c))
        #checking if the hypergeometric function value is NA(not assigned)values,
        #infinite values or NAN(not a number)values if so providing an error message and
        #stopping the function progress
        if(forward<=0 | is.infinite(forward) | is.nan(forward))
        {
          stop("Given parameter values generate error values for Gaussian Hypergeometric Function")
        }
        else
        {
          #for each random variable in the input vector below calculations occur
          ans<-sapply(1:length(x),function(i) (choose(n,x[i])*beta(x[i]+a,n-x[i]+b)*c^x[i])/(beta(a,b+n)*forward))
        }
        Tempsy<-dGHGBeta(0,n,a,b,c)
        # generating an output in list format consisting pdf,mean,variance and overdispersion value
        return(list("pdf"=ans,"mean"=n*Tempsy$mean ,
                    "var"=n*Tempsy$mean*(1-Tempsy$mean)+n*(n-1)*Tempsy$var,
                    "over.dis.para"=Tempsy$var/(Tempsy$mean*(1-Tempsy$mean))))
      }
    }
  }
}

#' Gaussian Hypergeometric Generalized Beta  Binomial Distribution
#'
#' These functions provide the ability for generating probability function values and
#' cumulative probability function values for the Gaussian Hypergeometric Generalized
#' Beta  Binomial distribution.
#'
#' @usage
#' pGHGBB(x,n,a,b,c)
#'
#' @param x        vector of binomial random variables.
#' @param n        single value for no of binomial trials.
#' @param a        single value for shape parameter alpha value representing a.
#' @param b        single value for shape parameter beta value representing b.
#' @param c        single value for shape parameter lambda value representing c.
#'
#' @details
#' Mixing Gaussian Hypergeometric Generalized Beta distribution with Binomial distribution will
#' create the Gaussian Hypergeometric Generalized Beta Binomial distribution.
#' The probability function and cumulative probability function can be constructed
#' and are denoted below.
#'
#' The cumulative probability function is the summation of probability function values.
#'
#' \deqn{P_{GHGBB}(x)=\frac{1}{2F1(-n,a;-b-n+1;c)}{n \choose x} \frac{B(x+a,n-x+b)}{B(a,b+n)}(c^x) }
#' \deqn{a,b,c > 0}
#' \deqn{x = 0,1,2,...n}
#' \deqn{n = 1,2,3,...}
#'
#' The mean, variance and over dispersion are denoted as
#' \deqn{E_{GHGBB}[x]= nE_{GHGBeta} }
#' \deqn{Var_{GHGBB}[x]= nE_{GHGBeta}(1-E_{GHGBeta})+ n(n-1)Var_{GHGBeta} }
#' \deqn{over dispersion= \frac{var_{GHGBeta}}{E_{GHGBeta}(1-E_{GHGBeta})} }
#'
#' Defined as \eqn{B(a,b)} is the beta function.
#' Defined as \eqn{2F1(a,b;c;d)} is the Gaussian Hypergeometric function.
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions necessary error
#' messages will be provided to go further.
#'
#' @return
#' The output of \code{pGHGBB} gives cumulative probability function values in vector form.
#'
#' @references
#' \insertRef{rodriguez2007generalization}{fitODBOD}
#' \insertRef{pearson2009computation}{fitODBOD}
#'
#' @seealso
#' \code{\link[hypergeo]{hypergeo_powerseries}}
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(6)
#' a <- c(.1,.2,.3,1.5,2.1,3)
#' plot(0,0,main="GHGBB probability function graph",xlab="Binomial random variable",
#' ylab="Probability function values",xlim = c(0,7),ylim = c(0,0.9))
#' for (i in 1:6)
#' {
#' lines(0:7,dGHGBB(0:7,7,1+a[i],0.3,1+a[i])$pdf,col = col[i],lwd=2.85)
#' points(0:7,dGHGBB(0:7,7,1+a[i],0.3,1+a[i])$pdf,col = col[i],pch=16)
#' }
#'
#' dGHGBB(0:7,7,1.3,0.3,1.3)$pdf      #extracting the pdf values
#' dGHGBB(0:7,7,1.3,0.3,1.3)$mean     #extracting the mean
#' dGHGBB(0:7,7,1.3,0.3,1.3)$var      #extracting the variance
#' dGHGBB(0:7,7,1.3,0.3,1.3)$over.dis.par  #extracting the over dispersion value
#'
#' #plotting the random variables and cumulative probability values
#' col <- rainbow(4)
#' a <- c(1,2,5,10)
#' plot(0,0,main="Cumulative probability function graph",xlab="Binomial random variable",
#' ylab="Cumulative probability function values",xlim = c(0,7),ylim = c(0,1))
#' for (i in 1:4)
#' {
#' lines(0:7,pGHGBB(0:7,7,1+a[i],0.3,1+a[i]),col = col[i])
#' points(0:7,pGHGBB(0:7,7,1+a[i],0.3,1+a[i]),col = col[i])
#' }
#'
#' pGHGBB(0:7,7,1.3,0.3,1.3)     #acquiring the cumulative probability values
#'
#' @export
pGHGBB<-function(x,n,a,b,c)
{
  #for each binomial random variable in the input vector the cumulative proability function
  #values are calculated
  ans<-sapply(1:length(x),function(i) sum(dGHGBB(0:x[i],n,a,b,c)$pdf))
  #generating an ouput vector cumulative probability function values
  return(ans)
}

#' Negative Log Likelihood value of Gaussian Hypergeometric Generalized Beta Binomial Distribution
#'
#' This function will calculate the negative log likelihood value when the vector of binomial random
#' variables and vector of corresponding frequencies are given with the shape parameters a,b and c.
#'
#' @usage
#' NegLLGHGBB(x,freq,a,b,c)
#'
#' @param x                vector of binomial random variables.
#' @param freq             vector of frequencies.
#' @param a                single value for shape parameter alpha representing a.
#' @param b                single value for shape parameter beta representing b.
#' @param c                single value for shape parameter lambda representing c.
#'
#' @details
#' \deqn{0 < a,b,c }
#' \deqn{freq \ge 0}
#' \deqn{x = 0,1,2,...}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions necessary
#' error messages will be provided to go further.
#'
#' @return
#' The output of \code{NegLLGHGBB} will produce a single numeric value.
#'
#' @references
#' \insertRef{rodriguez2007generalization}{fitODBOD}
#' \insertRef{pearson2009computation}{fitODBOD}
#'
#' @seealso
#' \code{\link[hypergeo]{hypergeo_powerseries}}
#'
#' @examples
#' No.D.D <- 0:7                    #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)     #assigning the corresponding frequencies
#' NegLLGHGBB(No.D.D,Obs.fre.1,.2,.3,1)     #acquiring the negative log likelihood value
#'
#' @export
NegLLGHGBB<-function(x,freq,a,b,c)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,freq,a,b,c))) | any(is.infinite(c(x,freq,a,b,c))) | any(is.nan(c(x,freq,a,b,c))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    #checking if any of the random variables of frequencies are less than zero if so
    #creating a error message as well as stopping the function progress
    if(any(c(x,freq) < 0))
    {
      stop("Binomial random variable or frequency values cannot be negative")
    }
    #checking if shape parameters are less than or equal to zero
    #if so creating an error message as well as stopping the function progress
    else if(a <= 0 | b <= 0 | c<=0)
    {
      stop("Shape parameters cannot be less than or equal to zero")
    }
    else
    {
      #constructing the data set using the random variables vector and frequency vector
      n<-max(x)
      data<-rep(x,freq)
      forward<-Re(hypergeo::hypergeo_powerseries(-n,a,-b-n+1,c))
      #checking if the hypergeometric function value is NA(not assigned)values,
      #infinite values or NAN(not a number)values if so providing an error message and
      #stopping the function progress
      if(forward<=0 | is.infinite(forward) | is.nan(forward))
      {
        stop("Given parameter values generate error values for Gaussian Hypergeometric Function")
      }
      else
      {
        #calculating the negative log likelihood value and representing as a single output value
        return(-(sum(log(choose(n,data[1:sum(freq)]))+
                       log(beta(data[1:sum(freq)]+a,n-data[1:sum(freq)]+b))+data[1:sum(freq)]*log(c))-
                   sum(freq)*log(beta(a,b+n))-sum(freq)*log(forward)))
      }
    }
  }
}

#' Estimating the shape parameters a,b and c for Gaussian Hypergeometric Generalized Beta  Binomial
#' Distribution
#'
#' The function will estimate the shape parameters using the maximum log likelihood method  for
#' the Gaussian Hypergeometric Generalized Beta  Binomial distribution when the binomial random
#' variables and corresponding frequencies are given.
#'
#' @usage
#' EstMLEGHGBB(x,freq,a,b,c,...)
#'
#' @param x                vector of binomial random variables.
#' @param freq             vector of frequencies.
#' @param a                single value for shape parameter alpha representing a.
#' @param b                single value for shape parameter beta representing b.
#' @param c                single value for shape parameter lambda representing c.
#' @param ...              mle2 function inputs except data and estimating parameter.
#'
#' @details
#' \deqn{0 < a,b,c}
#' \deqn{x = 0,1,2,...}
#' \deqn{freq \ge 0}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions necessary
#' error messages will be provided to go further.
#'
#' @return
#' \code{EstMLEGHGBB} here is used as a wrapper for the \code{mle2} function of
#' \pkg{bbmle} package therefore output is of class of mle2.
#'
#' @references
#' \insertRef{rodriguez2007generalization}{fitODBOD}
#' \insertRef{pearson2009computation}{fitODBOD}
#'
#' @seealso
#' \code{\link[hypergeo]{hypergeo_powerseries}}
#'
#' ----------------
#'
#' \code{\link[bbmle]{mle2}}
#'
#' @examples
#' No.D.D <- 0:7           #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)     #assigning the corresponding frequencies
#'
#' #estimating the parameters using maximum log likelihood value and assigning it
#' parameters <- EstMLEGHGBB(No.D.D,Obs.fre.1,a=0.1,b=0.2,c=0.5)
#'
#' bbmle::coef(parameters)   #extracting the parameters
#'
#'@export
EstMLEGHGBB<-function(x,freq,a,b,c,...)
{
  suppressWarnings2 <-function(expr, regex=character())
  {
    withCallingHandlers(expr, warning=function(w)
    {
      if (length(regex) == 1 && length(grep(regex, conditionMessage(w))))
      {
        invokeRestart("muffleWarning")
      }
    }                  )
  }
  output<-suppressWarnings2(bbmle::mle2(.EstMLEGHGBB,data=list(x=x,freq=freq),
                                        start = list(a=a,b=b,c=c),...),"NaN")
  return(output)
}


.EstMLEGHGBB<-function(x,freq,a,b,c)
{
  #with respective to using bbmle package function mle2 there is no need impose any restrictions
  #therefor the output is directly a single numeric value for the negative log likelihood value of
  #Gaussian Hypergeometric Generalized Beta binomial distribution
  n<-max(x)
  data<-rep(x,freq)
  return(-(sum(log(choose(n,data[1:sum(freq)]))+
                 log(beta(data[1:sum(freq)]+a,n-data[1:sum(freq)]+b))+
                 data[1:sum(freq)]*log(c))-sum(freq)*log(beta(a,b+n))-
             sum(freq)*log(Re(hypergeo::hypergeo_powerseries(-n,a,-b-n+1,c)))))
}

#' Fitting the Gaussian Hypergeometric Generalized Beta  Binomial Distribution when binomial
#' random variable, frequency and shape parameters a,b and c are given
#'
#' The function will fit the Gaussian Hypergeometric Generalized Beta Binomial Distribution
#' when random variables, corresponding frequencies and shape parameters are given. It will provide
#' the expected frequencies, chi-squared test statistics value, p value, degree of freedom
#' and over dispersion value so that it can be seen if this distribution fits the data.
#'
#' @usage fitGHGBB(x,obs.freq,a,b,c)
#'
#' @param x                  vector of binomial random variables.
#' @param obs.freq           vector of frequencies.
#' @param a                  single value for shape parameter alpha representing a.
#' @param b                  single value for shape parameter beta representing b.
#' @param c                  single value for shape parameter lambda representing c.
#'
#' @details
#' \deqn{0 < a,b,c}
#' \deqn{x = 0,1,2,...}
#' \deqn{obs.freq \ge 0}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions necessary
#' error messages will be provided to go further.
#'
#' @return
#' The output of \code{fitGHGBB} gives the class format \code{fitGB} and \code{fit} consisting a list
#'
#' \code{bin.ran.var} binomial random variables.
#'
#' \code{obs.freq} corresponding observed frequencies.
#'
#' \code{exp.freq} corresponding expected frequencies.
#'
#' \code{statistic} chi-squared test statistics.
#'
#' \code{df} degree of freedom.
#'
#' \code{p.value} probability value by chi-squared test statistic.
#'
#' \code{fitGB} fitted values of \code{dGHGBB}.
#'
#' \code{NegLL} Negative Loglikelihood value.
#'
#' \code{a} estimated value for alpha parameter as a.
#'
#' \code{b} estimated value for beta parameter as b.
#'
#' \code{c} estimated value for gamma parameter as c.
#'
#' \code{AIC} AIC value.
#'
#' \code{over.dis.para} over dispersion value.
#'
#' \code{call} the inputs of the function.
#'
#' Methods \code{summary}, \code{print}, \code{AIC}, \code{residuals} and \code{fitted} can be used
#' to extract specific outputs.
#'
#' @references
#' \insertRef{rodriguez2007generalization}{fitODBOD}
#' \insertRef{pearson2009computation}{fitODBOD}
#'
#' @seealso
#' \code{\link[hypergeo]{hypergeo_powerseries}}
#'
#' --------------------
#'
#' \code{\link[bbmle]{mle2}}
#'
#' @examples
#' No.D.D <- 0:7        #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)       #assigning the corresponding frequencies
#'
#' #estimating the parameters using maximum log likelihood value and assigning it
#' parameters <- EstMLEGHGBB(No.D.D,Obs.fre.1,0.1,20,1.3)
#'
#' bbmle::coef(parameters)         #extracting the parameters
#' aGHGBB <- bbmle::coef(parameters)[1]  #assigning the estimated a
#' bGHGBB <- bbmle::coef(parameters)[2]  #assigning the estimated b
#' cGHGBB <- bbmle::coef(parameters)[3]  #assigning the estimated c
#'
#' #fitting when the random variable,frequencies,shape parameter values are given.
#' results <- fitGHGBB(No.D.D,Obs.fre.1,aGHGBB,bGHGBB,cGHGBB)
#' results
#'
#' #extracting the expected frequencies
#' fitted(results)
#'
#' #extracting the residuals
#' residuals(results)
#'
#' @export
fitGHGBB<-function(x,obs.freq,a,b,c)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,obs.freq,a,b,c))) | any(is.infinite(c(x,obs.freq,a,b,c))) |
     any(is.nan(c(x,obs.freq,a,b,c))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    est<-dGHGBB(x,max(x),a,b,c)
    odp<-est$over.dis.para; names(odp)<-NULL
    #for given random variables and parameters calculating the estimated probability values
    est.prob<-est$pdf
    #using the estimated probability values the expected frequencies are calculated
    exp.freq<-round((sum(obs.freq)*est.prob),2)
    #chi-squared test statistics is calculated with observed frequency and expected frequency
    statistic<-sum(((obs.freq-exp.freq)^2)/exp.freq)
    #degree of freedom is calculated
    df<-length(x)-4
    #p value of chi-squared test statistic is calculated
    p.value<-1-stats::pchisq(statistic,df)
    #all the above information is mentioned as a message below
    #and if the user wishes they can print or not to

      #checking if df is less than or equal to zero
    if(df<0 | df==0)
    {
      stop("Degrees of freedom cannot be less than or equal to zero")
    }
    #checking if any of the expected frequencies are less than five and greater than zero, if so
    #a warning message is provided in interpreting the results
    if(min(exp.freq)<5 && min(exp.freq) > 0)
    {
      message("Chi-squared approximation may be doubtful because expected frequency is less than 5")
    }
    #checking if expected frequency is zero, if so providing a warning message in interpreting
    #the results
    if(min(exp.freq)==0)
    {
      message("Chi-squared approximation is not suitable because expected frequency approximates to zero")
    }
    NegLL<-NegLLGHGBB(x,obs.freq,a,b,c)
    names(NegLL)<-NULL
    #the final output is in a list format containing the calculated values
    final<-list("bin.ran.var"=x,"obs.freq"=obs.freq,"exp.freq"=exp.freq,
                "statistic"=round(statistic,4),"df"=df,"p.value"=round(p.value,4),
                "fitGB"=est, "NegLL"=NegLL, "a"=a, "b"=b, "c"=c,
                "AIC"=2*3+2*NegLL,"over.dis.para"=odp,"call"=match.call())
    class(final)<-c("fitGB","fit")
    return(final)
    }
  }

#' @method fitGHGBB default
#' @export
fitGHGBB.default<-function(x,obs.freq,a,b,c)
{
  return(fitGHGBB(x,obs.freq,a,b,c))
}

#' @method print fitGB
#' @export
print.fitGB<-function(x,...)
{
  cat("Call: \n")
  print(x$call)
  cat("\nChi-squared test for Gaussian Hypergeometric Generalized Beta-Binomial Distribution \n\t
      Observed Frequency : ",x$obs.freq,"\n\t
      expected Frequency : ",x$exp.freq,"\n\t
      estimated a parameter :",x$a, "  ,estimated b parameter :",x$b,",\n\t
      estimated c parameter :",x$c,"\n\t
      X-squared :",x$statistic,"  ,df :",x$df,"  ,p-value :",x$p.value,"\n\t
      over dispersion :",x$over.dis.para,"\n")
}

#' @method summary fitGB
#' @export
summary.fitGB<-function(object,...)
{
  cat("Call: \n")
  print(object$call)
  cat("\nChi-squared test for Gaussian Hypergeometric Generalized Beta-Binomial Distribution \n\t
      Observed Frequency : ",object$obs.freq,"\n\t
      expected Frequency : ",object$exp.freq,"\n\t
      estimated a parameter :",object$a," ,estimated b parameter :",object$b,",\n\t
      estimated c parameter :",object$c,"\n\t
      X-squared :",object$statistic,"  ,df :",object$df,"  ,p-value :",object$p.value,"\n\t
      over dispersion :",object$over.dis.para,"\n\t
      Negative Loglikehood value :",object$NegLL,"\n\t
      AIC value :",object$AIC,"\n")
}
