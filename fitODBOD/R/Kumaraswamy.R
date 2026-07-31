#' Kumaraswamy Distribution
#'
#' These functions provide the ability for generating probability density values,
#' cumulative probability density values and moment about zero values for the
#' Kumaraswamy Distribution bounded between [0,1].
#'
#' @usage
#' dKUM(p,a,b)
#'
#' @param p              vector of probabilities.
#' @param a              single value for shape parameter alpha representing as a.
#' @param b              single value for shape parameter beta representing as b.
#'
#' @details
#' The probability density function and cumulative density function of a unit
#' bounded Kumaraswamy Distribution with random variable P are given by
#'
#' \deqn{g_{P}(p)= abp^{a-1}(1-p^a)^{b-1} } ;        \eqn{0 \le p \le 1}
#' \deqn{G_{P}(p)= 1-(1-p^a)^b} ;                   \eqn{0 \le p \le 1}
#' \deqn{a,b > 0}
#'
#' The mean and the variance are denoted by
#' \deqn{E[P]= bB(1+\frac{1}{a},b)}
#' \deqn{var[P]= bB(1+\frac{2}{a},b)-(bB(1+\frac{1}{a},b))^2}
#'
#' The moments about zero is denoted as
#' \deqn{E[P^r]= bB(1+\frac{r}{a},b)}
#' \eqn{r = 1,2,3,...}
#'
#' Defined as \eqn{B(a,b)} is the beta function.
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#' The output of \code{dKUM} gives a list format consisting
#'
#' \code{pdf}             probability density values in vector form.
#'
#' \code{mean}            mean of the Kumaraswamy distribution.
#'
#' \code{var}             variance of the Kumaraswamy distribution.
#'
#' @references
#' \insertRef{kumaraswamy1980generalized}{fitODBOD}
#' \insertRef{jones2009kumaraswamy}{fitODBOD}
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(4)
#' a <- c(1,2,5,10)
#' plot(0,0,main="Probability density graph",xlab="Random variable",ylab="Probability density values",
#' xlim = c(0,1),ylim = c(0,6))
#' for (i in 1:4)
#' {
#' lines(seq(0,1,by=0.01),dKUM(seq(0,1,by=0.01),a[i],a[i])$pdf,col = col[i])
#' }
#'
#' dKUM(seq(0,1,by=0.01),2,3)$pdf   #extracting the probability values
#' dKUM(seq(0,1,by=0.01),2,3)$mean  #extracting the mean
#' dKUM(seq(0,1,by=0.01),2,3)$var   #extracting the variance
#'
#' #plotting the random variables and cumulative probability values
#' col <- rainbow(4)
#' a <- c(1,2,5,10)
#' plot(0,0,main="Cumulative density graph",xlab="Random variable",ylab="Cumulative density values",
#' xlim = c(0,1),ylim = c(0,1))
#' for (i in 1:4)
#' {
#' lines(seq(0,1,by=0.01),pKUM(seq(0,1,by=0.01),a[i],a[i]),col = col[i])
#' }
#'
#' pKUM(seq(0,1,by=0.01),2,3)    #acquiring the cumulative probability values
#'
#' mazKUM(1.4,3,2)               #acquiring the moment about zero values
#' mazKUM(2,2,3)-mazKUM(1,2,3)^2  #acquiring the variance for a=2,b=3
#'
#' #only the integer value of moments is taken here because moments cannot be decimal
#' mazKUM(1.9,5.5,6)
#'
#' @importFrom Rdpack reprompt
#' @export
dKUM<-function(p,a,b)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(p,a,b))) | any(is.infinite(c(p,a,b))) | any(is.nan(c(p,a,b))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    #checking if shape parameters are greater than zero, if not providing an error message and
    #stopping the function progress
    if(a <= 0 | b <= 0)
    {
      stop("Shape parameters cannot be less than or equal to zero")
    }
    else
    {
      #for each input values in the vector necessary calculations and conditions are applied
      if(any(p<0) | any(p>1)){
        stop("Invalid values in the input")
      }
      ans<-sapply(1:length(p),function(i) a*b*(p[i]^(a-1))*((1-p[i]^a)^(b-1)))
    }
  }
  # generating an output in list format consisting pdf,mean and variance
  return(list("pdf"=ans,"mean"=b*beta(1+(1/a),b),
              "var"=b*beta(1+(2/a),b)-(b*beta(1+(1/a),b))^2))
}

#' Kumaraswamy Distribution
#'
#' These functions provide the ability for generating probability density values,
#' cumulative probability density values and moment about zero values for the
#' Kumaraswamy Distribution bounded between [0,1].
#'
#' @usage
#' pKUM(p,a,b)
#'
#' @param p              vector of probabilities.
#' @param a              single value for shape parameter alpha representing as a.
#' @param b              single value for shape parameter beta representing as b.
#'
#' @details
#' The probability density function and cumulative density function of a unit
#' bounded Kumaraswamy Distribution with random variable P are given by
#'
#' \deqn{g_{P}(p)= abp^{a-1}(1-p^a)^{b-1} } ;        \eqn{0 \le p \le 1}
#' \deqn{G_{P}(p)= 1-(1-p^a)^b} ;                   \eqn{0 \le p \le 1}
#' \deqn{a,b > 0}
#'
#' The mean and the variance are denoted by
#' \deqn{E[P]= bB(1+\frac{1}{a},b)}
#' \deqn{var[P]= bB(1+\frac{2}{a},b)-(bB(1+\frac{1}{a},b))^2}
#'
#' The moments about zero is denoted as
#' \deqn{E[P^r]= bB(1+\frac{r}{a},b)}
#' \eqn{r = 1,2,3,...}
#'
#' Defined as \eqn{B(a,b)} is the beta function.
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#' The output of \code{pKUM} gives the cumulative density values in vector form.
#'
#' @references
#' \insertRef{kumaraswamy1980generalized}{fitODBOD}
#' \insertRef{jones2009kumaraswamy}{fitODBOD}
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(4)
#' a <- c(1,2,5,10)
#' plot(0,0,main="Probability density graph",xlab="Random variable",ylab="Probability density values",
#' xlim = c(0,1),ylim = c(0,6))
#' for (i in 1:4)
#' {
#' lines(seq(0,1,by=0.01),dKUM(seq(0,1,by=0.01),a[i],a[i])$pdf,col = col[i])
#' }
#'
#' dKUM(seq(0,1,by=0.01),2,3)$pdf   #extracting the probability values
#' dKUM(seq(0,1,by=0.01),2,3)$mean  #extracting the mean
#' dKUM(seq(0,1,by=0.01),2,3)$var   #extracting the variance
#'
#' #plotting the random variables and cumulative probability values
#' col <- rainbow(4)
#' a <- c(1,2,5,10)
#' plot(0,0,main="Cumulative density graph",xlab="Random variable",ylab="Cumulative density values",
#' xlim = c(0,1),ylim = c(0,1))
#' for (i in 1:4)
#' {
#' lines(seq(0,1,by=0.01),pKUM(seq(0,1,by=0.01),a[i],a[i]),col = col[i])
#' }
#'
#' pKUM(seq(0,1,by=0.01),2,3)    #acquiring the cumulative probability values
#'
#' mazKUM(1.4,3,2)               #acquiring the moment about zero values
#' mazKUM(2,2,3)-mazKUM(1,2,3)^2  #acquiring the variance for a=2,b=3
#'
#' #only the integer value of moments is taken here because moments cannot be decimal
#' mazKUM(1.9,5.5,6)
#'
#' @export
pKUM<-function(p,a,b)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #aif so creating an error message as well as stopping the function progress.
  if(any(is.na(c(p,a,b))) | any(is.infinite(c(p,a,b))) | any(is.nan(c(p,a,b))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    #checking if shape parameters are greater than zero and if not providing an error message
    #and stopping the function progress
    if(a <= 0 | b <= 0 )
    {
      stop("Shape parameters cannot be less than or equal to zero")
    }
    else
    {
      #for each input values in the vector necessary calculations and conditions are applied
      if(any(p<0) | any(p>1)){
        stop("Invalid values in the input")
      }
      ans<-sapply(1:length(p),function(i) 1-(1-p[i]^a)^b)
      #generating an ouput vector of cumulative probability values
      return(ans)
    }
  }
}

#' Kumaraswamy Distribution
#'
#' These functions provide the ability for generating probability density values,
#' cumulative probability density values and moment about zero values for the
#' Kumaraswamy Distribution bounded between [0,1].
#'
#' @usage
#' mazKUM(r,a,b)
#'
#' @param a              single value for shape parameter alpha representing as a.
#' @param b              single value for shape parameter beta representing as b.
#' @param r              vector of moments.
#'
#' @details
#' The probability density function and cumulative density function of a unit
#' bounded Kumaraswamy Distribution with random variable P are given by
#'
#' \deqn{g_{P}(p)= abp^{a-1}(1-p^a)^{b-1} } ;        \eqn{0 \le p \le 1}
#' \deqn{G_{P}(p)= 1-(1-p^a)^b} ;                   \eqn{0 \le p \le 1}
#' \deqn{a,b > 0}
#'
#' The mean and the variance are denoted by
#' \deqn{E[P]= bB(1+\frac{1}{a},b)}
#' \deqn{var[P]= bB(1+\frac{2}{a},b)-(bB(1+\frac{1}{a},b))^2}
#'
#' The moments about zero is denoted as
#' \deqn{E[P^r]= bB(1+\frac{r}{a},b)}
#' \eqn{r = 1,2,3,...}
#'
#' Defined as \eqn{B(a,b)} is the beta function.
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#' The output of \code{mazKUM} gives the moments about zero in vector form.
#'
#' @references
#' \insertRef{kumaraswamy1980generalized}{fitODBOD}
#' \insertRef{jones2009kumaraswamy}{fitODBOD}
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(4)
#' a <- c(1,2,5,10)
#' plot(0,0,main="Probability density graph",xlab="Random variable",ylab="Probability density values",
#' xlim = c(0,1),ylim = c(0,6))
#' for (i in 1:4)
#' {
#' lines(seq(0,1,by=0.01),dKUM(seq(0,1,by=0.01),a[i],a[i])$pdf,col = col[i])
#' }
#'
#' dKUM(seq(0,1,by=0.01),2,3)$pdf   #extracting the probability values
#' dKUM(seq(0,1,by=0.01),2,3)$mean  #extracting the mean
#' dKUM(seq(0,1,by=0.01),2,3)$var   #extracting the variance
#'
#' #plotting the random variables and cumulative probability values
#' col <- rainbow(4)
#' a <- c(1,2,5,10)
#' plot(0,0,main="Cumulative density graph",xlab="Random variable",ylab="Cumulative density values",
#' xlim = c(0,1),ylim = c(0,1))
#' for (i in 1:4)
#' {
#' lines(seq(0,1,by=0.01),pKUM(seq(0,1,by=0.01),a[i],a[i]),col = col[i])
#' }
#'
#' pKUM(seq(0,1,by=0.01),2,3)    #acquiring the cumulative probability values
#' mazKUM(1.4,3,2)               #acquiring the moment about zero values
#' mazKUM(2,2,3)-mazKUM(1,2,3)^2  #acquiring the variance for a=2,b=3
#'
#' #only the integer value of moments is taken here because moments cannot be decimal
#' mazKUM(1.9,5.5,6)
#'
#' @export
mazKUM<-function(r,a,b)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(r,a,b))) | any(is.infinite(c(r,a,b))) | any(is.nan(c(r,a,b))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    #checking if shape parameters are greater than zero, and if not providing an error
    #message and stopping the function progress
    if(a <= 0 | b <= 0)
    {
      stop("Shape parameters cannot be less than or equal to zero")
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
      ans<-sapply(1:length(r),function(i) b*beta(1+(r[i]/a),b))
      #generating an ouput vector of moment about zero values
      return(ans)
    }
  }
}

#' Kumaraswamy Binomial Distribution
#'
#' These functions provide the ability for generating probability function values and
#' cumulative probability function values for the Kumaraswamy Binomial Distribution.
#'
#' @usage
#' dKumBin(x,n,a,b,it=25000)
#'
#' @param x        vector of binomial random variables
#' @param n        single value for no of binomial trial
#' @param a        single value for shape parameter alpha representing a
#' @param b        single value for shape parameter beta representing b
#' @param it       number of iterations to converge as a proper
#'                 probability function replacing infinity
#'
#' @details
#' Mixing Kumaraswamy distribution with Binomial distribution will create the
#' Kumaraswamy Binomial distribution.  The probability function and cumulative
#' probability function can be constructed and are denoted below.
#'
#' The cumulative probability function is the summation of probability
#' function values.
#'
#' \deqn{P_{KumBin}(x)= ab{n \choose x} \sum_{j=0}^{it} (-1)^j{b-1 \choose j}B(x+a+aj,n-x+1) }
#' \deqn{a,b > 0}
#' \deqn{x = 0,1,2,...n}
#' \deqn{n = 1,2,3,...}
#' \deqn{it > 0}
#'
#' The mean, variance and over dispersion are denoted as
#' \deqn{E_{KumBin}[x]= nbB(1+\frac{1}{a},b) }
#' \deqn{Var_{KumBin}[x]= n^2 b(B(1+\frac{2}{a},b)-bB(1+\frac{1}{a},b)^2)+
#'                        nb(B(1+\frac{1}{a},b)-B(1+\frac{2}{a},b)) }
#' \deqn{over dispersion= \frac{(bB(1+\frac{2}{a},b)-(bB(1+\frac{1}{a},b))^2)}
#'                        {(bB(1+\frac{1}{a},b)-(bB(1+\frac{1}{a},b))^2)} }
#'
#' Defined as \eqn{B(a,b)} is the beta function.
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions necessary error
#' messages will be provided to go further.
#'
#' @return
#' The output of \code{dKumBin} gives a list format consisting
#'
#' \code{pdf}           probability function values in vector form.
#'
#' \code{mean}          mean of the Kumaraswamy Binomial Distribution.
#'
#' \code{var}           variance of the Kumaraswamy Binomial Distribution.
#'
#' \code{over.dis.para} over dispersion value of the Kumaraswamy Distribution.
#'
#' @references
#' \insertRef{xiaohu2011kumaraswamy}{fitODBOD}
#'
#' @examples
#' \dontrun{
#' #plotting the random variables and probability values
#' col <- rainbow(5)
#' a <- c(1,2,5,10,.85)
#' plot(0,0,main="Kumaraswamy binomial probability function graph",xlab="Binomial random variable",
#' ylab="Probability function values",xlim = c(0,10),ylim = c(0,0.5))
#' for (i in 1:5) {
#' lines(0:10,dKumBin(0:10,10,a[i],a[i])$pdf,col = col[i],lwd=2.85)
#' points(0:10,dKumBin(0:10,10,a[i],a[i])$pdf,col = col[i],pch=16)
#'   }
#' }
#'
#' dKumBin(0:10,10,4,2)$pdf  #extracting the pdf values
#' dKumBin(0:10,10,4,2)$mean #extracting the mean
#' dKumBin(0:10,10,4,2)$var  #extracting the variance
#' dKumBin(0:10,10,4,2)$over.dis.para #extracting the over dispersion value
#'
#' \dontrun{
#' #plotting the random variables and cumulative probability values
#' col <- rainbow(5)
#' a <- c(1,2,5,10,.85)
#' plot(0,0,main="Cumulative probability function graph",xlab="Binomial random variable",
#' ylab="Cumulative probability function values",xlim = c(0,10),ylim = c(0,1))
#' for (i in 1:5) {
#' lines(0:10,pKumBin(0:10,10,a[i],a[i]),col = col[i])
#' points(0:10,pKumBin(0:10,10,a[i],a[i]),col = col[i])
#'   }
#' }
#'
#' pKumBin(0:10,10,4,2)    #acquiring the cumulative probability values
#'
#' @export
dKumBin<-function(x,n,a,b,it=25000)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,n,a,b,it))) | any(is.infinite(c(x,n,a,b,it))) |any(is.nan(c(x,n,a,b,it))))
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    #checking if shape parameters are less than or equal zero ,
    #if so providing an error message and stopping the function progress
    if(a <= 0 | b <= 0)
    {
      stop("Shape parameters cannot be less than or equal to zero")
    }
    #checking if number of iterations are less than one
    #if so providing an error message and stopping the function progress
    else if(it < 1)
    {
      stop("Number of iterations cannot be less than one")
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
        #constructing the probability values for all random variables
        y<-0:n
        ans1<-sapply(1:length(y),function(i) a*b*choose(n,y[i])*sum(((-1)^(0:it))*choose(b-1,0:it)*beta(y[i]+a+a*(0:it),n-y[i]+1)))
        check<-sum(ans1)
        #checking if the sum of all probability values leads upto 1
        #if not providing an error message and stopping the function progress
        if(check < 0.9999 |check > 1.0001 | any(ans1>1) | any(ans1<0))
        {
          stop("Shape parameters and number of iterations combination does not create a proper probability mass function")
        }
        else
        {
          #for each random variable in the input vector below calculations occur
          ans<-sapply(1:length(x),function(i) a*b*choose(n,x[i])*sum(((-1)^(0:it))*choose(b-1,0:it)*beta(x[i]+a+a*(0:it),n-x[i]+1)))
        }
      }
    }
  }
  # generating an output in list format consisting pdf,mean,variance and overdispersion value
  return(list("pdf"=ans,"mean"=n*b*beta(1+(1/a),b),
              "var"=(n^2)*b*(beta(1+(2/a),b)-b*(beta(1+(1/a),b))^2)+n*b*(beta(1+(1/a),b)-beta(1+(2/a),b)),
              "over.dis.para"=((b*beta(1+(2/a),b))-(b*beta(1+(1/a),b))^2)/
                ((b*beta(1+(1/a),b))-(b*beta(1+(1/a),b))^2) ))
}


#' Kumaraswamy Binomial Distribution
#'
#' These functions provide the ability for generating probability function values and
#' cumulative probability function values for the Kumaraswamy Binomial Distribution.
#'
#' @usage
#' pKumBin(x,n,a,b,it=25000)
#'
#' @param x        vector of binomial random variables.
#' @param n        single value for no of binomial trial.
#' @param a        single value for shape parameter alpha representing a.
#' @param b        single value for shape parameter beta representing b.
#' @param it       number of iterations to converge as a proper
#'                 probability function replacing infinity.
#'
#' @details
#' Mixing Kumaraswamy distribution with Binomial distribution will create the
#' Kumaraswamy Binomial distribution.  The probability function and cumulative
#' probability function can be constructed and are denoted below.
#'
#' The cumulative probability function is the summation of probability
#' function values.
#'
#' \deqn{P_{KumBin}(x)= ab{n \choose x} \sum_{j=0}^{it} (-1)^j{b-1 \choose j}B(x+a+aj,n-x+1) }
#' \deqn{a,b > 0}
#' \deqn{x = 0,1,2,...n}
#' \deqn{n = 1,2,3,...}
#' \deqn{it > 0}
#'
#' The mean, variance and over dispersion are denoted as
#' \deqn{E_{KumBin}[x]= nbB(1+\frac{1}{a},b) }
#' \deqn{Var_{KumBin}[x]= (n^2)b(B(1+\frac{2}{a},b)-bB(1+\frac{1}{a},b)^2)+
#'                        nb(B(1+\frac{1}{a},b)-B(1+\frac{2}{a},b)) }
#' \deqn{over dispersion= \frac{(bB(1+\frac{2}{a},b)-(bB(1+\frac{1}{a},b))^2)}
#'                        {(bB(1+\frac{1}{a},b)-(bB(1+\frac{1}{a},b))^2)} }
#'
#' Defined as \eqn{B(a,b)} is the beta function.
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions necessary error
#' messages will be provided to go further.
#'
#' @return
#' The output of \code{pKumBin} gives cumulative probability values in vector form.
#'
#' @references
#' \insertRef{xiaohu2011kumaraswamy}{fitODBOD}
#'
#' @examples
#' \dontrun{
#' #plotting the random variables and probability values
#' col <- rainbow(5)
#' a <- c(1,2,5,10,.85)
#' plot(0,0,main="Kumaraswamy binomial probability function graph",xlab="Binomial random variable",
#' ylab="Probability function values",xlim = c(0,10),ylim = c(0,0.5))
#' for (i in 1:5) {
#' lines(0:10,dKumBin(0:10,10,a[i],a[i])$pdf,col = col[i],lwd=2.85)
#' points(0:10,dKumBin(0:10,10,a[i],a[i])$pdf,col = col[i],pch=16)
#'   }
#' }
#'
#' dKumBin(0:10,10,4,2)$pdf  #extracting the pdf values
#' dKumBin(0:10,10,4,2)$mean #extracting the mean
#' dKumBin(0:10,10,4,2)$var  #extracting the variance
#' dKumBin(0:10,10,4,2)$over.dis.para #extracting the over dispersion value
#'
#' \dontrun{
#' #plotting the random variables and cumulative probability values
#' col <- rainbow(5)
#' a <- c(1,2,5,10,.85)
#' plot(0,0,main="Cumulative probability function graph",xlab="Binomial random variable",
#' ylab="Cumulative probability function values",xlim = c(0,10),ylim = c(0,1))
#' for (i in 1:5) {
#' lines(0:10,pKumBin(0:10,10,a[i],a[i]),col = col[i])
#' points(0:10,pKumBin(0:10,10,a[i],a[i]),col = col[i])
#'   }
#' }
#' pKumBin(0:10,10,4,2)    #acquiring the cumulative probability values
#'
#' @export
pKumBin<-function(x,n,a,b,it=25000)
{
  #for each binomial random variable in the input vector the cumulative probability function
  #values are calculated
  ans<-sapply(1:length(x),function(i) sum(dKumBin(0:x[i],n,a,b,it)$pdf))
  #generating an ouput vector cumulative probability function values
  return(ans)
}

#' Negative Log Likelihood value of Kumaraswamy Binomial Distribution
#'
#' This function will calculate the Negative Log Likelihood value when the vector of binomial random
#' variables and vector of corresponding frequencies are given with the shape parameters a and b
#' and iterations it.
#'
#' @usage
#' NegLLKumBin(x,freq,a,b,it=25000)
#'
#' @param x                 vector of binomial random variables.
#' @param freq              vector of frequencies.
#' @param a                 single value for shape parameter alpha representing as a.
#' @param b                 single value for shape parameter beta representing as b.
#' @param it                number of iterations to converge as a proper probability function
#'                          replacing infinity.
#'
#' @details
#' \deqn{0 < a,b }
#' \deqn{x = 0,1,2,...}
#' \deqn{freq \ge 0 }
#' \deqn{it > 0}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions necessary error
#' messages will be provided to go further.
#'
#' @return
#' The output of \code{NegLLKumBin} will produce a single numeric value.
#'
#' @references
#' \insertRef{xiaohu2011kumaraswamy}{fitODBOD}
#'
#' @examples
#' No.D.D <- 0:7          #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)  #assigning the corresponding frequencies
#'
#' \dontrun{
#' NegLLKumBin(No.D.D,Obs.fre.1,1.3,4.4) #acquiring the negative log likelihood value
#' }
#'
#' @export
NegLLKumBin<-function(x,freq,a,b,it=25000)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,freq,a,b,it))) | any(is.infinite(c(x,freq,a,b,it)))
     |any(is.nan(c(x,freq,a,b,it))) )
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
    else if(a <= 0 | b <= 0)
    {
      stop("Shape parameters cannot be less than or equal to zero")
    }
    #checking if number of iterations are less than one
    #if so providing an error message and stopping the function from progressing further
    else if(it < 1)
    {
      stop("Number of iterations cannot be less than one")
    }
    else
    {
      n<-max(x)
      y<-0:n
      #constructing the probability values for all random variables
      ans1<-sapply(1:length(y),function(i) a*b*choose(n,y[i])*sum(((-1)^(0:it))*choose(b-1,(0:it))*beta(y[i]+a+a*(0:it),n-y[i]+1)))
      check<-sum(ans1)
      #checking if the sum of all probability values leads upto one
      #if not providing an error message and stopping the function progress
      if(check < 0.9999 |check > 1.0001 | any(ans1>1) | any(ans1<0))
      {
        stop("Shape parameters and number of iterations combination does not create a proper probability mass function")
      }
      else
      {
        #constructing the data set using the random variables vector and frequency vector
        data<-rep(x,freq)
        value<-sapply(1:sum(freq),function(i) sum(((-1)^(0:it))*choose(b-1,(0:it))*beta(data[i]+a+a*(0:it),n-data[i]+1)))
      }
      #calculating the negative log likelihood value and representing as a single output value
      return(-(sum(freq)*log(a*b)+sum(log(choose(n,data[1:sum(freq)])))+sum(log(value))))
    }
  }
}

#' Estimating the shape parameters a and b and iterations for  Kumaraswamy Binomial Distribution
#'
#' The function will estimate the shape parameters using the maximum log likelihood method  for
#' the Kumaraswamy Binomial distribution when the binomial random variables and
#' corresponding frequencies are given
#'
#' @usage
#' EstMLEKumBin(x,freq,a,b,it,...)
#'
#'
#' @param x                 vector of binomial random variables.
#' @param freq              vector of frequencies.
#' @param a                 single value for shape parameter alpha representing as a.
#' @param b                 single value for shape parameter beta representing as b.
#' @param it                number of iterations to converge as a proper probability function
#'                          replacing infinity.
#' @param ...               mle2 function inputs except data and estimating parameter.
#'
#' @details
#' \deqn{0 < a,b}
#' \deqn{x = 0,1,2,...}
#' \deqn{freq \ge 0}
#' \deqn{it > 0}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions necessary
#' error messages will be provided to go further.
#'
#' @return
#' \code{EstMLEKumBin} here is used as a wrapper for the \code{mle2} function of
#' \pkg{bbmle} package therefore output is of class of mle2.
#'
#' @references
#' \insertRef{xiaohu2011kumaraswamy}{fitODBOD}
#'
#' @seealso
#' \code{\link[bbmle]{mle2}}
#'
#' @examples
#' No.D.D <- 0:7     #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)  #assigning the corresponding frequencies
#'
#' \dontrun{
#' #estimating the parameters using maximum log likelihood value and assigning it
#' parameters1 <- EstMLEKumBin(x=No.D.D,freq=Obs.fre.1,a=10.1,b=1.1,it=10000)
#'
#' bbmle::coef(parameters1)   #extracting the parameters
#' }
#' @export
EstMLEKumBin<-function(x,freq,a,b,it,...)
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
  output<-suppressWarnings2(bbmle::mle2(.EstMLEKumBin,data=list(x=x,freq=freq),
                                        start = list(a=a,b=b,it=it),...),"NaN")
  return(output)
}


.EstMLEKumBin<-function(x,freq,a,b,it)
{
  #with respective to using bbmle package function mle2 there is no need impose any restrictions
  #therefor the output is directly a single numeric value for the negative log likelihood value of
  #Kumaraswamy binomial distribution
  n<-max(x)
  data<-rep(x,freq)
  value<-sapply(1:sum(freq),function(i) sum(((-1)^(0:it))*choose(b-1,(0:it))*beta(data[i]+a+a*(0:it),n-data[i]+1)))
  return(-(sum(freq)*log(a*b)+ sum(log(choose(n,data[1:sum(freq)])))+ sum(log(value))))
}

#' Fitting the Kumaraswamy Binomial Distribution when binomial random variable, frequency and shape
#' parameters a and b, iterations parameter it are given
#'
#' The function will fit the Kumaraswamy Binomial distribution when random variables,
#' corresponding frequencies and shape parameters are given. It will provide the expected
#' frequencies, chi-squared test statistics value, p value, degree of freedom and
#' over dispersion value so that it can be seen if this distribution fits the data.
#'
#' @usage fitKumBin(x,obs.freq,a,b,it)
#'
#' @param x                  vector of binomial random variables.
#' @param obs.freq           vector of frequencies.
#' @param a                  single value for shape parameter alpha representing a.
#' @param b                  single value for shape parameter beta representing b.
#' @param it                 number of iterations to converge as a proper probability
#'                           function replacing infinity.
#'
#' @details
#' \deqn{0 < a,b}
#' \deqn{x = 0,1,2,...n}
#' \deqn{obs.freq \ge 0}
#' \deqn{it > 0}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#' The output of \code{fitKumBin} gives the class format \code{fitKB} and \code{fit} consisting a list
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
#' \code{fitKB} fitted values of \code{dKumBin}.
#'
#' \code{NegLL} Negative Log Likelihood value.
#'
#' \code{a} estimated value for alpha parameter as a.
#'
#' \code{b} estimated value for beta parameter as b.
#'
#' \code{it} estimated it value for iterations.
#'
#' \code{AIC} AIC value.
#'
#' \code{over.dis.para} over dispersion value.
#'
#' \code{call} the inputs of the function.
#'
#' Methods \code{summary}, \code{print}, \code{AIC}, \code{residuals} and \code{fiited} can be used to
#' extract specific outputs.
#'
#' @references
#' \insertRef{xiaohu2011kumaraswamy}{fitODBOD}
#'
#' @seealso
#' \code{\link[bbmle]{mle2}}
#'
#' @examples
#' No.D.D <- 0:7   #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)   #assigning the corresponding frequencies
#'
#' \dontrun{
#' #estimating the parameters using maximum log likelihood value and assigning it
#' parameters <- EstMLEKumBin(x=No.D.D,freq=Obs.fre.1,a=10.1,b=1.1,it=10000)
#'
#' bbmle::coef(parameters)    #extracting the parameters
#' aKumBin <- bbmle::coef(parameters)[1] #assigning the estimated a
#' bKumBin <- bbmle::coef(parameters)[2] #assigning the estimated b
#' itKumBin <- bbmle::coef(parameters)[3] #assigning the estimated iterations
#'
#' #fitting when the random variable,frequencies,shape parameter values are given.
#' results <- fitKumBin(No.D.D,Obs.fre.1,aKumBin,bKumBin,itKumBin*100)
#' results
#'
#' #extracting the expected frequencies
#' fitted(results)
#'
#' #extracting the residuals
#' residuals(results)
#' }
#'
#' @export
fitKumBin<-function(x,obs.freq,a,b,it)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,obs.freq,a,b,it))) | any(is.infinite(c(x,obs.freq,a,b,it))) |
     any(is.nan(c(x,obs.freq,a,b,it))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    est<-dKumBin(x,max(x),a,b,it)
    odp<-est$over.dis.para; names(odp)<-NULL
    #for given random variables and parameters calculating the estimated probability values
    est.prob<-est$pdf
    #using the estimated probability values the expected frequencies are calculated
    exp.freq<-round((sum(obs.freq)*est.prob),2)
    #chi-squared test statistics is calculated with observed frequency and expected frequency
    statistic<-sum(((obs.freq-exp.freq)^2)/exp.freq)
    #degree of freedom is calculated
    df<-length(x)-3
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
    NegLL<-NegLLKumBin(x,obs.freq,a,b,it)
    names(NegLL)<-NULL
    #the final output is in a list format containing the calculated values
    final<-list("bin.ran.var"=x,"obs.freq"=obs.freq,"exp.freq"=exp.freq,
                "statistic"=round(statistic,4),"df"=df,"p.value"=round(p.value,4),
                "fitBB"=est,"NegLL"=NegLL,"a"=a,"b"=b,"it"=it,"AIC"=2*2+2*NegLL,
                "over.dis.para"=odp,"call"=match.call())
    class(final)<-c("fitKB","fit")
    return(final)
  }
}

#' @method fitKumBin default
#' @export
fitKumBin.default<-function(x,obs.freq,a,b,it)
{
  return(fitKumBin(x,obs.freq,a,b,it))
}

#' @method print fitKB
#' @export
print.fitKB<-function(x,...)
{
  cat("Call: \n")
  print(x$call)
  cat("\nChi-squared test for Kumaraswamy Binomial Distribution \n\t
      Observed Frequency : ",x$obs.freq,"\n\t
      expected Frequency : ",x$exp.freq,"\n\t
      estimated a parameter :",x$a, "  ,estimated b parameter :",x$b,",\n\t
      estimated it value :",x$it,"\n\t
      X-squared :",x$statistic,"  ,df :",x$df,"  ,p-value :",x$p.value,"\n\t
      over dispersion :",x$over.dis.para,"\n")
}

#' @method summary fitKB
#' @export
summary.fitKB<-function(object,...)
{
  cat("Call: \n")
  print(object$call)
  cat("\nChi-squared test for Kumaraswamy Binomial Distribution \n\t
      Observed Frequency : ",object$obs.freq,"\n\t
      expected Frequency : ",object$exp.freq,"\n\t
      estimated a parameter :",object$a,"  ,estimated b parameter :",object$b,",\n\t
      estimated it value :",object$it,"\n\t
      X-squared :",object$statistic,"  ,df :",object$df,"  ,p-value :",object$p.value,"\n\t
      over dispersion :",object$over.dis.para,"\n\t
      Negative Loglikehood value :",object$NegLL,"\n\t
      AIC value :",object$AIC,"\n")
}

#' @importFrom bbmle mle2
#' @importFrom stats pchisq
