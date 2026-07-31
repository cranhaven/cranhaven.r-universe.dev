#' Beta Distribution
#'
#' These functions provide the ability for generating probability density values,
#' cumulative probability density values and moment about zero values for the
#' Beta Distribution bounded between [0,1]
#'
#' @usage
#' dBETA(p,a,b)
#'
#' @param p              vector of probabilities.
#' @param a              single value for shape parameter alpha representing as a.
#' @param b              single value for shape parameter beta representing as b.
#'
#' @details
#' The probability density function and cumulative density function of a unit
#' bounded Beta distribution with random variable P are given by
#'
#' \deqn{g_{P}(p)= \frac{p^{a-1}(1-p)^{b-1}}{B(a,b)} } ;            \eqn{0 \le p \le 1}
#' \deqn{G_{P}(p)= \frac{B_p(a,b)}{B(a,b)} } ;                   \eqn{0 \le p \le 1}
#' \deqn{a,b > 0}
#'
#' The mean and the variance are denoted by
#' \deqn{E[P]= \frac{a}{a+b} }
#' \deqn{var[P]= \frac{ab}{(a+b)^2(a+b+1)} }
#'
#' The moments about zero is denoted as
#' \deqn{E[P^r]= \prod_{i=0}^{r-1} (\frac{a+i}{a+b+i}) }
#' \eqn{r = 1,2,3,...}
#'
#' Defined as \eqn{B_p(a,b)=\int^p_0 t^{a-1} (1-t)^{b-1}\,dt} is
#' incomplete beta integrals  and \eqn{B(a,b)} is the beta function.
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions necessary error
#' messages will be provided to go further.
#'
#' @return
#' The output of \code{dBETA} gives a list format consisting
#'
#' \code{pdf}            probability density values in vector form.
#'
#' \code{mean}           mean of the Beta distribution.
#'
#' \code{var}            variance of the Beta distribution.
#'
#' @references
#' \insertRef{johnson1995continuous}{fitODBOD}
#' \insertRef{trenkler1996continuous}{fitODBOD}
#'
#' @seealso
#' \code{\link[stats]{Beta}}
#'
#' or
#'
#' \url{https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Beta.html}
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(4)
#' a <- c(1,2,5,10)
#' plot(0,0,main="Probability density graph",xlab="Random variable",ylab="Probability density values",
#' xlim = c(0,1),ylim = c(0,4))
#' for (i in 1:4)
#' {
#' lines(seq(0,1,by=0.01),dBETA(seq(0,1,by=0.01),a[i],a[i])$pdf,col = col[i])
#' }
#'
#' dBETA(seq(0,1,by=0.01),2,3)$pdf   #extracting the pdf values
#' dBETA(seq(0,1,by=0.01),2,3)$mean  #extracting the mean
#' dBETA(seq(0,1,by=0.01),2,3)$var   #extracting the variance
#'
#' #plotting the random variables and cumulative probability values
#' col <- rainbow(4)
#' a <- c(1,2,5,10)
#' plot(0,0,main="Cumulative density graph",xlab="Random variable",ylab="Cumulative density values",
#' xlim = c(0,1),ylim = c(0,1))
#' for (i in 1:4)
#' {
#' lines(seq(0,1,by=0.01),pBETA(seq(0,1,by=0.01),a[i],a[i]),col = col[i])
#' }
#'
#' pBETA(seq(0,1,by=0.01),2,3)   #acquiring the cumulative probability values
#' mazBETA(1.4,3,2)              #acquiring the moment about zero values
#' mazBETA(2,3,2)-mazBETA(1,3,2)^2 #acquiring the variance for a=3,b=2
#'
#' #only the integer value of moments is taken here because moments cannot be decimal
#' mazBETA(1.9,5.5,6)
#'
#' @importFrom Rdpack reprompt
#' @export
dBETA<-function(p,a,b)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(p,a,b))) | any(is.infinite(c(p,a,b))) | any(is.nan(c(p,a,b))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    #checking if shape parameters are greater than zero, if not providing error message and
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
      ans<-sapply(1:length(p), function(i) (p[i]^(a-1)*(1-p[i])^(b-1))/beta(a,b))
    }
    # generating an output in list format consisting pdf,mean and variance
    return(list("pdf"=ans,"mean"=a/(a+b),"var"=(a*b)/(((a+b)^2)*(a+b+1)) ))
  }
}

#' Beta Distribution
#'
#' These functions provide the ability for generating probability density values,
#' cumulative probability density values and moment about zero values for the
#' Beta Distribution bounded between [0,1].
#'
#' @usage
#' pBETA(p,a,b)
#'
#' @param p              vector of probabilities.
#' @param a              single value for shape parameter alpha representing as a.
#' @param b              single value for shape parameter beta representing as b.
#'
#' @details
#' The probability density function and cumulative density function of a unit
#' bounded beta distribution with random variable P are given by
#'
#' \deqn{g_{P}(p)= \frac{p^{a-1}(1-p)^{b-1}}{B(a,b)} } ;            \eqn{0 \le p \le 1}
#' \deqn{G_{P}(p)= \frac{B_p(a,b)}{B(a,b)} } ;                   \eqn{0 \le p \le 1}
#' \deqn{a,b > 0}
#'
#' The mean and the variance are denoted by
#' \deqn{E[P]= \frac{a}{a+b} }
#' \deqn{var[P]= \frac{ab}{(a+b)^2(a+b+1)} }
#'
#' The moments about zero is denoted as
#' \deqn{E[P^r]= \prod_{i=0}^{r-1} (\frac{a+i}{a+b+i}) }
#' \eqn{r = 1,2,3,...}
#'
#' Defined as \eqn{B_p(a,b)=\int^p_0 t^{a-1} (1-t)^{b-1}\,dt} is
#' incomplete beta integrals  and \eqn{B(a,b)} is the beta function.
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions necessary error
#' messages will be provided to go further.
#'
#' @return
#' The output of \code{pBETA} gives the cumulative density values in vector form.
#'
#' @references
#' \insertRef{johnson1995continuous}{fitODBOD}
#' \insertRef{trenkler1996continuous}{fitODBOD}
#'
#' @seealso
#' \code{\link[stats]{Beta}}
#'
#' or
#'
#' \url{https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Beta.html}
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(4)
#' a <- c(1,2,5,10)
#' plot(0,0,main="Probability density graph",xlab="Random variable",ylab="Probability density values",
#' xlim = c(0,1),ylim = c(0,4))
#' for (i in 1:4)
#' {
#' lines(seq(0,1,by=0.01),dBETA(seq(0,1,by=0.01),a[i],a[i])$pdf,col = col[i])
#' }
#'
#' dBETA(seq(0,1,by=0.01),2,3)$pdf   #extracting the pdf values
#' dBETA(seq(0,1,by=0.01),2,3)$mean  #extracting the mean
#' dBETA(seq(0,1,by=0.01),2,3)$var   #extracting the variance
#'
#' #plotting the random variables and cumulative probability values
#' col <- rainbow(4)
#' a <- c(1,2,5,10)
#' plot(0,0,main="Cumulative density graph",xlab="Random variable",ylab="Cumulative density values",
#' xlim = c(0,1),ylim = c(0,1))
#' for (i in 1:4)
#' {
#' lines(seq(0,1,by=0.01),pBETA(seq(0,1,by=0.01),a[i],a[i]),col = col[i])
#' }
#'
#' pBETA(seq(0,1,by=0.01),2,3)   #acquiring the cumulative probability values
#' mazBETA(1.4,3,2)              #acquiring the moment about zero values
#' mazBETA(2,3,2)-mazBETA(1,3,2)^2 #acquiring the variance for a=3,b=2
#'
#' #only the integer value of moments is taken here because moments cannot be decimal
#' mazBETA(1.9,5.5,6)
#'
#' @export
pBETA<-function(p,a,b)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(p,a,b))) | any(is.infinite(c(p,a,b))) | any(is.nan(c(p,a,b))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    #checking if shape parameters are greater than zero and if not providing an error message
    #and stopping the function progress
    if(a <= 0 | b <= 0)
    {
      stop("Shape parameters cannot be less than or equal to zero")
    }
    else
    {
      #the equation contains partial beta integration, below is the integral function
      if(any(p<0) | any(p>1)){
        stop("Invalid values in the input")
      }
      #for each input values in the vector necessary calculations and conditions are applied
      ans<-sapply(1:length(p),function(i){
        #integrating the above mentioned function under limits of zero and vector p
        val<-stats::integrate(function(q){ (q^(a-1))*((1-q)^(b-1)) },lower = 0,upper = p[i])
        val$value/beta(a,b)
      })
      #generating an ouput vector of cumulative probability values
      return(ans)
    }
  }
}

#' Beta Distribution
#'
#' These functions provide the ability for generating probability density values,
#' cumulative probability density values and moment about zero values for the
#' Beta Distribution bounded between [0,1].
#'
#' @usage
#' mazBETA(r,a,b)
#'
#' @param a              single value for shape parameter alpha representing as a.
#' @param b              single value for shape parameter beta representing as b.
#' @param r              vector of moments.
#'
#' @details
#' The probability density function and cumulative density function of a unit
#' bounded beta distribution with random variable P are given by
#'
#' \deqn{g_{P}(p)= \frac{p^{a-1}(1-p)^{b-1}}{B(a,b)} } ;            \eqn{0 \le p \le 1}
#' \deqn{G_{P}(p)= \frac{B_p(a,b)}{B(a,b)} } ;                   \eqn{0 \le p \le 1}
#' \deqn{a,b > 0}
#'
#' The mean and the variance are denoted by
#' \deqn{E[P]= \frac{a}{a+b} }
#' \deqn{var[P]= \frac{ab}{(a+b)^2(a+b+1)} }
#'
#' The moments about zero is denoted as
#' \deqn{E[P^r]= \prod_{i=0}^{r-1} (\frac{a+i}{a+b+i}) }
#' \eqn{r = 1,2,3,...}
#'
#' Defined as \eqn{B_p(a,b)=\int^p_0 t^{a-1} (1-t)^{b-1}\,dt} is
#' incomplete beta integrals  and \eqn{B(a,b)} is the beta function.
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions necessary error
#' messages will be provided to go further.
#'
#' @return
#' The output of \code{mazBETA} gives the moments about zero in vector form.
#'
#' @references
#' \insertRef{johnson1995continuous}{fitODBOD}
#' \insertRef{trenkler1996continuous}{fitODBOD}
#'
#' @seealso
#' \code{\link[stats]{Beta}}
#'
#' or
#'
#' \url{https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Beta.html}
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(4)
#' a <- c(1,2,5,10)
#' plot(0,0,main="Probability density graph",xlab="Random variable",ylab="Probability density values",
#' xlim = c(0,1),ylim = c(0,4))
#' for (i in 1:4)
#' {
#' lines(seq(0,1,by=0.01),dBETA(seq(0,1,by=0.01),a[i],a[i])$pdf,col = col[i])
#' }
#'
#' dBETA(seq(0,1,by=0.01),2,3)$pdf   #extracting the pdf values
#' dBETA(seq(0,1,by=0.01),2,3)$mean  #extracting the mean
#' dBETA(seq(0,1,by=0.01),2,3)$var   #extracting the variance
#'
#' #plotting the random variables and cumulative probability values
#' col <- rainbow(4)
#' a <- c(1,2,5,10)
#' plot(0,0,main="Cumulative density graph",xlab="Random variable",ylab="Cumulative density values",
#' xlim = c(0,1),ylim = c(0,1))
#' for (i in 1:4)
#' {
#' lines(seq(0,1,by=0.01),pBETA(seq(0,1,by=0.01),a[i],a[i]),col = col[i])
#' }
#'
#' pBETA(seq(0,1,by=0.01),2,3)   #acquiring the cumulative probability values
#' mazBETA(1.4,3,2)              #acquiring the moment about zero values
#' mazBETA(2,3,2)-mazBETA(1,3,2)^2 #acquiring the variance for a=3,b=2
#'
#' #only the integer value of moments is taken here because moments cannot be decimal
#' mazBETA(1.9,5.5,6)
#'
#' @export
mazBETA<-function(r,a,b)
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
      ans<-sapply(1:length(r),function(i) prod((a+(0:(r[i]-1)))/(a+b+(0:(r[i]-1)))))
      #generating an ouput vector of moment about zero values
      return(ans)
    }
  }
}

#' Beta-Binomial Distribution
#'
#' These functions provide the ability for generating probability function values and
#' cumulative probability function values for the Beta-Binomial Distribution.
#'
#' @usage
#' dBetaBin(x,n,a,b)
#'
#' @param x        vector of binomial random variables.
#' @param n        single value for no of binomial trials.
#' @param a        single value for shape parameter alpha representing as a.
#' @param b        single value for shape parameter beta representing as b.
#'
#' @details
#' Mixing Beta distribution with Binomial distribution will create the Beta-Binomial
#' distribution. The probability function and cumulative probability function can be
#' constructed and are denoted below.
#'
#' The cumulative probability function is the summation of probability function values.
#'
#' \deqn{P_{BetaBin}(x)= {n \choose x} \frac{B(a+x,n+b-x)}{B(a,b)} }
#' \deqn{a,b > 0}
#' \deqn{x = 0,1,2,3,...n}
#' \deqn{n = 1,2,3,...}
#'
#' The mean, variance and over dispersion are denoted as
#' \deqn{E_{BetaBin}[x]= \frac{na}{a+b} }
#' \deqn{Var_{BetaBin}[x]= \frac{(nab)}{(a+b)^2} \frac{(a+b+n)}{(a+b+1)} }
#' \deqn{over dispersion= \frac{1}{a+b+1} }
#'
#' Defined as  \code{B(a,b)} is the beta function.
#'
#' @return
#' The output of \code{dBetaBin} gives a list format consisting
#'
#' \code{pdf}              probability function values in vector form.
#'
#' \code{mean}             mean of the Beta-Binomial Distribution.
#'
#' \code{var}              variance of the Beta-Binomial Distribution.
#'
#' \code{over.dis.para}    over dispersion value of the Beta-Binomial Distribution.
#'
#' @references
#' \insertRef{young2008pooling}{fitODBOD}
#' \insertRef{trenkler1996continuous}{fitODBOD}
#' \insertRef{hughes1993using}{fitODBOD}
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(5)
#' a <- c(1,2,5,10,0.2)
#' plot(0,0,main="Beta-binomial probability function graph",xlab="Binomial random variable",
#' ylab="Probability function values",xlim = c(0,10),ylim = c(0,0.5))
#' for (i in 1:5)
#' {
#' lines(0:10,dBetaBin(0:10,10,a[i],a[i])$pdf,col = col[i],lwd=2.85)
#' points(0:10,dBetaBin(0:10,10,a[i],a[i])$pdf,col = col[i],pch=16)
#' }
#'
#' dBetaBin(0:10,10,4,.2)$pdf    #extracting the pdf values
#' dBetaBin(0:10,10,4,.2)$mean   #extracting the mean
#' dBetaBin(0:10,10,4,.2)$var    #extracting the variance
#' dBetaBin(0:10,10,4,.2)$over.dis.para  #extracting the over dispersion value
#'
#' #plotting the random variables and cumulative probability values
#' col <- rainbow(4)
#' a <- c(1,2,5,10)
#' plot(0,0,main="Cumulative probability function graph",xlab="Binomial random variable",
#' ylab="Cumulative probability function values",xlim = c(0,10),ylim = c(0,1))
#' for (i in 1:4)
#' {
#' lines(0:10,pBetaBin(0:10,10,a[i],a[i]),col = col[i])
#' points(0:10,pBetaBin(0:10,10,a[i],a[i]),col = col[i])
#' }
#'
#' pBetaBin(0:10,10,4,.2)   #acquiring the cumulative probability values
#'
#' @export
dBetaBin<-function(x,n,a,b)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,n,a,b))) | any(is.infinite(c(x,n,a,b))) |any(is.nan(c(x,n,a,b))))
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
    else
    {
      #checking if at any chance the binomial random variable is greater than binomial trial value
      #if so providing an error message and stopping the function from progress
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
      #for each random variable in the input vector below calculations occur
      ans<-sapply(1:length(x),function(i) choose(n,x[i])*(beta(a+x[i],n+b-x[i])/beta(a,b)))
    }
  }

  # generating an output in list format consisting pdf,mean,variance and overdispersion value
  return(list('pdf'=ans,'mean'=n*(a/(a+b)) ,
              'var'=n*((a*b)/(a+b)^2)*((a+b+n)/(a+b+1)) ,
              'over.dis.para'=1/(a+b+1)))
}

#' Beta-Binomial Distribution
#'
#' These functions provide the ability for generating probability function values and
#' cumulative probability function values for the Beta-Binomial Distribution.
#'
#' @usage
#' pBetaBin(x,n,a,b)
#'
#' @param x        vector of binomial random variables.
#' @param n        single value for no of binomial trials.
#' @param a        single value for shape parameter alpha representing as a.
#' @param b        single value for shape parameter beta representing as b.
#'
#' @details
#' Mixing Beta distribution with Binomial distribution will create the Beta-Binomial
#' distribution. The probability function and cumulative probability function can be
#' constructed and are denoted below.
#'
#' The cumulative probability function is the summation of probability function values.
#'
#' \deqn{P_{BetaBin}(x)= {n \choose x} \frac{B(a+x,n+b-x)}{B(a,b)} }
#' \deqn{a,b > 0}
#' \deqn{x = 0,1,2,3,...n}
#' \deqn{n = 1,2,3,...}
#'
#' The mean, variance and over dispersion are denoted as
#' \deqn{E_{BetaBin}[x]= \frac{na}{a+b} }
#' \deqn{Var_{BetaBin}[x]= \frac{(nab)}{(a+b)^2} \frac{(a+b+n)}{(a+b+1)} }
#' \deqn{over dispersion= \frac{1}{a+b+1} }
#'
#' Defined as  \code{B(a,b)} is the beta function.
#'
#' @return
#' The output of \code{pBetaBin} gives cumulative probability  values in vector form.
#'
#' @references
#' \insertRef{young2008pooling}{fitODBOD}
#' \insertRef{trenkler1996continuous}{fitODBOD}
#' \insertRef{hughes1993using}{fitODBOD}
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(5)
#' a <- c(1,2,5,10,0.2)
#' plot(0,0,main="Beta-binomial probability function graph",xlab="Binomial random variable",
#' ylab="Probability function values",xlim = c(0,10),ylim = c(0,0.5))
#' for (i in 1:5)
#' {
#' lines(0:10,dBetaBin(0:10,10,a[i],a[i])$pdf,col = col[i],lwd=2.85)
#' points(0:10,dBetaBin(0:10,10,a[i],a[i])$pdf,col = col[i],pch=16)
#' }
#'
#' dBetaBin(0:10,10,4,.2)$pdf    #extracting the pdf values
#' dBetaBin(0:10,10,4,.2)$mean   #extracting the mean
#' dBetaBin(0:10,10,4,.2)$var    #extracting the variance
#' dBetaBin(0:10,10,4,.2)$over.dis.para  #extracting the over dispersion value
#'
#' #plotting the random variables and cumulative probability values
#' col <- rainbow(4)
#' a <- c(1,2,5,10)
#' plot(0,0,main="Cumulative probability function graph",xlab="Binomial random variable",
#' ylab="Cumulative probability function values",xlim = c(0,10),ylim = c(0,1))
#' for (i in 1:4)
#' {
#' lines(0:10,pBetaBin(0:10,10,a[i],a[i]),col = col[i])
#' points(0:10,pBetaBin(0:10,10,a[i],a[i]),col = col[i])
#' }
#'
#' pBetaBin(0:10,10,4,.2)   #acquiring the cumulative probability values
#'
#' @export
pBetaBin<-function(x,n,a,b)
{
  #for each binomial random variable in the input vector the cumulative proability function
  #values are calculated
  ans<-sapply(1:length(x),function(i) sum(dBetaBin(0:x[i],n,a,b)$pdf))
  #generating an ouput vector cumulative probability function values
  return(ans)
}

#' Negative Log Likelihood value of Beta-Binomial Distribution
#'
#' This function will calculate the Negative Log Likelihood value when the vector of binomial random
#' variables and vector of corresponding frequencies are given with the shape parameters a and b.
#'
#' @usage
#' NegLLBetaBin(x,freq,a,b)
#'
#' @param x              vector of binomial random variables.
#' @param freq           vector of frequencies.
#' @param a              single value for shape parameter alpha representing as a.
#' @param b              single value for shape parameter beta representing as b.
#'
#' @details
#' \deqn{0 < a,b }
#' \deqn{freq \ge 0}
#' \deqn{x = 0,1,2,...}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions necessary error
#' messages will be provided to go further.
#'
#' @return
#' The output of \code{NegLLBetaBin} will produce a single numeric value.
#'
#' @references
#' \insertRef{young2008pooling}{fitODBOD}
#' \insertRef{trenkler1996continuous}{fitODBOD}
#' \insertRef{hughes1993using}{fitODBOD}
#'
#' @examples
#' No.D.D <- 0:7       #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)   #assigning the corresponding frequencies
#'
#' NegLLBetaBin(No.D.D,Obs.fre.1,.3,.4)   #acquiring the negative log likelihood value
#'
#' @export
NegLLBetaBin<-function(x,freq,a,b)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,freq,a,b))) | any(is.infinite(c(x,freq,a,b)))| any(is.nan(c(x,freq,a,b))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    #checking if any of the random variables of frequencies are less than zero if so
    #creating an error message as well as stopping the function progress
    if( any(c(x,freq)< 0) )
    {
      stop("Binomial random variable or frequency values cannot be negative")
    }
    #checking if shape parameters are less than or equal to zero
    #if so creating an error message as well as stopping the function progress
    else if(a <= 0 | b <= 0)
    {
      stop("Shape parameters cannot be less than or equal to zero")
    }
    else
    {
      #constructing the data set using the random variables vector and frequency vector
      data<-rep(x,freq)

      #calculating the negative log likelihood value and representing as a single output value
      return(-(sum(log(choose(max(x),data[1:sum(freq)]))) +
                 sum(log(beta(a+data[1:sum(freq)],max(x)+b-data[1:sum(freq)]))) -
                 sum(freq)*log(beta(a,b))))
    }
  }
}

#' Estimating the shape parameters a and b for Beta-Binomial Distribution
#'
#' The functions will estimate the shape parameters using the maximum log likelihood method and
#' moment generating function method for the Beta-Binomial distribution when the binomial
#' random variables and corresponding frequencies are given.
#'
#' @usage
#' EstMLEBetaBin(x,freq,a,b,...)
#'
#'
#' @param x             vector of binomial random variables.
#' @param freq          vector of frequencies.
#' @param a             single value for shape parameter alpha representing as a.
#' @param b             single value for shape parameter beta representing as b.
#' @param ...           mle2 function inputs except data and estimating parameter.
#'
#'
#' @details
#' \deqn{a,b > 0}
#' \deqn{x = 0,1,2,...}
#' \deqn{freq \ge 0}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions necessary error
#'  messages will be provided to go further.
#'
#' @return
#' \code{EstMLEBetaBin} here is used as a wrapper for the \code{mle2} function of \pkg{bbmle} package
#' therefore output is of class of mle2.
#'
#' @references
#' \insertRef{young2008pooling}{fitODBOD}
#' \insertRef{trenkler1996continuous}{fitODBOD}
#' \insertRef{hughes1993using}{fitODBOD}
#'
#' @seealso
#' \code{\link[bbmle]{mle2}}
#'
#' @examples
#' No.D.D <- 0:7        #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)   #assigning the corresponding frequencies
#'
#' #estimating the parameters using maximum log likelihood value and assigning it
#' estimate <- EstMLEBetaBin(No.D.D,Obs.fre.1,a=0.1,b=0.1)
#'
#' bbmle::coef(estimate)   #extracting the parameters
#'
#' #estimating the parameters using moment generating function methods
#' EstMGFBetaBin(No.D.D,Obs.fre.1)
#'
#'@export
EstMLEBetaBin<-function(x,freq,a,b,...)
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
  suppressWarnings2(bbmle::mle2(.EstMLEBetaBin,data=list(x=x,freq=freq),
                      start = list(a=a,b=b),...),"NaN")
}

.EstMLEBetaBin<-function(x,freq,a,b)
{
  #with respective to using bbmle package function mle2 there is no need impose any restrictions
  #therefor the output is directly a single numeric value for the negative log likelihood value of
  #beta binomial distribution
  data<-rep(x,freq)

  return(-(sum(log(choose(max(x),data[1:sum(freq)]))) +
             sum(log(beta(a+data[1:sum(freq)],max(x)+b-data[1:sum(freq)]))) -
             sum(freq)*log(beta(a,b))))
}

#' Estimating the shape parameters a and b for Beta-Binomial Distribution
#'
#' The functions will estimate the shape parameters using the maximum log likelihood method and
#' moment generating function method for the Beta-Binomial distribution when the binomial
#' random variables and corresponding frequencies are given.
#'
#' @usage
#' EstMGFBetaBin(x,freq)
#'
#' @param x                  vector of binomial random variables.
#' @param freq               vector of frequencies.
#'
#' @details
#' \deqn{a,b > 0}
#' \deqn{x = 0,1,2,...}
#' \deqn{freq \ge 0}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions necessary error
#' messages will be provided to go further.
#'
#' @return
#' The output of \code{EstMGFBetaBin} will produce the class \code{mgf} format consisting
#'
#' \code{a} shape parameter of beta distribution representing for alpha
#'
#' \code{b} shape parameter of beta distribution representing for beta
#'
#' \code{min} Negative loglikelihood value
#'
#' \code{AIC} AIC value
#'
#' \code{call} the inputs for the function
#'
#' Methods \code{print}, \code{summary}, \code{coef} and \code{AIC} can be used to extract
#' specific outputs.
#'
#' @references
#' \insertRef{young2008pooling}{fitODBOD}
#' \insertRef{trenkler1996continuous}{fitODBOD}
#' \insertRef{hughes1993using}{fitODBOD}
#'
#' @seealso
#' \code{\link[bbmle]{mle2}}
#'
#' @examples
#' No.D.D <- 0:7        #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)   #assigning the corresponding frequencies
#'
#' #estimating the parameters using maximum log likelihood value and assigning it
#' estimate <- EstMLEBetaBin(No.D.D,Obs.fre.1,a=0.1,b=0.1)
#'
#' bbmle::coef(estimate)   #extracting the parameters
#'
#' #estimating the parameters using moment generating function methods
#' results <- EstMGFBetaBin(No.D.D,Obs.fre.1)
#'
#' # extract the estimated parameters and summary
#' coef(results)
#' summary(results)
#'
#' AIC(results) #show the AIC value
#'
#' @export
EstMGFBetaBin<-function(x,freq)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,freq))) | any(is.infinite(c(x,freq))) | any(is.nan(c(x,freq))))
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    #constructing the data set using the random variables vector and frequency vector
    data<-rep(x,freq)
    #creating necessary equations
    m1<-sum(data)/length(data)
    m2<-sum(data^2)/length(data)
    #constructing equations according to theory for a(alpha) and b(beta)
    a<-((max(x)*m1-m2)*m1)/(max(x)*(m2-m1-m1^2)+m1^2)
    b<-((max(x)*m1-m2)*(max(x)-m1))/(max(x)*(m2-m1-m1^2)+m1^2)
    #generating an output of list for shape parameters a(alpha) and b(beta)

    ans<-list("a"=a,"b"=b,"min"=NegLLBetaBin(x,freq,a,b),
              "AIC"=2*2+(2*NegLLBetaBin(x,freq,a,b)),"call"=match.call())
    class(ans)<-"mgf"
    return(ans)
  }
}

#' @method EstMGFBetaBin default
#' @export
EstMGFBetaBin.default<-function(x,freq)
{
  return(EstMGFBetaBin(x,freq))
}

#' @method print mgf
#' @export
print.mgf<-function(x,...)
{
  cat("Call: \n")
  print(x$call)
  cat("\nCoefficients: \n")
  coeff<-c(x$a,x$b)
  names(coeff)<-c("a","b")
  print(coeff)
}

#' @method summary mgf
#' @export
summary.mgf<-function(object,...)
{
  cat("Coefficients: \n a \t  b \n", object$a,object$b)
  cat("\n\nNegative Log-likelihood : ",object$min)
  cat("\n\nAIC : ",object$AIC,"\n")
}

#' @method coef mgf
#' @export
coef.mgf<-function(object,...)
{
  cat(" \t  a \t  b \n", object$a,object$b)
}

#' @method AIC mgf
#' @export
AIC.mgf<-function(object,...)
{
  return(object$AIC)
}


#' Fitting the Beta-Binomial Distribution when binomial random variable, frequency and shape
#' parameters a and b are given
#'
#' The function will fit the Beta-Binomial distribution when random variables, corresponding
#' frequencies and shape parameters are given. It will provide the expected frequencies, chi-squared
#' test statistics value, p value, degree of freedom and over dispersion value so that it can be
#' seen if this distribution fits the data.
#'
#' @usage fitBetaBin(x,obs.freq,a,b)
#'
#' @param x                  vector of binomial random variables.
#' @param obs.freq           vector of frequencies.
#' @param a                  single value for shape parameter alpha representing as a.
#' @param b                  single value for shape parameter beta representing as b.
#'
#' @details
#' \deqn{0 < a,b}
#' \deqn{x = 0,1,2,...,n}
#' \deqn{obs.freq \ge 0}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#' The output of \code{fitBetaBin} gives the class format \code{fitBB} and \code{fit} consisting a list
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
#' \code{fitBB} fitted values of \code{dBetaBin}.
#'
#' \code{NegLL} Negative Log Likelihood value.
#'
#' \code{a} estimated value for alpha parameter as a.
#'
#' \code{b} estimated value for alpha parameter as b.
#'
#' \code{AIC} AIC value.
#'
#' \code{over.dis.para} over dispersion value.
#'
#' \code{call} the inputs of the function.
#'
#' Methods \code{summary}, \code{print}, \code{AIC}, \code{residuals} and \code{fitted} can be
#' used to extract specific outputs.
#'
#' @references
#' \insertRef{young2008pooling}{fitODBOD}
#' \insertRef{trenkler1996continuous}{fitODBOD}
#' \insertRef{hughes1993using}{fitODBOD}
#'
#' @seealso
#' \code{\link[bbmle]{mle2}}
#'
#' @examples
#' No.D.D <- 0:7    #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)  #assigning the corresponding frequencies
#'
#' #estimating the parameters using maximum log likelihood value and assigning it
#' parameters <- EstMLEBetaBin(No.D.D,Obs.fre.1,0.1,0.1)
#'
#' bbmle::coef(parameters)   #extracting the parameters a and b
#' aBetaBin <- bbmle::coef(parameters)[1]  #assigning the parameter a
#' bBetaBin <- bbmle::coef(parameters)[2]  #assigning the parameter b
#'
#' #fitting when the random variable,frequencies,shape parameter values are given.
#' fitBetaBin(No.D.D,Obs.fre.1,aBetaBin,bBetaBin)
#'
#' #estimating the parameters using moment generating function methods
#' results <- EstMGFBetaBin(No.D.D,Obs.fre.1)
#' results
#'
#' aBetaBin1 <- results$a  #assigning the estimated a
#' bBetaBin1 <- results$b  #assigning the estimated b
#'
#' #fitting when the random variable,frequencies,shape parameter values are given.
#' BB <- fitBetaBin(No.D.D,Obs.fre.1,aBetaBin1,bBetaBin1)
#'
#' #extracting the expected frequencies
#' fitted(BB)
#'
#' #extracting the residuals
#' residuals(BB)
#'
#' @export
fitBetaBin<-function(x,obs.freq,a,b)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,obs.freq,a,b))) | any(is.infinite(c(x,obs.freq,a,b))) |
     any(is.nan(c(x,obs.freq,a,b))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    est<-dBetaBin(x,max(x),a,b)
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

    NegLL<-NegLLBetaBin(x,obs.freq,a,b)
    names(NegLL)<-NULL
    #the final output is in a list format containing the calculated values
    final<-list("bin.ran.var"=x,"obs.freq"=obs.freq,"exp.freq"=exp.freq,
                "statistic"=round(statistic,4),"df"=df,"p.value"=round(p.value,4),
                "fitBB"=est,"NegLL"=NegLL,"a"=a,"b"=b, "AIC"=2*2+2*NegLL,
                "over.dis.para"=odp,"call"=match.call())
    class(final)<-c("fitBB","fit")
    return(final)
    }
  }

#' @method fitBetaBin default
#' @export
fitBetaBin.default<-function(x,obs.freq,a,b)
{
  return(fitBetaBin(x,obs.freq,a,b))
}

#' @method print fitBB
#' @export
print.fitBB<-function(x,...)
{
  cat("Call: \n")
  print(x$call)
  cat("\nChi-squared test for Beta-Binomial Distribution \n\t
          Observed Frequency : ",x$obs.freq,"\n\t
          expected Frequency : ",x$exp.freq,"\n\t
          estimated a parameter :",x$a, "  ,estimated b parameter :",x$b,"\n\t
          X-squared :",x$statistic,"  ,df :",x$df,"  ,p-value :",x$p.value,"\n\t
          over dispersion :",x$over.dis.para,"\n")
}

#' @method summary fitBB
#' @export
summary.fitBB<-function(object,...)
{
  cat("Call: \n")
  print(object$call)
  cat("\nChi-squared test for Beta-Binomial Distribution \n\t
      Observed Frequency : ",object$obs.freq,"\n\t
      expected Frequency : ",object$exp.freq,"\n\t
      estimated a parameter :",object$a,"  ,estimated b parameter :",object$b,"\n\t
      X-squared :",object$statistic,"  ,df :",object$df,"  ,p-value :",object$p.value,"\n\t
      over dispersion :",object$over.dis.para,"\n\t
      Negative Loglikehood value :",object$NegLL,"\n\t
      AIC value :",object$AIC,"\n")
}
