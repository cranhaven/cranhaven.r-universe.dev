#' Gamma Distribution
#'
#' These functions provide the ability for generating probability density values,
#' cumulative probability density values and moment about zero values for
#' Gamma Distribution bounded between [0,1].
#'
#' @usage
#' dGAMMA(p,c,l)
#'
#' @param p                vector of probabilities.
#' @param c                single value for shape parameter c.
#' @param l                single value for shape parameter l.
#'
#' @details
#' The probability density function and cumulative density function of a
#' unit bounded Gamma distribution with random variable P are given by
#'
#' \deqn{g_{P}(p) = \frac{ c^l p^{c-1}}{\gamma(l)} [ln(1/p)]^{l-1} } ;    \eqn{0 \le p \le 1}
#' \deqn{G_{P}(p) = \frac{ Ig(l,cln(1/p))}{\gamma(l)} } ; \eqn{0 \le p \le 1}
#' \deqn{l,c > 0}
#'
#' The mean the variance are denoted by
#' \deqn{E[P] = (\frac{c}{c+1})^l }
#' \deqn{var[P] = (\frac{c}{c+2})^l - (\frac{c}{c+1})^{2l} }
#'
#' The moments about zero is denoted as
#' \deqn{E[P^r]=(\frac{c}{c+r})^l }
#' \eqn{r = 1,2,3,...}
#'
#' Defined as \eqn{\gamma(l) } is the gamma function
#' Defined as \eqn{Ig(l,cln(1/p))= \int_0^{cln(1/p)} t^{l-1} e^{-t}dt } is the Lower incomplete gamma function
#'
#'
#' \strong{NOTE} : If input parameters are not in given domain  conditions necessary error
#' messages will be provided to go further.
#'
#' @return
#' The output of \code{dGAMMA} gives a list format consisting
#'
#' \code{pdf}                   probability density values in vector form.
#'
#' \code{mean}                  mean of the Gamma distribution.
#'
#' \code{var}                   variance of Gamma distribution.
#'
#' @references
#' \insertRef{olshen1938transformations}{fitODBOD}
#'
#' @seealso
#' \code{\link[stats]{GammaDist}}
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(4)
#' a <- c(1,2,5,10)
#' plot(0,0,main="Probability density graph",xlab="Random variable",ylab="Probability density values",
#' xlim = c(0,1),ylim = c(0,4))
#' for (i in 1:4)
#' {
#' lines(seq(0,1,by=0.01),dGAMMA(seq(0,1,by=0.01),a[i],a[i])$pdf,col = col[i])
#' }
#'
#' dGAMMA(seq(0,1,by=0.01),5,6)$pdf   #extracting the pdf values
#' dGAMMA(seq(0,1,by=0.01),5,6)$mean  #extracting the mean
#' dGAMMA(seq(0,1,by=0.01),5,6)$var   #extracting the variance
#'
#' #plotting the random variables and cumulative probability values
#' col <- rainbow(4)
#' a <- c(1,2,5,10)
#' plot(0,0,main="Cumulative density graph",xlab="Random variable",ylab="Cumulative density values",
#' xlim = c(0,1),ylim = c(0,1))
#' for (i in 1:4)
#' {
#' lines(seq(0,1,by=0.01),pGAMMA(seq(0,1,by=0.01),a[i],a[i]),col = col[i])
#' }
#'
#' pGAMMA(seq(0,1,by=0.01),5,6)   #acquiring the cumulative probability values
#' mazGAMMA(1.4,5,6)              #acquiring the moment about zero values
#' mazGAMMA(2,5,6)-mazGAMMA(1,5,6)^2 #acquiring the variance for a=5,b=6
#'
#' #only the integer value of moments is taken here because moments cannot be decimal
#' mazGAMMA(1.9,5.5,6)
#'
#' @importFrom Rdpack reprompt
#' @export
dGAMMA<-function(p,c,l)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(p,c,l))) | any(is.infinite(c(p,c,l))) | any(is.nan(c(p,c,l))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    #checking if shape parameters are greater than zero, if not providing error message and
    #stopping the function progress
    if(c <= 0 | l <= 0)
    {
      stop("Shape parameters cannot be less than or equal to zero")
    }
    else
    {
      #for each input values in the vector necessary calculations and conditions are applied
      if(any(p<0) | any(p>1)){
        stop("Invalid values in the input")
      }
      ans<-sapply(1:length(p),function(i) (c^l* p[i]^(c-1)*(log(1/p[i]))^(l-1))/gamma(l))
    }
    # generating an output in list format consisting pdf,mean and variance
    return(list("pdf"=ans,"mean"=(c/(c+1))^l,
                "var"=(c/(c+2))^l - (c/(c+1))^(2*l)))
  }
}

#' Gamma Distribution
#'
#' These functions provide the ability for generating probability density values,
#' cumulative probability density values and moment about zero values for
#' Gamma Distribution bounded between [0,1].
#'
#' @usage
#' pGAMMA(p,c,l)
#'
#' @param p                vector of probabilities.
#' @param c                single value for shape parameter c.
#' @param l                single value for shape parameter l.
#'
#' @details
#' The probability density function and cumulative density function of a
#' unit bounded Gamma distribution with random variable P are given by
#'
#' \deqn{g_{P}(p) = \frac{ c^l p^{c-1}}{\gamma(l)} [ln(1/p)]^{l-1} } ;    \eqn{0 \le p \le 1}
#' \deqn{G_{P}(p) = \frac{ Ig(l,cln(1/p))}{\gamma(l)} } ; \eqn{0 \le p \le 1}
#' \deqn{l,c > 0}
#'
#' The mean the variance are denoted by
#' \deqn{E[P] = (\frac{c}{c+1})^l }
#' \deqn{var[P] = (\frac{c}{c+2})^l - (\frac{c}{c+1})^{2l} }
#'
#' The moments about zero is denoted as
#' \deqn{E[P^r]=(\frac{c}{c+r})^l }
#' \eqn{r = 1,2,3,...}
#'
#' Defined as \eqn{\gamma(l) } is the gamma function.
#' Defined as \eqn{Ig(l,cln(1/p))= \int_0^{cln(1/p)} t^{l-1} e^{-t}dt } is the Lower incomplete gamma function.
#'
#'
#' \strong{NOTE} : If input parameters are not in given domain  conditions necessary error
#' messages will be provided to go further.
#'
#' @return
#' The output of \code{pGAMMA} gives the cumulative density values in vector form.
#'
#' @references
#' \insertRef{olshen1938transformations}{fitODBOD}
#'
#' @seealso
#' \code{\link[stats]{GammaDist}}
#'
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(4)
#' a <- c(1,2,5,10)
#' plot(0,0,main="Probability density graph",xlab="Random variable",ylab="Probability density values",
#' xlim = c(0,1),ylim = c(0,4))
#' for (i in 1:4)
#' {
#' lines(seq(0,1,by=0.01),dGAMMA(seq(0,1,by=0.01),a[i],a[i])$pdf,col = col[i])
#' }
#'
#' dGAMMA(seq(0,1,by=0.01),5,6)$pdf   #extracting the pdf values
#' dGAMMA(seq(0,1,by=0.01),5,6)$mean  #extracting the mean
#' dGAMMA(seq(0,1,by=0.01),5,6)$var   #extracting the variance
#'
#' #plotting the random variables and cumulative probability values
#' col <- rainbow(4)
#' a <- c(1,2,5,10)
#' plot(0,0,main="Cumulative density graph",xlab="Random variable",ylab="Cumulative density values",
#' xlim = c(0,1),ylim = c(0,1))
#' for (i in 1:4)
#' {
#' lines(seq(0,1,by=0.01),pGAMMA(seq(0,1,by=0.01),a[i],a[i]),col = col[i])
#' }
#'
#' pGAMMA(seq(0,1,by=0.01),5,6)   #acquiring the cumulative probability values
#' mazGAMMA(1.4,5,6)              #acquiring the moment about zero values
#' mazGAMMA(2,5,6)-mazGAMMA(1,5,6)^2 #acquiring the variance for a=5,b=6
#'
#' #only the integer value of moments is taken here because moments cannot be decimal
#' mazGAMMA(1.9,5.5,6)
#'
#' @export
pGAMMA<-function(p,c,l)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(p,c,l))) | any(is.infinite(c(p,c,l))) | any(is.nan(c(p,c,l))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    #checking if shape parameters are greater than zero and if not providing an error message
    #and stopping the function progress
    if(c <= 0 | l <= 0)
    {
      stop("Shape parameters cannot be less than or equal to zero")
    }
    else
    {
      #for each input values in the vector necessary calculations and conditions are applied
      if(any(p<0) | any(p>1)){
        stop("Invalid values in the input")
      }
      #integrating the above mentioned function under limits of zero and vector p
      ans<-sapply(1:length(p),function(i){
        val<-stats::integrate(function(t){(t^(l-1))*(exp(-t))},lower = 0,upper = (c*log(1/p[i])))
        val$value/gamma(l) })

      #generating an ouput vector of cumulative probability values
      return(ans)
    }
  }
}

#' Gamma Distribution
#'
#' These functions provide the ability for generating probability density values,
#' cumulative probability density values and moment about zero values for
#' Gamma Distribution bounded between [0,1].
#'
#' @usage
#' mazGAMMA(r,c,l)
#'
#' @param r                vector of moments.
#' @param c                single value for shape parameter c.
#' @param l                single value for shape parameter l.
#'
#' @details
#' The probability density function and cumulative density function of a
#' unit bounded Gamma distribution with random variable P are given by
#'
#' \deqn{g_{P}(p) = \frac{ c^l p^{c-1}}{\gamma(l)} [ln(1/p)]^{l-1} } ;    \eqn{0 \le p \le 1}
#' \deqn{G_{P}(p) = \frac{ Ig(l,cln(1/p))}{\gamma(l)} } ; \eqn{0 \le p \le 1}
#' \deqn{l,c > 0}
#'
#' The mean the variance are denoted by
#' \deqn{E[P] = (\frac{c}{c+1})^l }
#' \deqn{var[P] = (\frac{c}{c+2})^l - (\frac{c}{c+1})^{2l} }
#'
#' The moments about zero is denoted as
#' \deqn{E[P^r]=(\frac{c}{c+r})^l }
#' \eqn{r = 1,2,3,...}
#'
#' Defined as \eqn{\gamma(l) } is the gamma function.
#' Defined as \eqn{Ig(l,cln(1/p))= \int_0^{cln(1/p)} t^{l-1} e^{-t}dt } is the Lower incomplete gamma function.
#'
#'
#' \strong{NOTE} : If input parameters are not in given domain  conditions necessary error
#' messages will be provided to go further.
#'
#' @return
#' The output of \code{mazGAMMA} gives the moments about zero in vector form.
#'
#' @references
#' \insertRef{olshen1938transformations}{fitODBOD}
#'
#' @seealso
#' \code{\link[stats]{GammaDist}}
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(4)
#' a <- c(1,2,5,10)
#' plot(0,0,main="Probability density graph",xlab="Random variable",ylab="Probability density values",
#' xlim = c(0,1),ylim = c(0,4))
#' for (i in 1:4)
#' {
#' lines(seq(0,1,by=0.01),dGAMMA(seq(0,1,by=0.01),a[i],a[i])$pdf,col = col[i])
#' }
#'
#' dGAMMA(seq(0,1,by=0.01),5,6)$pdf   #extracting the pdf values
#' dGAMMA(seq(0,1,by=0.01),5,6)$mean  #extracting the mean
#' dGAMMA(seq(0,1,by=0.01),5,6)$var   #extracting the variance
#'
#' #plotting the random variables and cumulative probability values
#' col <- rainbow(4)
#' a <- c(1,2,5,10)
#' plot(0,0,main="Cumulative density graph",xlab="Random variable",ylab="Cumulative density values",
#' xlim = c(0,1),ylim = c(0,1))
#' for (i in 1:4)
#' {
#' lines(seq(0,1,by=0.01),pGAMMA(seq(0,1,by=0.01),a[i],a[i]),col = col[i])
#' }
#'
#' pGAMMA(seq(0,1,by=0.01),5,6)   #acquiring the cumulative probability values
#' mazGAMMA(1.4,5,6)              #acquiring the moment about zero values
#' mazGAMMA(2,5,6)-mazGAMMA(1,5,6)^2 #acquiring the variance for a=5,b=6
#'
#' #only the integer value of moments is taken here because moments cannot be decimal
#' mazGAMMA(1.9,5.5,6)
#'
#' @export
mazGAMMA<-function(r,c,l)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(r,c,l))) | any(is.infinite(c(r,c,l))) | any(is.nan(c(r,c,l))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    #checking if shape parameters are greater than zero, and if not providing an error
    #message and stopping the function progress
    if(c <= 0 | l <= 0)
    {
      stop("Shape parameters cannot be less than or equal to zero")
    }
    else
    {
      #the moments cannot be a decimal value therefore converting it into an integer
      r<-as.integer(r)
      #for each input values in the vector necessary calculations and conditions are applied
      if(any(r<=0)){
        stop("Moments cannot be less than or equal to zero")
      }
      #checking if moment values are less than or equal to zero and creating
      # an error message as well as stopping the function progress
      ans<-sapply(1:length(r),function(i) (c/(c+r[i]))^l)
      #generating an ouput vector of moment about zero values
      return(ans)
    }
  }
}

#' Gamma Binomial Distribution
#'
#' These functions provide the ability for generating probability function values and
#' cumulative probability function values for the Gamma Binomial Distribution.
#'
#' @usage
#' dGammaBin(x,n,c,l)
#'
#' @param x             vector of binomial random variables.
#' @param n             single value for no of binomial trials.
#' @param c             single value for shape parameter c.
#' @param l             single value for shape parameter l.
#'
#' @details
#' Mixing Gamma distribution with Binomial distribution will create the the Gamma Binomial
#' distribution. The probability function and cumulative probability function can be
#' constructed and are denoted below.
#'
#' The cumulative probability function is the summation of probability function values.
#'
#' \deqn{P_{GammaBin}[x]= {n \choose x} \sum_{j=0}^{n-x} {n-x \choose j} (-1)^j (\frac{c}{c+x+j})^l }
#' \deqn{c,l > 0}
#' \deqn{x = 0,1,2,...,n}
#' \deqn{n = 1,2,3,...}
#'
#' The mean, variance and over dispersion are denoted as
#' \deqn{E_{GammaBin}[x] = (\frac{c}{c+1})^l}
#' \deqn{Var_{GammaBin}[x] = n^2[(\frac{c}{c+2})^l - (\frac{c}{c+1})^{2l}] + n(\frac{c}{c+1})^l{1-)(\frac{c+1}{c+2})^l}}
#' \deqn{over dispersion= \frac{(\frac{c}{c+2})^l - (\frac{c}{c+1})^{2l}}{(\frac{c}{c+1})^l[1-(\frac{c}{c+1})^l]}}
#'
#' @return
#' The output of \code{dGammaBin} gives a list format consisting
#'
#' \code{pdf}               probability function values in vector form.
#'
#' \code{mean}              mean of the Gamma Binomial Distribution.
#'
#' \code{var}               variance of the Gamma Binomial Distribution.
#'
#' \code{over.dis.para}     over dispersion value of the Gamma Binomial Distribution.
#'
#' @references
#' \insertRef{grassia1977family}{fitODBOD}
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(5)
#' a <- c(1,2,5,10,0.2)
#' plot(0,0,main="Gamma Binomial probability function graph",xlab="Binomial random variable",
#' ylab="Probability function values",xlim = c(0,10),ylim = c(0,0.5))
#' for (i in 1:5)
#' {
#' lines(0:10,dGammaBin(0:10,10,a[i],a[i])$pdf,col = col[i],lwd=2.85)
#' points(0:10,dGammaBin(0:10,10,a[i],a[i])$pdf,col = col[i],pch=16)
#' }
#'
#' dGammaBin(0:10,10,4,.2)$pdf    #extracting the pdf values
#' dGammaBin(0:10,10,4,.2)$mean   #extracting the mean
#' dGammaBin(0:10,10,4,.2)$var    #extracting the variance
#' dGammaBin(0:10,10,4,.2)$over.dis.para  #extracting the over dispersion value
#'
#' #plotting the random variables and cumulative probability values
#' col <- rainbow(4)
#' a <- c(1,2,5,10)
#' plot(0,0,main="Cumulative probability function graph",xlab="Binomial random variable",
#' ylab="Cumulative probability function values",xlim = c(0,10),ylim = c(0,1))
#' for (i in 1:4)
#' {
#' lines(0:10,pGammaBin(0:10,10,a[i],a[i]),col = col[i])
#' points(0:10,pGammaBin(0:10,10,a[i],a[i]),col = col[i])
#' }
#'
#' pGammaBin(0:10,10,4,.2)   #acquiring the cumulative probability values
#'
#' @export
dGammaBin<-function(x,n,c,l)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,n,c,l))) | any(is.infinite(c(x,n,c,l))) |any(is.nan(c(x,n,c,l))))
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    #checking if shape parameters are less than or equal zero ,
    #if so providing an error message and stopping the function progress
    if(c <= 0 | l <= 0)
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
      else if(any(x<0) | n<0)
      {
        #checking if any random variable or trial value is negative if so providig an error message
        #and stopping the function progress
        stop("Binomial random variable or binomial trial value cannot be negative")
      }
      #for each random variable in the input vector below calculations occur
      ans<-sapply(1:length(x),function(i) choose(n,x[i])*sum((-1)^(0:(n-x[i]))*choose(n-x[i],(0:(n-x[i])))*
                                                               (c/(c+x[i]+(0:(n-x[i]))))^l))
    }
  }
  # generating an output in list format consisting pdf,mean,variance and overdispersion value
  return(list('pdf'=ans,'mean'=n*((c/(c+1))^l),
              'var'=(n^2)*((c/(c+2))^l-(c/(c+1))^(2*l))+(n*(c/(c+1))^l)*(1-((c+1)/(c+2))^l),
              'over.dis.para'=((c/(c+2))^l-(c/(c+1))^(2*l))/(((c/(c+1))^l)*(1-(c/(c+1))^l))))
}

#' Gamma Binomial Distribution
#'
#' These functions provide the ability for generating probability function values and
#' cumulative probability function values for the Gamma Binomial Distribution.
#'
#' @usage
#' pGammaBin(x,n,c,l)
#'
#' @param x             vector of binomial random variables.
#' @param n             single value for no of binomial trials.
#' @param c             single value for shape parameter c.
#' @param l             single value for shape parameter l.
#'
#' @details
#' Mixing Gamma distribution with Binomial distribution will create the the Gamma Binomial
#' distribution. The probability function and cumulative probability function can be
#' constructed and are denoted below.
#'
#' The cumulative probability function is the summation of probability function values.
#'
#' \deqn{P_{GammaBin}[x]= {n \choose x} \sum_{j=0}^{n-x} {n-x \choose j} (-1)^j (\frac{c}{c+x+j})^l }
#' \deqn{c,l > 0}
#' \deqn{x = 0,1,2,...,n}
#' \deqn{n = 1,2,3,...}
#'
#' The mean, variance and over dispersion are denoted as
#' \deqn{E_{GammaBin}[x] = (\frac{c}{c+1})^l}
#' \deqn{Var_{GammaBin}[x] = n^2[(\frac{c}{c+2})^l - (\frac{c}{c+1})^{2l}] + n(\frac{c}{c+1})^l{1-)(\frac{c+1}{c+2})^l}}
#' \deqn{over dispersion= \frac{(\frac{c}{c+2})^l - (\frac{c}{c+1})^{2l}}{(\frac{c}{c+1})^l[1-(\frac{c}{c+1})^l]}}
#'
#' @return
#' The output of \code{pGammaBin} gives cumulative probability  values in vector form.
#'
#' @references
#' \insertRef{grassia1977family}{fitODBOD}
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(5)
#' a <- c(1,2,5,10,0.2)
#' plot(0,0,main="Gamma-binomial probability function graph",xlab="Binomial random variable",
#' ylab="Probability function values",xlim = c(0,10),ylim = c(0,0.5))
#' for (i in 1:5)
#' {
#' lines(0:10,dGammaBin(0:10,10,a[i],a[i])$pdf,col = col[i],lwd=2.85)
#' points(0:10,dGammaBin(0:10,10,a[i],a[i])$pdf,col = col[i],pch=16)
#' }
#'
#' dGammaBin(0:10,10,4,.2)$pdf    #extracting the pdf values
#' dGammaBin(0:10,10,4,.2)$mean   #extracting the mean
#' dGammaBin(0:10,10,4,.2)$var    #extracting the variance
#' dGammaBin(0:10,10,4,.2)$over.dis.para  #extracting the over dispersion value
#'
#' #plotting the random variables and cumulative probability values
#' col <- rainbow(4)
#' a <- c(1,2,5,10)
#' plot(0,0,main="Cumulative probability function graph",xlab="Binomial random variable",
#' ylab="Cumulative probability function values",xlim = c(0,10),ylim = c(0,1))
#' for (i in 1:4)
#' {
#' lines(0:10,pGammaBin(0:10,10,a[i],a[i]),col = col[i])
#' points(0:10,pGammaBin(0:10,10,a[i],a[i]),col = col[i])
#' }
#'
#' pGammaBin(0:10,10,4,.2)   #acquiring the cumulative probability values
#'
#' @export
pGammaBin<-function(x,n,c,l)
{
  #for each binomial random variable in the input vector the cumulative proability function
  #values are calculated
  ans<-sapply(1:length(x),function(i) sum(dGammaBin(0:x[i],n,c,l)$pdf))
  #generating an ouput vector cumulative probability function values
  return(ans)
}

#' Negative Log Likelihood value of Gamma Binomial Distribution
#'
#' This function will calculate  the Negative Log Likelihood value when the vector of binomial random
#' variables and vector of corresponding frequencies are given with the shape parameters l and c.
#'
#' @usage
#' NegLLGammaBin(x,freq,c,l)
#'
#' @param x                  vector of binomial random variables.
#' @param freq               vector of frequencies.
#' @param c                  single value for shape parameter c.
#' @param l                  single value for shape parameter l.
#'
#' @details
#' \deqn{0 < l,c}
#' \deqn{freq \ge 0}
#' \deqn{x = 0,1,2,...}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions necessary error
#' messages will be provided to go further.
#'
#' @return
#' The output of \code{NegLLGammaBin} will produce a single numeric value.
#'
#' @references
#' \insertRef{grassia1977family}{fitODBOD}
#'
#' @examples
#' No.D.D <- 0:7       #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)   #assigning the corresponding frequencies
#'
#' NegLLGammaBin(No.D.D,Obs.fre.1,.3,.4)   #acquiring the negative log likelihood value
#'
#' @export
NegLLGammaBin<-function(x,freq,c,l)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,freq,c,l))) | any(is.infinite(c(x,freq,c,l)))|any(is.nan(c(x,freq,c,l))) )
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
    else if(c <= 0 | l <= 0)
    {
      stop("Shape parameters cannot be less than or equal to zero")
    }
    else
    {
      #constructing the data set using the random variables vector and frequency vector
      n<-max(x)
      data<-rep(x,freq)
      value<-sapply(1:sum(freq),function(i) sum((-1)^(0:(n-data[i]))*choose(n-data[i],(0:(n-data[i])))*
                                                  (c/(c+data[i]+(0:(n-data[i]))))^l))

      #calculating the negative log likelihood value and representing as a single output value
      return(-(sum(log(choose(n,data[1:sum(freq)])))+sum(log(value))))
    }
  }
}

#' Estimating the shape parameters c and l for Gamma Binomial distribution
#'
#' The function will estimate the shape parameters using the maximum log likelihood method
#' for the Gamma Binomial distribution when the binomial random variables and corresponding frequencies
#' are given.
#'
#' @usage
#' EstMLEGammaBin(x,freq,c,l,...)
#'
#' @param x                   vector of binomial random variables.
#' @param freq                vector of frequencies.
#' @param c                   single value for shape parameter c.
#' @param l                   single value for shape parameter l.
#' @param ...                 mle2 function inputs except data and estimating parameter.
#'
#' @details
#' \deqn{0 < c,l}
#' \deqn{x = 0,1,2,...}
#' \deqn{freq \ge 0}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions necessary
#' error messages will be provided to go further.
#'
#' @return
#' \code{EstMLEGammaBin} here is used as a wrapper for the \code{mle2} function of \pkg{bbmle} package
#' therefore output is of class of mle2.
#'
#' @references
#' \insertRef{grassia1977family}{fitODBOD}
#'
#' @examples
#' No.D.D <- 0:7                   #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)  #assigning the corresponding frequencies
#'
#' #estimating the parameters using maximum log likelihood value and assigning it
#' parameters <- EstMLEGammaBin(x=No.D.D,freq=Obs.fre.1,c=0.1,l=0.1)
#'
#' bbmle::coef(parameters)         #extracting the parameters
#'
#' @export
EstMLEGammaBin<-function(x,freq,c,l,...)
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
  output<-suppressWarnings2(bbmle::mle2(.EstMLEGammaBin,data=list(x=x,freq=freq),
                                        start = list(c=c,l=l),...),"NaN")
  return(output)
}


.EstMLEGammaBin<-function(x,freq,c,l)
{
  #with respective to using bbmle package function mle2 there is no need impose any restrictions
  #therefor the output is directly a single numeric value for the negative log likelihood value of
  #Gamma binomial distribution
  n<-max(x)
  data<-rep(x,freq)

  value<-sapply(1:sum(freq),function(i) sum(((-1)^(0:n-data[i]))*choose(n-data[i],(0:n-data[i]))*
                                              (c/(c+data[i]+(0:n-data[i])))^l))

  return(-(sum(log(choose(n,data[1:sum(freq)])))+sum(log(value))))
}

#' Fitting the Gamma Binomial distribution when binomial random variable,
#' frequency and shape parameters are given
#'
#' The function will fit the Gamma Binomial Distribution when random variables,
#' corresponding frequencies and shape parameters are given. It will provide
#' the expected frequencies, chi-squared test statistics value, p value, degree of freedom
#' and over dispersion value so that it can be seen if this distribution fits the data.
#'
#' @usage fitGammaBin(x,obs.freq,c,l)
#'
#' @param x                vector of binomial random variables.
#' @param obs.freq         vector of frequencies.
#' @param c                single value for shape parameter c.
#' @param l                single value for shape parameter l.
#'
#' @details
#' \deqn{0 < c,l}
#' \deqn{x = 0,1,2,...}
#' \deqn{obs.freq \ge 0}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions necessary
#' error messages will be provided to go further.
#'
#' @return
#' The output of \code{fitGammaBin} gives the class format \code{fitGaB} and \code{fit} consisting a list
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
#' \code{fitMB} fitted values of \code{dGammaBin}.
#'
#' \code{NegLL} Negative Log Likelihood value.
#'
#' \code{c} estimated value for shape parameter c.
#'
#' \code{l} estimated value for shape parameter l.
#'
#' \code{AIC} AIC value.
#'
#' \code{over.dis.para} over dispersion value.
#'
#' \code{call} the inputs of the function.
#'
#' Methods \code{summary}, \code{print}, \code{AIC}, \code{residuals} and \code{fitted} can be used to
#' extract specific outputs.
#'
#' @references
#' \insertRef{grassia1977family}{fitODBOD}
#'
#' @examples
#' No.D.D <- 0:7       #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)          #assigning the corresponding frequencies
#'
#' #estimating the parameters using maximum log likelihood value and assigning it
#' parameters <- EstMLEGammaBin(x=No.D.D,freq=Obs.fre.1,c=0.1,l=0.1)
#'
#' cGBin <- bbmle::coef(parameters)[1]         #assigning the estimated c
#' lGBin <- bbmle::coef(parameters)[2]         #assigning the estimated l
#'
#' #fitting when the random variable,frequencies,shape parameter values are given.
#' results <- fitGammaBin(No.D.D,Obs.fre.1,cGBin,lGBin)
#' results
#'
#' #extracting the expected frequencies
#' fitted(results)
#'
#' #extracting the residuals
#' residuals(results)
#'
#' @export
fitGammaBin<-function(x,obs.freq,c,l)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,obs.freq,c,l))) | any(is.infinite(c(x,obs.freq,c,l))) |any(is.nan(c(x,obs.freq,c,l))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    est<-dGammaBin(x,max(x),c,l)
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
    NegLL<-NegLLGammaBin(x,obs.freq,c,l)
    names(NegLL)<-NULL
    #the final output is in a list format containing the calculated values
    final<-list("bin.ran.var"=x,"obs.freq"=obs.freq,"exp.freq"=exp.freq,
                "statistic"=round(statistic,4),"df"=df,"p.value"=round(p.value,4),
                "fitGaB"=est,"NegLL"=NegLL,"c"=c,"l"=l,"AIC"=2*2+2*NegLL,
                "over.dis.para"=est$over.dis.para,"call"=match.call())
    class(final)<-c("fitGaB","fit")
    return(final)
  }
}


#' @method fitGammaBin default
#' @export
fitGammaBin.default<-function(x,obs.freq,c,l)
{
  return(fitGammaBin(x,obs.freq,c,l))
}

#' @method print fitGaB
#' @export
print.fitGaB<-function(x,...)
{
  cat("Call: \n")
  print(x$call)
  cat("\nChi-squared test for Gamma Binomial Distribution \n\t
      Observed Frequency : ",x$obs.freq,"\n\t
      expected Frequency : ",x$exp.freq,"\n\t
      estimated c parameter :",x$c, "  ,estimated l parameter :",x$l," \n\t
      X-squared :",x$statistic,"  ,df :",x$df,"  ,p-value :",x$p.value,"\n\t
      over dispersion :",x$over.dis.para,"\n")
}

#' @method summary fitGaB
#' @export
summary.fitGaB<-function(object,...)
{
  cat("Call: \n")
  print(object$call)
  cat("\nChi-squared test for Gamma Binomial Distribution \n\t
      Observed Frequency : ",object$obs.freq,"\n\t
      expected Frequency : ",object$exp.freq,"\n\t
      estimated c parameter :",object$c,"  ,estimated l parameter :",object$l,"\n\t
      X-squared :",object$statistic,"  ,df :",object$df,"  ,p-value :",object$p.value,"\n\t
      over dispersion :",object$over.dis.para,"\n\t
      Negative Loglikehood value :",object$NegLL,"\n\t
      AIC value :",object$AIC,"\n")
}

#' Grassia-II-Binomial Distribution
#'
#' These functions provide the ability for generating probability function values and
#' cumulative probability function values for the Grassia-II-Binomial Distribution.
#'
#' @usage
#' dGrassiaIIBin(x,n,a,b)
#'
#' @param x             vector of binomial random variables.
#' @param n             single value for no of binomial trials.
#' @param a             single value for shape parameter a.
#' @param b             single value for shape parameter b.
#'
#' @details
#' Mixing Gamma distribution with Binomial distribution will create the the Grassia-II-Binomial
#' distribution, only when (1-p)=e^(-lambda) of the Binomial distribution. The probability function and
#' cumulative probability function can be constructed and are denoted below.
#'
#' The cumulative probability function is the summation of probability function values.
#'
#' \deqn{P_{GrassiaIIBin}[x]= {n \choose x} \sum_{j=0}^{x} {x \choose j} (-1)^{x-j} (1+b(n-j))^{-a} }
#' \deqn{a,b > 0}
#' \deqn{x = 0,1,2,...,n}
#' \deqn{n = 1,2,3,...}
#'
#' The mean, variance and over dispersion are denoted as
#' \deqn{E_{GrassiaIIBin}[x] = (\frac{b}{b+1})^a}
#' \deqn{Var_{GrassiaIIBin}[x] = n^2[(\frac{b}{b+2})^a - (\frac{b}{b+1})^{2a}] + n(\frac{b}{b+1})^a{1-(\frac{b+1}{b+2})^a}}
#' \deqn{over dispersion= \frac{(\frac{b}{b+2})^l - (\frac{b}{b+1})^{2a}}{(\frac{b}{b+1})^a[1-(\frac{b}{b+1})^a]}}
#'
#' @return
#' The output of \code{dGrassiaIIBin} gives a list format consisting
#'
#' \code{pdf}               probability function values in vector form.
#'
#' \code{mean}              mean of the Grassia II Binomial Distribution.
#'
#' \code{var}               variance of the Grassia II Binomial Distribution.
#'
#' \code{over.dis.para}     over dispersion value of the Grassia II Binomial Distribution.
#'
#' @references
#' \insertRef{grassia1977family}{fitODBOD}
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(5)
#' a <- c(1,2,5,10,0.2)
#' plot(0,0,main="Grassia II binomial probability function graph",xlab="Binomial random variable",
#' ylab="Probability function values",xlim = c(0,10),ylim = c(0,0.5))
#' for (i in 1:5)
#' {
#' lines(0:10,dGrassiaIIBin(0:10,10,a[i],a[i])$pdf,col = col[i],lwd=2.85)
#' points(0:10,dGrassiaIIBin(0:10,10,a[i],a[i])$pdf,col = col[i],pch=16)
#' }
#'
#' dGrassiaIIBin(0:10,10,4,.2)$pdf    #extracting the pdf values
#' dGrassiaIIBin(0:10,10,4,.2)$mean   #extracting the mean
#' dGrassiaIIBin(0:10,10,4,.2)$var    #extracting the variance
#' dGrassiaIIBin(0:10,10,4,.2)$over.dis.para  #extracting the over dispersion value
#'
#' #plotting the random variables and cumulative probability values
#' col <- rainbow(4)
#' a <-c (1,2,5,10)
#' plot(0,0,main="Cumulative probability function graph",xlab="Binomial random variable",
#' ylab="Cumulative probability function values",xlim = c(0,10),ylim = c(0,1))
#' for (i in 1:4)
#' {
#' lines(0:10,pGrassiaIIBin(0:10,10,a[i],a[i]),col = col[i])
#' points(0:10,pGrassiaIIBin(0:10,10,a[i],a[i]),col = col[i])
#' }
#'
#' pGrassiaIIBin(0:10,10,4,.2)   #acquiring the cumulative probability values
#'
#' @export
dGrassiaIIBin<-function(x,n,a,b)
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
      ans<-sapply(1:length(x),function(i) choose(n,x[i])*sum((-1)^(x[i]-(0:x[i]))*
                                                               choose(x[i],0:x[i])*(1+b*(n-(0:x[i])))^(-a)))
    }
  }
  # generating an output in list format consisting pdf,mean,variance and overdispersion value
  return(list('pdf'=ans,'mean'=n*((b/(b+1))^a) ,
              'var'=(n^2)*((b/(b+2))^a-(b/(b+1))^(2*a))+(n*(b/(b+1))^a)*(1-((b+1)/(b+2))^a),
              'over.dis.para'=((b/(b+2))^a-(b/(b+1))^(2*a))/(((b/(b+1))^a)*(1-(b/(b+1))^a))))
}

#' Grassia-II-Binomial Distribution
#'
#' These functions provide the ability for generating probability function values and
#' cumulative probability function values for the Grassia-II-Binomial Distribution.
#'
#' @usage
#' pGrassiaIIBin(x,n,a,b)
#'
#' @param x             vector of binomial random variables.
#' @param n             single value for no of binomial trials.
#' @param a             single value for shape parameter a.
#' @param b             single value for shape parameter b.
#'
#' @details
#' Mixing Gamma distribution with Binomial distribution will create the the Grassia-II-Binomial
#' distribution, only when (1-p)=e^(-lambda) of the Binomial distribution. The probability function and
#' cumulative probability function can be constructed and are denoted below.
#'
#' The cumulative probability function is the summation of probability function values.
#'
#' \deqn{P_{GrassiaIIBin}[x]= {n \choose x} \sum_{j=0}^{x} {x \choose j} (-1)^{x-j} (1+b(n-j))^{-a} }
#' \deqn{a,b > 0}
#' \deqn{x = 0,1,2,...,n}
#' \deqn{n = 1,2,3,...}
#'
#' The mean, variance and over dispersion are denoted as
#' \deqn{E_{GrassiaIIBin}[x] = (\frac{b}{b+1})^a}
#' \deqn{Var_{GrassiaIIBin}[x] = n^2[(\frac{b}{b+2})^a - (\frac{b}{b+1})^{2a}] + n(\frac{b}{b+1})^a{1-(\frac{b+1}{b+2})^a}}
#' \deqn{over dispersion= \frac{(\frac{b}{b+2})^a - (\frac{b}{b+1})^{2a}}{(\frac{b}{b+1})^a[1-(\frac{b}{b+1})^a]}}
#'
#' @return
#' The output of \code{pGrassiaIIBin} gives cumulative probability  values in vector form.
#'
#' @references
#' \insertRef{grassia1977family}{fitODBOD}
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(5)
#' a <- c(0.3,0.4,0.5,0.6,0.8)
#' plot(0,0,main="Grassia II binomial probability function graph",xlab="Binomial random variable",
#' ylab="Probability function values",xlim = c(0,10),ylim = c(0,0.5))
#' for (i in 1:5)
#' {
#' lines(0:10,dGrassiaIIBin(0:10,10,2*a[i],a[i])$pdf,col = col[i],lwd=2.85)
#' points(0:10,dGrassiaIIBin(0:10,10,2*a[i],a[i])$pdf,col = col[i],pch=16)
#' }
#'
#' dGrassiaIIBin(0:10,10,4,.2)$pdf    #extracting the pdf values
#' dGrassiaIIBin(0:10,10,4,.2)$mean   #extracting the mean
#' dGrassiaIIBin(0:10,10,4,.2)$var    #extracting the variance
#' dGrassiaIIBin(0:10,10,4,.2)$over.dis.para  #extracting the over dispersion value
#'
#' #plotting the random variables and cumulative probability values
#' col <- rainbow(4)
#' a <- c(0.3,0.4,0.5,0.6)
#' plot(0,0,main="Cumulative probability function graph",xlab="Binomial random variable",
#' ylab="Cumulative probability function values",xlim = c(0,10),ylim = c(0,1))
#' for (i in 1:4)
#' {
#' lines(0:10,pGrassiaIIBin(0:10,10,2*a[i],a[i]),col = col[i])
#' points(0:10,pGrassiaIIBin(0:10,10,2*a[i],a[i]),col = col[i])
#' }
#'
#' pGrassiaIIBin(0:10,10,4,.2)   #acquiring the cumulative probability values
#'
#' @export
pGrassiaIIBin<-function(x,n,a,b)
{
  #for each binomial random variable in the input vector the cumulative proability function
  #values are calculated
  ans<-sapply(1:length(x),function(i) sum(dGrassiaIIBin(0:x[i],n,a,b)$pdf))
  #generating an ouput vector cumulative probability function values
  return(ans)
}

#' Negative Log Likelihood value of Grassia II Binomial Distribution
#'
#' This function will calculate  the Negative Log Likelihood value when the vector of binomial random
#' variables and vector of corresponding frequencies are given with the shape parameters l and c.
#'
#' @usage
#' NegLLGrassiaIIBin(x,freq,a,b)
#'
#' @param x                  vector of binomial random variables.
#' @param freq               vector of frequencies.
#' @param a                  single value for shape parameter a.
#' @param b                  single value for shape parameter b.
#'
#' @details
#' \deqn{0 < a,b}
#' \deqn{freq \ge 0}
#' \deqn{x = 0,1,2,...}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions necessary error
#' messages will be provided to go further.
#'
#' @return
#' The output of \code{NegLLGrassiaIIBin} will produce a single numeric value.
#'
#' @references
#' \insertRef{grassia1977family}{fitODBOD}
#'
#' @examples
#' No.D.D <- 0:7       #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)   #assigning the corresponding frequencies
#'
#' NegLLGrassiaIIBin(No.D.D,Obs.fre.1,.3,.4)   #acquiring the negative log likelihood value
#'
#' @export
NegLLGrassiaIIBin<-function(x,freq,a,b)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,freq,a,b))) | any(is.infinite(c(x,freq,a,b)))|any(is.nan(c(x,freq,a,b))) )
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
      n<-max(x)
      data<-rep(x,freq)

      value<-sapply(1:sum(freq),function(i) sum((-1)^(data[i]-(0:data[i]))*
                                                  choose(data[i],(0:data[i]))*(1+b*(n-(0:data[i])))^(-a)))

      #calculating the negative log likelihood value and representing as a single output value
      return(-(sum(log(choose(n,data[1:sum(freq)])))+sum(log(value))))
    }
  }
}

#' Estimating the shape parameters a and b for Grassia II Binomial distribution
#'
#' The function will estimate the shape parameters using the maximum log likelihood method
#' for the Grassia II Binomial distribution when the binomial random variables and corresponding frequencies
#' are given.
#'
#' @usage
#' EstMLEGrassiaIIBin(x,freq,a,b,...)
#'
#' @param x                   vector of binomial random variables.
#' @param freq                vector of frequencies.
#' @param a                   single value for shape parameter a.
#' @param b                   single value for shape parameter b.
#' @param ...                 mle2 function inputs except data and estimating parameter.
#'
#' @details
#' \deqn{0 < a,b}
#' \deqn{x = 0,1,2,...}
#' \deqn{freq \ge 0}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions necessary
#' error messages will be provided to go further.
#'
#' @return
#' \code{EstMLEGrassiaIIBin} here is used as a wrapper for the \code{mle2} function of \pkg{bbmle} package
#' therefore output is of class of mle2.
#'
#' @references
#' \insertRef{grassia1977family}{fitODBOD}
#'
#' @examples
#' No.D.D <- 0:7                   #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)  #assigning the corresponding frequencies
#'
#' #estimating the parameters using maximum log likelihood value and assigning it
#' parameters <- EstMLEGrassiaIIBin(x=No.D.D,freq=Obs.fre.1,a=0.1,b=0.1)
#'
#' bbmle::coef(parameters)         #extracting the parameters
#'
#' @export
EstMLEGrassiaIIBin<-function(x,freq,a,b,...)
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
  output<-suppressWarnings2(bbmle::mle2(.EstMLEGrassiaIIBin,data=list(x=x,freq=freq),
                                        start = list(a=a,b=b),...),"NaN")
  return(output)
}


.EstMLEGrassiaIIBin<-function(x,freq,a,b)
{
  #with respective to using bbmle package function mle2 there is no need impose any restrictions
  #therefor the output is directly a single numeric value for the negative log likelihood value of
  #Gamma binomial distribution
  n<-max(x)
  data<-rep(x,freq)
  value<-sapply(1:sum(freq),function(i) sum(((-1)^(data[i]-(0:data[i])))*
                                              choose(data[i],0:data[i])*(1+b*(n-(0:data[i])))^(-a)))

  return(-(sum(log(choose(n,data[1:sum(freq)])))+sum(log(value))))
}

#' Fitting the Grassia II Binomial distribution when binomial random variable,
#' frequency and shape parameters are given
#'
#' The function will fit the Grassia II Binomial Distribution when random variables,
#' corresponding frequencies and shape parameters are given. It will provide
#' the expected frequencies, chi-squared test statistics value, p value, degree of freedom
#' and over dispersion value so that it can be seen if this distribution fits the data.
#'
#' @usage fitGrassiaIIBin(x,obs.freq,a,b)
#'
#' @param x                vector of binomial random variables.
#' @param obs.freq         vector of frequencies.
#' @param a                single value for shape parameter a.
#' @param b                single value for shape parameter b.
#'
#' @details
#' \deqn{0 < a,b}
#' \deqn{x = 0,1,2,...}
#' \deqn{obs.freq \ge 0}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions necessary
#' error messages will be provided to go further.
#'
#' @return
#' The output of \code{fitGrassiaIIBin} gives the class format \code{fitGrIIB} and \code{fit} consisting a list
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
#' \code{fitGrIIB} fitted values of \code{dGrassiaIIBin}.
#'
#' \code{NegLL} Negative Log Likelihood value.
#'
#' \code{a} estimated value for shape parameter a.
#'
#' \code{b} estimated value for shape parameter b.
#'
#' \code{AIC} AIC value.
#'
#' \code{over.dis.para} over dispersion value.
#'
#' \code{call} the inputs of the function.
#'
#' Methods \code{summary}, \code{print}, \code{AIC}, \code{residuals} and \code{fitted} can be used to
#' extract specific outputs.
#'
#' @references
#' \insertRef{grassia1977family}{fitODBOD}
#'
#' @examples
#' No.D.D <- 0:7       #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)          #assigning the corresponding frequencies
#'
#' #estimating the parameters using maximum log likelihood value and assigning it
#' parameters <- EstMLEGrassiaIIBin(x=No.D.D,freq=Obs.fre.1,a=0.1,b=0.1)
#'
#' aGIIBin <- bbmle::coef(parameters)[1]         #assigning the estimated a
#' bGIIBin <- bbmle::coef(parameters)[2]         #assigning the estimated b
#'
#' #fitting when the random variable,frequencies,shape parameter values are given.
#' results <- fitGrassiaIIBin(No.D.D,Obs.fre.1,aGIIBin,bGIIBin)
#' results
#'
#' #extracting the expected frequencies
#' fitted(results)
#'
#' #extracting the residuals
#' residuals(results)
#' @export
fitGrassiaIIBin<-function(x,obs.freq,a,b)
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
    est<-dGrassiaIIBin(x,max(x),a,b)
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
    NegLL<-NegLLGrassiaIIBin(x,obs.freq,a,b)
    names(NegLL)<-NULL
    #the final output is in a list format containing the calculated values
    final<-list("bin.ran.var"=x,"obs.freq"=obs.freq,"exp.freq"=exp.freq,
                "statistic"=round(statistic,4),"df"=df,"p.value"=round(p.value,4),
                "fitGrIIB"=est,"NegLL"=NegLL,"a"=a,"b"=b,"AIC"=2*2+2*NegLL,
                "over.dis.para"=odp,"call"=match.call())
    class(final)<-c("fitGrIIB","fit")
    return(final)
  }
}

#' @method fitGrassiaIIBin default
#' @export
fitGrassiaIIBin.default<-function(x,obs.freq,a,b)
{
  return(fitGrassiaIIBin(x,obs.freq,a,b))
}

#' @method print fitGrIIB
#' @export
print.fitGrIIB<-function(x,...)
{
  cat("Call: \n")
  print(x$call)
  cat("\nChi-squared test for Grassia II Binomial Distribution \n\t
      Observed Frequency : ",x$obs.freq,"\n\t
      expected Frequency : ",x$exp.freq,"\n\t
      estimated a parameter :",x$a, "  ,estimated b parameter :",x$b," \n\t
      X-squared :",x$statistic,"  ,df :",x$df,"  ,p-value :",x$p.value,"\n\t
      over dispersion :",x$over.dis.para,"\n")
}

#' @method summary fitGrIIB
#' @export
summary.fitGrIIB<-function(object,...)
{
  cat("Call: \n")
  print(object$call)
  cat("\nChi-squared test for Grassia II Binomial Distribution \n\t
      Observed Frequency : ",object$obs.freq,"\n\t
      expected Frequency : ",object$exp.freq,"\n\t
      estimated a parameter :",object$a,"  ,estimated b parameter :",object$b,"\n\t
      X-squared :",object$statistic,"  ,df :",object$df,"  ,p-value :",object$p.value,"\n\t
      over dispersion :",object$over.dis.para,"\n\t
      Negative Loglikehood value :",object$NegLL,"\n\t
      AIC value :",object$AIC,"\n")
}
