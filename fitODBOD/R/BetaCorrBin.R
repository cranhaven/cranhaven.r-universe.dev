#' Beta-Correlated  Binomial Distribution
#'
#' These functions provide the ability for generating probability function values and
#' cumulative probability function values for the Beta-Correlated  Binomial Distribution.
#'
#' @usage
#' dBetaCorrBin(x,n,cov,a,b)
#'
#' @param x        vector of binomial random variables.
#' @param n        single value for no of binomial trials.
#' @param cov      single value for covariance.
#' @param a        single value for alpha parameter.
#' @param b        single value for beta parameter.
#'
#' @details
#' The probability function and cumulative function can be constructed and are denoted below
#'
#' The cumulative probability function is the summation of probability function values.
#'
#' \if{html}{\figure{Capture.png}{options: style="width: 50\%;"}}
#' \if{latex}{\figure{Capture.png}{options: width=7cm}}
#'
#' \deqn{x = 0,1,2,3,...n}
#' \deqn{n = 1,2,3,...}
#' \deqn{0 < a,b}
#' \deqn{-\infty < cov < +\infty }
#' \deqn{0 < p < 1}
#'
#' \deqn{p=\frac{a}{a+b}}
#' \deqn{\Theta=\frac{1}{a+b}}
#'
#'  The Correlation is in between
#' \deqn{\frac{-2}{n(n-1)} min(\frac{p}{1-p},\frac{1-p}{p}) \le correlation \le \frac{2p(1-p)}{(n-1)p(1-p)+0.25-fo} }
#' where \eqn{fo=min [(x-(n-1)p-0.5)^2] }
#'
#' The mean and the variance are denoted as
#' \deqn{E_{BetaCorrBin}[x]= np}
#' \deqn{Var_{BetaCorrBin}[x]= np(1-p)(n\Theta+1)(1+\Theta)^{-1}+n(n-1)cov}
#' \deqn{Corr_{BetaCorrBin}[x]=\frac{cov}{p(1-p)}}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#' The output of \code{dBetaCorrBin} gives a list format consisting
#'
#' \code{pdf}           probability function values in vector form.
#'
#' \code{mean}          mean of Beta-Correlated  Binomial Distribution.
#'
#' \code{var}           variance of Beta-Correlated  Binomial Distribution.
#'
#' \code{corr}          correlation of Beta-Correlated Binomial Distribution.
#'
#' \code{mincorr}       minimum correlation value possible.
#'
#' \code{maxcorr}       maximum correlation value possible.
#'
#' @references
#' \insertRef{paul1985three}{fitODBOD}
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(5)
#' a <- c(9.0,10,11,12,13)
#' b <- c(8.0,8.1,8.2,8.3,8.4)
#' plot(0,0,main="Beta-Correlated binomial probability function graph",xlab="Binomial random variable",
#' ylab="Probability function values",xlim = c(0,10),ylim = c(0,0.5))
#' for (i in 1:5)
#' {
#' lines(0:10,dBetaCorrBin(0:10,10,0.001,a[i],b[i])$pdf,col = col[i],lwd=2.85)
#' points(0:10,dBetaCorrBin(0:10,10,0.001,a[i],b[i])$pdf,col = col[i],pch=16)
#' }
#'
#' dBetaCorrBin(0:10,10,0.001,10,13)$pdf      #extracting the pdf values
#' dBetaCorrBin(0:10,10,0.001,10,13)$mean     #extracting the mean
#' dBetaCorrBin(0:10,10,0.001,10,13)$var      #extracting the variance
#' dBetaCorrBin(0:10,10,0.001,10,13)$corr     #extracting the correlation
#' dBetaCorrBin(0:10,10,0.001,10,13)$mincorr  #extracting the minimum correlation value
#' dBetaCorrBin(0:10,10,0.001,10,13)$maxcorr  #extracting the maximum correlation value
#'
#' #plotting the random variables and cumulative probability values
#' col <- rainbow(5)
#' a <- c(9.0,10,11,12,13)
#' b <- c(8.0,8.1,8.2,8.3,8.4)
#' plot(0,0,main="Beta-Correlated binomial probability function graph",xlab="Binomial random variable",
#' ylab="Probability function values",xlim = c(0,10),ylim = c(0,1))
#' for (i in 1:5)
#' {
#' lines(0:10,pBetaCorrBin(0:10,10,0.001,a[i],b[i]),col = col[i],lwd=2.85)
#' points(0:10,pBetaCorrBin(0:10,10,0.001,a[i],b[i]),col = col[i],pch=16)
#' }
#'
#' pBetaCorrBin(0:10,10,0.001,10,13)      #acquiring the cumulative probability values
#'
#' @importFrom Rdpack reprompt
#' @export
dBetaCorrBin<-function(x,n,cov,a,b)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,n,cov,a,b))) | any(is.infinite(c(x,n,cov,a,b))) | any(is.nan(c(x,n,cov,a,b))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    #checking if at any chance the binomial random variable is greater than binomial trial value
    #if so providing an error message and stopping the function progress
    if(max(x) > n )
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
      p<-a/(a+b)
      shi<-1/(a+b)
      correlation<-cov/(p*(1-p))
      #checking the probability value is inbetween zero and one
      if( p <= 0 | p >= 1 )
      {
        stop("Probability value doesnot satisfy conditions")
      }
      else
      {
        #creating the necessary limits for correlation, the left hand side and right hand side limits
        left.h<-(-2/(n*(n-1)))*min(p/(1-p),(1-p)/p)
        right.h<-(2*p*(1-p))/(((n-1)*p*(1-p))+0.25- min(((0:n)-(n-1)*p-0.5)^2))
        # checking if the correlation output satisfies conditions mentioned above
        if(correlation < -1 | correlation > 1 | correlation < left.h | correlation > right.h)
        {
          stop("Correlation cannot be greater than 1 or Lesser than -1 or it cannot be greater than Maximum Correlation or Lesser than Minimum Correlation")
        }
        else
        {
          #constructing the probability values for all random variables
          y<-0:n
          value1<-sapply(1:length(y), function(i) ((choose(n,y[i]))*(beta(a+y[i],b+n-y[i])/beta(a,b))*
                                                     (1+(cov/2)*(((y[i]*(y[i]-1)*(a+b+n-4)*(a+b+n-3)*(a+b+n-2)*(a+b+n-1))/
                                                                    ((y[i]+a-2)*(y[i]+a-1)*(n-y[i]+b-2)*(n-y[i]+b-1)))-
                                                                   ((2*y[i]*(n-1)*(a+b+n-3)*(a+b+n-2)*(a+b+n-1))/
                                                                      ((y[i]+a-1)*(n-y[i]+b-2)*(n-y[i]+b-1)))+
                                                                   ((n*(n-1)*(a+b+n-2)*(a+b+n-1))/((n-y[i]+b-2)*(n-y[i]+b-1)))))))
          check1<-sum(value1)

          #checking if the sum of all probability values leads upto one
          #if not providing an error message and stopping the function progress
          if(check1 < 0.9999 | check1 >1.0001 | any(value1 < 0) | any(value1 >1))
          {
            stop("Input parameter combinations of probability of success and covariance does
                 not create proper probability function")
          }
          else
          {
            #for each random variable in the input vector below calculations occur
            value<-sapply(1:length(x),function(i) ((choose(n,x[i]))*(beta(a+x[i],b+n-x[i])/beta(a,b))*
                                                     (1+(cov/2)*(((x[i]*(x[i]-1)*(a+b+n-4)*(a+b+n-3)*(a+b+n-2)*(a+b+n-1))/
                                                                    ((x[i]+a-2)*(x[i]+a-1)*(n-x[i]+b-2)*(n-x[i]+b-1)))-
                                                                   ((2*x[i]*(n-1)*(a+b+n-3)*(a+b+n-2)*(a+b+n-1))/
                                                                      ((x[i]+a-1)*(n-x[i]+b-2)*(n-x[i]+b-1)))+
                                                                   ((n*(n-1)*(a+b+n-2)*(a+b+n-1))/((n-x[i]+b-2)*(n-x[i]+b-1)))))))

            # generating an output in list format consisting pdf,mean and variance
            return(list("pdf"=value,"mean"=n*p,"var"=n*p*(1-p)*(n*shi+1)/(1+shi)+n*(n-1)*cov,
                        "corr"=cov/(p*(1-p)),"mincorr"=left.h,"maxcorr"=right.h))
          }
        }
      }
    }
  }
}

#' Beta-Correlated  Binomial Distribution
#'
#' These functions provide the ability for generating probability function values and
#' cumulative probability function values for the Beta-Correlated  Binomial Distribution.
#'
#' @usage
#' pBetaCorrBin(x,n,cov,a,b)
#'
#' @param x        vector of binomial random variables.
#' @param n        single value for no of binomial trials.
#' @param cov      single value for covariance.
#' @param a        single value for alpha parameter
#' @param b        single value for beta parameter.
#'
#' @details
#' The probability function and cumulative function can be constructed and are denoted below
#'
#' The cumulative probability function is the summation of probability function values.
#'
#' \if{html}{\figure{Capture.png}{options: style="width: 50\%;"}}
#' \if{latex}{\figure{Capture.png}{options: width=7cm}}
#'
#' \deqn{x = 0,1,2,3,...n}
#' \deqn{n = 1,2,3,...}
#' \deqn{-\infty < cov < +\infty }
#' \deqn{0< a,b}
#' \deqn{0 < p < 1}
#'
#' \deqn{p=\frac{a}{a+b}}
#' \deqn{\Theta=\frac{1}{a+b}}
#'
#' The Correlation is in between
#' \deqn{\frac{-2}{n(n-1)} min(\frac{p}{1-p},\frac{1-p}{p}) \le correlation \le \frac{2p(1-p)}{(n-1)p(1-p)+0.25-fo} }
#' where \eqn{fo=min (x-(n-1)p-0.5)^2 }
#'
#' The mean and the variance are denoted as
#' \deqn{E_{BetaCorrBin}[x]= np}
#' \deqn{Var_{BetaCorrBin}[x]= np(1-p)(n\Theta+1)(1+\Theta)^{-1}+n(n-1)cov}
#' \deqn{Corr_{BetaCorrBin}[x]=\frac{cov}{p(1-p)}}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#' The output of \code{pBetaCorrBin} gives cumulative probability  values in vector form.
#'
#' @references
#' \insertRef{paul1985three}{fitODBOD}
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(5)
#' a <- c(9.0,10,11,12,13)
#' b <- c(8.0,8.1,8.2,8.3,8.4)
#' plot(0,0,main="Beta-Correlated binomial probability function graph",xlab="Binomial random variable",
#' ylab="Probability function values",xlim = c(0,10),ylim = c(0,0.5))
#' for (i in 1:5)
#' {
#' lines(0:10,dBetaCorrBin(0:10,10,0.001,a[i],b[i])$pdf,col = col[i],lwd=2.85)
#' points(0:10,dBetaCorrBin(0:10,10,0.001,a[i],b[i])$pdf,col = col[i],pch=16)
#' }
#'
#' dBetaCorrBin(0:10,10,0.001,10,13)$pdf      #extracting the pdf values
#' dBetaCorrBin(0:10,10,0.001,10,13)$mean     #extracting the mean
#' dBetaCorrBin(0:10,10,0.001,10,13)$var      #extracting the variance
#' dBetaCorrBin(0:10,10,0.001,10,13)$corr     #extracting the correlation
#' dBetaCorrBin(0:10,10,0.001,10,13)$mincorr  #extracting the minimum correlation value
#' dBetaCorrBin(0:10,10,0.001,10,13)$maxcorr  #extracting the maximum correlation value
#'
#' #plotting the random variables and cumulative probability values
#' col <- rainbow(5)
#' a <- c(9.0,10,11,12,13)
#' b <- c(8.0,8.1,8.2,8.3,8.4)
#' plot(0,0,main="Beta-Correlated binomial probability function graph",xlab="Binomial random variable",
#' ylab="Probability function values",xlim = c(0,10),ylim = c(0,1))
#' for (i in 1:5)
#' {
#' lines(0:10,pBetaCorrBin(0:10,10,0.001,a[i],b[i]),col = col[i],lwd=2.85)
#' points(0:10,pBetaCorrBin(0:10,10,0.001,a[i],b[i]),col = col[i],pch=16)
#' }
#'
#' pBetaCorrBin(0:10,10,0.001,10,13)      #acquiring the cumulative probability values
#'
#' @export
pBetaCorrBin<-function(x,n,cov,a,b)
{
  #for each binomial random variable in the input vector the cumulative proability function
  #values are calculated
  ans<-sapply(1:length(x),function(i) sum(dBetaCorrBin(0:x[i],n,cov,a,b)$pdf))

  #generating an ouput vector cumulative probability function values
  return(ans)
}

#' Negative Log Likelihood value of Beta-Correlated Binomial distribution
#'
#' This function will calculate the negative log likelihood value when the vector of binomial random
#' variables and vector of corresponding frequencies are given with the input parameters.
#'
#' @usage
#' NegLLBetaCorrBin(x,freq,cov,a,b)
#'
#' @param x                 vector of binomial random variables.
#' @param freq              vector of frequencies.
#' @param cov               single value for covariance.
#' @param a                 single value for alpha parameter.
#' @param b                 single value for beta parameter.
#'
#' @details
#' \deqn{freq \ge 0}
#' \deqn{x = 0,1,2,..}
#' \deqn{-\infty < cov < +\infty}
#' \deqn{0 < a,b}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#' The output of \code{NegLLBetaCorrBin} will produce a single numeric value.
#'
#' @references
#' \insertRef{paul1985three}{fitODBOD}
#'
#' @examples
#' No.D.D <- 0:7         #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)      #assigning the corresponding frequencies
#'
#' NegLLBetaCorrBin(No.D.D,Obs.fre.1,0.001,9.03,10)     #acquiring the negative log likelihood value
#'
#' @export
NegLLBetaCorrBin<-function(x,freq,cov,a,b)
{
  #constructing the data set using the random variables vector and frequency vector
  n<-max(x)
  data<-rep(x,freq)
  p<-a/(a+b)
  shi<-1/(a+b)
  correlation<-cov/(p*(1-p))
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,freq,cov,a,b))) | any(is.infinite(c(x,freq,cov,a,b))) |
     any(is.nan(c(x,freq,cov,a,b))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    #checking if any of the random variables of frequencies are less than zero if so
    #creating a error message as well as stopping the function progress
    if(any(c(x,freq) < 0) )
    {
      stop("Binomial random variable or frequency values cannot be negative")
    }
    #checking the probability value is inbetween zero and one or covariance is greater than zero
    else if( p <= 0 | p >= 1)
    {
      stop("Probability value doesnot satisfy conditions")
    }
    else
    {
      #creating the necessary limits for correlation, the left hand side and right hand side limits
      left.h<-(-2/(n*(n-1)))*min(p/(1-p),(1-p)/p)
      right.h<-(2*p*(1-p))/(((n-1)*p*(1-p))+0.25- min(((0:n)-(n-1)*p-0.5)^2))

      # checking if the correlation output satisfies conditions mentioned above
      if(correlation < -1 | correlation > 1 | correlation < left.h | correlation > right.h)
      {
        stop("Correlation cannot be greater than 1 or Lesser than -1 or it cannot be greater than Maximum Correlation or Lesser than Minimum Correlation")
      }
      else
      {
      #constructing the probability values for all random variables
      y<-0:n
      value1<-sapply(1:length(y),function(i) ((choose(n,y[i]))*(beta(a+y[i],b+n-y[i])/beta(a,b))*
                                                (1+(cov/2)*(((y[i]*(y[i]-1)*(a+b+n-4)*(a+b+n-3)*(a+b+n-2)*(a+b+n-1))/
                                                               ((y[i]+a-2)*(y[i]+a-1)*(n-y[i]+b-2)*(n-y[i]+b-1)))-
                                                              ((2*y[i]*(n-1)*(a+b+n-3)*(a+b+n-2)*(a+b+n-1))/
                                                                 ((y[i]+a-1)*(n-y[i]+b-2)*(n-y[i]+b-1)))+
                                                              ((n*(n-1)*(a+b+n-2)*(a+b+n-1))/((n-y[i]+b-2)*(n-y[i]+b-1)))))))

      check1<-sum(value1)

      #checking if the sum of all probability values leads upto one
      #if not providing an error message and stopping the function progress
        if(check1 < 0.9999 | check1 >1.0001 | any(value1 < 0) | any(value1 >1))
        {
          stop("Input parameter combinations of probability of success and covariance does
             not create proper probability function")
        }
        else
        {
          value<-sapply(1:sum(freq),function(i) log((1+(cov/2)*(((data[i]*(data[i]-1)*(a+b+n-4)*(a+b+n-3)*(a+b+n-2)*(a+b+n-1))/
                                                                   ((data[i]+a-2)*(data[i]+a-1)*(n-data[i]+b-2)*(n-data[i]+b-1)))-
                                                                  ((2*data[i]*(n-1)*(a+b+n-3)*(a+b+n-2)*(a+b+n-1))/
                                                                     ((data[i]+a-1)*(n-data[i]+b-2)*(n-data[i]+b-1)))+
                                                                  ((n*(n-1)*(a+b+n-2)*(a+b+n-1))/((n-data[i]+b-2)*(n-data[i]+b-1)))))))

        #calculating the negative log likelihood value and representing as a single output value
        return(-(sum(log(choose(n,data[1:sum(freq)]))) - length(data)*log(beta(a,b)) +
                   sum(log(beta(a+data[1:sum(freq)],b+n-data[1:sum(freq)]))) + sum(value)))
        }
      }
    }
  }
}

#' Estimating the covariance, alpha and beta parameter values for Beta-Correlated Binomial
#' Distribution
#'
#' The function will estimate the covariance, alpha and beta parameter values using the maximum log
#' likelihood method for the Beta-Correlated Binomial distribution when the binomial random
#' variables and corresponding frequencies are given.
#'
#' @usage
#' EstMLEBetaCorrBin(x,freq,cov,a,b,...)
#'
#'
#' @param x       vector of binomial random variables.
#' @param freq    vector of frequencies.
#' @param cov     single value for covariance.
#' @param a       single value for alpha parameter.
#' @param b       single value for beta parameter.
#' @param ...     mle2 function inputs except data and estimating parameter.
#'
#' @details
#' \deqn{x = 0,1,2,...}
#' \deqn{freq \ge 0}
#' \deqn{-\infty < cov < +\infty}
#' \deqn{0 < a,b}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#' \code{EstMLEBetaCorrBin} here is used as a wrapper for the \code{mle2} function of \pkg{bbmle} package
#' therefore output is of class of mle2.
#'
#' @references
#' \insertRef{paul1985three}{fitODBOD}
#'
#' @seealso
#' \code{\link[bbmle]{mle2}}
#'
#' @examples
#' No.D.D <- 0:7               #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)     #assigning the corresponding frequencies
#'
#' #estimating the parameters using maximum log likelihood value and assigning it
#' parameters <- EstMLEBetaCorrBin(x=No.D.D,freq=Obs.fre.1,cov=0.0050,a=10,b=10)
#'
#' bbmle::coef(parameters)           #extracting the parameters
#'
#'@export
EstMLEBetaCorrBin<-function(x,freq,cov,a,b,...)
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
  output<-suppressWarnings2(bbmle::mle2(.EstMLEBetaCorrBin,data=list(x=x,freq=freq),
                                        start = list(a=a,b=b,cov=cov),...),"NaN")
  return(output)
}


.EstMLEBetaCorrBin<-function(x,freq,cov,a,b)
{
  #with respective to using bbmle package function mle2 there is no need impose any restrictions
  #therefor the output is directly a single numeric value for the negative log likelihood value of
  #Beta-Correlated Binomial distribution
  shi<-1/(a+b)
  n<-max(x)
  data<-rep(x,freq)

  value<-sapply(1:sum(freq),function(i) log((1+(cov/2)*(((data[i]*(data[i]-1)*(a+b+n-4)*(a+b+n-3)*(a+b+n-2)*(a+b+n-1))/
                                                           ((data[i]+a-2)*(data[i]+a-1)*(n-data[i]+b-2)*(n-data[i]+b-1)))-
                                                          ((2*data[i]*(n-1)*(a+b+n-3)*(a+b+n-2)*(a+b+n-1))/
                                                             ((data[i]+a-1)*(n-data[i]+b-2)*(n-data[i]+b-1)))+
                                                          ((n*(n-1)*(a+b+n-2)*(a+b+n-1))/((n-data[i]+b-2)*(n-data[i]+b-1)))))))

  return(-(sum(log(choose(n,data[1:sum(freq)])))- length(data)*log(beta(a,b)) +
             sum(log(beta(a+data[1:sum(freq)],b+n-data[1:sum(freq)]))) + sum(value)))
}

#' Fitting the Beta-Correlated Binomial Distribution when binomial
#' random variable, frequency, covariance, alpha and beta parameters are given
#'
#' The function will fit the Beta-Correlated Binomial Distribution
#' when random variables, corresponding frequencies, covariance, alpha and beta parameters are given.
#' It will provide the expected frequencies, chi-squared test statistics value, p value,
#' and degree of freedom so that it can be seen if this distribution fits the data.
#'
#' @usage
#' fitBetaCorrBin(x,obs.freq,cov,a,b)
#'
#' @param x                  vector of binomial random variables.
#' @param obs.freq           vector of frequencies.
#' @param cov                single value for covariance.
#' @param a                  single value for alpha parameter.
#' @param b                  single value for beta parameter.
#'
#' @details
#' \deqn{obs.freq \ge 0}
#' \deqn{x = 0,1,2,..}
#' \deqn{-\infty < cov < +\infty}
#' \deqn{0 < a,b}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#' The output of \code{fitBetaCorrBin} gives the class format \code{fitBCB} and \code{fit} consisting a list
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
#' \code{p.value} probability value by chi-squared test statistic
#'
#' \code{corr}    Correlation value.
#'
#' \code{fitBCB} fitted probability values of \code{dBetaCorrBin}.
#'
#' \code{NegLL} Negative Log Likelihood value.
#'
#' \code{a} estimated shape parameter value a.
#'
#' \code{b} estimated shape parameter value b.
#'
#' \code{cov} estimated covariance value.
#'
#' \code{AIC} AIC value.
#'
#' \code{call} the inputs of the function.
#'
#' Methods \code{summary}, \code{print}, \code{AIC}, \code{residuals} and \code{fitted}
#' can be used to extract specific outputs.
#'
#' @references
#' \insertRef{paul1985three}{fitODBOD}
#'
#' @examples
#' No.D.D <- 0:7                    #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)      #assigning the corresponding frequencies
#'
#' #estimating the parameters using maximum log likelihood value and assigning it
#' parameters <- EstMLEBetaCorrBin(x=No.D.D,freq=Obs.fre.1,cov=0.0050,a=10,b=10)
#'
#' covBetaCorrBin <- bbmle::coef(parameters)[1]
#' aBetaCorrBin <- bbmle::coef(parameters)[2]
#' bBetaCorrBin <- bbmle::coef(parameters)[3]
#'
#' #fitting when the random variable,frequencies,covariance, a and b are given
#' results <- fitBetaCorrBin(No.D.D,Obs.fre.1,covBetaCorrBin,aBetaCorrBin,bBetaCorrBin)
#' results
#'
#' #extract AIC value
#' AIC(results)
#'
#' #extract fitted values
#' fitted(results)
#'
#' @export
fitBetaCorrBin<-function(x,obs.freq,cov,a,b)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,obs.freq,cov,a,b))) | any(is.infinite(c(x,obs.freq,cov,a,b))) |
     any(is.nan(c(x,obs.freq,cov,a,b))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    est<-dBetaCorrBin(x,max(x),cov,a,b)
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
    NegLL<-NegLLBetaCorrBin(x,obs.freq,cov,a,b)
    names(NegLL)<-NULL
    #the final output is in a list format containing the calculated values
    final<-list("bin.ran.var"=x,"obs.freq"=obs.freq,"exp.freq"=exp.freq,"statistic"=round(statistic,4),
                "df"=df,"p.value"=round(p.value,4),"cov"=cov,"a"=a,"b"=b,"corr"=est$corr,"fitBCB"=est,
                "NegLL"=NegLL,"AIC"=2*3+2*NegLL,"call"=match.call())

    class(final)<-c("fitBCB","fit")
    return(final)
    }
  }

#' @method fitBetaCorrBin default
#' @export
fitBetaCorrBin.default<-function(x,obs.freq,cov,a,b)
{
  return(fitBetaCorrBin(x,obs.freq,cov,a,b))
}

#' @method print fitBCB
#' @export
print.fitBCB<-function(x,...)
{
  cat("Call: \n")
  print(x$call)
  cat("\nChi-squared test for Beta-Correlated Binomial Distribution \n\t
      Observed Frequency : ",x$obs.freq,"\n\t
      expected Frequency : ",x$exp.freq,"\n\t
      estimated covariance value:",x$cov,"\n\t
      estimated a parameter :",x$a," , estimated b parameter :",x$b,"\n\t
      X-squared :",x$statistic,"  ,df :",x$df,"  ,p-value :",x$p.value,"\n")
}

#' @method summary fitBCB
#' @export
summary.fitBCB<-function(object,...)
{
  cat("Call: \n")
  print(object$call)
  cat("\nChi-squared test for Beta-Correlated Binomial Distribution \n\t
      Observed Frequency : ",object$obs.freq,"\n\t
      expected Frequency : ",object$exp.freq,"\n\t
      estimated covariance value:",object$cov,"\n\t
      estimated a parameter :",object$a," , estimated b parameter :",object$b,"\n\t
      X-squared :",object$statistic,"  ,df :",object$df,"  ,p-value :",object$p.value,"\n\t
      Negative Loglikehood value :",object$NegLL,"\n\t
      AIC value :",object$AIC,"\n")
}

#' @importFrom bbmle mle2
#' @importFrom stats pchisq
