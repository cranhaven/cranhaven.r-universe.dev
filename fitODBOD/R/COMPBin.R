#' COM Poisson Binomial Distribution
#'
#' These functions provide the ability for generating probability function values and
#' cumulative probability function values for the COM Poisson  Binomial Distribution.
#'
#' @usage
#' dCOMPBin(x,n,p,v)
#'
#' @param x        vector of binomial random variables.
#' @param n        single value for no of binomial trials.
#' @param p        single value for probability of success.
#' @param v        single value for  v.
#'
#' @details
#' The probability function and cumulative function can be constructed and are denoted below
#'
#' The cumulative probability function is the summation of probability function values.
#'
#' \deqn{P_{COMPBin}(x) = \frac{{n \choose x}^v p^x (1-p)^{n-x}}{\sum_{j=0}^{n} {n \choose j}^v p^j (1-p)^{(n-j)}}}
#' \deqn{x = 0,1,2,3,...n}
#' \deqn{n = 1,2,3,...}
#' \deqn{0 < p < 1}
#' \deqn{-\infty < v < +\infty }
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#' The output of \code{dCOMPBin} gives a list format consisting
#'
#' \code{pdf}           probability function values in vector form.
#'
#' \code{mean}          mean of COM Poisson  Binomial Distribution.
#'
#' \code{var}           variance of COM Poisson  Binomial Distribution.
#'
#' @references
#' \insertRef{borges2014poisson}{fitODBOD}
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(5)
#' a <- c(0.58,0.59,0.6,0.61,0.62)
#' b <- c(0.022,0.023,0.024,0.025,0.026)
#' plot(0,0,main="COM Poisson Binomial probability function graph",xlab="Binomial random variable",
#' ylab="Probability function values",xlim = c(0,10),ylim = c(0,0.5))
#' for (i in 1:5)
#' {
#' lines(0:10,dCOMPBin(0:10,10,a[i],b[i])$pdf,col = col[i],lwd=2.85)
#' points(0:10,dCOMPBin(0:10,10,a[i],b[i])$pdf,col = col[i],pch=16)
#' }
#'
#' dCOMPBin(0:10,10,0.58,0.022)$pdf      #extracting the pdf values
#' dCOMPBin(0:10,10,0.58,0.022)$mean     #extracting the mean
#' dCOMPBin(0:10,10,0.58,0.022)$var      #extracting the variance
#'
#' #plotting the random variables and cumulative probability values
#' col <- rainbow(5)
#' a <- c(0.58,0.59,0.6,0.61,0.62)
#' b <- c(0.022,0.023,0.024,0.025,0.026)
#' plot(0,0,main="COM Poisson Binomial probability function graph",xlab="Binomial random variable",
#' ylab="Probability function values",xlim = c(0,10),ylim = c(0,1))
#' for (i in 1:5)
#' {
#' lines(0:10,pCOMPBin(0:10,10,a[i],b[i]),col = col[i],lwd=2.85)
#' points(0:10,pCOMPBin(0:10,10,a[i],b[i]),col = col[i],pch=16)
#' }
#'
#' pCOMPBin(0:10,10,0.58,0.022)      #acquiring the cumulative probability values
#'
#' @importFrom Rdpack reprompt
#' @export
dCOMPBin<-function(x,n,p,v)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,n,p,v))) | any(is.infinite(c(x,n,p,v))) | any(is.nan(c(x,n,p,v))) )
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
      #checking the probability value is inbetween zero and one
      if( p <= 0 | p >= 1 )
      {
        stop("Probability value doesnot satisfy conditions")
      }
      else
      {
        #constructing the probability values for all random variables
        y<-0:n
        value1<-sapply(1:length(y),function(i) (((choose(n,y[i]))^v)*(p^y[i])*((1-p)^(n-y[i])))/
                         (sum(((choose(n,y))^v)*(p^y)*((1-p)^(n-y)))))
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
          value<-sapply(1:length(x),function(i) (((choose(n,x[i]))^v)*(p^x[i])*((1-p)^(n-x[i])))/
                          (sum(((choose(n,y))^v)*(p^y)*((1-p)^(n-y)))))

          # generating an output in list format consisting pdf,mean and variance
          return(list("pdf"=value,"mean"=sum(value1*y),
                      "var"=sum((y^2)*value1)-(sum(value1*y))^2))
        }
      }
    }
  }
}

#' COM Poisson Binomial Distribution
#'
#' These functions provide the ability for generating probability function values and
#' cumulative probability function values for the COM Poisson  Binomial Distribution.
#'
#' @usage
#' pCOMPBin(x,n,p,v)
#'
#' @param x        vector of binomial random variables.
#' @param n        single value for no of binomial trials.
#' @param p        single value for probability of success.
#' @param v        single value for  v.
#'
#' @details
#' The probability function and cumulative function can be constructed and are denoted below
#'
#' The cumulative probability function is the summation of probability function values.
#'
#' \deqn{P_{COMPBin}(x) = \frac{{n \choose x}^v p^x (1-p)^{n-x}}{\sum_{j=0}^{n} {n \choose j}^v p^j (1-p)^{(n-j)}}}
#' \deqn{x = 0,1,2,3,...n}
#' \deqn{n = 1,2,3,...}
#' \deqn{0 < p < 1}
#' \deqn{-\infty < v < +\infty }
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#' The output of \code{pCOMPBin} gives  cumulative probability  values in vector form.
#'
#' @references
#' \insertRef{borges2014poisson}{fitODBOD}
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(5)
#' a <- c(0.58,0.59,0.6,0.61,0.62)
#' b <- c(0.022,0.023,0.024,0.025,0.026)
#' plot(0,0,main="COM Poisson Binomial probability function graph",xlab="Binomial random variable",
#' ylab="Probability function values",xlim = c(0,10),ylim = c(0,0.5))
#' for (i in 1:5)
#' {
#' lines(0:10,dCOMPBin(0:10,10,a[i],b[i])$pdf,col = col[i],lwd=2.85)
#' points(0:10,dCOMPBin(0:10,10,a[i],b[i])$pdf,col = col[i],pch=16)
#' }
#'
#' dCOMPBin(0:10,10,0.58,0.022)$pdf      #extracting the pdf values
#' dCOMPBin(0:10,10,0.58,0.022)$mean     #extracting the mean
#' dCOMPBin(0:10,10,0.58,0.022)$var      #extracting the variance
#'
#' #plotting the random variables and cumulative probability values
#' col <- rainbow(5)
#' a <- c(0.58,0.59,0.6,0.61,0.62)
#' b <- c(0.022,0.023,0.024,0.025,0.026)
#' plot(0,0,main="COM Poisson Binomial probability function graph",xlab="Binomial random variable",
#' ylab="Probability function values",xlim = c(0,10),ylim = c(0,1))
#' for (i in 1:5)
#' {
#' lines(0:10,pCOMPBin(0:10,10,a[i],b[i]),col = col[i],lwd=2.85)
#' points(0:10,pCOMPBin(0:10,10,a[i],b[i]),col = col[i],pch=16)
#' }
#'
#' pCOMPBin(0:10,10,0.58,0.022)      #acquiring the cumulative probability values
#'
#' @export
pCOMPBin<-function(x,n,p,v)
{
  #for each binomial random variable in the input vector the cumulative proability function
  #values are calculated
  ans<-sapply(1:length(x),function(i) sum(dCOMPBin(0:x[i],n,p,v)$pdf))

  #generating an ouput vector cumulative probability function values
  return(ans)
}

#' Negative Log Likelihood value of COM Poisson Binomial distribution
#'
#' This function will calculate the negative log likelihood value when the vector of binomial random
#' variables and vector of corresponding frequencies are given with the input parameters.
#'
#' @usage
#' NegLLCOMPBin(x,freq,p,v)
#'
#' @param x                 vector of binomial random variables.
#' @param freq              vector of frequencies.
#' @param p                 single value for probability of success.
#' @param v                 single value for  v.
#'
#' @details
#' \deqn{freq \ge 0}
#' \deqn{x = 0,1,2,..}
#' \deqn{0 < p < 1}
#' \deqn{-\infty < v < +\infty}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#' The output of \code{NegLLCOMPBin} will produce a single numeric value.
#'
#' @references
#' \insertRef{borges2014poisson}{fitODBOD}
#'
#' @examples
#' No.D.D <- 0:7         #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)      #assigning the corresponding frequencies
#'
#' NegLLCOMPBin(No.D.D,Obs.fre.1,.5,.03)     #acquiring the negative log likelihood value
#'
#' @export
NegLLCOMPBin<-function(x,freq,p,v)
{
  #constructing the data set using the random variables vector and frequency vector
  n<-max(x)
  data<-rep(x,freq)
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,freq,p,v))) | any(is.infinite(c(x,freq,p,v))) |
     any(is.nan(c(x,freq,p,v))) )
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
      #constructing the probability values for all random variables
      y<-0:n
      value1<-sapply(1:length(y),function(i) (((choose(n,y[i]))^v)*(p^y[i])*((1-p)^(n-y[i])))/
                       (sum(((choose(n,y))^v)*(p^y)*((1-p)^(n-y)))))
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
        #calculating the negative log likelihood value and representing as a single output value
        return(-(v*sum(log(choose(n,data[1:sum(freq)]))) +
                   log(p)*sum(data[1:sum(freq)]) + log(1-p)*sum(n-data[1:sum(freq)]) -
                   sum(freq)*log(sum(((choose(n,y))^v)*(p^y)*((1-p)^(n-y))))))
      }
    }
  }
}

#' Estimating the probability of success and v parameter for COM Poisson Binomial
#' Distribution
#'
#' The function will estimate the probability of success and v parameter using the maximum log
#' likelihood method for the COM Poisson Binomial distribution when the binomial random
#' variables and corresponding frequencies are given.
#'
#' @usage
#' EstMLECOMPBin(x,freq,p,v,...)
#'
#' @param x       vector of binomial random variables.
#' @param freq    vector of frequencies.
#' @param p       single value for probability of success.
#' @param v       single value for v.
#' @param ...     mle2 function inputs except data and estimating parameter.
#'
#' @details
#' \deqn{x = 0,1,2,...}
#' \deqn{freq \ge 0}
#' \deqn{0 < p < 1}
#' \deqn{-\infty < v < +\infty}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#' \code{EstMLECOMPBin} here is used as a wrapper for the \code{mle2} function of \pkg{bbmle} package
#' therefore output is of class of mle2.
#'
#' @references
#' \insertRef{borges2014poisson}{fitODBOD}
#'
#' @examples
#' No.D.D <- 0:7               #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)     #assigning the corresponding frequencies
#'
#' #estimating the parameters using maximum log likelihood value and assigning it
#' parameters <- EstMLECOMPBin(x=No.D.D,freq=Obs.fre.1,p=0.5,v=0.1)
#'
#' bbmle::coef(parameters)           #extracting the parameters
#'
#'@export
EstMLECOMPBin<-function(x,freq,p,v,...)
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
  output<-suppressWarnings2(bbmle::mle2(.EstMLECOMPBin,data=list(x=x,freq=freq),
                                        start = list(p=p,v=v),...),"NaN")
  return(output)
}

.EstMLECOMPBin<-function(x,freq,p,v)
{
  #with respective to using bbmle package function mle2 there is no need impose any restrictions
  #therefor the output is directly a single numeric value for the negative log likelihood value of
  #COM Poisson Binomial distribution
  n<-max(x)
  y<-0:n
  data<-rep(x,freq)

  return(-(v*sum(log(choose(n,data[1:sum(freq)]))) + log(p)*sum(data[1:sum(freq)]) +
             log(1-p)*sum(n-data[1:sum(freq)]) - sum(freq)*log(sum(((choose(n,y))^v)*(p^y)*((1-p)^(n-y))))))
}

#' Fitting the COM Poisson Binomial Distribution when binomial
#' random variable, frequency, probability of success and v parameter are given
#'
#' The function will fit the COM Poisson Binomial Distribution
#' when random variables, corresponding frequencies, probability of success and v parameter are given.
#' It will provide the expected frequencies, chi-squared test statistics value, p value,
#' and degree of freedom so that it can be seen if this distribution fits the data.
#'
#' @usage
#' fitCOMPBin(x,obs.freq,p,v)
#'
#' @param x                  vector of binomial random variables.
#' @param obs.freq           vector of frequencies.
#' @param p                  single value for probability of success.
#' @param v                  single value for v.
#'
#' @details
#' \deqn{obs.freq \ge 0}
#' \deqn{x = 0,1,2,..}
#' \deqn{0 < p < 1}
#' \deqn{-\infty < v < +\infty}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#' The output of \code{fitCOMPBin} gives the class format \code{fitCPB} and \code{fit} consisting a list
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
#' \code{fitCPB} fitted probability values of \code{dCOMPBin}.
#'
#' \code{NegLL} Negative Log Likelihood value.
#'
#' \code{p} estimated probability value.
#'
#' \code{v} estimated v parameter value.
#'
#' \code{AIC} AIC value.
#'
#' \code{call} the inputs of the function.
#'
#' Methods \code{summary}, \code{print}, \code{AIC}, \code{residuals} and \code{fitted}
#' can be used to extract specific outputs.
#'
#' @references
#' \insertRef{borges2014poisson}{fitODBOD}
#'
#' @examples
#' No.D.D <- 0:7                    #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)      #assigning the corresponding frequencies
#'
#' #estimating the parameters using maximum log likelihood value and assigning it
#' parameters <- EstMLECOMPBin(x=No.D.D,freq=Obs.fre.1,p=0.5,v=0.050)
#'
#' pCOMPBin <- bbmle::coef(parameters)[1]
#' vCOMPBin <- bbmle::coef(parameters)[2]
#'
#' #fitting when the random variable,frequencies,probability and v parameter are given
#' results <- fitCOMPBin(No.D.D,Obs.fre.1,pCOMPBin,vCOMPBin)
#' results
#'
#' #extracting the AIC value
#' AIC(results)
#'
#' #extract fitted values
#' fitted(results)
#'
#' @export
fitCOMPBin<-function(x,obs.freq,p,v)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,obs.freq,p,v))) | any(is.infinite(c(x,obs.freq,p,v))) |
     any(is.nan(c(x,obs.freq,p,v))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    est<-dCOMPBin(x,max(x),p,v)
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
    NegLL<-NegLLCOMPBin(x,obs.freq,p,v)
    names(NegLL)<-NULL
    #the final output is in a list format containing the calculated values
    final<-list("bin.ran.var"=x,"obs.freq"=obs.freq,"exp.freq"=exp.freq,"statistic"=round(statistic,4),
                "df"=df,"p.value"=round(p.value,4),"fitCPB"=est,
                "NegLL"=NegLL,"p"=p,"v"=v,"AIC"=2*2+2*NegLL,"call"=match.call())
    class(final)<-c("fitCPB","fit")
    return(final)
  }
}

#' @method fitCOMPBin default
#' @export
fitCOMPBin.default<-function(x,obs.freq,p,v)
{
  return(fitCOMPBin(x,obs.freq,p,v))
}

#' @method print fitCPB
#' @export
print.fitCPB<-function(x,...)
{
  cat("Call: \n")
  print(x$call)
  cat("\nChi-squared test for COM Poisson Binomial Distribution \n\t
      Observed Frequency : ",x$obs.freq,"\n\t
      expected Frequency : ",x$exp.freq,"\n\t
      estimated p value :",x$p," ,estimated v parameter :",x$v,"\n\t
      X-squared :",x$statistic,"  ,df :",x$df,"  ,p-value :",x$p.value,"\n")
}

#' @method summary fitCPB
#' @export
summary.fitCPB<-function(object,...)
{
  cat("Call: \n")
  print(object$call)
  cat("\nChi-squared test for COM Poisson Binomial Distribution \n\t
      Observed Frequency : ",object$obs.freq,"\n\t
      expected Frequency : ",object$exp.freq,"\n\t
      estimated p value :",object$p," ,estimated v parameter :",object$v,"\n\t
      X-squared :",object$statistic,"  ,df :",object$df,"  ,p-value :",object$p.value,"\n\t
      Negative Loglikehood value :",object$NegLL,"\n\t
      AIC value :",object$AIC,"\n")
}
