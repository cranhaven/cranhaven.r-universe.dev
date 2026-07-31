#' Lovinson Multiplicative  Binomial Distribution
#'
#' These functions provide the ability for generating probability function values and
#' cumulative probability function values for the Lovinson Multiplicative Binomial Distribution.
#'
#' @usage
#' dLMBin(x,n,p,phi)
#'
#' @param x        vector of binomial random variables.
#' @param n        single value for no of binomial trials.
#' @param p        single value for probability of success.
#' @param phi      single value for phi.
#'
#' @details
#' The probability function and cumulative function can be constructed and are denoted below
#'
#' The cumulative probability function is the summation of probability function values.
#'
#' \deqn{P_{LMBin}(x)= {n \choose x} p^x (1-p)^{n-x} \frac{(phi^{x(n-x)}}{f(p,phi,n)} }
#'
#' here \eqn{f(p,phi,n)} is
#' \deqn{f(p,phi,n)= \sum_{k=0}^{n} {n \choose k} p^k (1-p)^{n-k} (phi^{k(n-k)} )}
#'
#' \deqn{x = 0,1,2,3,...n}
#' \deqn{n = 1,2,3,...}
#' \deqn{k = 0,1,2,...,n}
#' \deqn{0 < p < 1}
#' \deqn{0 < phi }
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#' The output of \code{dLMBin} gives a list format consisting
#'
#' \code{pdf}         probability function values in vector form.
#'
#' \code{mean}        mean of Lovinson Multiplicative Binomial Distribution.
#'
#' \code{var}        variance of Lovinson Multiplicative Binomial Distribution.
#'
#' @references
#' \insertRef{elamir2013multiplicative}{fitODBOD}
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(5)
#' a <- c(0.58,0.59,0.6,0.61,0.62)
#' b <- c(0.022,0.023,0.024,0.025,0.026)
#' plot(0,0,main="Lovinson Multiplicative binomial probability
#'      function graph",xlab="Binomial random variable",
#'      ylab="Probability function values",xlim = c(0,10),ylim = c(0,0.5))
#' for (i in 1:5)
#' {
#' lines(0:10,dLMBin(0:10,10,a[i],1+b[i])$pdf,col = col[i],lwd=2.85)
#' points(0:10,dLMBin(0:10,10,a[i],1+b[i])$pdf,col = col[i],pch=16)
#' }
#'
#' dLMBin(0:10,10,.58,10.022)$pdf   #extracting the pdf values
#' dLMBin(0:10,10,.58,10.022)$mean   #extracting the mean
#' dLMBin(0:10,10,.58,10.022)$var   #extracting the variance
#'
#'
#' #plotting random variables and cumulative probability values
#' col <- rainbow(5)
#' a <- c(0.58,0.59,0.6,0.61,0.62)
#' b <- c(0.022,0.023,0.024,0.025,0.026)
#' plot(0,0,main="Lovinson Multiplicative binomial probability
#'      function graph",xlab="Binomial random variable",
#'      ylab="Probability function values",xlim = c(0,10),ylim = c(0,1))
#' for (i in 1:5)
#' {
#' lines(0:10,pLMBin(0:10,10,a[i],1+b[i]),col = col[i],lwd=2.85)
#' points(0:10,pLMBin(0:10,10,a[i],1+b[i]),col = col[i],pch=16)
#' }
#'
#' pLMBin(0:10,10,.58,10.022)     #acquiring the cumulative probability values
#'
#' @importFrom Rdpack reprompt
#' @export
dLMBin<-function(x,n,p,phi)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,n,p,phi))) | any(is.infinite(c(x,n,p,phi))) |
     any(is.nan(c(x,n,p,phi))) )
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
      #checking the probability value is inbetween zero and one if so providig an error message
      #and stopping the function progress
      if( p <= 0 | p >= 1)
      {
        stop("Probability value doesnot satisfy conditions")
      }
      else
      {
        #constructing the probability values for all random variables
        y<-0:n
        j<-0:n
        func1<-sum(choose(n,j)*(p^j)*((1-p)^(n-j))*(phi^(j*(n-j))))
        value1<-sapply(1:length(y),function(i) choose(n,y[i])*(p^y[i])*((1-p)^(n-y[i]))*(phi^(y[i]*(n-y[i])))/func1)
        check1<-sum(value1)
        #checking if the theta value is less than or equal to zero if so providig an error message
        #and stopping the function progress
        if(phi <= 0)
        {
          stop("Phi parameter value cannot be zero or less than zero")
        }
        #checking if the sum of all probability values leads upto one
        #if not providing an error message and stopping the function progress
        else if(check1 < 0.9999 | check1 >1.0001 | any(value1 < 0) | any(value1 >1))
        {
          stop("Input parameter combinations of probability of success and theta does
               not create proper probability function")
        }
        else
        {
          #for each random variable in the input vector below calculations occur
          value<-sapply(1:length(x),function(i) choose(n,x[i])*(p^x[i])*((1-p)^(n-x[i]))*(phi^(x[i]*(n-x[i])))/func1)
          # generating an output in list format consisting pdf,mean and variance
          return(list("pdf"=value,"mean"=n*.pi_func(phi,p,1,n),
                      "var"=n*.pi_func(phi,p,1,n)+n*(n-1)*.pi_func(phi,p,2,n)-(n*.pi_func(phi,p,1,n))^2))
        }
      }
    }
  }
}

.pi_func<-function(phi,p,i,n)
{
  Ktop<-sum(choose(n-i,0:n-i)*(p^(0:n-i))*((1-p)^(n-i-(0:n-i)))*(phi^((n-i-(0:n-i))*(0:n-i+i))))
  Kbottom<-sum(choose(n,0:n)*(p^(0:n))*((1-p)^(n-(0:n)))*(phi^((n-(0:n))*(0:n))))
  return(p^i*Ktop/Kbottom)
}

#' Lovinson Multiplicative  Binomial Distribution
#'
#' These functions provide the ability for generating probability function values and
#' cumulative probability function values for the Lovinson Multiplicative Binomial Distribution.
#'
#' @usage
#' pLMBin(x,n,p,phi)
#'
#' @param x        vector of binomial random variables.
#' @param n        single value for no of binomial trials.
#' @param p        single value for probability of success.
#' @param phi      single value for phi.
#'
#' @details
#' The probability function and cumulative function can be constructed and are denoted below
#'
#' The cumulative probability function is the summation of probability function values.
#'
#' \deqn{P_{LMBin}(x)= {n \choose x} p^x (1-p)^{n-x} \frac{(phi^{x(n-x)}}{f(p,phi,n)} }
#'
#' here \eqn{f(p,phi,n)} is
#' \deqn{f(p,phi,n)= \sum_{k=0}^{n} {n \choose k} p^k (1-p)^{n-k} (phi^{k(n-k)} )}
#'
#' \deqn{x = 0,1,2,3,...n}
#' \deqn{n = 1,2,3,...}
#' \deqn{k = 0,1,2,...,n}
#' \deqn{0 < p < 1}
#' \deqn{0 < phi }
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#' The output of \code{pLMBin} gives cumulative probability values in vector form.
#'
#' @references
#' \insertRef{elamir2013multiplicative}{fitODBOD}
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(5)
#' a <- c(0.58,0.59,0.6,0.61,0.62)
#' b <- c(0.022,0.023,0.024,0.025,0.026)
#' plot(0,0,main="Lovinson Multiplicative binomial probability
#'      function graph",xlab="Binomial random variable",
#'      ylab="Probability function values",xlim = c(0,10),ylim = c(0,0.5))
#' for (i in 1:5)
#' {
#' lines(0:10,dLMBin(0:10,10,a[i],1+b[i])$pdf,col = col[i],lwd=2.85)
#' points(0:10,dLMBin(0:10,10,a[i],1+b[i])$pdf,col = col[i],pch=16)
#' }
#'
#' dLMBin(0:10,10,.58,10.022)$pdf   #extracting the pdf values
#' dLMBin(0:10,10,.58,10.022)$mean   #extracting the mean
#' dLMBin(0:10,10,.58,10.022)$var   #extracting the variance
#'
#' #plotting random variables and cumulative probability values
#' col <- rainbow(5)
#' a <- c(0.58,0.59,0.6,0.61,0.62)
#' b <- c(0.022,0.023,0.024,0.025,0.026)
#' plot(0,0,main="Lovinson Multiplicative binomial probability
#'      function graph",xlab="Binomial random variable",
#'      ylab="Probability function values",xlim = c(0,10),ylim = c(0,1))
#' for (i in 1:5)
#' {
#' lines(0:10,pLMBin(0:10,10,a[i],1+b[i]),col = col[i],lwd=2.85)
#' points(0:10,pLMBin(0:10,10,a[i],1+b[i]),col = col[i],pch=16)
#' }
#'
#' pLMBin(0:10,10,.58,10.022)     #acquiring the cumulative probability values
#'
#' @export
pLMBin<-function(x,n,p,phi)
{
  #for each binomial random variable in the input vector the cumulative proability function
  #values are calculated
  ans<-sapply(1:length(x),function(i) sum(dLMBin(0:x[i],n,p,phi)$pdf))
  #generating an ouput vector cumulative probability function values
  return(ans)
}

#' Negative Log Likelihood value of Lovinson Multiplicative Binomial distribution
#'
#' This function will calculate the negative log likelihood value when the vector of binomial random
#' variable and vector of corresponding frequencies are given with the input parameters.
#'
#' @usage
#' NegLLLMBin(x,freq,p,phi)
#'
#' @param x                 vector of binomial random variables.
#' @param freq              vector of frequencies.
#' @param p                 single value for probability of success.
#' @param phi               single value for phi parameter.
#'
#' @details
#' \deqn{freq \ge 0}
#' \deqn{x = 0,1,2,..}
#' \deqn{0 < p < 1}
#' \deqn{0 < phi }
#'
#' @return
#' The output of \code{NegLLLMBin} will produce a single numeric value.
#'
#' @references
#' \insertRef{elamir2013multiplicative}{fitODBOD}
#'
#' @examples
#' No.D.D <- 0:7       #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)    #assigning the corresponding frequencies
#'
#' NegLLLMBin(No.D.D,Obs.fre.1,.5,3)    #acquiring the negative log likelihood value
#'
#' @export
NegLLLMBin<-function(x,freq,p,phi)
{
  #constructing the data set using the random variables vector and frequency vector
  n<-max(x)
  data<-rep(x,freq)
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,freq,p,phi))) | any(is.infinite(c(x,freq,p,phi))) |
     any(is.nan(c(x,freq,p,phi))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    #constructing the probability values for all random variables
    y<-0:n
    j<-0:n
    func1<-sum(choose(n,j)*(p^j)*((1-p)^(n-j))*(phi^(j*(n-j))))
    value1<-sapply(1:length(y),function(i) choose(n,y[i])*(p^y[i])*((1-p)^(n-y[i]))*(phi^(y[i]*(n-y[i])))/func1)
    check1<-sum(value1)
    #checking if any of the random variables of frequencies are less than zero if so
    #creating a error message as well as stopping the function progress
    if(any(c(x,freq) < 0) )
    {
      stop("Binomial random variable or frequency values cannot be negative")
    }
    #checking if probability value is less than zero or greater than one and
    #theta value greater than zero or equal to zero
    #if so creating an error message as well as stopping the function progress
    else if( p <= 0 | p >= 1 | phi <= 0)
    {
      stop("Probability or Phi parameter value doesnot satisfy conditions")
    }
    #checking if the sum of all probability values leads upto 1
    #if not providing an error message and stopping the function progress
    else if(check1 < 0.9999 | check1 >1.0001 | any(value1 < 0) | any(value1 >1))
    {
      stop("Input parameter combinations of probability of success and theta does
           not create proper probability function")
    }
    else
    {
      k<-0:n
      #calculating the negative log likelihood value and representing as a single output value
      return(-(sum(log(choose(n,data[1:sum(freq)]))) + log(p)*sum(data[1:sum(freq)]) +
                 log(1-p)*sum(n-data[1:sum(freq)]) + log(phi)*sum(data[1:sum(freq)]*(n-data[1:sum(freq)])) -
                 sum(freq)*log(sum(choose(n,k)*(p^k)*((1-p)^(n-k))*(phi^(k*(n-k)))))))
    }
  }
}

#' Estimating the probability of success and theta for Lovinson Multiplicative Binomial
#' Distribution
#'
#' The function will estimate the probability of success and phi parameter using the
#' maximum log likelihood method for the Lovinson Multiplicative Binomial distribution when the binomial random
#' variables and corresponding frequencies are given.
#'
#' @usage
#' EstMLELMBin(x,freq,p,phi,...)
#'
#' @param x                 vector of binomial random variables.
#' @param freq              vector of frequencies.
#' @param p                 single value for probability of success.
#' @param phi               single value for phi parameter.
#' @param ...               mle2 function inputs except data and estimating parameter.
#'
#' @details
#' \deqn{freq \ge 0}
#' \deqn{x = 0,1,2,..}
#' \deqn{0 < p < 1}
#' \deqn{0 < phi }
#'
#' @return
#' \code{EstMLELMBin} here is used as a wrapper for the \code{mle2} function of
#' \pkg{bbmle} package therefore output is of class of mle2.
#'
#' @references
#' \insertRef{elamir2013multiplicative}{fitODBOD}
#'
#' @seealso
#' \code{\link[bbmle]{mle2}}
#'
#' @examples
#' No.D.D <- 0:7         #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)    #assigning the corresponding frequencies
#'
#' #estimating the parameters using maximum log likelihood value and assigning it
#' parameters <- EstMLELMBin(x=No.D.D,freq=Obs.fre.1,p=0.5,phi=15)
#'
#' bbmle::coef(parameters)           #extracting the parameters
#'
#' @export
EstMLELMBin<-function(x,freq,p,phi,...)
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
  output<-suppressWarnings2(bbmle::mle2(.EstMLELMBin,data=list(x=x,freq=freq),
                                        start = list(p=p,phi=phi),...),"NaN")
  return(output)
}


.EstMLELMBin<-function(x,freq,p,phi)
{
  #with respective to using bbmle package function mle2 there is no need impose any restrictions
  #therefor the output is directly a single numeric value for the negative log likelihood value of
  #MultiBinomial distribution
  n<-max(x)
  data<-rep(x,freq)
  k<-0:n
  return(-(sum(log(choose(n,data[1:sum(freq)]))) + log(p)*sum(data[1:sum(freq)]) +
             log(1-p)*sum(n-data[1:sum(freq)]) + log(phi)*sum(data[1:sum(freq)]*(n-data[1:sum(freq)])) -
             sum(freq)*log(sum(choose(n,k)*(p^k)*((1-p)^(n-k))*(phi^(k*(n-k)))))))
}

#' Fitting the Lovinson Multiplicative Binomial Distribution when binomial
#' random variable, frequency, probability of success and theta parameter are given
#'
#' The function will fit the Lovinson Multiplicative Binomial distribution
#' when random variables, corresponding frequencies, probability of success and phi parameter
#' are given. It will provide the expected frequencies, chi-squared test statistics value,
#' p value and degree of freedom  value so that it can be seen if this distribution
#' fits the data.
#'
#' @usage fitLMBin(x,obs.freq,p,phi)
#'
#' @param x                  vector of binomial random variables.
#' @param obs.freq           vector of frequencies.
#' @param p                  single value for probability of success.
#' @param phi                single value for phi parameter.
#'
#' @details
#' \deqn{obs.freq \ge 0}
#' \deqn{x = 0,1,2,..}
#' \deqn{0 < p < 1}
#' \deqn{0 < phi }
#'
#' @return
#' The output of \code{fitLMBin} gives the class format \code{fitLMB} and \code{fit} consisting a list
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
#' \code{fitLMB} fitted probability values of \code{dLMBin}.
#'
#' \code{NegLL} Negative Log Likelihood value.
#'
#' \code{p} estimated probability value.
#'
#' \code{phi} estimated phi parameter value.
#'
#' \code{AIC} AIC value.
#'
#' \code{call} the inputs of the function.
#'
#' Methods \code{summary}, \code{print}, \code{AIC}, \code{residuals} and \code{fitted}
#' can be used to extract specific outputs.
#'
#' @references
#' \insertRef{elamir2013multiplicative}{fitODBOD}
#'
#' @seealso
#' \code{\link[bbmle]{mle2}}
#'
#' @examples
#' No.D.D <- 0:7       #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)     #assigning the corresponding frequencies
#'
#' #estimating the parameters using maximum log likelihood value and assigning it
#' parameters <- EstMLELMBin(x=No.D.D,freq=Obs.fre.1,p=0.1,phi=.3)
#'
#' pLMBin=bbmle::coef(parameters)[1]    #assigning the estimated probability value
#' phiLMBin <- bbmle::coef(parameters)[2]  #assigning the estimated phi value
#'
#' #fitting when the random variable,frequencies,probability and phi are given
#' results <- fitLMBin(No.D.D,Obs.fre.1,pLMBin,phiLMBin)
#' results
#'
#' #extracting the AIC value
#' AIC(results)
#'
#' #extract fitted values
#' fitted(results)
#'
#' @export
fitLMBin<-function(x,obs.freq,p,phi)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,obs.freq,p,phi))) | any(is.infinite(c(x,obs.freq,p,phi))) |
     any(is.nan(c(x,obs.freq,p,phi))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    est<-dLMBin(x,max(x),p,phi)
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
    #calculating Negative log likelihood value and AIC
    NegLL<-NegLLLMBin(x,obs.freq,p,phi)
    names(NegLL)<-NULL
    #the final output is in a list format containing the calculated values
    final<-list("bin.ran.var"=x,"obs.freq"=obs.freq,"exp.freq"=exp.freq,
                "statistic"=round(statistic,4),"df"=df,"p.value"=round(p.value,4),"fitLMB"=est,
                "NegLL"=NegLL,"p"=p,"phi"=phi,"AIC"=2*2+2*NegLL,"call"=match.call())
    class(final)<-c("fitLMB","fit")
    return(final)
  }
}

#' @method fitLMBin default
#' @export
fitLMBin.default<-function(x,obs.freq,p,phi)
{
  return(fitLMBin(x,obs.freq,p,phi))
}

#' @method print fitLMB
#' @export
print.fitLMB<-function(x,...)
{
  cat("Call: \n")
  print(x$call)
  cat("\nChi-squared test for Lovinson Multiplicative Binomial Distribution \n\t
      Observed Frequency : ",x$obs.freq,"\n\t
      expected Frequency : ",x$exp.freq,"\n\t
      estimated p value :",x$p," ,estimated phi parameter :",x$phi,"\n\t
      X-squared :",x$statistic,"  ,df :",x$df,"  ,p-value :",x$p.value,"\n")
}

#' @method summary fitLMB
#' @export
summary.fitLMB<-function(object,...)
{
  cat("Call: \n")
  print(object$call)
  cat("\nChi-squared test for Lovinson Multiplicative Binomial Distribution \n\t
      Observed Frequency : ",object$obs.freq,"\n\t
      expected Frequency : ",object$exp.freq,"\n\t
      estimated p value :",object$p," ,estimated phi parameter :",object$phi,"\n\t
      X-squared :",object$statistic,"  ,df :",object$df,"  ,p-value :",object$p.value,"\n\t
      Negative Loglikehood value :",object$NegLL,"\n\t
      AIC value :",object$AIC,"\n")
}

#' @importFrom bbmle mle2
#' @importFrom stats pchisq
