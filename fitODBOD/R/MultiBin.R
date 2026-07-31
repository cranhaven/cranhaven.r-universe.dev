#' Multiplicative  Binomial Distribution
#'
#' These functions provide the ability for generating probability function values and
#' cumulative probability function values for the Multiplicative Binomial Distribution.
#'
#' @usage
#' dMultiBin(x,n,p,theta)
#'
#' @param x        vector of binomial random variables.
#' @param n        single value for no of binomial trials.
#' @param p        single value for probability of success.
#' @param theta    single value for theta.
#'
#' @details
#' The probability function and cumulative function can be constructed and are denoted below
#'
#' The cumulative probability function is the summation of probability function values.
#'
#' \deqn{P_{MultiBin}(x)= {n \choose x} p^x (1-p)^{n-x} \frac{(theta^{x(n-x)}}{f(p,theta,n)} }
#'
#' here \eqn{f(p,theta,n)} is
#' \deqn{f(p,theta,n)= \sum_{k=0}^{n} {n \choose k} p^k (1-p)^{n-k} (theta^{k(n-k)} )}
#'
#' \deqn{x = 0,1,2,3,...n}
#' \deqn{n = 1,2,3,...}
#' \deqn{k = 0,1,2,...,n}
#' \deqn{0 < p < 1}
#' \deqn{0 < theta }
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#' The output of \code{dMultiBin} gives a list format consisting
#'
#' \code{pdf}         probability function values in vector form.
#'
#' \code{mean}        mean of Multiplicative Binomial Distribution.
#'
#' \code{var}        variance of Multiplicative Binomial Distribution.
#'
#' @references
#' \insertRef{johnson2005univariate}{fitODBOD}
#' \insertRef{kupper1978use}{fitODBOD}
#' \insertRef{paul1985three}{fitODBOD}
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(5)
#' a <- c(0.58,0.59,0.6,0.61,0.62)
#' b <- c(0.022,0.023,0.024,0.025,0.026)
#' plot(0,0,main="Multiplicative binomial probability function graph",xlab="Binomial random variable",
#' ylab="Probability function values",xlim = c(0,10),ylim = c(0,0.5))
#' for (i in 1:5)
#' {
#' lines(0:10,dMultiBin(0:10,10,a[i],1+b[i])$pdf,col = col[i],lwd=2.85)
#' points(0:10,dMultiBin(0:10,10,a[i],1+b[i])$pdf,col = col[i],pch=16)
#' }
#'
#' dMultiBin(0:10,10,.58,10.022)$pdf   #extracting the pdf values
#' dMultiBin(0:10,10,.58,10.022)$mean   #extracting the mean
#' dMultiBin(0:10,10,.58,10.022)$var   #extracting the variance
#'
#'
#' #plotting random variables and cumulative probability values
#' col <- rainbow(5)
#' a <- c(0.58,0.59,0.6,0.61,0.62)
#' b <- c(0.022,0.023,0.024,0.025,0.026)
#' plot(0,0,main="Multiplicative binomial probability function graph",xlab="Binomial random variable",
#' ylab="Probability function values",xlim = c(0,10),ylim = c(0,1))
#' for (i in 1:5)
#' {
#' lines(0:10,pMultiBin(0:10,10,a[i],1+b[i]),col = col[i],lwd=2.85)
#' points(0:10,pMultiBin(0:10,10,a[i],1+b[i]),col = col[i],pch=16)
#' }
#'
#' pMultiBin(0:10,10,.58,10.022)     #acquiring the cumulative probability values
#'
#' @importFrom Rdpack reprompt
#' @export
dMultiBin<-function(x,n,p,theta)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,n,p,theta))) | any(is.infinite(c(x,n,p,theta))) |
     any(is.nan(c(x,n,p,theta))) )
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
        value1<-sapply(1:length(y),function(i) choose(n,y[i])*(p^y[i])*((1-p)^(n-y[i]))*(theta^(y[i]*(n-y[i])))/
                         (sum(choose(n,0:n)*(p^(0:n))*((1-p)^(n-(0:n)))*(theta^((0:n)*(n-(0:n)))))))
        check1<-sum(value1)
        #checking if the theta value is less than or equal to zero if so providig an error message
        #and stopping the function progress
        if(theta <= 0)
        {
          stop("Theta parameter value cannot be zero or less than zero")
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
          value<-sapply(1:length(x),function(i) choose(n,x[i])*(p^x[i])*((1-p)^(n-x[i]))*(theta^(x[i]*(n-x[i])))/
                          (sum(choose(n,0:n)*(p^(0:n))*((1-p)^(n-(0:n)))*(theta^((0:n)*(n-(0:n)))))))
          # generating an output in list format consisting pdf,mean and variance
          return(list("pdf"=value,"mean"=sum((0:n)*value1),
                      "var"=sum(((0:n)^2)*value1)-(sum((0:n)*value1))^2))
        }
      }
    }
  }
}

#' Multiplicative  Binomial Distribution
#'
#' These functions provide the ability for generating probability function values and
#' cumulative probability function values for the Multiplicative Binomial Distribution.
#'
#' @usage
#' pMultiBin(x,n,p,theta)
#'
#' @param x        vector of binomial random variables.
#' @param n        single value for no of binomial trials.
#' @param p        single value for probability of success.
#' @param theta    single value for theta.
#'
#' @details
#' The probability function and cumulative function can be constructed and are denoted below
#'
#' The cumulative probability function is the summation of probability function values.
#'
#' \deqn{P_{MultiBin}(x)= {n \choose x} p^x (1-p)^{n-x} \frac{(theta^{x(n-x)}}{f(p,theta,n)} }
#'
#' here \eqn{f(p,theta,n)} is
#' \deqn{f(p,theta,n)= \sum_{k=0}^{n} {n \choose k} p^k (1-p)^{n-k} (theta^{k(n-k)} )}
#'
#' \deqn{x = 0,1,2,3,...n}
#' \deqn{n = 1,2,3,...}
#' \deqn{k = 0,1,2,...,n}
#' \deqn{0 < p < 1}
#' \deqn{0 < theta }
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#' The output of \code{pMultiBin} gives cumulative probability values in vector form.
#'
#' @references
#' \insertRef{johnson2005univariate}{fitODBOD}
#' \insertRef{kupper1978use}{fitODBOD}
#' \insertRef{paul1985three}{fitODBOD}
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(5)
#' a <- c(0.58,0.59,0.6,0.61,0.62)
#' b <- c(0.022,0.023,0.024,0.025,0.026)
#' plot(0,0,main="Multiplicative binomial probability function graph",xlab="Binomial random variable",
#' ylab="Probability function values",xlim = c(0,10),ylim = c(0,0.5))
#' for (i in 1:5)
#' {
#' lines(0:10,dMultiBin(0:10,10,a[i],1+b[i])$pdf,col = col[i],lwd=2.85)
#' points(0:10,dMultiBin(0:10,10,a[i],1+b[i])$pdf,col = col[i],pch=16)
#' }
#'
#' dMultiBin(0:10,10,.58,10.022)$pdf   #extracting the pdf values
#' dMultiBin(0:10,10,.58,10.022)$mean   #extracting the mean
#' dMultiBin(0:10,10,.58,10.022)$var   #extracting the variance
#'
#' #plotting random variables and cumulative probability values
#' col <- rainbow(5)
#' a <- c(0.58,0.59,0.6,0.61,0.62)
#' b <- c(0.022,0.023,0.024,0.025,0.026)
#' plot(0,0,main="Multiplicative binomial probability function graph",xlab="Binomial random variable",
#' ylab="Probability function values",xlim = c(0,10),ylim = c(0,1))
#' for (i in 1:5)
#' {
#' lines(0:10,pMultiBin(0:10,10,a[i],1+b[i]),col = col[i],lwd=2.85)
#' points(0:10,pMultiBin(0:10,10,a[i],1+b[i]),col = col[i],pch=16)
#' }
#'
#' pMultiBin(0:10,10,.58,10.022)     #acquiring the cumulative probability values
#'
#' @export
pMultiBin<-function(x,n,p,theta)
{
  #for each binomial random variable in the input vector the cumulative proability function
  #values are calculated
  ans<-sapply(1:length(x),function(i) sum(dMultiBin(0:x[i],n,p,theta)$pdf))
  #generating an ouput vector cumulative probability function values
  return(ans)
}

#' Negative Log Likelihood value of Multiplicative Binomial distribution
#'
#' This function will calculate the negative log likelihood value when the vector of binomial random
#' variable and vector of corresponding frequencies are given with the input parameters.
#'
#' @usage
#' NegLLMultiBin(x,freq,p,theta)
#'
#' @param x                 vector of binomial random variables.
#' @param freq              vector of frequencies.
#' @param p                 single value for probability of success.
#' @param theta             single value for theta parameter.
#'
#' @details
#' \deqn{freq \ge 0}
#' \deqn{x = 0,1,2,..}
#' \deqn{0 < p < 1}
#' \deqn{0 < theta }
#'
#' @return
#' The output of \code{NegLLMultiBin} will produce a single numeric value.
#'
#' @references
#' \insertRef{johnson2005univariate}{fitODBOD}
#' \insertRef{kupper1978use}{fitODBOD}
#' \insertRef{paul1985three}{fitODBOD}
#'
#' @examples
#' No.D.D <- 0:7       #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)    #assigning the corresponding frequencies
#'
#' NegLLMultiBin(No.D.D,Obs.fre.1,.5,3)    #acquiring the negative log likelihood value
#'
#' @export
NegLLMultiBin<-function(x,freq,p,theta)
{
  #constructing the data set using the random variables vector and frequency vector
  n<-max(x)
  data<-rep(x,freq)
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,freq,p,theta))) | any(is.infinite(c(x,freq,p,theta))) |
     any(is.nan(c(x,freq,p,theta))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    #constructing the probability values for all random variables
    y<-0:n
    value1<-sapply(1:length(y),function(i) choose(n,y[i])*(p^y[i])*((1-p)^(n-y[i]))*(theta^(y[i]*(n-y[i])))/
                     sum(choose(n,0:n)*(p^(0:n))*((1-p)^(n-(0:n)))*(theta^((0:n)*(n-(0:n))))))
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
    else if( p <= 0 | p >= 1 | theta <= 0)
    {
      stop("Probability or Theta parameter value doesnot satisfy conditions")
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
                 log(1-p)*sum(n-data[1:sum(freq)])+
                 log(theta)*sum(data[1:sum(freq)]*(n-data[1:sum(freq)]))-
                 sum(freq)*log(sum(choose(n,k)*(p^k)*((1-p)^(n-k))*(theta^(k*(n-k)))))))
    }
  }
}

#' Estimating the probability of success and theta for Multiplicative Binomial
#' Distribution
#'
#' The function will estimate the probability of success and theta parameter using the
#' maximum log likelihood method for the Multiplicative Binomial distribution when the binomial random
#' variables and corresponding frequencies are given.
#'
#' @usage
#' EstMLEMultiBin(x,freq,p,theta,...)
#'
#' @param x                 vector of binomial random variables.
#' @param freq              vector of frequencies.
#' @param p                 single value for probability of success.
#' @param theta             single value for theta parameter.
#' @param ...               mle2 function inputs except data and estimating parameter.
#'
#' @details
#' \deqn{freq \ge 0}
#' \deqn{x = 0,1,2,..}
#' \deqn{0 < p < 1}
#' \deqn{0 < theta }
#'
#' @return
#' \code{EstMLEMultiBin} here is used as a wrapper for the \code{mle2} function of
#' \pkg{bbmle} package therefore output is of class of mle2.
#'
#' @references
#' \insertRef{johnson2005univariate}{fitODBOD}
#' \insertRef{kupper1978use}{fitODBOD}
#' \insertRef{paul1985three}{fitODBOD}
#'
#' @seealso
#' \code{\link[bbmle]{mle2}}
#'
#' @examples
#' No.D.D <- 0:7         #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)    #assigning the corresponding frequencies
#'
#' #estimating the parameters using maximum log likelihood value and assigning it
#' parameters <- EstMLEMultiBin(x=No.D.D,freq=Obs.fre.1,p=0.5,theta=15)
#'
#' bbmle::coef(parameters)           #extracting the parameters
#'
#' @export
EstMLEMultiBin<-function(x,freq,p,theta,...)
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
  output<-suppressWarnings2(bbmle::mle2(.EstMLEMultiBin,data=list(x=x,freq=freq),
                                        start = list(p=p,theta=theta),...),"NaN")
  return(output)
}


.EstMLEMultiBin<-function(x,freq,p,theta)
{
  #with respective to using bbmle package function mle2 there is no need impose any restrictions
  #therefor the output is directly a single numeric value for the negative log likelihood value of
  #MultiBinomial distribution
  n<-max(x)
  data<-rep(x,freq)
  k<-0:n
  return(-(sum(log(choose(n,data[1:sum(freq)])))+ log(p)*sum(data[1:sum(freq)])+
             log(1-p)*sum(n-data[1:sum(freq)])+
             log(theta)*sum(data[1:sum(freq)]*(n-data[1:sum(freq)]))-
             sum(freq)*log(sum(choose(n,k)*(p^k)*((1-p)^(n-k))*(theta^(k*(n-k)))))))
}

#' Fitting the Multiplicative Binomial Distribution when binomial
#' random variable, frequency, probability of success and theta parameter are given
#'
#' The function will fit the Multiplicative Binomial distribution
#' when random variables, corresponding frequencies, probability of success and theta parameter
#' are given. It will provide the expected frequencies, chi-squared test statistics value,
#' p value and degree of freedom  value so that it can be seen if this distribution
#' fits the data.
#'
#' @usage fitMultiBin(x,obs.freq,p,theta)
#'
#' @param x                  vector of binomial random variables.
#' @param obs.freq           vector of frequencies.
#' @param p                  single value for probability of success.
#' @param theta              single value for theta parameter.
#'
#' @details
#' \deqn{obs.freq \ge 0}
#' \deqn{x = 0,1,2,..}
#' \deqn{0 < p < 1}
#' \deqn{0 < theta }
#'
#' @return
#' The output of \code{fitMultiBin} gives the class format \code{fitMuB} and \code{fit} consisting a list
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
#' \code{fitMuB} fitted probability values of \code{dMultiBin}.
#'
#' \code{NegLL} Negative Log Likelihood value.
#'
#' \code{p} estimated probability value.
#'
#' \code{theta} estimated theta parameter value.
#'
#' \code{AIC} AIC value.
#'
#' \code{call} the inputs of the function.
#'
#' Methods \code{summary}, \code{print}, \code{AIC}, \code{residuals} and \code{fitted}
#' can be used to extract specific outputs.
#'
#' @references
#' \insertRef{johnson2005univariate}{fitODBOD}
#' \insertRef{kupper1978use}{fitODBOD}
#' \insertRef{paul1985three}{fitODBOD}
#'
#' @seealso
#' \code{\link[bbmle]{mle2}}
#'
#' @examples
#' No.D.D <- 0:7       #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)     #assigning the corresponding frequencies
#'
#' #estimating the parameters using maximum log likelihood value and assigning it
#' parameters <- EstMLEMultiBin(x=No.D.D,freq=Obs.fre.1,p=0.1,theta=.3)
#'
#' pMultiBin <- bbmle::coef(parameters)[1]    #assigning the estimated probability value
#' thetaMultiBin <- bbmle::coef(parameters)[2]  #assigning the estimated theta value
#'
#' #fitting when the random variable,frequencies,probability and theta are given
#' results <- fitMultiBin(No.D.D,Obs.fre.1,pMultiBin,thetaMultiBin)
#' results
#'
#' #extracting the AIC value
#' AIC(results)
#'
#' #extract fitted values
#' fitted(results)
#'
#' @export
fitMultiBin<-function(x,obs.freq,p,theta)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,obs.freq,p,theta))) | any(is.infinite(c(x,obs.freq,p,theta))) |
     any(is.nan(c(x,obs.freq,p,theta))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    est<-dMultiBin(x,max(x),p,theta)
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
    NegLL<-NegLLMultiBin(x,obs.freq,p,theta)
    names(NegLL)<-NULL
    #the final output is in a list format containing the calculated values
    final<-list("bin.ran.var"=x,"obs.freq"=obs.freq,"exp.freq"=exp.freq,
                "statistic"=round(statistic,4),"df"=df,"p.value"=round(p.value,4),"fitMuB"=est,
                "NegLL"=NegLL,"p"=p,"theta"=theta,"AIC"=2*2+2*NegLL,"call"=match.call())
    class(final)<-c("fitMuB","fit")
    return(final)
  }
}

#' @method fitMultiBin default
#' @export
fitMultiBin.default<-function(x,obs.freq,p,theta)
{
  return(fitMultiBin(x,obs.freq,p,theta))
}

#' @method print fitMuB
#' @export
print.fitMuB<-function(x,...)
{
  cat("Call: \n")
  print(x$call)
  cat("\nChi-squared test for Multiplicative Binomial Distribution \n\t
      Observed Frequency : ",x$obs.freq,"\n\t
      expected Frequency : ",x$exp.freq,"\n\t
      estimated p value :",x$p," ,estimated theta parameter :",x$theta,"\n\t
      X-squared :",x$statistic,"  ,df :",x$df,"  ,p-value :",x$p.value,"\n")
}

#' @method summary fitMuB
#' @export
summary.fitMuB<-function(object,...)
{
  cat("Call: \n")
  print(object$call)
  cat("\nChi-squared test for Multiplicative Binomial Distribution \n\t
      Observed Frequency : ",object$obs.freq,"\n\t
      expected Frequency : ",object$exp.freq,"\n\t
      estimated p value :",object$p," ,estimated theta parameter :",object$theta,"\n\t
      X-squared :",object$statistic,"  ,df :",object$df,"  ,p-value :",object$p.value,"\n\t
      Negative Loglikehood value :",object$NegLL,"\n\t
      AIC value :",object$AIC,"\n")
}

#' @importFrom bbmle mle2
#' @importFrom stats pchisq
