#' Correlated  Binomial Distribution
#'
#' These functions provide the ability for generating probability function values and
#' cumulative probability function values for the Correlated  Binomial Distribution.
#'
#' @usage
#' dCorrBin(x,n,p,cov)
#'
#' @param x        vector of binomial random variables.
#' @param n        single value for no of binomial trials.
#' @param p        single value for probability of success.
#' @param cov      single value for covariance.
#'
#' @details
#' The probability function and cumulative function can be constructed and are denoted below
#'
#' The cumulative probability function is the summation of probability function values.
#'
#' \deqn{P_{CorrBin}(x) = {n \choose x}(p^x)(1-p)^{n-x}(1+(\frac{cov}{2p^2(1-p)^2})((x-np)^2+x(2p-1)-np^2)) }
#' \deqn{x = 0,1,2,3,...n}
#' \deqn{n = 1,2,3,...}
#' \deqn{0 < p < 1}
#' \deqn{-\infty < cov < +\infty }
#'
#' The Correlation is in between
#' \deqn{\frac{-2}{n(n-1)} min(\frac{p}{1-p},\frac{1-p}{p}) \le correlation \le \frac{2p(1-p)}{(n-1)p(1-p)+0.25-fo} }
#' where \eqn{fo=min [(x-(n-1)p-0.5)^2] }
#'
#' The mean and the variance are denoted as
#' \deqn{E_{CorrBin}[x]= np}
#' \deqn{Var_{CorrBin}[x]= n(p(1-p)+(n-1)cov)}
#' \deqn{Corr_{CorrBin}[x]=\frac{cov}{p(1-p)}}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#' The output of \code{dCorrBin} gives a list format consisting
#'
#' \code{pdf}           probability function values in vector form.
#'
#' \code{mean}          mean of Correlated  Binomial Distribution.
#'
#' \code{var}           variance of Correlated  Binomial Distribution.
#'
#' \code{corr}          correlation of Correlated Binomial Distribution.
#'
#' \code{mincorr}       minimum correlation value possible.
#'
#' \code{maxcorr}       maximum correlation value possible.
#'
#' @references
#' \insertRef{johnson2005univariate}{fitODBOD}
#' \insertRef{kupper1978use}{fitODBOD}
#' \insertRef{paul1985three}{fitODBOD}
#' \insertRef{morel2012overdispersion}{fitODBOD}
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(5)
#' a <- c(0.58,0.59,0.6,0.61,0.62)
#' b <- c(0.022,0.023,0.024,0.025,0.026)
#' plot(0,0,main="Correlated binomial probability function graph",xlab="Binomial random variable",
#' ylab="Probability function values",xlim = c(0,10),ylim = c(0,0.5))
#' for (i in 1:5)
#' {
#' lines(0:10,dCorrBin(0:10,10,a[i],b[i])$pdf,col = col[i],lwd=2.85)
#' points(0:10,dCorrBin(0:10,10,a[i],b[i])$pdf,col = col[i],pch=16)
#' }
#'
#' dCorrBin(0:10,10,0.58,0.022)$pdf      #extracting the pdf values
#' dCorrBin(0:10,10,0.58,0.022)$mean     #extracting the mean
#' dCorrBin(0:10,10,0.58,0.022)$var      #extracting the variance
#' dCorrBin(0:10,10,0.58,0.022)$corr     #extracting the correlation
#' dCorrBin(0:10,10,0.58,0.022)$mincorr  #extracting the minimum correlation value
#' dCorrBin(0:10,10,0.58,0.022)$maxcorr  #extracting the maximum correlation value
#'
#' #plotting the random variables and cumulative probability values
#' col <- rainbow(5)
#' a <- c(0.58,0.59,0.6,0.61,0.62)
#' b <- c(0.022,0.023,0.024,0.025,0.026)
#' plot(0,0,main="Correlated binomial probability function graph",xlab="Binomial random variable",
#' ylab="Probability function values",xlim = c(0,10),ylim = c(0,1))
#' for (i in 1:5)
#' {
#' lines(0:10,pCorrBin(0:10,10,a[i],b[i]),col = col[i],lwd=2.85)
#' points(0:10,pCorrBin(0:10,10,a[i],b[i]),col = col[i],pch=16)
#' }
#'
#' pCorrBin(0:10,10,0.58,0.022)      #acquiring the cumulative probability values
#'
#' @importFrom Rdpack reprompt
#' @export
dCorrBin<-function(x,n,p,cov)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,n,p,cov))) | any(is.infinite(c(x,n,p,cov))) | any(is.nan(c(x,n,p,cov))) )
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
        right.h<-(2*p*(1-p))/(((n-1)*p*(1-p))+0.25-min(((0:n)-(n-1)*p-0.5)^2))
        # checking if the correlation output satisfies conditions mentioned above
        if(correlation < -1 | correlation > 1 | correlation < left.h | correlation > right.h)
        {
          stop("Correlation cannot be greater than 1 or Lesser than -1 or it cannot be greater than Maximum Correlation or Lesser than Minimum Correlation")
        }
        else
        {
          #constructing the probability values for all random variables
          y<-0:n
          value1<-sapply(1:length(y),function(i) ((choose(n,y[i]))*(p^y[i])*((1-p)^(n-y[i]))*
                            (1+(cov/(2*(p^2)*((1-p)^2)))*(((y[i]-n*p)^2)+(y[i]*(2*p-1))-(n*(p^2))))))

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
            value<-sapply(1:length(x),function(i) ((choose(n,x[i]))*(p^x[i])*((1-p)^(n-x[i]))*
                            (1+(cov/(2*(p^2)*((1-p)^2)))*(((x[i]-n*p)^2)+(x[i]*(2*p-1))-(n*(p^2))))))

            # generating an output in list format consisting pdf,mean and variance
            return(list("pdf"=value,"mean"=n*p,"var"=n*(p*(1-p)+(n-1)*cov),
                        "corr"=cov/(p*(1-p)),"mincorr"=left.h,"maxcorr"=right.h))
          }
        }
      }
    }
  }
}

#' Correlated  Binomial Distribution
#'
#' These functions provide the ability for generating probability function values and
#' cumulative probability function values for the Correlated  Binomial Distribution.
#'
#' @usage
#' pCorrBin(x,n,p,cov)
#'
#' @param x        vector of binomial random variables.
#' @param n        single value for no of binomial trials.
#' @param p        single value for probability of success.
#' @param cov      single value for covariance.
#'
#' @details
#' The probability function and cumulative function can be constructed and are denoted below
#'
#' The cumulative probability function is the summation of probability function values.
#'
#' \deqn{P_{CorrBin}(x) = {n \choose x}(p^x)(1-p)^{n-x}(1+(\frac{cov}{2p^2(1-p)^2})((x-np)^2+x(2p-1)-np^2)) }
#' \eqn{x = 0,1,2,3,...n}
#' \eqn{n = 1,2,3,...}
#' \eqn{0 < p < 1}
#' \eqn{-\infty < cov < +\infty }
#'
#' The Correlation is in between
#' \deqn{\frac{-2}{n(n-1)} min(\frac{p}{1-p},\frac{1-p}{p}) \le cov \le \frac{2p(1-p)}{(n-1)p(1-p)+0.25-fo} }
#' where \eqn{fo=min (x-(n-1)p-0.5)^2 }
#'
#' The mean and the variance are denoted as
#' \deqn{E_{CorrBin}[x]= np}
#' \deqn{Var_{CorrBin}[x]= n(p(1-p)+(n-1)cov)}
#' \deqn{Corr_{CorrBin}[x]=\frac{cov}{p(1-p)}}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#' The output of \code{pCorrBin} gives cumulative probability  values in vector form.
#'
#' @references
#' \insertRef{johnson2005univariate}{fitODBOD}
#' \insertRef{kupper1978use}{fitODBOD}
#' \insertRef{paul1985three}{fitODBOD}
#' \insertRef{morel2012overdispersion}{fitODBOD}
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(5)
#' a <- c(0.58,0.59,0.6,0.61,0.62)
#' b <- c(0.022,0.023,0.024,0.025,0.026)
#' plot(0,0,main="Correlated binomial probability function graph",xlab="Binomial random variable",
#' ylab="Probability function values",xlim = c(0,10),ylim = c(0,0.5))
#' for (i in 1:5)
#' {
#' lines(0:10,dCorrBin(0:10,10,a[i],b[i])$pdf,col = col[i],lwd=2.85)
#' points(0:10,dCorrBin(0:10,10,a[i],b[i])$pdf,col = col[i],pch=16)
#' }
#'
#' dCorrBin(0:10,10,0.58,0.022)$pdf      #extracting the pdf values
#' dCorrBin(0:10,10,0.58,0.022)$mean     #extracting the mean
#' dCorrBin(0:10,10,0.58,0.022)$var      #extracting the variance
#' dCorrBin(0:10,10,0.58,0.022)$corr     #extracting the correlation
#' dCorrBin(0:10,10,0.58,0.022)$mincorr  #extracting the minimum correlation value
#' dCorrBin(0:10,10,0.58,0.022)$maxcorr  #extracting the maximum correlation value
#'
#' #plotting the random variables and cumulative probability values
#' col <- rainbow(5)
#' a <- c(0.58,0.59,0.6,0.61,0.62)
#' b <- c(0.022,0.023,0.024,0.025,0.026)
#' plot(0,0,main="Correlated binomial probability function graph",xlab="Binomial random variable",
#' ylab="Probability function values",xlim = c(0,10),ylim = c(0,1))
#' for (i in 1:5)
#' {
#' lines(0:10,pCorrBin(0:10,10,a[i],b[i]),col = col[i],lwd=2.85)
#' points(0:10,pCorrBin(0:10,10,a[i],b[i]),col = col[i],pch=16)
#' }
#'
#' pCorrBin(0:10,10,0.58,0.022)      #acquiring the cumulative probability values
#'
#' @export
pCorrBin<-function(x,n,p,cov)
{
  #for each binomial random variable in the input vector the cumulative proability function
  #values are calculated
  ans<-sapply(1:length(x),function(i) sum(dCorrBin(0:x[i],n,p,cov)$pdf))
  #generating an ouput vector cumulative probability function values
  return(ans)
}

#' Negative Log Likelihood value of Correlated Binomial distribution
#'
#' This function will calculate the negative log likelihood value when the vector of binomial random
#' variables and vector of corresponding frequencies are given with the input parameters.
#'
#' @usage
#' NegLLCorrBin(x,freq,p,cov)
#'
#' @param x                 vector of binomial random variables.
#' @param freq              vector of frequencies.
#' @param p                 single value for probability of success.
#' @param cov               single value for covariance.
#'
#' @details
#' \deqn{freq \ge 0}
#' \deqn{x = 0,1,2,..}
#' \deqn{0 < p < 1}
#' \deqn{-\infty < cov < +\infty}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#' The output of \code{NegLLCorrBin} will produce a single numeric value.
#'
#' @references
#' \insertRef{johnson2005univariate}{fitODBOD}
#' \insertRef{kupper1978use}{fitODBOD}
#' \insertRef{paul1985three}{fitODBOD}
#' \insertRef{morel2012overdispersion}{fitODBOD}
#'
#' @examples
#' No.D.D <- 0:7         #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)      #assigning the corresponding frequencies
#'
#' NegLLCorrBin(No.D.D,Obs.fre.1,.5,.03)     #acquiring the negative log likelihood value
#'
#' @export
NegLLCorrBin<-function(x,freq,p,cov)
{
  #constructing the data set using the random variables vector and frequency vector
  n<-max(x)
  data<-rep(x,freq)
  correlation<-cov/(p*(1-p))
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,freq,p,cov))) | any(is.infinite(c(x,freq,p,cov))) |
     any(is.nan(c(x,freq,p,cov))) )
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
      right.h<-(2*p*(1-p))/(((n-1)*p*(1-p))+0.25-(min(((0:n)-(n-1)*p-0.5)^2)))
      # checking if the correlation output satisfies conditions mentioned above
      if(correlation < -1 | correlation > 1 | correlation < left.h | correlation > right.h)
      {
        stop("Correlation cannot be greater than 1 or Lesser than -1 or it cannot be greater than Maximum Correlation or Lesser than Minimum Correlation")
      }
      else
      {
        #constructing the probability values for all random variables
        y<-0:n
        value1<-sapply(1:length(y),function(i) ((choose(n,y[i]))*(p^y[i])*((1-p)^(n-y[i]))*
                          (1+(cov/(2*(p^2)*((1-p)^2)))*(((y[i]-n*p)^2)+(y[i]*(2*p-1))-(n*(p^2))))))

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
          value<-sapply(1:sum(freq),function(i) log(1+((cov/(2*(p^2)*((1-p)^2)))*(((data[i]-n*p)^2)+(data[i]*(2*p-1))-(n*(p^2))))))
          #calculating the negative log likelihood value and representing as a single output value
          return(-(sum(log(choose(n,data[1:sum(freq)]))) +
                     log(p)*sum(data[1:sum(freq)]) + log(1-p)*sum(n-data[1:sum(freq)]) + sum(value)))
        }
      }
    }
  }
}

#' Estimating the probability of success and correlation for Correlated Binomial
#' Distribution
#'
#' The function will estimate the probability of success and correlation using the maximum log
#' likelihood method for the Correlated Binomial distribution when the binomial random
#' variables and corresponding frequencies are given.
#'
#' @usage
#' EstMLECorrBin(x,freq,p,cov,...)
#'
#'
#' @param x       vector of binomial random variables.
#' @param freq    vector of frequencies.
#' @param p       single value for probability of success.
#' @param cov     single value for covariance.
#' @param ...     mle2 function inputs except data and estimating parameter.
#'
#' @details
#' \deqn{x = 0,1,2,...}
#' \deqn{freq \ge 0}
#' \deqn{0 < p < 1}
#' \deqn{-\infty < cov < +\infty}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#' \code{EstMLECorrBin} here is used as a wrapper for the \code{mle2} function of \pkg{bbmle} package
#' therefore output is of class of mle2.
#'
#' @references
#' \insertRef{johnson2005univariate}{fitODBOD}
#' \insertRef{kupper1978use}{fitODBOD}
#' \insertRef{paul1985three}{fitODBOD}
#' \insertRef{morel2012overdispersion}{fitODBOD}
#'
#' @seealso
#' \code{\link[bbmle]{mle2}}
#'
#' @examples
#' No.D.D <- 0:7               #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)     #assigning the corresponding frequencies
#'
#' #estimating the parameters using maximum log likelihood value and assigning it
#' parameters <- EstMLECorrBin(x=No.D.D,freq=Obs.fre.1,p=0.5,cov=0.0050)
#'
#' bbmle::coef(parameters)           #extracting the parameters
#'
#'@export
EstMLECorrBin<-function(x,freq,p,cov,...)
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
  output<-suppressWarnings2(bbmle::mle2(.EstMLECorrBin,data=list(x=x,freq=freq),
                                        start = list(p=p,cov=cov),...),"NaN")
  return(output)
}


.EstMLECorrBin<-function(x,freq,p,cov)
{
  #with respective to using bbmle package function mle2 there is no need impose any restrictions
  #therefor the output is directly a single numeric value for the negative log likelihood value of
  #Correlated Binomial distribution
  n<-max(x)
  data<-rep(x,freq)
  value<-sapply(1:sum(freq),function(i) log(1+((cov/(2*(p^2)*((1-p)^2)))*(((data[i]-n*p)^2)+(data[i]*(2*p-1))-(n*(p^2))))))

  return(-(sum(log(choose(n,data[1:sum(freq)]))) + log(p)*sum(data[1:sum(freq)]) +
             log(1-p)*sum(n-data[1:sum(freq)]) + sum(value)))
}

#' Fitting the Correlated Binomial Distribution when binomial
#' random variable, frequency, probability of success and covariance are given
#'
#' The function will fit the Correlated Binomial Distribution
#' when random variables, corresponding frequencies, probability of success and covariance are given.
#' It will provide the expected frequencies, chi-squared test statistics value, p value,
#' and degree of freedom so that it can be seen if this distribution fits the data.
#'
#' @usage
#' fitCorrBin(x,obs.freq,p,cov)
#'
#' @param x                  vector of binomial random variables.
#' @param obs.freq           vector of frequencies.
#' @param p                  single value for probability of success.
#' @param cov                single value for covariance.
#'
#' @details
#' \deqn{obs.freq \ge 0}
#' \deqn{x = 0,1,2,..}
#' \deqn{0 < p < 1}
#' \deqn{-\infty < cov < +\infty}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#' The output of \code{fitCorrBin} gives the class format \code{fitCB} and \code{fit} consisting a list
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
#' \code{corr}    Correlation value.
#'
#' \code{fitCB} fitted probability values of \code{dCorrBin}.
#'
#' \code{NegLL} Negative Log Likelihood value.
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
#' \insertRef{morel2012overdispersion}{fitODBOD}
#'
#' @examples
#' No.D.D <- 0:7       #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)      #assigning the corresponding frequencies
#'
#' #estimating the parameters using maximum log likelihood value and assigning it
#' parameters <- EstMLECorrBin(x=No.D.D,freq=Obs.fre.1,p=0.5,cov=0.0050)
#'
#' pCorrBin <- bbmle::coef(parameters)[1]
#' covCorrBin <- bbmle::coef(parameters)[2]
#'
#' #fitting when the random variable,frequencies,probability and covariance are given
#' results <- fitCorrBin(No.D.D,Obs.fre.1,pCorrBin,covCorrBin)
#' results
#'
#' #extracting the AIC value
#' AIC(results)
#'
#' #extract fitted values
#' fitted(results)
#'
#' @export
fitCorrBin<-function(x,obs.freq,p,cov)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,obs.freq,p,cov))) | any(is.infinite(c(x,obs.freq,p,cov))) |
     any(is.nan(c(x,obs.freq,p,cov))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    est<-dCorrBin(x,max(x),p,cov)
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
    NegLL<-NegLLCorrBin(x,obs.freq,p,cov)
    names(NegLL)<-NULL
    #the final output is in a list format containing the calculated values
    final<-list("bin.ran.var"=x,"obs.freq"=obs.freq,"exp.freq"=exp.freq,"statistic"=round(statistic,4),
                "df"=df,"p.value"=round(p.value,4),"corr"=est$corr,"fitCB"=est,"NegLL"=NegLL,
                "p"=p,"cov"=cov,"AIC"=2*2+2*NegLL,"call"=match.call())
    class(final)<-c("fitCB","fit")
    return(final)
    }
  }

#' @method fitCorrBin default
#' @export
fitCorrBin.default<-function(x,obs.freq,p,cov)
{
  return(fitCorrBin(x,obs.freq,p,cov))
}

#' @method print fitCB
#' @export
print.fitCB<-function(x,...)
{
  cat("Call: \n")
  print(x$call)
  cat("\nChi-squared test for Correlated Binomial Distribution \n\t
      Observed Frequency : ",x$obs.freq,"\n\t
      expected Frequency : ",x$exp.freq,"\n\t
      estimated p value :",x$p," ,estimated cov value :",x$cov,"\n\t
      X-squared :",x$statistic,"  ,df :",x$df,"  ,p-value :",x$p.value,"\n")
}

#' @method summary fitCB
#' @export
summary.fitCB<-function(object,...)
{
  cat("Call: \n")
  print(object$call)
  cat("\nChi-squared test for Correlated Binomial Distribution \n\t
      Observed Frequency : ",object$obs.freq,"\n\t
      expected Frequency : ",object$exp.freq,"\n\t
      estimated p value :",object$p," ,estimated cov value :",object$cov,"\n\t
      X-squared :",object$statistic,"  ,df :",object$df,"  ,p-value :",object$p.value,"\n\t
      Negative Loglikehood value :",object$NegLL,"\n\t
      AIC value :",object$AIC,"\n")
}


#' @importFrom bbmle mle2
#' @importFrom stats pchisq
