#' Additive  Binomial Distribution
#'
#' These functions provide the ability for generating probability function values and
#' cumulative probability function values for the Additive Binomial Distribution.
#'
#' @usage
#' dAddBin(x,n,p,alpha)
#'
#' @param x        vector of binomial random variables.
#' @param n        single value for no of binomial trials.
#' @param p        single value for probability of success
#' @param alpha    single value for alpha parameter.
#'
#' @details
#' The probability function and cumulative function can be constructed and are denoted below
#'
#' The cumulative probability function is the summation of probability function values.
#'
#' \deqn{P_{AddBin}(x)= {n \choose x} p^x (1-p)^{n-x}(\frac{alpha}{2}(\frac{x(x-1)}{p}+\frac{(n-x)(n-x-1)}{(1-p)}-\frac{alpha(n-1)n}{2})+1)}
#'
#' The alpha is in between
#' \deqn{\frac{-2}{n(n-1)}min(\frac{p}{1-p},\frac{1-p}{p}) \le alpha \le (\frac{n+(2p-1)^2}{4p(1-p)})^{-1}}
#'
#' \deqn{x = 0,1,2,3,...n}
#' \deqn{n = 1,2,3,...}
#' \deqn{0 < p < 1}
#' \deqn{-1 < alpha < 1}
#'
#' The mean and the variance are denoted as
#' \deqn{E_{Addbin}[x]=np}
#' \deqn{Var_{Addbin}[x]=np(1-p)(1+(n-1)alpha)}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#' The output of \code{dAddBin} gives a list format consisting
#'
#' \code{pdf} probability function values in vector form.
#'
#' \code{mean} mean of Additive Binomial Distribution.
#'
#' \code{var} variance of Additive Binomial Distribution.
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
#' plot(0,0,main="Additive binomial probability function graph",xlab="Binomial random variable",
#' ylab="Probability function values",xlim = c(0,10),ylim = c(0,0.5))
#' for (i in 1:5)
#' {
#'   lines(0:10,dAddBin(0:10,10,a[i],b[i])$pdf,col = col[i],lwd=2.85)
#'   points(0:10,dAddBin(0:10,10,a[i],b[i])$pdf,col = col[i],pch=16)
#' }
#'
#' dAddBin(0:10,10,0.58,0.022)$pdf     #extracting the probability values
#' dAddBin(0:10,10,0.58,0.022)$mean    #extracting the mean
#' dAddBin(0:10,10,0.58,0.022)$var     #extracting the variance
#'
#' #plotting the random variables and cumulative probability values
#' col <- rainbow(5)
#' a <- c(0.58,0.59,0.6,0.61,0.62)
#' b <- c(0.022,0.023,0.024,0.025,0.026)
#' plot(0,0,main="Additive binomial probability function graph",xlab="Binomial random variable",
#' ylab="Probability function values",xlim = c(0,10),ylim = c(0,1))
#' for (i in 1:5)
#' {
#' lines(0:10,pAddBin(0:10,10,a[i],b[i]),col = col[i],lwd=2.85)
#' points(0:10,pAddBin(0:10,10,a[i],b[i]),col = col[i],pch=16)
#' }
#'
#' pAddBin(0:10,10,0.58,0.022)       #acquiring the cumulative probability values
#'
#' @importFrom Rdpack reprompt
#' @export
dAddBin<-function(x,n,p,alpha)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,n,p,alpha))) | any(is.infinite(c(x,n,p,alpha))) |
     any(is.nan(c(x,n,p,alpha))))
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
      #checking the probability value is inbetween zero and one or alpha value is inbetween negative one
      #and positive one
      if( p <= 0 | p >= 1| alpha > 1 | alpha < -1)
      {
        stop("Probability or alpha value doesnot satisfy conditions")
      }
      else
      {
        #creating the necessary limits for alpha, the left hand side and right hand side limits
        right.h<-2*(n+((2*p-1)^2)/(4*p*(1-p)))^(-1)
        left.h<-(-2/(n*(n-1)))*min(p/(1-p),(1-p)/p)

        #constructing the probability values for all random variables
        y<-0:n
        value1<-sapply(1:length(y),function(i) (choose(n,y[i])*(p^y[i])*((1-p)^(n-y[i])))*
                         ((alpha/2)*((y[i]*(y[i]-1)/p)+((n-y[i])*(n-y[i]-1)/(1-p)))-(alpha*n*(n-1)/2) + 1))
        check1<-sum(value1)
        #checking if the alpha is inbetween the limits given
        if(left.h > alpha | alpha >right.h)
        {
          stop("alpha parameter doesnot satisfy the conditions")
        }
        #checking if the sum of all probability values leads upto one
        #if not providing an error message and stopping the function progress
        else if(check1 < 0.9999 | check1 >1.0001 | any(value1 < 0) | any(value1 >1))
        {
          stop("Input parameter combinations of probability of success and alpha does
               not create proper probability function")
        }
        else
        {
          #for each random variable in the input vector below calculations occur
          value<-sapply(1:length(x), function(i) (choose(n,x[i])*(p^x[i])*((1-p)^(n-x[i])))*
                          ((alpha/2)*((x[i]*(x[i]-1)/p)+((n-x[i])*(n-x[i]-1)/(1-p)))-(alpha*n*(n-1)/2) + 1))

          # generating an output in list format consisting pdf,mean and variance
          return(list("pdf"=value,"mean"=n*p,"var"=n*p*(1-p)*(1+(n-1)*alpha)))
        }
      }
    }
  }
}

#' Additive  Binomial Distribution
#'
#' These functions provide the ability for generating probability function values and
#' cumulative probability function values for the Additive Binomial Distribution.
#'
#' @usage
#' pAddBin(x,n,p,alpha)
#'
#' @param x        vector of binomial random variables.
#' @param n        single value for no of binomial trials.
#' @param p        single value for probability of success.
#' @param alpha    single value for alpha parameter.
#'
#' @details
#' The probability function and cumulative function can be constructed and are denoted below
#'
#' The cumulative probability function is the summation of probability function values.
#'
#' \deqn{P_{AddBin}(x)= {n \choose x} p^x (1-p)^{n-x}(\frac{alpha}{2}(\frac{x(x-1)}{p}+\frac{(n-x)(n-x-1)}{(1-p)}-\frac{alpha n(n-1)}{2})+1)}
#'
#' \deqn{x = 0,1,2,3,...n}
#' \deqn{n = 1,2,3,...}
#' \deqn{0 < p < 1}
#' \deqn{-1 < alpha < 1}
#'
#' The alpha is in between
#' \deqn{\frac{-2}{n(n-1)}min(\frac{p}{1-p},\frac{1-p}{p}) \le alpha \le (\frac{n+(2p-1)^2}{4p(1-p)})^{-1}}
#'
#' The mean and the variance are denoted as
#' \deqn{E_{Addbin}[x]=np}
#' \deqn{Var_{Addbin}[x]=np(1-p)(1+(n-1)alpha)}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#' The output of \code{pAddBin} gives  cumulative probability  values in vector form.
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
#' plot(0,0,main="Additive binomial probability function graph",xlab="Binomial random variable",
#' ylab="Probability function values",xlim = c(0,10),ylim = c(0,0.5))
#' for (i in 1:5)
#' {
#'   lines(0:10,dAddBin(0:10,10,a[i],b[i])$pdf,col = col[i],lwd=2.85)
#'   points(0:10,dAddBin(0:10,10,a[i],b[i])$pdf,col = col[i],pch=16)
#' }
#'
#' dAddBin(0:10,10,0.58,0.022)$pdf     #extracting the probability values
#' dAddBin(0:10,10,0.58,0.022)$mean    #extracting the mean
#' dAddBin(0:10,10,0.58,0.022)$var     #extracting the variance
#'
#' #plotting the random variables and cumulative probability values
#' col <- rainbow(5)
#' a <- c(0.58,0.59,0.6,0.61,0.62)
#' b <- c(0.022,0.023,0.024,0.025,0.026)
#' plot(0,0,main="Additive binomial probability function graph",xlab="Binomial random variable",
#' ylab="Probability function values",xlim = c(0,10),ylim = c(0,1))
#' for (i in 1:5)
#' {
#' lines(0:10,pAddBin(0:10,10,a[i],b[i]),col = col[i],lwd=2.85)
#' points(0:10,pAddBin(0:10,10,a[i],b[i]),col = col[i],pch=16)
#' }
#'
#' pAddBin(0:10,10,0.58,0.022)       #acquiring the cumulative probability values
#'
#' @export
pAddBin<-function(x,n,p,alpha)
{
  #for each binomial random variable in the input vector the cumulative proability function
  #values are calculated
  ans<-sapply(1:length(x), function(i) sum(dAddBin(0:x[i],n,p,alpha)$pdf))
  #generating an ouput vector cumulative probability function values
  return(ans)
}

#' Negative Log Likelihood value of Additive Binomial distribution
#'
#' This function will calculate the negative log likelihood value when the vector of binomial random
#' variable and vector of corresponding frequencies are given with the input parameters.
#'
#' @usage
#' NegLLAddBin(x,freq,p,alpha)
#'
#' @param x                 vector of binomial random variables.
#' @param freq              vector of frequencies.
#' @param p                 single value for probability of success.
#' @param alpha             single value for alpha parameter.
#'
#' @details
#' \deqn{freq \ge 0}
#' \deqn{x = 0,1,2,..}
#' \deqn{0 < p < 1}
#' \deqn{-1 < alpha < 1}
#'
#' @return
#' The output of \code{NegLLAddBin} will produce a single  numeric value.
#'
#' @references
#' \insertRef{johnson2005univariate}{fitODBOD}
#' \insertRef{kupper1978use}{fitODBOD}
#' \insertRef{paul1985three}{fitODBOD}
#' \insertRef{morel2012overdispersion}{fitODBOD}
#'
#' @examples
#' No.D.D <- 0:7          #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)         #assigning the corresponding frequencies
#'
#' NegLLAddBin(No.D.D,Obs.fre.1,.5,.03)         #acquiring the negative log likelihood value
#'
#' @export
NegLLAddBin<-function(x,freq,p,alpha)
{
  #constructing the data set using the random variables vector and frequency vector
  n<-max(x)
  data<-rep(x,freq)
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,freq,p,alpha))) | any(is.infinite(c(x,freq,p,alpha))) |
     any(is.nan(c(x,freq,p,alpha))) )
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
    #checking the probability value is inbetween zero and one or alpha value is inbetween negative one
    #and positive one
    else if( p <= 0 | p >= 1 | alpha > 1 | alpha < -1)
    {
      stop("Probability or alpha value doesnot satisfy conditions")
    }
    else
    {
      #creating the necessary limits for alpha, the left hand side and right hand side limits
      right.h<-2*(n+((2*p-1)^2)/(4*p*(1-p)))^(-1)
      left.h<-(-2/(n*(n-1)))*min(p/(1-p),(1-p)/p)
      #constructing the probability values for all random variables
      y<-0:n
      value1<-sapply(1:length(y), function(i) (choose(n,y[i])*(p^y[i])*((1-p)^(n-y[i])))*
                       ((alpha/2)*((y[i]*(y[i]-1)/p)+((n-y[i])*(n-y[i]-1)/(1-p)))-(alpha*n*(n-1)/2) + 1))
      check1<-sum(value1)
      #checking if the alpha is inbetween the limits given
      if(left.h > alpha | alpha > right.h)
      {
        stop("alpha parameter doesnot satisfy the conditions")
      }
      #checking if the sum of all probability values leads upto one
      #if not providing an error message and stopping the function progress
      else if(check1 < 0.9999 | check1 >1.0001 | any(value1 < 0) | any(value1 >1))
      {
        stop("Input parameter combinations of probability of success and alpha does
             not create proper probability function")
      }
      else
      {
        value<-sapply(1:sum(freq),function(i) ((alpha/2)*((data[i]*(data[i]-1)/p)+((n-data[i])*(n-data[i]-1)/(1-p)))-
                                                 (alpha*n*(n-1)/2) + 1))

      }
      #calculating the negative log likelihood value and representing as a single output value
      return(-(sum(log(choose(n,data[1:sum(freq)]))) + log(p)*sum(data[1:sum(freq)]) +
                 log(1-p)*sum(n-data[1:sum(freq)]) + sum(log(value))))
    }
  }
}

#' Estimating the probability of success and alpha for Additive Binomial
#' Distribution
#'
#' The function will estimate the probability of success and alpha using the maximum log likelihood method
#' for the Additive Binomial distribution when the binomial random
#' variables and corresponding frequencies are given.
#'
#' @usage
#' EstMLEAddBin(x,freq)
#'
#' @param x                vector of binomial random variables.
#' @param freq             vector of frequencies.
#'
#' @details
#' \deqn{freq \ge 0}
#' \deqn{x = 0,1,2,..}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#' The output of \code{EstMLEAddBin} will produce the class \code{mlAB} and \code{ml} with a list consisting
#'
#' \code{min} Negative Log Likelihood value.
#'
#' \code{p} estimated probability of success.
#'
#' \code{alpha} estimated alpha parameter.
#'
#' \code{AIC}  AIC value.
#'
#' \code{call} the inputs for the function.
#'
#' Methods \code{print}, \code{summary}, \code{coef} and \code{AIC} can be used to extract specific outputs.
#'
#' @references
#' \insertRef{johnson2005univariate}{fitODBOD}
#' \insertRef{kupper1978use}{fitODBOD}
#' \insertRef{paul1985three}{fitODBOD}
#' \insertRef{morel2012overdispersion}{fitODBOD}
#'
#' @examples
#' No.D.D <- 0:7         #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)     #assigning the corresponding frequencies
#'
#' \dontrun{
#' #estimating the probability value and alpha value
#' results <- EstMLEAddBin(No.D.D,Obs.fre.1)
#'
#' #printing the summary of results
#' summary(results)
#'
#' #extracting the estimated parameters
#' coef(results)
#' }
#' @export
EstMLEAddBin<-function(x,freq)
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
  suppressWarnings2(.EstMLEAddBin(x=x,freq=freq),"NaN")
}

.EstMLEAddBin<-function(x,freq)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values if so
  #creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,freq))) | any(is.infinite(c(x,freq))) |
     any(is.nan(c(x,freq))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    #consider the probability values from 0.1 to 0.9 and alpha values from -0.9 to 0.9 and
    #estimate the best probability value in between 0.1 and 0.9  and alpha value inbetween
    # -0.9 to 0.9 for first decimal point
    answer1<-.looping_AddBin(x,freq,0.1,0.9,0.1,9,-0.9,0.9,0.1,19)
    #assign the found best estimated probability value to p1 and alpha value to alpha1
    p1<-answer1$p ; alpha1<-answer1$alpha
    #consider the second decimal point of p1 and alpha1, now estimate the best probability and alpha value
    answer2<-.looping_AddBin(x,freq,p1-0.05,p1+0.04,0.01,10,alpha1-0.05,alpha1+0.04,0.01,10)
    #assign the found best estimated probability value to p2 and alpha value to alpha2
    p2<-answer2$p ; alpha2<-answer2$alpha
    #consider the third decimal point of p2 and alpha2, now estimate the best probability and alpha value
    answer3<-.looping_AddBin(x,freq,p2-0.005,p2+0.004,0.001,10,alpha2-0.005,alpha2+0.004,0.001,10)
    #assign the found best estimated probability value to p3 and alpha value to alpha3
    p3<-answer3$p ; alpha3<-answer3$alpha
    #consider the fourth decimal point of p3 and alpha3, now estimate the best probability and alpha value
    answer4<-.looping_AddBin(x,freq,p3-0.0005,p3+0.0004,0.0001,10,alpha3-0.0005,alpha3+0.0004,0.0001,10)
    #assign the found best estimated probability value to p4 and alpha value to alpha4
    p4<-answer4$p ; alpha4<-answer4$alpha
    #consider the fifth decimal point of p4 and alpha4, now estimate the best probability and alpha value
    answer5<-.looping_AddBin(x,freq,p4-0.00005,p4+0.00004,0.00001,10,alpha4-0.00005,alpha4+0.00004,0.00001,10)
    #assign the found best estimated probability value to p5 and alpha value to alpha5
    p5<-answer5$p ; alpha5<-answer5$alpha
    #consider the sixth decimal point of p5 and alpha5, now estimate the best probability and alpha value
    answerfin<-.looping_AddBin(x,freq,p5-0.000005,p5+0.000004,0.000001,10,alpha5-0.000005,alpha5+0.000004,0.000001,10)
    #finally the found best estimated p5 and alpha5 value to pfin and alphafin and find the corresponding log likelihood
    #value as well
    pfin<-answerfin$p ; alphafin<-answerfin$alpha ; NegLLAddBinfin<-answerfin$NegLLAddBin

    output<-list("min"=NegLLAddBinfin,"p"=pfin,"alpha"=alphafin,"AIC"=2*2+(2*NegLLAddBinfin),
                 "call"=match.call())
    class(output)<-c("mlAB","ml")
    return(output)

  }
}

.findNegLL_AddBin<-function(x,freq,p,alpha)
{
  n<-max(x)
  data<-rep(x,freq)
  value<-sapply(1:sum(freq),function(i) log(((alpha/2)*((data[i]*(data[i]-1)/p)+((n-data[i])*(n-data[i]-1)/(1-p)))-
                                               (alpha*n*(n-1)/2) + 1)))

  return(-(sum(log(choose(n,data[1:sum(freq)]))) + log(p)*sum(data[1:sum(freq)]) +
             log(1-p)*sum(n-data[1:sum(freq)]) + sum(value)))
}

.looping_AddBin<-function(x,freq,startp,endp,repp,itp,startc,endc,repc,itc)
{
  #for a given starting value and end value with the sequence function of R, a seq of probability values
  #and alpha values are created
  p<-seq(startp,endp,by=repp)
  alpha<-seq(startc,endc,by=repc)
  #create a matrix with itc columns and itp rows
  value1<-matrix(ncol=itc,nrow=itp)
  #name the row names as the below probability values
  rownames(value1)<-p
  #name the column names as the below alpha values
  colnames(value1)<-alpha
  #now for each row and column using the probability and alpha values calculate the negative log likelihood values
  #and save them in the matrix
  for (j in 1:itc)
  {
    for (i in 1:itp)
    {
      value1[i,j]<-.findNegLL_AddBin(x,freq,p[i],alpha[j])
    }
  }
  #find the minimum value of the matrix
  minimum<-min(value1,na.rm=TRUE)
  #which is the minimum negative loglikelihood value
  AddBinNegLL<-minimum
  #finding which row and column values gives the minimum negative loglikelihood value
  #and save it as inds
  inds<-which(value1==min(value1,na.rm=TRUE),arr.ind = TRUE)
  #acquire the name of the row which will give the probability value, assign it to rnames
  rnames<-as.numeric(rownames(value1)[inds[,1]])
  #acquire the name of the column which will give the alpha value, assign it to cnames
  cnames<-as.numeric(colnames(value1)[inds[,2]])
  #generate the output as a list format where NegLLAddBin is the minimum negative loglikelihood
  #value and probability and alpha are the corresponding estimated probability and alpha
  #parameter values.
  output<-list("NegLLAddBin"=AddBinNegLL,"p"=rnames,"alpha"=cnames)
  return(output)
}

#' @method EstMLEAddBin default
#' @export
EstMLEAddBin.default<-function(x,freq)
{
  #class(est)<-"mlAB"
  return(EstMLEAddBin(x,freq))
}

#' @method print mlAB
#' @export
print.mlAB<-function(x,...)
{
  cat("Call: \n")
  print(x$call)
  cat("\nCoefficients: \n")
  coeff<-c(x$p,x$alpha)
  names(coeff)<-c("p","alpha")
  print(coeff)
}

#' @method summary mlAB
#' @export
summary.mlAB<-function(object,...)
{
  cat("Coefficients: \n \t p \t alpha \n", object$p,object$alpha)
  cat("\n\nNegative Log-likelihood : ",object$min)
  cat("\n\nAIC : ",object$AIC,"\n")
}

#' @method coef mlAB
#' @export
coef.mlAB<-function(object,...)
{
  cat(" \t p  \t alpha  \n", object$p, object$alpha)
}

#' Fitting the Additive Binomial Distribution when binomial
#' random variable, frequency, probability of success and alpha are given
#'
#' The function will fit the Additive Binomial distribution when random variables,
#' corresponding frequencies, probability of success and alpha are given.
#' It will provide the expected frequencies, chi-squared test statistics value, p value,
#' and degree of freedom value so that it can be seen if this distribution fits the data.
#'
#' @usage fitAddBin(x,obs.freq,p,alpha)
#'
#' @param x                  vector of binomial random variables.
#' @param obs.freq           vector of frequencies.
#' @param p                  single value for probability of success.
#' @param alpha              single value for alpha.
#'
#' @details
#' \deqn{obs.freq \ge 0}
#' \deqn{x = 0,1,2,..}
#' \deqn{0 < p < 1}
#' \deqn{-1 < alpha < 1}
#'
#' @return
#' The output of \code{fitAddBin} gives the class format \code{fitAB} and \code{fit} consisting a list
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
#' \code{fitAB} fitted probability values of \code{dAddBin}.
#'
#' \code{NegLL} Negative Log Likelihood value.
#'
#' \code{p} estimated probability value.
#'
#' \code{alpha} estimated alpha parameter value.
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
#' No.D.D <- 0:7         #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)            #assigning the corresponding the frequencies
#'
#' \dontrun{
#' #assigning the estimated probability value
#' paddbin <- EstMLEAddBin(No.D.D,Obs.fre.1)$p
#'
#' #assigning the estimated alpha value
#' alphaaddbin <- EstMLEAddBin(No.D.D,Obs.fre.1)$alpha
#'
#' #fitting when the random variable,frequencies,probability and alpha are given
#' results <- fitAddBin(No.D.D,Obs.fre.1,paddbin,alphaaddbin)
#' results
#'
#' #extracting the AIC value
#' AIC(results)
#'
#' #extract fitted values
#' fitted(results)
#' }
#'
#' @export
fitAddBin<-function(x,obs.freq,p,alpha)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,obs.freq,p,alpha))) | any(is.infinite(c(x,obs.freq,p,alpha))) |
     any(is.nan(c(x,obs.freq,p,alpha))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    est<-dAddBin(x,max(x),p,alpha)
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
    if(df<=0)
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
    NegLL<-NegLLAddBin(x,obs.freq,p,alpha)
    names(NegLL)<-NULL
    #the final output is in a list format containing the calculated values
    final<-list("bin.ran.var"=x,"obs.freq"=obs.freq,"exp.freq"=exp.freq,
                "statistic"=round(statistic,4),"df"=df,"p.value"=round(p.value,4),
                "fitAB"=est,"NegLL"=NegLL,"p"=p,"alpha"=alpha,
                "AIC"=2*2+2*NegLL,"call"=match.call())
    class(final)<-c("fitAB","fit")
    return(final)
    }
  }

#' @method fitAddBin default
#' @export
fitAddBin.default<-function(x,obs.freq,p,alpha)
{
  return(fitAddBin(x,obs.freq,p,alpha))
}

#' @method print fitAB
#' @export
print.fitAB<-function(x,...)
{
  cat("Call: \n")
  print(x$call)
  cat("\nChi-squared test for Additive Binomial Distribution \n\t
      Observed Frequency : ",x$obs.freq,"\n\t
      expected Frequency : ",x$exp.freq,"\n\t
      estimated p value :",x$p," ,estimated alpha parameter :",x$alpha,"\n\t
      X-squared :",x$statistic,"  ,df :",x$df,"  ,p-value :",x$p.value,"\n")
}

#' @method summary fitAB
#' @export
summary.fitAB<-function(object,...)
{
  cat("Call: \n")
  print(object$call)
  cat("\nChi-squared test for Additive Binomial Distribution \n\t
      Observed Frequency : ",object$obs.freq,"\n\t
      expected Frequency : ",object$exp.freq,"\n\t
      estimated p value :",object$p," ,estimated alpha parameter :",object$alpha,"\n\t
      X-squared :",object$statistic,"  ,df :",object$df,"  ,p-value :",object$p.value,"\n\t
      Negative Loglikehood value :",object$NegLL,"\n\t
      AIC value :",object$AIC,"\n")
}

#' @importFrom stats pchisq
