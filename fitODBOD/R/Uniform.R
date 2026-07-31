#' Uniform Distribution Bounded Between [0,1]
#'
#' These functions provide the ability for generating probability density values,
#' cumulative probability density values and moments about zero values for the
#' Uniform Distribution bounded between [0,1].
#'
#' @usage
#' dUNI(p)
#'
#' @param p    vector of probabilities.
#'
#' @details
#' Setting \eqn{a=0} and \eqn{b=1} in the Uniform Distribution
#' a unit bounded Uniform Distribution can be obtained. The probability density function
#' and cumulative density function of a unit bounded Uniform Distribution with random
#' variable P are given by
#'
#' \deqn{g_{P}(p) = 1}    \eqn{0 \le p \le 1}
#' \deqn{G_{P}(p) = p}    \eqn{0 \le p \le 1}
#'
#' The mean and the variance are denoted as
#' \deqn{E[P]= \frac{1}{a+b}= 0.5}
#' \deqn{var[P]= \frac{(b-a)^2}{12}= 0.0833}
#'
#' Moments about zero is denoted as
#' \deqn{E[P^r]= \frac{e^{rb}-e^{ra}}{r(b-a)}= \frac{e^r-1}{r} }
#' \eqn{r = 1,2,3,...}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions necessary
#' error messages will be provided to go further.
#'
#' @return
#' The output of \code{dUNI} gives a list format consisting
#'
#' \code{pdf}              probability density values in vector form.
#'
#' \code{mean}             mean of unit bounded uniform distribution.
#'
#' \code{var}              variance of unit bounded uniform distribution.
#'
#' @references
#' \insertRef{horsnell1957economical}{fitODBOD}
#' \insertRef{johnson1995continuous}{fitODBOD}
#'
#' @seealso
#' \code{\link[stats]{Uniform}}
#'
#' or
#'
#' \url{https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Uniform.html}
#'
#' @examples
#' #plotting the random variables and probability values
#' plot(seq(0,1,by=0.01),dUNI(seq(0,1,by=0.01))$pdf,type = "l",main="Probability density graph",
#' xlab="Random variable",ylab="Probability density values")
#'
#' dUNI(seq(0,1,by=0.05))$pdf     #extract the pdf values
#' dUNI(seq(0,1,by=0.01))$mean    #extract the mean
#' dUNI(seq(0,1,by=0.01))$var     #extract the variance
#'
#' #plotting the random variables and cumulative probability values
#' plot(seq(0,1,by=0.01),pUNI(seq(0,1,by=0.01)),type = "l",main="Cumulative density graph",
#' xlab="Random variable",ylab="Cumulative density values")
#'
#' pUNI(seq(0,1,by=0.05))     #acquiring the cumulative probability values
#'
#' mazUNI(c(1,2,3))    #acquiring the moment about zero values
#'
#' #only the integer value of moments is taken here because moments cannot be decimal
#' mazUNI(1.9)
#'
#' @importFrom Rdpack reprompt
#' @export
dUNI<-function(p)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(p)) | any(is.infinite(p)) | any(is.nan(p)))
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    #for each input values in the vector necessary calculations and conditions are applied
    if(any(p<0) | any(p>1)){
      #checking if probability values are greater than one or less than zero and creating
      # an error message as well as stopping the function progress
      stop("Invalid values in the input")
    }
    ans<-sapply(1:length(p),function(i) 1)
  }
  # generating an output in list format consisting pdf,mean and variance
  return(list("pdf"=ans,"mean"=1/2,"var"=1/12))
}

#' Uniform Distribution Bounded Between [0,1]
#'
#' These functions provide the ability for generating probability density values,
#' cumulative probability density values and moments about zero values for the
#' Uniform Distribution bounded between [0,1].
#'
#' @usage
#' pUNI(p)
#'
#' @param p    vector of probabilities.
#'
#' @details
#' Setting \eqn{a=0} and \eqn{b=1} in the Uniform Distribution
#' a unit bounded Uniform Distribution can be obtained. The probability density function
#' and cumulative density function of a unit bounded Uniform Distribution with random
#' variable P are given by
#'
#' \deqn{g_{P}(p) = 1}    \eqn{0 \le p \le 1}
#' \deqn{G_{P}(p) = p}    \eqn{0 \le p \le 1}
#'
#' The mean and the variance are denoted as
#' \deqn{E[P]= \frac{1}{a+b}= 0.5}
#' \deqn{var[P]= \frac{(b-a)^2}{12}= 0.0833}
#'
#' Moments about zero is denoted as
#' \deqn{E[P^r]= \frac{e^{rb}-e^{ra}}{r(b-a)}= \frac{e^r-1}{r} }
#' \eqn{r = 1,2,3,...}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions necessary
#' error messages will be provided to go further.
#'
#' @return
#' The output of \code{pUNI} gives the cumulative density values in vector form.
#'
#' @references
#' \insertRef{horsnell1957economical}{fitODBOD}
#' \insertRef{johnson1995continuous}{fitODBOD}
#'
#' @seealso
#' \code{\link[stats]{Uniform}}
#'
#' or
#'
#' \url{https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Uniform.html}
#'
#' @examples
#' #plotting the random variables and probability values
#' plot(seq(0,1,by=0.01),dUNI(seq(0,1,by=0.01))$pdf,type = "l",main="Probability density graph",
#' xlab="Random variable",ylab="Probability density values")
#'
#' dUNI(seq(0,1,by=0.05))$pdf     #extract the pdf values
#' dUNI(seq(0,1,by=0.01))$mean    #extract the mean
#' dUNI(seq(0,1,by=0.01))$var     #extract the variance
#'
#' #plotting the random variables and cumulative probability values
#' plot(seq(0,1,by=0.01),pUNI(seq(0,1,by=0.01)),type = "l",main="Cumulative density graph",
#' xlab="Random variable",ylab="Cumulative density values")
#'
#' pUNI(seq(0,1,by=0.05))     #acquiring the cumulative probability values
#'
#' mazUNI(c(1,2,3))    #acquiring the moment about zero values
#'
#' #only the integer value of moments is taken here because moments cannot be decimal
#' mazUNI(1.9)
#'
#' @export
pUNI<-function(p)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(p)) | any(is.infinite(p)) | any(is.nan(p)))
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    #for each input values in the vector necessary calculations and conditions are applied
    if(any(p<0) | any(p>1)){
      #checking if probability values are greater than one or less than zero and creating
      # an error message as well as stopping the function progress
      stop("Invalid values in the input")
    }
    ans<-sapply(1:length(p),function(i) p[i])
    #generating an ouput vector of cumulative probability values
    return(ans)
  }
}

#' Uniform Distribution Bounded Between [0,1]
#'
#' These functions provide the ability for generating probability density values,
#' cumulative probability density values and moments about zero values for the
#' Uniform Distribution bounded between [0,1].
#'
#' @usage
#' mazUNI(r)
#'
#' @param r    vector of moments
#'
#' @details
#' Setting \eqn{a=0} and \eqn{b=1} in the Uniform Distribution
#' a unit bounded Uniform Distribution can be obtained. The probability density function
#' and cumulative density function of a unit bounded Uniform Distribution with random
#' variable P are given by
#'
#' \deqn{g_{P}(p) = 1}    \eqn{0 \le p \le 1}
#' \deqn{G_{P}(p) = p}    \eqn{0 \le p \le 1}
#'
#' The mean and the variance are denoted as
#' \deqn{E[P]= \frac{1}{a+b}= 0.5}
#' \deqn{var[P]= \frac{(b-a)^2}{12}= 0.0833}
#'
#' Moments about zero is denoted as
#' \deqn{E[P^r]= \frac{e^{rb}-e^{ra}}{r(b-a)}= \frac{e^r-1}{r} }
#' \eqn{r = 1,2,3,...}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions necessary
#' error messages will be provided to go further.
#'
#' @return
#' The output of \code{mazUNI} gives the moments about zero in vector form.
#'
#' @references
#' \insertRef{horsnell1957economical}{fitODBOD}
#' \insertRef{johnson1995continuous}{fitODBOD}
#'
#' @seealso
#' \code{\link[stats]{Uniform}}
#'
#' or
#'
#' \url{https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Uniform.html}
#'
#' @examples
#' #plotting the random variables and probability values
#' plot(seq(0,1,by=0.01),dUNI(seq(0,1,by=0.01))$pdf,type = "l",main="Probability density graph",
#' xlab="Random variable",ylab="Probability density values")
#'
#' dUNI(seq(0,1,by=0.05))$pdf     #extract the pdf values
#' dUNI(seq(0,1,by=0.01))$mean    #extract the mean
#' dUNI(seq(0,1,by=0.01))$var     #extract the variance
#'
#' #plotting the random variables and cumulative probability values
#' plot(seq(0,1,by=0.01),pUNI(seq(0,1,by=0.01)),type = "l",main="Cumulative density graph",
#' xlab="Random variable",ylab="Cumulative density values")
#'
#' pUNI(seq(0,1,by=0.05))     #acquiring the cumulative probability values
#'
#' mazUNI(c(1,2,3))    #acquiring the moment about zero values
#'
#' #only the integer value of moments is taken here because moments cannot be decimal
#' mazUNI(1.9)
#'
#' @export
mazUNI<-function(r)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(r)) | any(is.infinite(r)) | any(is.nan(r)))
  {
    stop("NA or Infinite or NAN values in the Input")
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
    ans<-sapply(1:length(r),function(i) 1/(1+r[i]))
    #generating an ouput vector of moment about zero values
    return(ans)
  }
}

#' Uniform Binomial Distribution
#'
#' These functions provide the ability for generating probability function values and
#' cumulative probability function values for the Uniform Binomial Distribution.
#'
#' @usage
#' dUniBin(x,n)
#'
#' @param x      vector of binomial random variables.
#' @param n      single value for no of binomial trials.
#'
#' @details
#' Mixing unit bounded uniform distribution with binomial distribution will create
#' the Uniform Binomial Distribution. The  probability function
#' and cumulative probability function can be constructed and are denoted below.
#'
#' The cumulative probability function is the summation of probability function values
#'
#' \deqn{P_{UniBin}(x)= \frac{1}{n+1} }
#' \deqn{n = 1,2,...}
#' \deqn{x = 0,1,2,...n}
#'
#' The mean, variance and over dispersion are denoted as
#' \deqn{E_{UniBin}[X]= \frac{n}{2} }
#' \deqn{Var_{UniBin}[X]= \frac{n(n+2)}{12} }
#' \deqn{over dispersion= \frac{1}{3} }
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions necessary
#' error messages will be provided to go further.
#'
#' @return
#' The output of \code{dUniBin} gives a list format consisting
#'
#' \code{pdf}            probability function values in vector form.
#'
#' \code{mean}           mean of the Uniform Binomial Distribution.
#'
#' \code{var}            variance of the Uniform Binomial Distribution.
#'
#' \code{ove.dis.para}   over dispersion value of Uniform Binomial Distribution.
#'
#' @references
#' \insertRef{horsnell1957economical}{fitODBOD}
#' \insertRef{okagbue2014using}{fitODBOD}
#'
#' @examples
#' #plotting the binomial random variables and probability values
#' plot(0:10,dUniBin(0:10,10)$pdf,type="l",main="Uniform binomial probability function graph",
#' xlab=" Binomial random variable",ylab="Probability function values")
#' points(0:10,dUniBin(0:10,10)$pdf)
#'
#' dUniBin(0:300,300)$pdf  #extracting the pdf values
#' dUniBin(0:10,10)$mean   #extracting the mean
#' dUniBin(0:10,10)$var    #extracting the variance
#' dUniBin(0:10,10)$over.dis.para  #extracting the over dispersion
#'
#' #plotting the binomial random variables and cumulative probability values
#' plot(0:10,pUniBin(0:10,10),type="l",main="Cumulative probability function graph",
#' xlab=" Binomial random variable",ylab="Cumulative probability function values")
#' points(0:10,pUniBin(0:10,10))
#'
#' pUniBin(0:15,15)       #acquiring the cumulative probability values
#'
#' @export
dUniBin<-function(x,n)
{
  #for given input parameters by setting shape parameters alpha=1 and beta=1 in beta binomial
  #distribution the output is derived
  return(dBetaBin(x,n,a=1,b=1))
}

#' Uniform Binomial Distribution
#'
#' These functions provide the ability for generating probability function values and
#' cumulative probability function values for the Uniform Binomial Distribution.
#'
#' @usage
#' pUniBin(x,n)
#'
#' @param x      vector of binomial random variables.
#' @param n      single value for no of binomial trials.
#'
#' @details
#' Mixing unit bounded uniform distribution with binomial distribution will create
#' the Uniform Binomial Distribution. The  probability function
#' and cumulative probability function can be constructed and are denoted below.
#'
#' The cumulative probability function is the summation of probability function values.
#'
#' \deqn{P_{UniBin}(x)= \frac{1}{n+1} }
#' \deqn{n = 1,2,...}
#' \deqn{x = 0,1,2,...n}
#'
#' The mean, variance and over dispersion are denoted as
#' \deqn{E_{UniBin}[X]= \frac{n}{2} }
#' \deqn{Var_{UniBin}[X]= \frac{n(n+2)}{12} }
#' \deqn{over dispersion= \frac{1}{3} }
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions necessary
#' error messages will be provided to go further.
#'
#' @return
#' The output of \code{pUniBin} gives cumulative probability function values in vector form.
#'
#' @references
#' \insertRef{horsnell1957economical}{fitODBOD}
#' \insertRef{okagbue2014using}{fitODBOD}
#'
#' @examples
#' #plotting the binomial random variables and probability values
#' plot(0:10,dUniBin(0:10,10)$pdf,type="l",main="Uniform binomial probability function graph",
#' xlab=" Binomial random variable",ylab="Probability function values")
#' points(0:10,dUniBin(0:10,10)$pdf)
#'
#' dUniBin(0:300,300)$pdf  #extracting the pdf values
#' dUniBin(0:10,10)$mean   #extracting the mean
#' dUniBin(0:10,10)$var    #extracting the variance
#' dUniBin(0:10,10)$over.dis.para  #extracting the over dispersion
#'
#' #plotting the binomial random variables and cumulative probability values
#' plot(0:10,pUniBin(0:10,10),type="l",main="Cumulative probability function graph",
#' xlab=" Binomial random variable",ylab="Cumulative probability function values")
#' points(0:10,pUniBin(0:10,10))
#'
#' pUniBin(0:15,15)       #acquiring the cumulative probability values
#'
#' @export
pUniBin<-function(x,n)
{
  #for each binomial random variable in the input vector the cumulative proability function
  #values are calculated
  ans<-sapply(1:length(x),function(i) sum(dUniBin(0:x[i],n)$pdf))
  #generating an ouput vector cumulative probability function values
  return(ans)
}

#' @import stats
