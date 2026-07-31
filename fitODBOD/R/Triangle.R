#' Triangular Distribution Bounded Between [0,1]
#'
#' These functions provide the ability for generating probability density values,
#' cumulative probability density values and moments about zero values for the
#' Triangular Distribution bounded between [0,1].
#'
#' @usage
#' dTRI(p,mode)
#'
#' @param p                vector of probabilities.
#' @param mode             single value for mode.
#'
#' @details
#' Setting \eqn{min=0} and \eqn{max=1} \eqn{mode=c} in the Triangular distribution
#' a unit bounded Triangular distribution can be obtained. The probability density function
#' and cumulative density function of a unit bounded Triangular distribution with random
#' variable P are given by
#'
#' \deqn{g_{P}(p)= \frac{2p}{c} } ;            \eqn{0 \le p < c}
#' \deqn{g_{P}(p)= \frac{2(1-p)}{(1-c)} } ;    \eqn{c \le p \le 1}
#' \deqn{G_{P}(p)= \frac{p^2}{c} } ;           \eqn{0 \le p < c}
#' \deqn{G_{P}(p)= 1-\frac{(1-p)^2}{(1-c)} } ; \eqn{c \le p \le 1}
#' \deqn{0 \le mode=c \le 1}
#'
#' The mean and the variance are denoted by
#' \deqn{E[P]= \frac{(a+b+c)}{3}= \frac{(1+c)}{3} }
#' \deqn{var[P]= \frac{a^2+b^2+c^2-ab-ac-bc}{18}= \frac{(1+c^2-c)}{18} }
#'
#' Moments about zero is denoted as
#' \deqn{E[P^r]= \frac{2c^{r+2}}{c(r+2)}+\frac{2(1-c^{r+1})}{(1-c)(r+1)}+\frac{2(c^{r+2}-1)}{(1-c)(r+2)} }
#' \eqn{r = 1,2,3,...}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#' The output of \code{dTRI} gives a list format consisting
#'
#' \code{pdf}             probability density values in vector form.
#'
#' \code{mean}            mean of the unit bounded Triangular distribution.
#'
#' \code{variance}        variance of the unit bounded Triangular distribution
#'
#' @references
#' \insertRef{horsnell1957economical}{fitODBOD}
#' \insertRef{johnson1995continuous}{fitODBOD}
#' \insertRef{karlis2008polygonal}{fitODBOD}
#' \insertRef{okagbue2014using}{fitODBOD}
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(4)
#' x <- seq(0.2,0.8,by=0.2)
#' plot(0,0,main="Probability density graph",xlab="Random variable",
#' ylab="Probability density values",xlim = c(0,1),ylim = c(0,3))
#' for (i in 1:4)
#' {
#' lines(seq(0,1,by=0.01),dTRI(seq(0,1,by=0.01),x[i])$pdf,col = col[i])
#' }
#'
#' dTRI(seq(0,1,by=0.05),0.3)$pdf     #extracting the pdf values
#' dTRI(seq(0,1,by=0.01),0.3)$mean    #extracting the mean
#' dTRI(seq(0,1,by=0.01),0.3)$var     #extracting the variance
#'
#' #plotting the random variables and cumulative probability values
#' col <- rainbow(4)
#' x <- seq(0.2,0.8,by=0.2)
#' plot(0,0,main="Cumulative density graph",xlab="Random variable",
#' ylab="Cumulative density values",xlim = c(0,1),ylim = c(0,1))
#' for (i in 1:4)
#' {
#' lines(seq(0,1,by=0.01),pTRI(seq(0,1,by=0.01),x[i]),col = col[i])
#' }
#'
#' pTRI(seq(0,1,by=0.05),0.3)      #acquiring the cumulative probability values
#' mazTRI(1.4,.3)                  #acquiring the moment about zero values
#' mazTRI(2,.3)-mazTRI(1,.3)^2     #variance for when is mode 0.3
#'
#' #only the integer value of moments is taken here because moments cannot be decimal
#' mazTRI(1.9,0.5)
#'
#' @importFrom Rdpack reprompt
#' @export
dTRI<-function(p,mode)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values if so
  #creating an error message as well as stopping the function progress.
  if(any(is.na(c(p,mode))) | any(is.infinite(c(p,mode))) | any(is.nan(c(p,mode))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    #checking if mode parameter is between zero and one , if not providing an error message and
    #stopping the function progress
    if(mode < 0 | mode > 1)
    {
      stop("Mode cannot be less than zero or greater than one")
    }
    else
    {
      ans<-NULL
      #for each input values in the vector necessary calculations and conditions are applied
      if(any(p<0) | any(p>1)){
        stop("Invalid values in the input")
      }
      for(i in 1:length(p))
      {
        if(0<=p[i] && p[i]<mode)
        {
          ans[i]<-(2*p[i]) /mode
        }
        else if(mode<=p[i] && p[i]<=1)
        {
          ans[i]<-(2*(1-p[i]))/(1-mode)
        }
      }
    }
  }
  # generating an output in list format consisting pdf,mean and variance
  return(list("pdf"=ans,"mean"=(1+mode)/3,"var"=(1+mode^2-mode)/18))
}

#' Triangular Distribution Bounded Between [0,1]
#'
#' These functions provide the ability for generating probability density values,
#' cumulative probability density values and moments about zero values for the
#' Triangular Distribution bounded between [0,1].
#'
#' @usage
#' pTRI(p,mode)
#'
#' @param p                vector of probabilities.
#' @param mode             single value for mode.
#'
#' @details
#' Setting \eqn{min=0} and \eqn{max=1} \eqn{mode=c} in the Triangular distribution
#' a unit bounded Triangular distribution can be obtained. The probability density function
#' and cumulative density function of a unit bounded Triangular distribution with random
#' variable P are given by
#'
#' \deqn{g_{P}(p)= \frac{2p}{c} } ;            \eqn{0 \le p < c}
#' \deqn{g_{P}(p)= \frac{2(1-p)}{(1-c)} } ;    \eqn{c \le p \le 1}
#' \deqn{G_{P}(p)= \frac{p^2}{c} } ;           \eqn{0 \le p < c}
#' \deqn{G_{P}(p)= 1-\frac{(1-p)^2}{(1-c)} } ; \eqn{c \le p \le 1}
#' \deqn{0 \le mode=c \le 1}
#'
#' The mean and the variance are denoted by
#' \deqn{E[P]= \frac{(a+b+c)}{3}= \frac{(1+c)}{3} }
#' \deqn{var[P]= \frac{a^2+b^2+c^2-ab-ac-bc}{18}= \frac{(1+c^2-c)}{18} }
#'
#' Moments about zero is denoted as
#' \deqn{E[P^r]= \frac{2c^{r+2}}{c(r+2)}+\frac{2(1-c^{r+1})}{(1-c)(r+1)}+\frac{2(c^{r+2}-1)}{(1-c)(r+2)} }
#' \eqn{r = 1,2,3,...}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#'
#' The output of \code{pTRI} gives the cumulative density values in vector form.
#'
#' @references
#' \insertRef{horsnell1957economical}{fitODBOD}
#' \insertRef{johnson1995continuous}{fitODBOD}
#' \insertRef{karlis2008polygonal}{fitODBOD}
#' \insertRef{okagbue2014using}{fitODBOD}
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(4)
#' x <- seq(0.2,0.8,by=0.2)
#' plot(0,0,main="Probability density graph",xlab="Random variable",
#' ylab="Probability density values",xlim = c(0,1),ylim = c(0,3))
#' for (i in 1:4)
#' {
#' lines(seq(0,1,by=0.01),dTRI(seq(0,1,by=0.01),x[i])$pdf,col = col[i])
#' }
#'
#' dTRI(seq(0,1,by=0.05),0.3)$pdf     #extracting the pdf values
#' dTRI(seq(0,1,by=0.01),0.3)$mean    #extracting the mean
#' dTRI(seq(0,1,by=0.01),0.3)$var     #extracting the variance
#'
#' #plotting the random variables and cumulative probability values
#' col <- rainbow(4)
#' x <- seq(0.2,0.8,by=0.2)
#' plot(0,0,main="Cumulative density graph",xlab="Random variable",
#' ylab="Cumulative density values",xlim = c(0,1),ylim = c(0,1))
#' for (i in 1:4)
#' {
#' lines(seq(0,1,by=0.01),pTRI(seq(0,1,by=0.01),x[i]),col = col[i])
#' }
#'
#' pTRI(seq(0,1,by=0.05),0.3)      #acquiring the cumulative probability values
#' mazTRI(1.4,.3)                  #acquiring the moment about zero values
#' mazTRI(2,.3)-mazTRI(1,.3)^2     #variance for when is mode 0.3
#'
#' #only the integer value of moments is taken here because moments cannot be decimal
#' mazTRI(1.9,0.5)
#'
#' @export
pTRI<-function(p,mode)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values if so
  #creating an error message as well as stopping the function progress.
  if(any(is.na(c(p,mode))) | any(is.infinite(c(p,mode))) | any(is.nan(c(p,mode))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    #checking if mode parameter is between zero and one , if not providing an error message and
    #stopping the function progress
    if(mode < 0 | mode > 1)
    {
      stop("Mode cannot be less than zero or greater than one")
    }
    else
    {
      ans<-NULL
      #for each input values in the vector necessary calculations and conditions are applied
      if(any(p<0) | any(p>1)){
        stop("Invalid values in the input")
      }

      for(i in 1:length(p))
      {
        if(0<=p[i] && p[i]<mode)
        {
          ans[i]<-(p[i])^2/mode
        }
        else if(mode<= p[i] && p[i]<=1)
        {
          ans[i]<-1-((1-p[i])^2/(1-mode))
        }
      }
    }
    #generating an ouput vector of cumulative probability values
    return(ans)
  }
}

#' Triangular Distribution Bounded Between [0,1]
#'
#' These functions provide the ability for generating probability density values,
#' cumulative probability density values and moments about zero values for the
#' Triangular Distribution bounded between [0,1].
#'
#' @usage
#' mazTRI(r,mode)
#'
#' @param mode             single value for mode.
#' @param r                vector of moments.
#'
#' @details
#' Setting \eqn{min=0} and \eqn{max=1} \eqn{mode=c} in the Triangular distribution
#' a unit bounded Triangular distribution can be obtained. The probability density function
#' and cumulative density function of a unit bounded Triangular distribution with random
#' variable P are given by
#'
#' \deqn{g_{P}(p)= \frac{2p}{c} } ;            \eqn{0 \le p < c}
#' \deqn{g_{P}(p)= \frac{2(1-p)}{(1-c)} } ;    \eqn{c \le p \le 1}
#' \deqn{G_{P}(p)= \frac{p^2}{c} } ;           \eqn{0 \le p < c}
#' \deqn{G_{P}(p)= 1-\frac{(1-p)^2}{(1-c)} } ; \eqn{c \le p \le 1}
#' \deqn{0 \le mode=c \le 1}
#'
#' The mean and the variance are denoted by
#' \deqn{E[P]= \frac{(a+b+c)}{3}= \frac{(1+c)}{3} }
#' \deqn{var[P]= \frac{a^2+b^2+c^2-ab-ac-bc}{18}= \frac{(1+c^2-c)}{18} }
#'
#' Moments about zero is denoted as
#' \deqn{E[P^r]= \frac{2c^{r+2}}{c(r+2)}+\frac{2(1-c^{r+1})}{(1-c)(r+1)}+\frac{2(c^{r+2}-1)}{(1-c)(r+2)} }
#' \eqn{r = 1,2,3,...}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#'
#' The output of \code{mazTRI} give the moments about zero in vector form.
#'
#' @references
#' \insertRef{horsnell1957economical}{fitODBOD}
#' \insertRef{johnson1995continuous}{fitODBOD}
#' \insertRef{karlis2008polygonal}{fitODBOD}
#' \insertRef{okagbue2014using}{fitODBOD}
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(4)
#' x <- seq(0.2,0.8,by=0.2)
#' plot(0,0,main="Probability density graph",xlab="Random variable",
#' ylab="Probability density values",xlim = c(0,1),ylim = c(0,3))
#' for (i in 1:4)
#' {
#' lines(seq(0,1,by=0.01),dTRI(seq(0,1,by=0.01),x[i])$pdf,col = col[i])
#' }
#'
#' dTRI(seq(0,1,by=0.05),0.3)$pdf     #extracting the pdf values
#' dTRI(seq(0,1,by=0.01),0.3)$mean    #extracting the mean
#' dTRI(seq(0,1,by=0.01),0.3)$var     #extracting the variance
#'
#' #plotting the random variables and cumulative probability values
#' col <- rainbow(4)
#' x <- seq(0.2,0.8,by=0.2)
#' plot(0,0,main="Cumulative density graph",xlab="Random variable",
#' ylab="Cumulative density values",xlim = c(0,1),ylim = c(0,1))
#' for (i in 1:4)
#' {
#' lines(seq(0,1,by=0.01),pTRI(seq(0,1,by=0.01),x[i]),col = col[i])
#' }
#'
#' pTRI(seq(0,1,by=0.05),0.3)      #acquiring the cumulative probability values
#' mazTRI(1.4,.3)                  #acquiring the moment about zero values
#' mazTRI(2,.3)-mazTRI(1,.3)^2     #variance for when is mode 0.3
#'
#' #only the integer value of moments is taken here because moments cannot be decimal
#' mazTRI(1.9,0.5)
#'
#' @export
mazTRI<-function(r,mode)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values if so
  #creating an error message as well as stopping the function progress.
  if(any(is.na(c(r,mode))) | any(is.infinite(c(r,mode))) | any(is.nan(c(r,mode))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    #checking if mode parameter is between zero and one , if not providing an error message and
    #stopping the function progress
    if(mode < 0 | mode > 1)
    {
      stop("Mode cannot be less than zero or greater than one")
    }
    else
    {
      #the moments cannot be a decimal value therefore converting it into an integer
      r<-as.integer(r)
      #for each input values in the vector necessary calculations and conditions are applied
      if(any(r<=0)){
        #checking if moment values are less than or equal to zero if so
        #creating an error message as well as stopping the function progress
        stop("Moments cannot be less than or equal to zero")
      }
      ans<-sapply(1:length(r),function(i) ((2*(mode^(r[i]+2)))/(mode*(r[i]+2)))+((2*(1-mode^(r[i]+1)))/((r[i]+1)*(1-mode)))+
                    ((2*(mode^(r[i]+2)-1))/((r[i]+2)*(1-mode))))
      #generating an ouput vector of moment about zero values
      return(ans)
    }
  }
}

#' Triangular Binomial Distribution
#'
#' These functions provide the ability for generating probability function values and
#' cumulative probability function values for the Triangular Binomial distribution.
#'
#' @usage
#' dTriBin(x,n,mode)
#'
#' @param x        vector of binomial random variables.
#' @param n        single value for no of binomial trials.
#' @param mode     single value for mode.
#'
#' @details
#' Mixing unit bounded Triangular distribution with Binomial distribution will create
#' Triangular Binomial distribution. The probability function and cumulative probability function
#' can be constructed and are denoted below.
#'
#' The cumulative probability function is the summation of probability function values.
#'
#' \deqn{P_{TriBin}(x)= 2 {n \choose x}(c^{-1}B_c(x+2,n-x+1)+(1-c)^{-1}B(x+1,n-x+2)-(1-c)^{-1}B_c(x+1,n-x+2))}
#' \deqn{0 < mode=c < 1}
#' \deqn{x = 0,1,2,...n}
#' \deqn{n = 1,2,3...}
#'
#' The mean, variance and over dispersion are denoted as
#' \deqn{E_{TriiBin}[x]= \frac{n(1+c)}{3} }
#' \deqn{Var_{TriBin}[x]= \frac{n(n+3)}{18}-\frac{n(n-3)c(1-c)}{18} }
#' \deqn{over dispersion= \frac{(1-c+c^2)}{2(2+c-c^2)} }
#'
#' Defined as \eqn{B_c(a,b)=\int^c_0 t^{a-1} (1-t)^{b-1} \,dt} is incomplete beta integrals
#' and \eqn{B(a,b)} is the beta function.
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#' The output of \code{dTriBin} gives a list format consisting
#'
#' \code{pdf}             probability function values in vector form.
#'
#' \code{mean}            mean of the Triangular Binomial Distribution.
#'
#' \code{var}             variance of the Triangular Binomial Distribution.
#'
#' \code{over.dis.para}   over dispersion value of the Triangular Binomial Distribution.
#'
#' @references
#' \insertRef{horsnell1957economical}{fitODBOD}
#' \insertRef{karlis2008polygonal}{fitODBOD}
#' \insertRef{okagbue2014using}{fitODBOD}
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(7)
#' x <- seq(0.1,0.7,by=0.1)
#' plot(0,0,main="Triangular binomial probability function graph",xlab="Binomial random variable",
#' ylab="Probability function values",xlim = c(0,10),ylim = c(0,.3))
#' for (i in 1:7)
#' {
#' lines(0:10,dTriBin(0:10,10,x[i])$pdf,col = col[i],lwd=2.85)
#' points(0:10,dTriBin(0:10,10,x[i])$pdf,col = col[i],pch=16)
#' }
#'
#' dTriBin(0:10,10,.4)$pdf        #extracting the pdf values
#' dTriBin(0:10,10,.4)$mean       #extracting the mean
#' dTriBin(0:10,10,.4)$var        #extracting the variance
#' dTriBin(0:10,10,.4)$over.dis.para  #extracting the over dispersion value
#'
#' #plotting the random variables and cumulative probability values
#' col <- rainbow(7)
#' x <- seq(0.1,0.7,by=0.1)
#' plot(0,0,main="Triangular binomial probability function graph",xlab="Binomial random variable",
#' ylab="Probability function values",xlim = c(0,10),ylim = c(0,1))
#' for (i in 1:7)
#' {
#' lines(0:10,pTriBin(0:10,10,x[i]),col = col[i],lwd=2.85)
#' points(0:10,pTriBin(0:10,10,x[i]),col = col[i],pch=16)
#' }
#'
#' pTriBin(0:10,10,.4)    #acquiring the cumulative probability values
#'
#' @export
dTriBin<-function(x,n,mode)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values if so
  #creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,n,mode))) | any(is.nan(c(x,n,mode))) |any(is.infinite(c(x,n,mode))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    #checking if mode parameter is less than or equal to zero and more than or equal to one,
    #if so providing an error message and stopping the function progress
    if(mode <= 0 | mode >= 1)
    {
      stop("Mode cannot be less than or equal to zero or greater than or equal to one")
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
        a<-NULL
        b<-NULL

        integrate1<-NULL
        integrate2<-NULL
        ans<-NULL
        #for each random variable in the input vector below calculations occur
        for(i in 1:length(x))
        {
          #setting necessary values for alpha and beta and integrating the first term with the help
          #of R function
          a<-x[i]+2
          b<-n-x[i]+1
          integrate1<-stats::integrate(function(q){ (q^(a-1))*((1-q)^(b-1)) },lower=0,upper=mode)
          #setting necessary values for alpha and beta and integrating the second term with the help
          #of R function
          a<-x[i]+1
          b<-n-x[i]+2
          integrate2<-stats::integrate(function(q){ (q^(a-1))*((1-q)^(b-1)) },lower=0,upper=mode)

          ans[i]<-2*choose(n,x[i])*(((1/mode)*(integrate1$value))+((1/(1-mode))*beta(x[i]+1,n-x[i]+2))-
                  ((1/(1-mode))*(integrate2$value)))
        }
      }
    }
    # generating an output in list format consisting pdf,mean,variance and overdispersion value
    return(list("pdf"=ans,"mean"=n*(1+mode)/3,
                "var"=n*(n+3)/18 - n*(n-3)*mode*(1-mode)/18,
                "over.dis.para"=0.5*(1-mode+mode^2)/(2+mode-mode^2)))
  }
}

#' Triangular Binomial Distribution
#'
#' These functions provide the ability for generating probability function values and
#' cumulative probability function values for the Triangular Binomial distribution.
#'
#' @usage
#' pTriBin(x,n,mode)
#'
#' @param x        vector of binomial random variables
#' @param n        single value for no of binomial trials
#' @param mode     single value for mode
#'
#' @details
#' Mixing unit bounded Triangular distribution with Binomial distribution will create
#' Triangular Binomial distribution. The probability function and cumulative probability function
#' can be constructed and are denoted below.
#'
#' The cumulative probability function is the summation of probability function values.
#'
#' \deqn{P_{TriBin}(x)= 2 {n \choose x}(c^{-1}B_c(x+2,n-x+1)+(1-c)^{-1}B(x+1,n-x+2)-(1-c)^{-1}B_c(x+1,n-x+2))}
#' \deqn{0 < mode=c < 1}
#' \deqn{x = 0,1,2,...n}
#' \deqn{n = 1,2,3...}
#'
#' The mean, variance and over dispersion are denoted as
#' \deqn{E_{TriiBin}[x]= \frac{n(1+c)}{3} }
#' \deqn{Var_{TriBin}[x]= \frac{n(n+3)}{18}-\frac{n(n-3)c(1-c)}{18} }
#' \deqn{over dispersion= \frac{(1-c+c^2)}{2(2+c-c^2)} }
#'
#' Defined as \eqn{B_c(a,b)=\int^c_0 t^{a-1} (1-t)^{b-1} \,dt} is incomplete beta integrals
#' and \eqn{B(a,b)} is the beta function.
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#'
#' The output of \code{pTriBin} gives cumulative probability function values in vector form.
#'
#' @references
#' \insertRef{horsnell1957economical}{fitODBOD}
#' \insertRef{karlis2008polygonal}{fitODBOD}
#' \insertRef{okagbue2014using}{fitODBOD}
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(7)
#' x <- seq(0.1,0.7,by=0.1)
#' plot(0,0,main="Triangular binomial probability function graph",xlab="Binomial random variable",
#' ylab="Probability function values",xlim = c(0,10),ylim = c(0,.3))
#' for (i in 1:7)
#' {
#' lines(0:10,dTriBin(0:10,10,x[i])$pdf,col = col[i],lwd=2.85)
#' points(0:10,dTriBin(0:10,10,x[i])$pdf,col = col[i],pch=16)
#' }
#'
#' dTriBin(0:10,10,.4)$pdf        #extracting the pdf values
#' dTriBin(0:10,10,.4)$mean       #extracting the mean
#' dTriBin(0:10,10,.4)$var        #extracting the variance
#' dTriBin(0:10,10,.4)$over.dis.para  #extracting the over dispersion value
#'
#' #plotting the random variables and cumulative probability values
#' col <- rainbow(7)
#' x <- seq(0.1,0.7,by=0.1)
#' plot(0,0,main="Triangular binomial probability function graph",xlab="Binomial random variable",
#' ylab="Probability function values",xlim = c(0,10),ylim = c(0,1))
#' for (i in 1:7)
#' {
#' lines(0:10,pTriBin(0:10,10,x[i]),col = col[i],lwd=2.85)
#' points(0:10,pTriBin(0:10,10,x[i]),col = col[i],pch=16)
#' }
#'
#' pTriBin(0:10,10,.4)    #acquiring the cumulative probability values
#'
#' @export
pTriBin<-function(x,n,mode)
{
  #for each binomial random variable in the input vector the cumulative proability function
  #values are calculated
  ans<-sapply(1:length(x),function(i) sum(dTriBin(0:x[i],n,mode)$pdf))
  #generating an ouput vector of cumulative probability function values
  return(ans)
}

#' Negative Log Likelihood value of Triangular Binomial Distribution
#'
#' This function will calculate the Negative Log Likelihood value when the vector of binomial random
#' variables and vector of corresponding frequencies are given with the mode value.
#'
#' @usage
#' NegLLTriBin(x,freq,mode)
#'
#' @param x                  vector of binomial random variables.
#' @param freq               vector of frequencies.
#' @param mode               single value for mode.
#'
#' @details
#' \deqn{0 < mode=c < 1}
#' \deqn{x = 0,1,2,,...}
#' \deqn{freq \ge 0}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions necessary error
#'  messages will be provided to go further.
#'
#' @return
#' The output of \code{NegLLTriBin} will produce a single numeric value.
#'
#' @references
#' \insertRef{horsnell1957economical}{fitODBOD}
#' \insertRef{karlis2008polygonal}{fitODBOD}
#' \insertRef{okagbue2014using}{fitODBOD}
#'
#' @examples
#' No.D.D <- 0:7    #assigning the Random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95) #assigning the corresponding frequencies
#'
#' NegLLTriBin(No.D.D,Obs.fre.1,.023)   #acquiring the Negative log likelihood value
#'
#' @export
NegLLTriBin<-function(x,freq,mode)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values if so
  #creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,freq,mode))) | any(is.infinite(c(x,freq,mode)))
     |any(is.nan(c(x,freq,mode))) )
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
    #checking if the mode parameter is less than or equal to zero or more than or equal to one
    #if so creating a error message as well as stopping the function progress
    else if(mode <= 0 | mode >= 1)
    {
      stop("Mode cannot be less than zero or greater than one")
    }
    else
    {
      #as the equation contains partial beta integration here an integrative function is written
      inte<-function(a,b)
      {
        return(function(q){ ((q^(a-1))*((1-q)^(b-1))) })
      }
      #constructing the data set using the random variables vector and frequency vector
      n<-max(x)
      data<-rep(x,freq)

      term2<-NULL

      temp1<-NULL
      temp2<-NULL
      #creating an instance for the output of the integration
      temp<-function(a,b)
      {
        stats::integrate(inte(a,b),lower=0,upper=mode)$value
      }
      #doing the calculations for all binomial random variables under assigned data vector
      for(i in 1:sum(freq))
      {
        temp1[i]<-temp(data[i]+2,n-data[i]+1)
        temp2[i]<-temp(data[i]+1,n-data[i]+2)

        term2[i]<-log((mode^(-1)*temp1[i])+((1-mode)^(-1)*beta(data[i]+1,n-data[i]+2))-((1-mode)^(-1)*temp2[i]))
      }
      #calculating the negative log likelihood value and representing as a single output value
      return(-(sum(log(choose(n,data[1:sum(freq)]))) + sum(term2)+ sum(freq)*log(2)))
    }
  }
}

#' Estimating the mode value for Triangular Binomial Distribution
#'
#' The function will estimate the mode value using the maximum log likelihood method for the
#' Triangular Binomial Distribution when the binomial random variables and corresponding frequencies
#' are given.
#'
#' @usage
#' EstMLETriBin(x,freq)
#'
#' @param x                  vector of binomial random variables.
#' @param freq               vector of frequencies.
#'
#' @details
#' \deqn{0 < mode=c < 1}
#' \deqn{x = 0,1,2,...}
#' \deqn{freq \ge 0}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions necessary error
#'  messages will be provided to go further.
#'
#' @return
#' The output of \code{EstMLETriBin} will produce the classes of \code{ml} and \code{mlTB}
#' format consisting
#'
#' \code{min}  Negative log likelihood value.
#'
#' \code{mode}  Estimated mode value.
#'
#' \code{AIC}   AIC value.
#'
#' \code{call} the inputs for the function.
#'
#' Methods \code{print}, \code{summary}, \code{coef} and \code{AIC} can be used to
#' extract specific outputs.
#'
#' @references
#' \insertRef{horsnell1957economical}{fitODBOD}
#' \insertRef{karlis2008polygonal}{fitODBOD}
#' \insertRef{okagbue2014using}{fitODBOD}
#'
#' @examples
#' No.D.D <- 0:7   #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)   #assigning the corresponding frequencies
#'
#' \dontrun{
#' #estimating the mode value and extracting the mode value
#' results <- EstMLETriBin(No.D.D,Obs.fre.1)
#'
#' # extract the mode value and summary
#' coef(results)
#' summary(results)
#'
#' AIC(results)  #show the AIC value
#' }
#'
#' @export
EstMLETriBin<-function(x,freq)
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
  suppressWarnings2(.EstMLETriBin(x=x,freq=freq),"NaN")
}


.EstMLETriBin<-function(x,freq)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values if so
  #creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,freq))) | any(is.infinite(c(x,freq))) | any(is.nan(c(x,freq))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    #below looping function is to find the best estimated mode parameter which minimizes the
    #negative log likelihood value by increasing the decimal point to precision of six
    #consider the mode values from 0.1 to 0.9 estimate the best mode value in between 0.1 and 0.9 for first decimal point
    answer1<-.looping_TriBin(x,freq,0.1,0.9,0.1,9)
    #assign the found best estimated mode value to mode1
    mode1<-answer1$mode
    #consider the second decimal point of mode1, now estimate the best mode value
    answer2<-.looping_TriBin(x,freq,mode1-0.05,mode1+0.04,0.01,10)
    #assign the found best estimated mode1 value to mode2
    mode2<-answer2$mode
    #consider the third decimal point of mode2, now estimate the best mode value
    answer3<-.looping_TriBin(x,freq,mode2-0.005,mode2+0.004,0.001,10)
    #assign the found best estimated mode 2 value to mode3
    mode3<-answer3$mode
    #consider the fourth decimal point of mode3, now estimate the best mode value
    answer4<-.looping_TriBin(x,freq,mode3-0.0005,mode3+0.0004,0.0001,10)
    #assign the found best estimated mode 3 value to mode 4
    mode4<-answer4$mode
    #consider the fifth decimal point of mode4, now estimate the best mode value
    answer5<-.looping_TriBin(x,freq,mode4-0.00005,mode4+0.00004,0.00001,10)
    #assign the found best estimated mode 4 value to mode 5
    mode5<-answer5$mode
    #consider the sixth decimal point of mode5, now estimate the best mode value
    answerfin<-.looping_TriBin(x,freq,mode5-0.000005,mode5+0.000004,0.000001,10)

    #generate the output as a list format where TriBinNegLL is the minimum negative loglikelihood
    #value and mode is the corresponding estimated mode parameter value.
    output<-list("min"=answerfin$TriBinNegLL,"mode"=answerfin$mode,
                 "AIC"=2*1+(2*answerfin$TriBinNegLL),"call"=match.call())
    class(output)<-c("ml","mlTRI")
    return(output)
  }
}

.looping_TriBin<-function(x,freq,startmode,endmode,repmode,itmode)
{
  #for a given starting value and end value with the sequence function of R, a seq of mode values
  #are created
  mode<-seq(startmode,endmode,by=repmode)
  #create a matrix with one column and itmode as rows
  value1<-matrix(ncol=1,nrow=itmode)
  #name the row names as the above mode values
  rownames(value1)<-mode
  #now for each row using the mode value calculate the negative log likelihood values
  #and save them in the vector
  for (i in 1:itmode)
  {
    value1[i,1]<-NegLLTriBin(x,freq,mode[i])
  }
  #find the minimum value of the matrix
  minimum<-min(value1,na.rm=TRUE)
  #which is the minimum negative loglikelihood value
  TriBinNegLL<-minimum
  #finding which row(mode value) gives the minimum negative loglikelihood value
  #and save it as inds
  inds<-which(value1==min(value1,na.rm=TRUE),arr.ind = TRUE)
  #acquire the name of the row which will give the mode value, assign it to rnames
  rnames<-as.numeric(rownames(value1)[inds[,1]])
  #generate the output as a list format where TriBinNegLL is the minimum negative loglikelihood
  #value and mode is the corresponding estimated mode parameter value.
  return(list("TriBinNegLL"=TriBinNegLL,"mode"=rnames))
}

#' @method EstMLETriBin default
#' @export
EstMLETriBin.default<-function(x,freq)
{
  return(EstMLETriBin(x,freq))
}

#' @method print mlTRI
#' @export
print.mlTRI<-function(x,...)
{
  cat("Call: \n")
  print(x$call)
  cat("\nCoefficient :\n")
  coeff<-x$mode
  names(coeff)<-"mode"
  print(coeff)
}

#' @method summary mlTRI
#' @export
summary.mlTRI<-function(object,...)
{
  cat("Coefficients: \n mode \n", object$mode)
  cat("\n\nNegative Log-likelihood : ",object$min)
  cat("\n\nAIC : ",object$AIC,"\n")
}

#' @method coef mlTRI
#' @export
coef.mlTRI<-function(object,...)
{
  cat(" mode \n", object$mode)
}

#' Fitting the Triangular Binomial Distribution when binomial random variable, frequency and mode
#' value are given
#'
#' The function will fit the Triangular Binomial distribution when random variables, corresponding
#' frequencies and mode parameter are given. It will provide the expected frequencies, chi-squared
#' test statistics value, p value, degree of freedom and over dispersion value so that it can be
#' seen if this distribution fits the data.
#'
#' @usage fitTriBin(x,obs.freq,mode)
#'
#' @param x                  vector of binomial random variables.
#' @param obs.freq           vector of frequencies.
#' @param mode               single value for mode.
#'
#' @details
#' \deqn{0 < mode=c < 1}
#' \deqn{x = 0,1,2,...}
#' \deqn{0 < mode < 1}
#' \deqn{obs.freq \ge 0}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#' The output of \code{fitTriBin} gives the class format \code{fitTB} and \code{fit} consisting a list
#'
#' \code{bin.ran.var} binomial random variables.
#'
#' \code{obs.freq} corresponding observed frequencies.
#'
#' \code{exp.freq} corresponding expected frequencies.
#'
#' \code{statistic} chi-squared test statistics value.
#'
#' \code{df} degree of freedom.
#'
#' \code{p.value} probability value by chi-squared test statistic.
#'
#' \code{fitTB} fitted probability values of \code{dTriBin}.
#'
#' \code{NegLL} Negative Log Likelihood value.
#'
#' \code{mode} estimated mode value.
#'
#' \code{AIC} AIC value.
#'
#' \code{over.dis.para} over dispersion value.
#'
#' \code{call} the inputs of the function.
#'
#' Methods \code{summary}, \code{print}, \code{AIC}, \code{residuals} and \code{fitted}
#' can be used to extract specific outputs.
#'
#' @references
#' \insertRef{horsnell1957economical}{fitODBOD}
#' \insertRef{karlis2008polygonal}{fitODBOD}
#' \insertRef{okagbue2014using}{fitODBOD}
#'
#' @examples
#' No.D.D <- 0:7      #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)  #assigning the corresponding frequencies
#'
#' modeTriBin <- EstMLETriBin(No.D.D,Obs.fre.1)$mode  #assigning the extracted the mode value
#'
#' #fitting when the random variable,frequencies,mode value are given.
#' results <- fitTriBin(No.D.D,Obs.fre.1,modeTriBin)
#' results
#'
#' #extract AIC value
#' AIC(results)
#'
#' #extract fitted values
#' fitted(results)
#'
#' @export
fitTriBin<-function(x,obs.freq,mode)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values if so
  #creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,obs.freq,mode))) | any(is.infinite(c(x,obs.freq,mode))) |
     any(is.nan(c(x,obs.freq,mode))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    est<-dTriBin(x,max(x),mode)
    odp<-est$over.dis.para; names(odp)<-NULL
    #for given random variables and mode parameter calculating the estimated probability values
    est.prob<-est$pdf
    #using the estimated probability values the expected frequencies are calculated
    exp.freq<-round((sum(obs.freq)*est.prob),2)
    #chi-squared test statistics is calculated with observed frequency and expected frequency
    statistic<-sum(((obs.freq-exp.freq)^2)/exp.freq)
    #degree of freedom is calculated
    df<-length(x)-2
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
    NegLL<-NegLLTriBin(x,obs.freq,mode)
    names(NegLL)<-NULL
    #the final output is in a list format containing the calculated values
    final<-list("bin.ran.var"=x,"obs.freq"=obs.freq,"exp.freq"=exp.freq,
                "statistic"=round(statistic,4),"df"=df,"p.value"=round(p.value,4),
                "fitTB"=est,"NegLL"=NegLL,"mode"=mode,"AIC"=2*1+2*NegLL,
                "over.dis.para"=odp,"call"=match.call())
    class(final)<-c("fitTB","fit")
    return(final)
    }
  }

#' @method fitTriBin default
#' @export
fitTriBin.default<-function(x,obs.freq,mode)
{
  return(fitTriBin(x,obs.freq,mode))
}

#' @method print fitTB
#' @export
print.fitTB<-function(x,...)
{
  cat("Call: \n")
  print(x$call)
  cat("\nChi-squared test for Triangular Binomial Distribution \n\t
      Observed Frequency : ",x$obs.freq,"\n\t
      expected Frequency : ",x$exp.freq,"\n\t
      estimated Mode value:",x$mode,"\n\t
      X-squared :",x$statistic,"  ,df :",x$df,"  ,p-value :",x$p.value,"\n\t
      over dispersion :",x$over.dis.para,"\n")
}

#' @method summary fitTB
#' @export
summary.fitTB<-function(object,...)
{
  cat("Call: \n")
  print(object$call)
  cat("\nChi-squared test for Triangular Binomial Distribution \n\t
      Observed Frequency : ",object$obs.freq,"\n\t
      expected Frequency : ",object$exp.freq,"\n\t
      estimated Mode value:",object$mode,"\n\t
      X-squared :",object$statistic,"  ,df :",object$df,"  ,p-value :",object$p.value,"\n\t
      over dispersion :",object$over.dis.para,"\n\t
      Negative Loglikehood value :",object$NegLL,"\n\t
      AIC value :",object$AIC,"\n")
}
