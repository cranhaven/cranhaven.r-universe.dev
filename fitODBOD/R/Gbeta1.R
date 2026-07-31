#' Generalized Beta Type-1 Distribution
#'
#' These functions provide the ability for generating probability density values,
#' cumulative probability density values and moment about zero values for the
#' Generalized Beta Type-1 Distribution bounded between [0,1].
#'
#' @usage
#' dGBeta1(p,a,b,c)
#'
#' @param p              vector of probabilities.
#' @param a              single value for shape parameter alpha representing as a.
#' @param b              single value for shape parameter beta representing as b.
#' @param c              single value for shape parameter gamma representing as c.
#'
#' @details
#' The probability density function and cumulative density function of a unit bounded
#' Generalized Beta Type-1 Distribution with random variable P are given by
#'
#' \deqn{g_{P}(p)= \frac{c}{B(a,b)} p^{ac-1} (1-p^c)^{b-1} };      \eqn{0 \le p \le 1}
#' \deqn{G_{P}(p)= \frac{p^{ac}}{aB(a,b)}  2F1(a,1-b;p^c;a+1) }    \eqn{0 \le p \le 1}
#' \deqn{a,b,c > 0}
#'
#' The mean and the variance are denoted by
#' \deqn{E[P]= \frac{B(a+b,\frac{1}{c})}{B(a,\frac{1}{c})} }
#' \deqn{var[P]= \frac{B(a+b,\frac{2}{c})}{B(a,\frac{2}{c})}-(\frac{B(a+b,\frac{1}{c})}{B(a,\frac{1}{c})})^2 }
#'
#' The moments about zero is denoted as
#' \deqn{E[P^r]= \frac{B(a+b,\frac{r}{c})}{B(a,\frac{r}{c})} }
#' \eqn{r = 1,2,3,....}
#'
#' Defined as \eqn{B(a,b)} is Beta function.
#' Defined as \eqn{2F1(a,b;c;d)} is Gaussian Hypergeometric function.
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#' The output of \code{dGBeta1} gives a list format consisting
#'
#' \code{pdf}                   probability density values in vector form.
#'
#' \code{mean}                  mean of the Generalized Beta Type-1 Distribution.
#'
#' \code{var}                   variance of the Generalized Beta Type-1 Distribution.
#'
#' @references
#' \insertRef{manoj2013mcdonald}{fitODBOD}
#' \insertRef{janiffer2014estimating}{fitODBOD}
#' \insertRef{roozegar2017mcdonald}{fitODBOD}
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(5)
#' a <- c(.1,.2,.3,1.5,2.15)
#' plot(0,0,main="Probability density graph",xlab="Random variable",ylab="Probability density values",
#' xlim = c(0,1),ylim = c(0,10))
#' for (i in 1:5)
#' {
#' lines(seq(0,1,by=0.001),dGBeta1(seq(0,1,by=0.001),a[i],1,2*a[i])$pdf,col = col[i])
#' }
#'
#' dGBeta1(seq(0,1,by=0.01),2,3,1)$pdf    #extracting the pdf values
#' dGBeta1(seq(0,1,by=0.01),2,3,1)$mean   #extracting the mean
#' dGBeta1(seq(0,1,by=0.01),2,3,1)$var    #extracting the variance
#'
#' pGBeta1(0.04,2,3,4)        #acquiring the cdf values for a=2,b=3,c=4
#' mazGBeta1(1.4,3,2,2)              #acquiring the moment about zero values
#' mazGBeta1(2,3,2,2)-mazGBeta1(1,3,2,2)^2        #acquiring the variance for a=3,b=2,c=2
#'
#' #only the integer value of moments is taken here because moments cannot be decimal
#' mazGBeta1(3.2,3,2,2)
#'
#' @importFrom Rdpack reprompt
#' @export
dGBeta1<-function(p,a,b,c)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(p,a,b,c))) | any(is.infinite(c(p,a,b,c))) | any(is.nan(c(p,a,b,c))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    #checking if shape parameters are greater than zero, if not providing an error message and
    #stopping the function progress
    if(a <= 0 | b <= 0 | c <= 0)
    {
      stop("Shape parameters cannot be less than or equal to zero")
    }
    else
    {
      #for each input values in the vector necessary calculations and conditions are applied
      if(any(p<0) | any(p>1)){
        stop("Invalid values in the input")
      }
      ans<-sapply(1:length(p),function(i) (c/beta(a,b))*(p[i]^(a*c-1))*((1-p[i]^c)^(b-1)))
    }
  }
  # generating an output in list format consisting pdf,mean and variance
  return(list("pdf"=ans,"mean"=beta(a+b,(1/c))/beta(a,(1/c)),
              "var"=(beta(a+b,(2/c))/beta(a,(2/c)))-(beta(a+b,(1/c))/beta(a,(1/c)))^2 ))
}

#' Generalized Beta Type-1 Distribution
#'
#' These functions provide the ability for generating probability density values,
#' cumulative probability density values and moment about zero values for the
#' Generalized Beta Type-1 Distribution bounded between [0,1].
#'
#' @usage
#' pGBeta1(p,a,b,c)
#'
#' @param p              vector of probabilities.
#' @param a              single value for shape parameter alpha representing as a.
#' @param b              single value for shape parameter beta representing as b.
#' @param c              single value for shape parameter gamma representing as c.
#'
#' @details
#' The probability density function and cumulative density function of a unit bounded
#' Generalized Beta Type-1 Distribution with random variable P are given by
#'
#' \deqn{g_{P}(p)= \frac{c}{B(a,b)} p^{ac-1} (1-p^c)^{b-1} };      \eqn{0 \le p \le 1}
#' \deqn{G_{P}(p)= \frac{p^{ac}}{aB(a,b)}  2F1(a,1-b;p^c;a+1) }    \eqn{0 \le p \le 1}
#' \deqn{a,b,c > 0}
#'
#' The mean and the variance are denoted by
#' \deqn{E[P]= \frac{B(a+b,\frac{1}{c})}{B(a,\frac{1}{c})} }
#' \deqn{var[P]= \frac{B(a+b,\frac{2}{c})}{B(a,\frac{2}{c})}-(\frac{B(a+b,\frac{1}{c})}{B(a,\frac{1}{c})})^2 }
#'
#' The moments about zero is denoted as
#' \deqn{E[P^r]= \frac{B(a+b,\frac{r}{c})}{B(a,\frac{r}{c})} }
#' \eqn{r = 1,2,3,....}
#'
#' Defined as \eqn{B(a,b)} is Beta function.
#' Defined as \eqn{2F1(a,b;c;d)} is Gaussian Hypergeometric function.
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#' The output \code{pGBeta1} gives the cumulative density values in vector form.
#'
#' @references
#' \insertRef{manoj2013mcdonald}{fitODBOD}
#' \insertRef{janiffer2014estimating}{fitODBOD}
#' \insertRef{roozegar2017mcdonald}{fitODBOD}
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(5)
#' a <- c(.1,.2,.3,1.5,2.15)
#' plot(0,0,main="Probability density graph",xlab="Random variable",ylab="Probability density values",
#' xlim = c(0,1),ylim = c(0,10))
#' for (i in 1:5)
#' {
#' lines(seq(0,1,by=0.001),dGBeta1(seq(0,1,by=0.001),a[i],1,2*a[i])$pdf,col = col[i])
#' }
#'
#' dGBeta1(seq(0,1,by=0.01),2,3,1)$pdf    #extracting the pdf values
#' dGBeta1(seq(0,1,by=0.01),2,3,1)$mean   #extracting the mean
#' dGBeta1(seq(0,1,by=0.01),2,3,1)$var    #extracting the variance
#'
#' pGBeta1(0.04,2,3,4)        #acquiring the cdf values for a=2,b=3,c=4
#' mazGBeta1(1.4,3,2,2)              #acquiring the moment about zero values
#' mazGBeta1(2,3,2,2)-mazGBeta1(1,3,2,2)^2        #acquiring the variance for a=3,b=2,c=2
#'
#' #only the integer value of moments is taken here because moments cannot be decimal
#' mazGBeta1(3.2,3,2,2)
#'
#' @export
pGBeta1<-function(p,a,b,c)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(p,a,b,c))) | any(is.infinite(c(p,a,b,c))) | any(is.nan(c(p,a,b,c))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    #checking if shape parameters are greater than zero, if not providing an error message and
    #stopping the function progress
    if(a <= 0 | b <= 0 | c <= 0)
    {
      stop("Shape parameters cannot be less than or equal to zero")
    }
    else
    {
      #for each input values in the vector necessary calculations and conditions are applied
      if(any(p<0) | any(p>1)){
        stop("Invalid values in the input")
      }
      term<-sapply(1:length(p),function(i) Re(hypergeo::hypergeo_powerseries(a,1-b,(p[i])^c,a+1)))

      #checking if the hypergeometric function value is NA(not assigned)values,
      #infinite values or NAN(not a number)values if so providing an error message and
      #stopping the function progress
      if( any(is.nan(term)) | any(term<=0) | any(is.infinite(term)) | any(is.na(term)) ){
        stop("Given input values generate error values for Gaussian Hypergeometric Function")
      }

      ans<-sapply(1:length(p),function(i) (((p[i])^(a*c))*term[i])/(a*beta(a,b)))

      #generating an ouput vector of cumulative probability values
      return(ans)
    }
  }
}

#' Generalized Beta Type-1 Distribution
#'
#' These functions provide the ability for generating probability density values,
#' cumulative probability density values and moment about zero values for the
#' Generalized Beta Type-1 Distribution bounded between [0,1].
#'
#' @usage
#' mazGBeta1(r,a,b,c)
#'
#' @param a              single value for shape parameter alpha representing as a.
#' @param b              single value for shape parameter beta representing as b.
#' @param c              single value for shape parameter gamma representing as c.
#' @param r              vector of moments
#'
#' @details
#' The probability density function and cumulative density function of a unit bounded
#' Generalized Beta Type-1 Distribution with random variable P are given by
#'
#' \deqn{g_{P}(p)= \frac{c}{B(a,b)} p^{ac-1} (1-p^c)^{b-1} };      \eqn{0 \le p \le 1}
#' \deqn{G_{P}(p)= \frac{p^{ac}}{aB(a,b)}  2F1(a,1-b;p^c;a+1) }    \eqn{0 \le p \le 1}
#' \deqn{a,b,c > 0}
#'
#' The mean and the variance are denoted by
#' \deqn{E[P]= \frac{B(a+b,\frac{1}{c})}{B(a,\frac{1}{c})} }
#' \deqn{var[P]= \frac{B(a+b,\frac{2}{c})}{B(a,\frac{2}{c})}-(\frac{B(a+b,\frac{1}{c})}{B(a,\frac{1}{c})})^2 }
#'
#' The moments about zero is denoted as
#' \deqn{E[P^r]= \frac{B(a+b,\frac{r}{c})}{B(a,\frac{r}{c})} }
#' \eqn{r = 1,2,3,....}
#'
#' Defined as \eqn{B(a,b)} is Beta function.
#' Defined as \eqn{2F1(a,b;c;d)} is Gaussian Hypergeometric function.
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions
#' necessary error messages will be provided to go further.
#'
#' @return
#' The output \code{mazGBeta1} gives the moments about zero in vector form.
#'
#' @references
#' \insertRef{manoj2013mcdonald}{fitODBOD}
#' \insertRef{janiffer2014estimating}{fitODBOD}
#' \insertRef{roozegar2017mcdonald}{fitODBOD}
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(5)
#' a <- c(.1,.2,.3,1.5,2.15)
#' plot(0,0,main="Probability density graph",xlab="Random variable",ylab="Probability density values",
#' xlim = c(0,1),ylim = c(0,10))
#' for (i in 1:5)
#' {
#' lines(seq(0,1,by=0.001),dGBeta1(seq(0,1,by=0.001),a[i],1,2*a[i])$pdf,col = col[i])
#' }
#'
#' dGBeta1(seq(0,1,by=0.01),2,3,1)$pdf    #extracting the pdf values
#' dGBeta1(seq(0,1,by=0.01),2,3,1)$mean   #extracting the mean
#' dGBeta1(seq(0,1,by=0.01),2,3,1)$var    #extracting the variance
#'
#' pGBeta1(0.04,2,3,4)        #acquiring the cdf values for a=2,b=3,c=4
#' mazGBeta1(1.4,3,2,2)              #acquiring the moment about zero values
#' mazGBeta1(2,3,2,2)-mazGBeta1(1,3,2,2)^2        #acquiring the variance for a=3,b=2,c=2
#'
#' #only the integer value of moments is taken here because moments cannot be decimal
#' mazGBeta1(3.2,3,2,2)
#'
#' @export
mazGBeta1<-function(r,a,b,c)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.

  if(any(is.na(c(r,a,b,c))) | any(is.infinite(c(r,a,b,c))) | any(is.nan(c(r,a,b,c))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    #checking if shape parameters are greater than zero, if not providing an error message and
    #stopping the function progress
    if(a <= 0 | b <= 0 |c <= 0)
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
      ans<-sapply(1:length(r),function(i) beta(a+b,r[i]/c)/beta(a,r[i]/c))

      #generating an ouput vector of moment about zero values
      return(ans)
    }
  }
}

#' McDonald Generalized Beta Binomial Distribution
#'
#' These functions provide the ability for generating probability function values and
#' cumulative probability function values for the McDonald Generalized Beta
#' Binomial Distribution.
#'
#' @usage
#' dMcGBB(x,n,a,b,c)
#'
#' @param x        vector of binomial random variables.
#' @param n        single value for no of binomial trials.
#' @param a        single value for shape parameter alpha representing as a.
#' @param b        single value for shape parameter beta representing as b.
#' @param c        single value for shape parameter gamma representing as c.
#'
#' @details
#' Mixing Generalized Beta Type-1 Distribution with  Binomial distribution
#' the probability function value and cumulative probability function can be constructed
#' and are denoted below.
#'
#' The cumulative probability function is the summation of probability function values.
#'
#' \deqn{P_{McGBB}(x)= {n \choose x} \frac{1}{B(a,b)} (\sum_{j=0}^{n-x} (-1)^j {n-x \choose j} B(\frac{x}{c}+a+\frac{j}{c},b) ) }
#' \deqn{a,b,c > 0}
#'
#' The mean, variance and over dispersion are denoted as
#' \deqn{E_{McGBB}[x]= n\frac{B(a+b,\frac{1}{c})}{B(a,\frac{1}{c})} }
#' \deqn{Var_{McGBB}[x]= n^2(\frac{B(a+b,\frac{2}{c})}{B(a,\frac{2}{c})}-(\frac{B(a+b,\frac{1}{c})}{B(a,\frac{1}{c})})^2) +n(\frac{B(a+b,\frac{1}{c})}{B(a,\frac{1}{c})}-\frac{B(a+b,\frac{2}{c})}{B(a,\frac{2}{c})}) }
#' \deqn{over dispersion= \frac{\frac{B(a+b,\frac{2}{c})}{B(a,\frac{2}{c})}-(\frac{B(a+b,\frac{1}{c})}{B(a,\frac{1}{c})})^2}{\frac{B(a+b,\frac{1}{c})}{B(a,\frac{1}{c})}-(\frac{B(a+b,\frac{1}{c})}{B(a,\frac{1}{c})})^2}}
#' \deqn{x = 0,1,2,...n}
#' \deqn{n = 1,2,3,...}
#'
#' @return
#' The output of \code{dMcGBB} gives a list format consisting
#'
#' \code{pdf}            probability function values in vector form.
#'
#' \code{mean}           mean of McDonald Generalized Beta Binomial Distribution.
#'
#' \code{var}            variance of McDonald Generalized Beta Binomial Distribution.
#'
#' \code{over.dis.para}  over dispersion value of McDonald Generalized Beta Binomial Distribution.
#'
#' @references
#' \insertRef{manoj2013mcdonald}{fitODBOD}
#' \insertRef{janiffer2014estimating}{fitODBOD}
#' \insertRef{roozegar2017mcdonald}{fitODBOD}
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(5)
#' a <- c(1,2,5,10,0.6)
#' plot(0,0,main="Mcdonald generalized beta-binomial probability function graph",
#' xlab="Binomial random variable",ylab="Probability function values",xlim = c(0,10),ylim = c(0,0.5))
#' for (i in 1:5)
#' {
#' lines(0:10,dMcGBB(0:10,10,a[i],2.5,a[i])$pdf,col = col[i],lwd=2.85)
#' points(0:10,dMcGBB(0:10,10,a[i],2.5,a[i])$pdf,col = col[i],pch=16)
#' }
#'
#' dMcGBB(0:10,10,4,2,1)$pdf             #extracting the pdf values
#' dMcGBB(0:10,10,4,2,1)$mean            #extracting the mean
#' dMcGBB(0:10,10,4,2,1)$var             #extracting the variance
#' dMcGBB(0:10,10,4,2,1)$over.dis.para   #extracting the over dispersion value
#'
#' #plotting the random variables and cumulative probability values
#' col <- rainbow(4)
#' a <- c(1,2,5,10)
#' plot(0,0,main="Cumulative probability function graph",xlab="Binomial random variable",
#' ylab="Cumulative probability function values",xlim = c(0,10),ylim = c(0,1))
#' for (i in 1:4)
#' {
#' lines(0:10,pMcGBB(0:10,10,a[i],a[i],2),col = col[i])
#' points(0:10,pMcGBB(0:10,10,a[i],a[i],2),col = col[i])
#' }
#'
#' pMcGBB(0:10,10,4,2,1)       #acquiring the cumulative probability values
#'
#' @export
dMcGBB<-function(x,n,a,b,c)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,n,a,b,c))) | any(is.infinite(c(x,n,a,b,c))) | any(is.nan(c(x,n,a,b,c))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    #checking if shape parameters are less than or equal zero ,
    #if so providing an error message and stopping the function progress
    if(a <= 0 | b <= 0 | c <= 0)
    {
      stop("Shape parameters cannot be less than or equal to zero")
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
        #for each random variable in the input vector below calculations occur
        final<-sapply(1:length(x),function(i){
          value<-sum(((-1)^(0:(n-x[i])))*choose(n-x[i],(0:(n-x[i])))*beta((x[i]/c+a+(0:(n-x[i]))/c),b))
          choose(n,x[i])*(1/beta(a,b))*value} )
      }
    }
  }
  # generating an output in list format consisting pdf,mean,variance and overdispersion value
  return(list("pdf"=final,"mean"=n*beta(a+b,1/c)/beta(a,1/c),
              "var"=n^2*((beta(a+b,2/c)/beta(a,2/c))-(beta(a+b,1/c)/beta(a,1/c))^2)+
                n*((beta(a+b,1/c)/beta(a,1/c))-(beta(a+b,2/c)/beta(a,2/c))),
              "over.dis.para"=(((beta(a+b,2/c))/(beta(a,2/c)))-((beta(a+b,1/c))/(beta(a,1/c)))^2)/
                (((beta(a+b,1/c))/(beta(a,1/c)))-((beta(a+b,1/c))/(beta(a,1/c)))^2)))
}

#' McDonald Generalized Beta Binomial Distribution
#'
#' These functions provide the ability for generating probability function values and
#' cumulative probability function values for the McDonald Generalized Beta
#' Binomial Distribution.
#'
#' @usage
#' pMcGBB(x,n,a,b,c)
#'
#' @param x        vector of binomial random variables.
#' @param n        single value for no of binomial trials.
#' @param a        single value for shape parameter alpha representing as a.
#' @param b        single value for shape parameter beta representing as b.
#' @param c        single value for shape parameter gamma representing as c.
#'
#' @details
#' Mixing Generalized Beta Type-1 Distribution with  Binomial distribution
#' the probability function value and cumulative probability function can be constructed
#' and are denoted below.
#'
#' The cumulative probability function is the summation of probability function values.
#'
#' \deqn{P_{McGBB}(x)= {n \choose x} \frac{1}{B(a,b)} (\sum_{j=0}^{n-x} (-1)^j {n-x \choose j} B(\frac{x}{c}+a+\frac{j}{c},b) ) }
#' \deqn{a,b,c > 0}
#'
#' The mean, variance and over dispersion are denoted as
#' \deqn{E_{McGBB}[x]= n\frac{B(a+b,\frac{1}{c})}{B(a,\frac{1}{c})} }
#' \deqn{Var_{McGBB}[x]= n^2(\frac{B(a+b,\frac{2}{c})}{B(a,\frac{2}{c})}-(\frac{B(a+b,\frac{1}{c})}{B(a,\frac{1}{c})})^2) +n(\frac{B(a+b,\frac{1}{c})}{B(a,\frac{1}{c})}-\frac{B(a+b,\frac{2}{c})}{B(a,\frac{2}{c})}) }
#' \deqn{over dispersion= \frac{\frac{B(a+b,\frac{2}{c})}{B(a,\frac{2}{c})}-(\frac{B(a+b,\frac{1}{c})}{B(a,\frac{1}{c})})^2}{\frac{B(a+b,\frac{1}{c})}{B(a,\frac{1}{c})}-(\frac{B(a+b,\frac{1}{c})}{B(a,\frac{1}{c})})^2}}
#' \deqn{x = 0,1,2,...n}
#' \deqn{n = 1,2,3,...}
#'
#' @return
#' The output of \code{pMcGBB} gives cumulative probability function values in vector form.
#'
#' @references
#' \insertRef{manoj2013mcdonald}{fitODBOD}
#' \insertRef{janiffer2014estimating}{fitODBOD}
#' \insertRef{roozegar2017mcdonald}{fitODBOD}
#'
#' @examples
#' #plotting the random variables and probability values
#' col <- rainbow(5)
#' a <- c(1,2,5,10,0.6)
#' plot(0,0,main="Mcdonald generalized beta-binomial probability function graph",
#' xlab="Binomial random variable",ylab="Probability function values",xlim = c(0,10),ylim = c(0,0.5))
#' for (i in 1:5)
#' {
#' lines(0:10,dMcGBB(0:10,10,a[i],2.5,a[i])$pdf,col = col[i],lwd=2.85)
#' points(0:10,dMcGBB(0:10,10,a[i],2.5,a[i])$pdf,col = col[i],pch=16)
#' }
#'
#' dMcGBB(0:10,10,4,2,1)$pdf             #extracting the pdf values
#' dMcGBB(0:10,10,4,2,1)$mean            #extracting the mean
#' dMcGBB(0:10,10,4,2,1)$var             #extracting the variance
#' dMcGBB(0:10,10,4,2,1)$over.dis.para   #extracting the over dispersion value
#'
#' #plotting the random variables and cumulative probability values
#' col <- rainbow(4)
#' a <- c(1,2,5,10)
#' plot(0,0,main="Cumulative probability function graph",xlab="Binomial random variable",
#' ylab="Cumulative probability function values",xlim = c(0,10),ylim = c(0,1))
#' for (i in 1:4)
#' {
#' lines(0:10,pMcGBB(0:10,10,a[i],a[i],2),col = col[i])
#' points(0:10,pMcGBB(0:10,10,a[i],a[i],2),col = col[i])
#' }
#'
#' pMcGBB(0:10,10,4,2,1)       #acquiring the cumulative probability values
#'
#' @export
pMcGBB<-function(x,n,a,b,c)
{
  #for each binomial random variable in the input vector the cumulative proability function
  #values are calculated
  ans<-sapply(1:length(x),function(i) sum(dMcGBB(0:x[i],n,a,b,c)$pdf))
  #generating an ouput vector cumulative probability function values
  return(ans)
}

#' Negative Log Likelihood value of McDonald Generalized Beta  Binomial Distribution
#'
#' This function will calculate the negative log likelihood value when the vector of binomial random
#' variables and vector of corresponding frequencies are given with the shape parameters a,b and c.
#'
#' @usage
#' NegLLMcGBB(x,freq,a,b,c)
#'
#' @param x                 vector of binomial random variables.
#' @param freq              vector of frequencies.
#' @param a                 single value for shape parameter alpha representing as a.
#' @param b                 single value for shape parameter beta representing as b.
#' @param c                 single value for shape parameter gamma representing as c.
#'
#' @details
#' \deqn{0 < a,b,c }
#' \deqn{freq \ge 0}
#' \deqn{x = 0,1,2,...}
#'
#' @return
#' The output of \code{NegLLMcGBB} will produce a single numeric value.
#'
#' @references
#' \insertRef{manoj2013mcdonald}{fitODBOD}
#' \insertRef{janiffer2014estimating}{fitODBOD}
#' \insertRef{roozegar2017mcdonald}{fitODBOD}
#'
#' @examples
#' No.D.D <- 0:7            #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)    #assigning the corresponding frequencies
#'
#' NegLLMcGBB(No.D.D,Obs.fre.1,.2,.3,1)    #acquiring the negative log likelihood value
#'
#' @export
NegLLMcGBB<-function(x,freq,a,b,c)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,freq,a,b,c))) | any(is.infinite(c(x,freq,a,b,c)))
     |any(is.nan(c(x,freq,a,b,c))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    #checking if any of the random variables of frequencies are less than zero if so
    #creating a error message as well as stopping the function progress
    if(any(c(x,freq) < 0))
    {
      stop("Binomial random variable or frequency values cannot be negative")
    }
    #checking if shape parameters are less than or equal to zero
    #if so creating an error message as well as stopping the function progress
    else if( a <= 0 | b <= 0 | c <= 0)
    {
      stop("Shape parameters cannot be less than or equal to zero")
    }
    else
    {
      #constructing the data set using the random variables vector and frequency vector
      n<-max(x)
      data<-rep(x,freq)
      value<-sapply(1:sum(freq),function(i) sum(((-1)^(0:n-data[i]))*choose(n-data[i],(0:n-data[i]))*
                                                  beta((data[i]/c)+a+((0:n-data[i])/c),b)))

      #calculating the negative log likelihood value and representing as a single output value
      return(-(sum(log(choose(n,data[1:sum(freq)])))+sum(log(value))+
                 sum(freq)*log(1/beta(a,b))))
    }
  }
}

#' Estimating the shape parameters a,b and c for McDonald Generalized Beta Binomial
#' distribution
#'
#' The function will estimate the shape parameters using the maximum log likelihood method  for
#' the McDonald Generalized Beta  Binomial distribution when the binomial random
#' variables and corresponding frequencies are given.
#'
#' @usage
#' EstMLEMcGBB(x,freq,a,b,c,...)
#'
#' @param x                  vector of binomial random variables.
#' @param freq               vector of frequencies.
#' @param a                  single value for shape parameter alpha representing as a.
#' @param b                  single value for shape parameter beta representing as b.
#' @param c                  single value for shape parameter gamma representing as c.
#' @param ...                mle2 function inputs except data and estimating parameter.
#'
#' @details
#' \deqn{0 < a,b,c}
#' \deqn{x = 0,1,2,...}
#' \deqn{freq \ge 0}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions necessary
#' error messages will be provided to go further.
#'
#' @return
#' \code{EstMLEMcGBB} here is used as a wrapper for the \code{mle2} function of \pkg{bbmle} package
#' therefore output is of class of mle2.
#'
#' @references
#' \insertRef{manoj2013mcdonald}{fitODBOD}
#' \insertRef{janiffer2014estimating}{fitODBOD}
#' \insertRef{roozegar2017mcdonald}{fitODBOD}
#'
#' @seealso
#' \code{\link[bbmle]{mle2}}
#'
#' @examples
#' No.D.D <- 0:7                   #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)  #assigning the corresponding frequencies
#'
#' \dontrun{
#' #estimating the parameters using maximum log likelihood value and assigning it
#' parameters <- EstMLEMcGBB(x=No.D.D,freq=Obs.fre.1,a=0.1,b=0.1,c=0.2)
#'
#' bbmle::coef(parameters)         #extracting the parameters
#' }
#' @export
EstMLEMcGBB<-function(x,freq,a,b,c,...)
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
  output<-suppressWarnings2(bbmle::mle2(.EstMLEMcGBB,data=list(x=x,freq=freq),
                                        start = list(a=a,b=b,c=c),...),"NaN")
  return(output)
}


.EstMLEMcGBB<-function(x,freq,a,b,c)
{
  #with respective to using bbmle package function mle2 there is no need impose any restrictions
  #therefor the output is directly a single numeric value for the negative log likelihood value of
  #Gaussian Hypergeometric Generalized Beta binomial distribution
  n<-max(x)
  data<-rep(x,freq)

  value<-sapply(1:sum(freq),function(i) sum(((-1)^(0:n-data[i]))*choose(n-data[i],(0:n-data[i]))*
                                              beta((data[i]/c)+a+((0:n-data[i])/c),b)))

  return(-(sum(log(choose(n,data[1:sum(freq)])))+sum(log(value))+
             sum(freq)*log(1/beta(a,b))))
}

#' Fitting the McDonald Generalized Beta  Binomial distribution when binomial
#' random variable, frequency and shape parameters are given
#'
#' The function will fit the McDonald Generalized Beta  Binomial Distribution
#' when random variables, corresponding frequencies and shape parameters are given. It will provide
#' the expected frequencies, chi-squared test statistics value, p value, degree of freedom
#' and over dispersion value so that it can be seen if this distribution fits the data.
#'
#' @usage fitMcGBB(x,obs.freq,a,b,c)
#'
#' @param x                  vector of binomial random variables.
#' @param obs.freq           vector of frequencies.
#' @param a                  single value for shape parameter alpha representing a.
#' @param b                  single value for shape parameter beta representing b.
#' @param c                  single value for shape parameter gamma representing c.
#'
#' @details
#' \deqn{0 < a,b,c}
#' \deqn{x = 0,1,2,...}
#' \deqn{obs.freq \ge 0}
#'
#' \strong{NOTE} : If input parameters are not in given domain conditions necessary
#' error messages will be provided to go further.
#'
#' @return
#' The output of \code{fitMcGBB} gives the class format \code{fitMB} and \code{fit} consisting a list
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
#' \code{fitMB} fitted values of \code{dMcGBB}.
#'
#' \code{NegLL} Negative Log Likelihood value.
#'
#' \code{a} estimated value for alpha parameter as a.
#'
#' \code{b} estimated value for beta parameter as b.
#'
#' \code{c} estimated value for gamma parameter as c.
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
#' \insertRef{manoj2013mcdonald}{fitODBOD}
#' \insertRef{janiffer2014estimating}{fitODBOD}
#' \insertRef{roozegar2017mcdonald}{fitODBOD}
#'
#' @seealso
#' \code{\link[bbmle]{mle2}}
#'
#' @examples
#' No.D.D <- 0:7       #assigning the random variables
#' Obs.fre.1 <- c(47,54,43,40,40,41,39,95)          #assigning the corresponding frequencies
#'
#' \dontrun{
#' #estimating the parameters using maximum log likelihood value and assigning it
#' parameters <- EstMLEMcGBB(x=No.D.D,freq=Obs.fre.1,a=0.1,b=0.1,c=3.2)
#'
#' aMcGBB <- bbmle::coef(parameters)[1]         #assigning the estimated a
#' bMcGBB <- bbmle::coef(parameters)[2]         #assigning the estimated b
#' cMcGBB <- bbmle::coef(parameters)[3]         #assigning the estimated c
#'
#' #fitting when the random variable,frequencies,shape parameter values are given.
#' results <- fitMcGBB(No.D.D,Obs.fre.1,aMcGBB,bMcGBB,cMcGBB)
#' results
#'
#' #extracting the expected frequencies
#' fitted(results)
#'
#' #extracting the residuals
#' residuals(results)
#' }
#'
#' @export
 fitMcGBB<-function(x,obs.freq,a,b,c)
{
  #checking if inputs consist NA(not assigned)values, infinite values or NAN(not a number)values
  #if so creating an error message as well as stopping the function progress.
  if(any(is.na(c(x,obs.freq,a,b,c))) | any(is.infinite(c(x,obs.freq,a,b,c))) |
     any(is.nan(c(x,obs.freq,a,b,c))) )
  {
    stop("NA or Infinite or NAN values in the Input")
  }
  else
  {
    est<-dMcGBB(x,max(x),a,b,c)
    odp<-est$over.dis.para; names(odp)<-NULL
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
    NegLL<-NegLLMcGBB(x,obs.freq,a,b,c)
    names(NegLL)<-NULL
    #the final output is in a list format containing the calculated values
    final<-list("bin.ran.var"=x,"obs.freq"=obs.freq,"exp.freq"=exp.freq,
                "statistic"=round(statistic,4),"df"=df,"p.value"=round(p.value,4),
                "fitMB"=est,"NegLL"=NegLL,"a"=a,"b"=b,"c"=c,"AIC"=2*3+2*NegLL,
                "over.dis.para"=odp,"call"=match.call())
    class(final)<-c("fitMB","fit")
    return(final)
    }
}

#' @method fitMcGBB default
#' @export
fitMcGBB.default<-function(x,obs.freq,a,b,c)
{
  return(fitMcGBB(x,obs.freq,a,b,c))
}

#' @method print fitMB
#' @export
print.fitMB<-function(x,...)
{
  cat("Call: \n")
  print(x$call)
  cat("\nChi-squared test for Mc-Donald Generalized Beta-Binomial Distribution \n\t
      Observed Frequency : ",x$obs.freq,"\n\t
      expected Frequency : ",x$exp.freq,"\n\t
      estimated a parameter :",x$a, "  ,estimated b parameter :",x$b,",\n\t
      estimated c parameter :",x$c,"\n\t
      X-squared :",x$statistic,"  ,df :",x$df,"  ,p-value :",x$p.value,"\n\t
      over dispersion :",x$over.dis.para,"\n")
}

#' @method summary fitMB
#' @export
summary.fitMB<-function(object,...)
{
  cat("Call: \n")
  print(object$call)
  cat("\nChi-squared test for Mc-Donald Generalized Beta-Binomial Distribution \n\t
      Observed Frequency : ",object$obs.freq,"\n\t
      expected Frequency : ",object$exp.freq,"\n\t
      estimated a parameter :",object$a,"  ,estimated b parameter :",object$b,",\n\t
      estimated c parameter :",object$c,"\n\t
      X-squared :",object$statistic,"  ,df :",object$df,"  ,p-value :",object$p.value,"\n\t
      over dispersion :",object$over.dis.para,"\n\t
      Negative Loglikehood value :",object$NegLL,"\n\t
      AIC value :",object$AIC,"\n")
}

#' @importFrom bbmle mle2
#' @import hypergeo
#' @importFrom stats pchisq
