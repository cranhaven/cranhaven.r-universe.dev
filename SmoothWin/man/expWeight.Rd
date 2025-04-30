\name{expWeight}
\alias{expWeight}
\title{
  This function computes the smooth windowing weights
}
\description{
  The symmetric weight generating function (SWGF). This function computes the exponential weights/kernel (soft windowing weights) for different shapes (k)  and bandwidth (l) and plots the weights.
}
\usage{
  expWeight(
      t                        ,
      k                        ,
      l                        ,
      m = 0                    ,
      direction = c(1, 1)      ,
      plot = FALSE             ,
      zeroCompensation = 0     ,
      cdf   = plogis           ,
      progress         = FALSE ,
      ...
)
}
\arguments{
  \item{t}{
    Vector of numeric time. A vector of positive continuous values for time 
  }
  \item{k}{
    A single positive value for sharpness
  }
  \item{l}{
    A single non-negative value for bandwidth
  }
  \item{m}{
    Vector of indices. The index of the modes on `t` (modes are the peak of the windows)
  }  
  \item{direction}{
  Vector of two numeric values. A vector of the form on (Left,right). The first element specifies the speed of expansion of the window(s) from the left and the second value for the right expansion. Setting to (0,1) and (1,0) lead to right and left expansions of the windows respectively. Default (1,1) that is the window(s) expand symmetrically from both sides. 
  }
  \item{plot}{
    Logical flag. Setting to TRUE shows a plot of the weights
  }
  \item{zeroCompensation}{
   Single non-negative value. Setting to any non-negative value would replace all (weights =< zeroCompensation) with zeroCompensation. Default 0 (zero)
  }
  \item{cdf}{
      A cdf function preferably symmetric. The cdf function is used for the (window) weight generating function. The function must have two parameters precisely a location such as mean and a scale. Standard cdf functions such as pnorm, pcauchy and plogis (default) can be used. For an example of custom made function we define uniform function as below:\cr\cr
      \deqn{
      punif0 = function(x,mean=0.5,sd=sqrt(1/12)){
             a = mean - sqrt(3) *sd;
             b = mean + sqrt(3) *sd;
             r =  punif(q = x,min = a,max = b) ;
             return(r)    
          }
    }
  }
  \item{progress}{
  Logical flag. Setting to TRUE shows the progress of the function
  }
  \item{\dots}{
    Other parameters that can be passed to the `plot()` function such as pch, colour etc.
  }
}
\value{
  A numeric vector of weights
}

\author{
  Hamed Haselimashhadi <hamedhm@ebi.ac.uk>
}

\seealso{
  \code{\link{SmoothWin}}
}
\examples{
  par(mfrow = c(4, 1))
  ##################################################
  # Example 1 - no merging happends between windows
  ##################################################
  weight = expWeight(
    t = 1:100                                       ,
    k = 5                                           ,
    l = 10                                          ,
    m = c(25, 50, 75)                               ,
    plot = TRUE                                     ,
    ### Passed parameters to the plot function
    type = 'l'                                      ,
    lty = 2                                         ,
    lwd = 3                                         ,
    main = '1. If windows do not intersect, then wont merge! (l=10, k=5)'
  )
  
  ##################################################
  # Example 2 - merging in windows
  ##################################################
  weight = expWeight(
    t = 1:100                                       ,
    k = 5                                           ,
    l = 15                                          ,
    m = c(25, 50, 75)                               ,
    plot = TRUE                                     ,
    ### Passed parameters to the plot function
    type = 'l'                                      ,
    lty = 2                                         ,
    lwd = 3                                         ,
    main = '2. If windows intersect, then merge! (l=15, k=5)'
  )
  
  ##################################################
  # Example 3.1  - partial merging in windows
  ##################################################
  weight = expWeight(
    t = 1:100                                       ,
    k = 1                                           ,
    l = 12                                          ,
    m = c(25, 50, 75)                               ,
    plot = TRUE                                     ,
    ### Passed parameters to the plot function
    type = 'l'                                      ,
    lty = 2                                         ,
    lwd = 3                                         ,
    main = '3.1 If windows intersect with small k, then partially merge! (l=12, k=1)'
  )
  
  ##################################################
  # Example 3.2  - partial merging in windows
  ##################################################
    weight = expWeight(
    t = 1:100                                       ,
    k = .1                                           ,
    l = 12                                          ,
    m = c(25, 50, 75)                               ,
    plot = TRUE                                     ,
    ### Passed parameters to the plot function
    type = 'l'                                      ,
    lty = 2                                         ,
    lwd = 3                                         ,
    main = '3.2 If windows intersect with small k, then partially merge! (l=12, k=0.1)'
  )
  
}

