Generate_BM = function(x, t, n){
  
  Bt = rep(0, times=(n+1))
  tn = seq(0, t, t/n)
  Bt[1] = x
  for(i in 1:n){
    Bt[i+1] = Bt[i] + rnorm(1, 0, sqrt(t/n))
  }
  
  lst = list()
  lst[[1]] = tn
  lst[[2]] = Bt
  
  return(lst)
  
}



#' Brownian Motion
#' 
#' Get a d-dimension Brownian motion B(t) and its graph
#'
#'@param x a starting point of the Brownian motion, B(0).
#'@param t end of time, that is, \eqn{0=t_{0}<t_{1}<...<t_{n}=t}
#'@param n the last index of the time, that is, \eqn{t_{n}=t}  
#'@param d a dimension of the Brownian motion: 1 or 2.
#'@return A list of 
#'\describe{
#'\item{tn}{\eqn{t_{n}}}
#'\item{Bt1}{a vector of the Brownian motion (\eqn{B_{1}(t_{0},...,B_{1}(t_{n}))})}
#'\item{Bt2}{a vector of the Brownian motion (\eqn{B_{2}(t_{0},...,B_{2}(t_{n}))})}
#'}
#'
#'
#'@examples
#'
#'#### Generate an 2-dimensional Brownian motion and its graph
#'x0=c(0,0)
#'lst = BM(x=x0, t=1, n=10, d=2)
#'
#'@export

BM = function(x=0, t=1, n=10, d=1){
  
  
  mainval=1.5
  
  
  if(d==1){
    
    if(length(x)!=1){
      x=0
    }
    
    lst0 = Generate_BM(x,t,n)
    
    tn = lst0[[1]]
    Bt1 = lst0[[2]]
    
    lst = list(tn=tn, Bt1=Bt1)
    
    xlabstr="t"
    ylabstr="B(t)"
    
    mainstr = "Graph of a Browian motion B(t)"
    
    plot(tn, Bt1,
         xlab=xlabstr, ylab=ylabstr, main=mainstr,
         type="l", lty=1, col="red")
    
    
  }else{
    
    if(length(x)!=2){
      x = c(0,0)
    }
    
    x1 = x[1]
    x2 = x[2]
    lst1 = Generate_BM(x1,t,n)
    lst2 = Generate_BM(x2,t,n)
    
    tn = lst1[[1]]
    Bt1 = lst1[[2]]
    Bt2 = lst2[[2]]
    
    
    
    xlabstr=expression("B"[1])
    ylabstr=expression("B"[2])
    mainstr = expression("Graph of a Brownian motion (B"[1]*"(t), B"[2]*"(t))"  )
    plot(Bt1, Bt2, xlab=xlabstr, ylab=ylabstr, main=mainstr, cex.main=mainval,
         type="l", lty=2, col="red")
    
    lst = list(tn=tn, Bt1=Bt1, Bt2=Bt2)
    
    
  }
  
  return(lst)
  
}
