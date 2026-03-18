 hierarch.bs <- function(x, d, plot.bsp, typ, penden.env, int=FALSE) #int optional! 
  {
    q <- get("q",penden.env)
    base <- get("base",penden.env)
    alpha <- get("alpha",penden.env)
    symmetric <- get("symmetric",penden.env)
    K <-  0
    ddb <- get("ddb",penden.env)#new
    
    knots <- knots.transform( d=K , alpha = get("alpha",penden.env),get("symmetric",penden.env))
    if(get("margin.normal",penden.env)) {
      knots<-qnorm(knots)
      knots[1]<-qnorm(0.0000001)
      knots[length(knots)]<-qnorm(1-0.0000001)
    }
    h <- 1
    if(base=="B-spline") {
      B.tilde <- my.bspline(h=h,q=q+1,knots=knots,y=x,K=length(knots),plot.bsp=plot.bsp,typ=typ)$base.den
        }

    if(int& base=="B-spline") {
      index.h <- c(1,2)
      int.B.tilde <- round(distr.func.help(B.tilde,knots,penden.env,q,y=x,index.h),5)
      index.h<-c(1,2)
    }

    if(base=="B-spline") {
      for ( K in 1:d)
        {
          h <- 1/(2**K)
          index <- (q-1) + 2*seq(1,2**(K-1),by=1)
                 knots <- knots.transform(d=K, alpha = get("alpha",penden.env),get("symmetric",penden.env))
          
          BB <-  my.bspline(h,q=q+1,knots, y=x,K=length(knots),plot.bsp=plot.bsp,typ=typ)$base.den
              
          #integriere BB
        
          if(int & base=="B-spline") {
            index.h <- seq(max(index.h)+1,max(index.h)+length(index))
            BB.int <- round(distr.func.help(BB,knots,penden.env,q,y=x,index.h),5)
          }
          
          dimBB <- dim(BB)
       
          if(dimBB[1]>1) {
            B.tilde <-  cbind(B.tilde,BB[,index])
            if(int) int.B.tilde <- cbind(int.B.tilde,BB.int)
          }
          else {
            B.tilde <-  c(B.tilde,BB[,index])
            if(int) int.B.tilde <- c(int.B.tilde,BB.int)
          }
        }
      dimBB <- dim(BB)
    }
    jacobi <- matrix(1, length(x),1)#new
    if(dimBB[1]>1) B.tilde.transform <- B.tilde * kronecker( matrix(jacobi) , matrix(1,1,dim(B.tilde)[2]))
    else B.tilde.transform <- B.tilde * kronecker( matrix(jacobi) , matrix(1,1,length(B.tilde)))
    if(int) {
      int.B.tilde.transform <- int.B.tilde * kronecker( matrix(jacobi) , matrix(1,1,dim(int.B.tilde)[2]))
      return(list(B.tilde=B.tilde.transform,int.B.tilde=int.B.tilde.transform))
    }
    else return(list(B.tilde=B.tilde.transform))
  }
    
