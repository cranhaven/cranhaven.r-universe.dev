ss.2way<-function(a=a, b=b, alpha=alpha, beta=beta, f.A=NULL, f.B=NULL, delta.A=NULL,  delta.B=NULL, sigma.A=NULL, sigma.B=NULL, B=B){
  
  ss.fA<-function(a=a, b=b, alpha=alpha, beta=beta, f.A=NULL, delta.A=NULL, sigma.A=NULL, B=B){
    if (is.null(f.A)) {f.A<-sqrt(((1/a)*(delta.A/2)^2*2)/sigma.A^2)
    power<-NULL
    
    for (i in 1:B){
      ni=i+1
      N=ni*a*b
      lambda<-N*f.A^2 
      q<-qf(alpha, a-1, N-a-b+1, lower.tail=FALSE)
      power.new<-pf(q, a-1, N-a-b+1, lambda, lower.tail=FALSE)
      power<-c(power, power.new)
      if(power[i]>=1-beta) break
    }
    ssize<-length(power)+1
    return(ssize)
    } else {f.A=f.A
    power<-NULL
    
    for (i in 1:B){
      ni=i+1
      N=ni*a*b
      lambda<-N*f.A^2 
      q<-qf(alpha, a-1, N-a-b+1, lower.tail=FALSE)
      power.new<-pf(q, a-1, N-a-b+1, lambda, lower.tail=FALSE)
      power<-c(power, power.new)
      if(power[i]>=1-beta) break
    }
    ssize<-length(power)+1
    return(ssize)
    }
  }
  
  ss.fB<-function(a=a, b=b, alpha=alpha, beta=beta, f.B=NULL, delta.B=NULL, sigma.B=NULL, B=B){
    if (is.null(f.B)) {f.B<-sqrt(((1/b)*(delta.B/2)^2*2)/sigma.B^2)
    power<-NULL
    
    for (i in 1:B){
      ni=i+1
      N=ni*a*b
      f<-sqrt(((1/b)*(delta.B/2)^2*2)/sigma.B^2)
      lambda<-N*f.B^2 
      q<-qf(alpha, b-1, N-a-b+1, lower.tail=FALSE)
      power.new<-pf(q, b-1, N-a-b+1, lambda, lower.tail=FALSE)
      power<-c(power, power.new)
      if(power[i]>=1-beta) break
    }
    ssize<-length(power)+1
    return(ssize)
    } else {f.B=f.B
    
    power<-NULL
    
    for (i in 1:B){
      ni=i+1
      N=ni*a*b
      f<-sqrt(((1/b)*(delta.B/2)^2*2)/sigma.B^2)
      lambda<-N*f.B^2 
      q<-qf(alpha, b-1, N-a-b+1, lower.tail=FALSE)
      power.new<-pf(q, b-1, N-a-b+1, lambda, lower.tail=FALSE)
      power<-c(power, power.new)
      if(power[i]>=1-beta) break
    }
    ssize<-length(power)+1
    return(ssize)
    }
    
  }  
  
  ssizeA<-ss.fA(a=a, b=b, alpha=alpha, beta=beta, f.A=f.A, delta.A=delta.A, sigma.A=sigma.A, B=B)
  ssizeB<-ss.fB(a=a, b=b, alpha=alpha, beta=beta, f.B=f.B, delta.B=delta.B, sigma.B=sigma.B, B=B)
  ssize<-max(ssizeA, ssizeB)
  
  NOTE <- paste("n is number in each group, total sample =", ssize*a*b) 
  METHOD <- "Balanced two-way analysis of variance sample size adjustment"
  structure(list(a = a, b=b, sig.level=alpha, power = 1-beta, n = ssize, note = NOTE, method = METHOD), class = "power.htest")
}
