pwr.2way<-function(a=a, b=b, alpha=alpha, size.A=size.A, size.B=size.B, f.A=NULL, f.B=NULL, delta.A=NULL, delta.B=NULL, sigma.A=NULL, sigma.B=NULL){

  pwr.fA<-function(a=a, b=b, alpha=alpha, size.A=size.A, f.A = NULL, delta.A=NULL, sigma.A=NULL){
    N=size.A*a*b
    if (is.null(f.A)) {f.A<-sqrt(((1/a)*(delta.A/2)^2*2)/sigma.A^2)
    lambda<-N*f.A^2 
    q<-qf(alpha, a-1, N-a-b+1, lower.tail=FALSE)
    power<-pf(q, a-1, N-a-b+1, lambda, lower.tail=FALSE)
    return(power)
    } else {f.A<-f.A
    lambda<-N*f.A^2 
    q<-qf(alpha, a-1, N-a-b+1, lower.tail=FALSE)
    power<-pf(q, a-1, N-a-b+1, lambda, lower.tail=FALSE)
    return(power)
    }
  }
  
  pwr.fB<-function(a=a, b=b, alpha=alpha, size.B=size.B, f.B=NULL, delta.B=NULL, sigma.B=NULL){
    N=size.B*a*b
    if (is.null(f.B)) {f.B<-sqrt(((1/b)*(delta.B/2)^2*2)/sigma.B^2)
    lambda<-N*f.B^2 
    q<-qf(alpha, b-1, N-a-b+1, lower.tail=FALSE)
    pwr<-pf(q, b-1, N-a-b+1, lambda, lower.tail=FALSE)
    return(pwr)
    } else { f.B<-f.B
    lambda<-N*f.B^2 
    q<-qf(alpha, b-1, N-a-b+1, lower.tail=FALSE)
    pwr<-pf(q, b-1, N-a-b+1, lambda, lower.tail=FALSE)
    return(pwr)
    }
  }
  
    
  if (is.null(f.A)) {
  pwrA<-pwr.fA(a=a, b=b, alpha=alpha, size.A=size.A, f.A=NULL, delta.A=delta.A, sigma.A=sigma.A)
  pwrB<-pwr.fB(a=a, b=b, alpha=alpha, size.B=size.B, f.B=NULL, delta.B=delta.B, sigma.B=sigma.B)}
  
  else { 
  pwrA<-pwr.fA(a=a, b=b, alpha=alpha, size.A=size.A, f.A=f.A)
  pwrB<-pwr.fB(a=a, b=b, alpha=alpha, size.B=size.B, f.B=f.B)}
  
  pwr<-min(pwrA, pwrB)
  
  NOTE <- "power is the minimum power among two factors"
  METHOD <- "Balanced two-way analysis of variance power calculation"
  structure(list(a = a, b=b, n.A = size.A, n.B = size.B, sig.level=alpha, power.A = pwrA, power.B = pwrB, power = pwr, note = NOTE, method = METHOD), class = "power.htest")
}