ss.1way<-function(k=k, alpha=alpha, beta=beta, f=NULL, delta=delta, sigma=sigma, B=B){
  if (is.null(f)) {f<-sqrt(((1/k)*(delta/2)^2*2)/sigma^2) 
  
  power<-NULL
  
  for (i in 1:B){
    ni=i+1
    N=ni*k
    f<-sqrt(((1/(1*k))*(delta/2)^2*2)/sigma^2)
    lambda<-N*f^2 
    q<- qf(alpha, k - 1, (ni - 1) * k, lower.tail = FALSE)
    power.new<-pf(q,  k - 1, (ni - 1) * k, lambda, lower.tail = FALSE)
    power<-c(power, power.new)
    if(power[i]>=1-beta) break
  }
  ssize<-length(power)+1
  
  NOTE <- paste("n is number in each group, total sample =", k*ssize) 
  METHOD <- "Balanced one-way analysis of variance sample size adjustment"
  structure(list(k=k, sig.level=alpha, power = 1-beta, n = ssize, note = NOTE, method = METHOD), class = "power.htest")
  } else {f<-f
   power<-NULL
  
   for (i in 1:B){
    ni=i+1
    N=ni*k
    lambda<-N*f^2 
    q<- qf(alpha, k - 1, (ni - 1) * k, lower.tail = FALSE)
    power.new<-pf(q,  k - 1, (ni - 1) * k, lambda, lower.tail = FALSE)
    power<-c(power, power.new)
    if(power[i]>=1-beta) break
  }
  ssize<-length(power)+1
  
  NOTE <- paste("n is number in each group, total sample =", k*ssize) 
  METHOD <- "Balanced one-way analysis of variance sample size adjustment"
  structure(list(k=k, sig.level=alpha, power = 1-beta, n = ssize, note = NOTE, method = METHOD), class = "power.htest")
  } 
}

