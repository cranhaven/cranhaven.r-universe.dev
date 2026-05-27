pwr.1way<-function(k=k, n=n,alpha=alpha, f=NULL, delta=delta, sigma=sigma){
  if (is.null(f)) {f<-sqrt(((1/k)*(delta/2)^2*2)/sigma^2)     
     lambda<-n*k*f^2 
     q<- qf(alpha, k - 1, (n - 1) * k, lower.tail = FALSE)
     pwr<-pf(q,  k - 1, (n - 1) * k, lambda, lower.tail = FALSE)
  
     NOTE <- paste("n is number in each group, total sample =", k*n, "power =", pwr) 
     METHOD <- "Balanced one-way analysis of variance power calculation"
     structure(list(k=k, n=n, delta=delta, sigma=sigma, effect.size=f, sig.level=alpha, power = pwr, note = NOTE, method = METHOD), class = "power.htest")
  } else {f<-f  
         lambda<-n*k*f^2 
         q<- qf(alpha, k - 1, (n - 1) * k, lower.tail = FALSE)
         pwr<-pf(q,  k - 1, (n - 1) * k, lambda, lower.tail = FALSE)
  
         NOTE <- paste("n is number in each group, total sample =", k*n, "power =", pwr) 
         METHOD <- "Balanced one-way analysis of variance power calculation"
         structure(list(k=k, n=n, effect.size=f, sig.level=alpha, power = pwr, note = NOTE, method = METHOD), class = "power.htest")
    }
}

