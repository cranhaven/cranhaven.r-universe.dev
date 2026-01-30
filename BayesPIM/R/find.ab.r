find.ab = function(m,s){
  b0 = function(a0,m) a0/m-a0
  obj = function(a,m,s) (a*b0(a,m)/((a+b0(a,m))^2*(a+b0(a,m)+1)) - s^2)^2
  o = optim(0.5, obj, s=s, m=m, method= "Brent", lower=0, upper=1e3)
  a = o$par
  b = b0(a,m)
  ret=list()
  ret$a = a
  ret$b = b
  ret$check.m = (a/(a+b))
  ret$check.s =(sqrt(a*b/((a+b)^2*(a+b+1))))
  ret$conv = o$convergence
  ret$value = o$value
  return(ret)
}
