dlaplace <- function(dist, g, d, k){
if(dist=="gamma")  factorial(k+d-1)*(1+g/k)^(-k-d)/factorial(k)/k^(d-1)
#else if(dist=="positives") DD(expression(exp(-g^k)), name="g", order=d) # 0<k<=1
#else if(dist=="power") DD(expression(1-(1-exp(-g))^k), name="g", order=d)
#else if(dist=="logarithmic") DD(expression(-log(exp(-g)*(exp(-k)-1)+1)/k), name="g", order=d ) # 0<k<=1
else if(dist=="lognormal") gh(g,d, k)
else stop("Unrecognized frailty distribution")
}