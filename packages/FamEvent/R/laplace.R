laplace <- function(dist, g, k){
if(dist=="gamma")  (1+g/k)^(-k)
else if(dist=="positives") exp(-g^k)
else if(dist=="powerseries") 1-(1-exp(-g))^k
else if(dist=="logarithmic") -log(exp(-g)*(exp(-k)-1)+1)/k
else if(dist=="lognormal") gh(g, 0, k)
else stop("Unrecognized frailty distribution")
}


