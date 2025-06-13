# funcao de verossimilhanca

lv_pn <- function(A,x){
  E <- 0; S <- 1;
  n <- length(x) ; z <- (x-E)/S
  n*(log(A)-log(S)-.5*log(2*pi))-.5*sum(z^2)+(A-1)*sum(log(pnorm(z)))
}



# funcao de densidade dpn
pn.d <- function(x, alpha ){
  sigma<- 1
  mu <- 0
  (alpha/sigma)*dnorm((x-mu)/sigma,0,1)*(pnorm((x-mu)/sigma,0,1))^(alpha-1)
}

