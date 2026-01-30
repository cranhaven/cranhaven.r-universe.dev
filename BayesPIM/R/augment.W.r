augment.W <- function(g, mu_w){
  n = length(g)
  n1 = sum(g)
  n0 = n - n1
  w = numeric(n)
  w[g==0] <- rtruncnorm_inv(n0, mu = mu_w[g == 0], sd = 1, b = 0)
  w[g==1] <- rtruncnorm_inv(n1, mu = mu_w[g == 1], sd = 1, a = 0)
  w
}




