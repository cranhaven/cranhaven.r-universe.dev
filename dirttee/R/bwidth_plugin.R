#' @importFrom stats dnorm

bwidth_plugin <-
function(residuals, X){

  if(!is.numeric(residuals) || !is.numeric(X)) stop("residuals and X have to be numeric")
  if(!is.matrix(X)) stop("X has to be a matrix")
  if(length(residuals) != nrow(X)) stop("length of residuals does not match the row length of X")
  
  n <- length(residuals)
  
  dens <- KDE(residuals, from = min(residuals), to = max(residuals), adaptive = FALSE, n = 10000)

  bw_inner <- dens$bw

  m <- dens$x[which.max(dens$y)]

  g0 <- 1 / (n * bw_inner) * sum(dnorm((residuals - m) / bw_inner))
  
  L <- 1 / n * g0 * t(X) %*% X
  
  g3 <- 1 / (n * bw_inner ^ 4) * sum(norm3((residuals - m) / bw_inner))
  
  K <- 1 / n * g3 * matrix(colSums(X), ncol = 1)
  
  v <- integrate(function(x) x ^ 2 * dnorm(x) ^ 2, -Inf, Inf)$value

  en <- (3 * v * ncol(X)) ^ (1 / 7)
  dn <- (t(K) %*% solve(L) %*% K) ^ (1 / 7)
  
  c("bw" = as.numeric(en / dn * n ^ (- 1 / 7)))
}
