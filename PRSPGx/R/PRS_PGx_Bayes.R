#' Construct PGx PRS using Bayesian regression
#'
#' Flexibly shrink prognostic and predictive effect sizes simutaneously with glocal-local shrinkage parameters
#' @param PGx_GWAS a numeric list containing PGx GWAS summary statistics (with SNP ID, position, \eqn{\beta}, \eqn{\alpha}, 2-df p-value, MAF and N), SD(Y), and mean(T)
#' @param G_reference a numeric matrix containing the individual-level genotype information from the reference panel (e.g., 1KG)
#' @param n.itr a numeric value indicating the total number of MCMC iteration
#' @param n.burnin a numeric value indicating the number of burn in
#' @param n.gap a numeric value indicating the MCMC gap
#' @param paras a numeric vector containg hyper-parameters (\eqn{v}, \eqn{\phi})
#' @param standardize a logical flag indicating should phenotype and genotype be standardized
#' @details PRS-PGx-Bayes only needs PGx summary statistics and external reference genotype
#' @return A numeric list, the first sublist contains estimated prognostic effect sizes, the second sublist contains estimated predictive effect sizes
#' @references Ge, T., Chen, CY., Ni, Y. et al. Polygenic prediction via Bayesian regression and continuous shrinkage priors. Nat. Commun. 10, 1776 (2019).
#' @references Zhai, S., Zhang, H., Mehrotra, D.V. & Shen, J. Paradigm Shift from Disease PRS to PGx PRS for Drug Response Prediction using PRS-PGx Methods (submitted).
#' @author Song Zhai
#' @export
#' @examples
#' \donttest{
#' data(PRSPGx.example); attach(PRSPGx.example)
#' paras = c(3, 5)
#' coef_est <- PRS_PGx_Bayes(PGx_GWAS, G_reference, paras = paras, n.itr = 10, n.burnin = 5, n.gap = 1)
#' summary(coef_est$coef.G)
#' summary(coef_est$coef.TG)
#' }
#'
PRS_PGx_Bayes <- function(PGx_GWAS, G_reference, n.itr = 1000, n.burnin = 500, n.gap = 10, paras, standardize = TRUE){
  N = PGx_GWAS$G_INFO$N[1]; MAF = PGx_GWAS$G_INFO$MAF
  sd.y = PGx_GWAS$sd.y; meanT = PGx_GWAS$meanT
  varT = meanT*(1-meanT)
  mu.G = 2*MAF; var.G = 2*MAF*(1-MAF); sd.G = sqrt(var.G)
  var.TG = meanT^2*var.G + varT*mu.G^2 + varT*var.G; sd.TG = sqrt(var.TG)

  coef.G <- coef.TG <- double()

  beta_hat <- PGx_GWAS$G_INFO$beta_hat; alpha_hat <- PGx_GWAS$G_INFO$alpha_hat
  if(standardize == TRUE){
    beta_hat <- beta_hat*sd.G/sd.y; alpha_hat <- alpha_hat*sd.TG/sd.y
  }
  b_hat <- c(beta_hat, alpha_hat)

  D <- calculate_D(PGx_GWAS, G_reference)

  mod <- one_block_Bayes(D, b_hat, N, n.itr, n.burnin, n.gap, paras)
  coef.G <- mod$coef.G; coef.TG <- mod$coef.TG

  if(standardize == TRUE){
    coef.G <- coef.G*sd.y/sd.G; coef.TG <- coef.TG*sd.y/sd.TG
  }

  names(coef.G) <- names(coef.TG) <- PGx_GWAS$G_INFO$ID

  re <- list(coef.G = coef.G, coef.TG = coef.TG)
  return(re)
}

calculate_D <- function(PGx_GWAS, G_reference){
  M <- ncol(G_reference); G_reference <- as.matrix(G_reference)
  meanT = PGx_GWAS$meanT; MAF <- PGx_GWAS$G_INFO$MAF
  varT = meanT*(1-meanT)

  lefttop <- cora(G_reference); Sigma <- cova(G_reference)
  righttop <- matrix(NA, ncol = M, nrow = M)
  for (i in 1:M) {
    for (j in 1:M) {
      righttop[i,j] <- meanT*Sigma[i,j]/sqrt(Sigma[i,i]*(meanT^2*Sigma[j,j]+4*MAF[j]^2*varT+Sigma[j,j]*varT))
    }
  }

  leftbottom <- t(righttop)

  rightbottom <- matrix(NA, ncol = M, nrow = M)
  for (i in 1:M) {
    for (j in 1:M) {
      up <- (meanT^2+varT)*Sigma[i,j]+4*varT*MAF[i]*MAF[j]
      down <- sqrt((meanT^2*Sigma[i,i]+4*MAF[i]^2*varT+Sigma[i,i]*varT)*(meanT^2*Sigma[j,j]+4*MAF[j]^2*varT+Sigma[j,j]*varT))
      rightbottom[i,j] <- up/down
    }
  }

  D <- rbind(cbind(lefttop, righttop), cbind(leftbottom, rightbottom))
  return(D)
}

one_block_Bayes <- function(D, b_hat, N, n.itr, n.burnin, n.gap, paras){

  # initialization
  M <- length(b_hat)/2
  b0 <- rep(0, 2*M); sigma20 <- 1
  psi0 <- rep(1,M); xi0 <- rep(1,M); rho0 <- rep(0,M)
  cov0 <- rho0*sqrt(psi0*xi0); Psi0 <- rbind(cbind(diag(psi0),diag(cov0)),cbind(diag(cov0),diag(xi0)))
  delta0 <- rep(1, M); lambda0 <- rep(1, M)
  b1 <- b2 <- 1/2

  # Gibbs sampler
  n.pst <- (n.itr-n.burnin)/n.gap

  b.old <- b0; sigma2.old <- sigma20
  psi.old <- psi0; xi.old <- xi0; rho.old <- rho0; Psi.old <- Psi0
  delta.old <- delta0; lambda.old <- lambda0

  b.mat <- matrix(NA, ncol = n.itr, nrow = 2*M)

  for (i in 1:n.itr) {
    stats <- update.once(b.old = b.old, sigma2.old = sigma2.old, psi.old = psi.old, xi.old = xi.old, rho.old = rho.old, Psi.old = Psi.old, delta.old = delta.old, lambda.old = lambda.old, D = D, b_hat = b_hat, paras = paras, N=N, M=M)

    b.old <- b.mat[,i] <- stats$b.new

    sigma2.old <- stats$sigma2.new
    psi.old <- stats$psi.new; xi.old <- stats$xi.new; rho.old <- stats$rho.new
    Psi.old <- stats$Psi.new

    delta.old <- stats$delta.new; lambda.old <- stats$lambda.new
  }

  b.est <- rep(0,2*M)
  for (i in 1:n.itr) {
    if(i > n.burnin && ((i-n.burnin) %% n.gap == 0)){
      b.est <- b.est + b.mat[,i]/n.pst
    }
  }

  coef.G <- b.est[1:M]; coef.TG <- b.est[(M+1):(2*M)]

  re <- list(coef.G = coef.G, coef.TG = coef.TG, b.mat=b.mat)
  return(re)
}

update.once <- function(b.old, sigma2.old, psi.old, xi.old, rho.old, Psi.old, delta.old, lambda.old, D, b_hat, paras, N, M){

  # extract "paras"
  v <- paras[1]; phi <- paras[2]; b1 = b2 = 1/2

  # update b = (beta, alpha)
  Psi.old.inv <- Psi.Inv(psi.old, xi.old, rho.old, M)
  D.Psi.old.inv <- solve(D+Psi.old.inv, tol=1e-40)

  mu <- D.Psi.old.inv%*%b_hat; mu <- as.vector(mu)
  Sigma0 <- sigma2.old/N*D.Psi.old.inv; Sigma0 <- round(Sigma0, 5)
  if(is.positive.definite(Sigma0) == FALSE){Sigma0 <- nearPD(Sigma0, corr = FALSE); Sigma0 <- as.matrix(Sigma0$mat)}
  Sigma0_chol <- chol(Sigma0)
  b.new <- mu + as.vector(rnorm(length(mu))%*%Sigma0_chol)

  # update sigma2

  input.shape <- M + N/2

  input.scale1 <- N/2*(t(b.new)%*%(D+Psi.old.inv)%*%b.new+1-2*t(b_hat)%*%b.new)
  input.scale1 <- as.vector(input.scale1)
  input.scale2 <- N/2*(t(b.new)%*%Psi.old.inv%*%b.new)
  input.scale2 <- as.vector(input.scale2)
  input.scale0 <- max(input.scale1, input.scale2)

  sigma2.new <- rinvgamma(1, shape = input.shape, scale = input.scale0)

  # update Psi

  beta.new <- b.new[1:M]; alpha.new <- b.new[(M+1):(2*M)]

  psi.new <- double(); xi.new <- double(); rho.new <- double()

  Comb.new <- sapply(1:M, generate.M, N, sigma2.new, beta.new, alpha.new, delta.old, lambda.old, paras)

  psi.new <- Comb.new[1,]; xi.new <- Comb.new[2,]; rho.new <- Comb.new[3,]

  corr <- rho.new*sqrt(psi.new*xi.new)
  Psi.new <- rbind(cbind(diag(psi.new),diag(corr)),cbind(diag(corr),diag(xi.new)))

  # update delta and lambda

  delta.new <- sapply(1:M, generate.delta, psi.new, rho.new, paras)
  lambda.new <- sapply(1:M, generate.lambda, xi.new, rho.new, paras)

  # posterior

  re <- list(b.new = b.new,
             sigma2.new = sigma2.new,
             psi.new = psi.new,
             xi.new = xi.new,
             rho.new = rho.new,
             Psi.new = Psi.new,
             delta.new = delta.new,
             lambda.new = lambda.new)
  return(re)
}

Psi.Inv <- function(psi,xi,rho,n){
  n <- length(psi)
  Psi_inv = matrix(0, ncol=2*n, nrow=2*n)

  rho11 = 1/(psi*(1-rho^2)); rho22 = 1/(xi*(1-rho^2))
  rho12 = -rho/(1-rho^2)/(sqrt(psi)*sqrt(xi))

  Psi_inv[row(Psi_inv)==(col(Psi_inv)-n)] = Psi_inv[col(Psi_inv)==(row(Psi_inv)-n)] = rho12
  diag(Psi_inv)[1:n] = rho11; diag(Psi_inv)[(n+1):(2*n)] = rho22

  return(Psi_inv)
}

generate.delta <- function(i, psi, rho, paras){
  v <- paras[1]; phi <- paras[2]; b1 = b2 = 1/2

  rate <- phi+(2*v)/(psi[i]*(1-rho[i]^2))
  re <- rgamma(1, shape = (v+b1+1/2), rate = rate)
  return(re)
}

generate.lambda <- function(i, xi, rho, paras){
  v <- paras[1]; phi <- paras[2]; b1 = b2 = 1/2

  rate <- phi+(2*v)/(xi[i]*(1-rho[i]^2))
  re <- rgamma(1, shape = (v+b2+1/2), rate = rate)
  return(re)
}

generate.M <- function(j, N, sigma2.new, beta.new, alpha.new, delta.old, lambda.old, paras){
  v <- paras[1]; phi <- paras[2]

  A <- N/sigma2.new*matrix(c(beta.new[j]^2, alpha.new[j]*beta.new[j], alpha.new[j]*beta.new[j], alpha.new[j]^2),ncol=2)
  B <- diag(c(4*v*delta.old[j],4*v*lambda.old[j]))
  S <- A+B; S <- nearPD(S, corr = FALSE); S <- as.matrix(S$mat)

  Mj <- riwish(v = 2*v+2, S = S)

  psi.new <- ifelse(Mj[1,1] > 1/phi, 1/phi, Mj[1,1])
  xi.new <- ifelse(Mj[2,2] > 1/phi, 1/phi, Mj[2,2])
  rho.new <- Mj[1,2]/sqrt(Mj[1,1]*Mj[2,2])

  re <- c(psi.new, xi.new, rho.new); names(re) <- c("psi","xi","rho")
  return(re)
}




