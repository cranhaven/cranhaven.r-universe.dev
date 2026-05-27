# <R package ACSSpack, providing ACSS, Corresponding ACSS, and GLP Algorithm>
#       Copyright (C) <2024>  <Ziqian Yang>

## adaptively choose burnin in two steps
## use q as the bench mark
## potential problem: should take a smaller window for the cumsum
## change order to the same as GLP
## change beta_inital
## add maj_vot
## add sampleR2 (useful for GLP)

# library(HDCI) ## use HDCI::Lasso,mypredict
# Rcpp::sourceCpp("Normal_cpp.cpp")

Gib_samp_adap_burnin <- function(method,Y,X,paralist,MAX_STEPS,hard_burnin=1000,nmc=5000,q_thresh=0.1,sampleR2=FALSE){
  minburnin=100;
  auxlist=method$aux_fun(X);
  iterpara=method$init_fun(Y,X,paralist);
  SSY=sum((Y-mean(Y))^2);
  
  betamat <- c();
  q <- c();
  lambdasq <- c();
  R_2 <- c();
  ## First-step burnin and first sample
  for(i in 1:(nmc+hard_burnin-minburnin))
  {
    iterpara=method$step_fun(Y,X,paralist,auxlist,iterpara,sampleR2);
    
    betamat=cbind(betamat,iterpara$beta);
    q=c(q,iterpara$rq);
    lambdasq=c(lambdasq,1/(iterpara$y));
    if(sampleR2) R_2=c(R_2,1-sum((iterpara$res)^2)/SSY);
  }
  betamat = betamat[,-(1:(hard_burnin-minburnin))]
  q = q[-(1:(hard_burnin-minburnin))]
  lambdasq = lambdasq[-(1:(hard_burnin-minburnin))]
  if(sampleR2) R_2 = R_2[-(1:(hard_burnin-minburnin))]
  
  ## adaptively choose burnin
  q_sum=c();
  for(STEP in 1:MAX_STEPS){
    betamat = betamat[,-(1:minburnin)]
    q = q[-(1:minburnin)]
    lambdasq = lambdasq[-(1:minburnin)]
    for(i in 1:minburnin)
    {
      iterpara=method$step_fun(Y,X,paralist,auxlist,iterpara,sampleR2);
      
      betamat=cbind(betamat,iterpara$beta);
      q=c(q,iterpara$rq);
      lambdasq=c(lambdasq,1/(iterpara$y));
      if(sampleR2) R_2=c(R_2,1-sum((iterpara$res)^2)/SSY);
    }
    q_cum=(cumsum(q)+sum(q_sum))/(1:length(q)+length(q_sum)*minburnin);
    if((max(q_cum)-min(q_cum))<q_thresh*min(q_cum)) break;
    q_sum = c(q_sum,sum(q[1:minburnin]));
    if(length(q_sum)>50) q_sum = q_sum[-1];
  }
  return(list("betamat" = betamat, "q" = q, "lambdasq" = lambdasq, "Burnin"=STEP+9, "R_2"=R_2));
}


### FUNCTION TO COMPUTE l2 NROM SQUARED OF A VECTOR
normsq <- function(x) return(sum(x*x));
## Add on
GaussHyper=function(a,b,c,z,acc=3){
  n=1
  gap <- 10^{-acc}
  x <- seq(gap,1-gap,by=gap)
  lprob <- (a-1)*log(x) + (b-1)*log(1-x) - (c)*log(1+z*x)
  lprob <- lprob - max(lprob)
  sample <- sample(x,n,replace=TRUE,prob=exp(lprob))
  return(sample)
}


SS_aux=function(X){
  n <- nrow(X); p <- ncol(X);
  XtXdiag <- apply(X,2,normsq);
  return(list("n"=n,"p"=p,"XtXdiag"=XtXdiag));
}
SS_init=function(Y,X,paralist){
  a=paralist$a;b=paralist$b;c=paralist$c;s=paralist$s;
  
  beta <- HDCI::Lasso(X, Y, fix.lambda = FALSE)$beta
  res <- Y - X%*%beta;
  e <- sum(beta!=0);
  ### INITIALIZE x=q/(1-q), y=1/lambda^2 and sigma^2
  rq <- NaN ## runif(1); ## not used
  y <- NaN ## rgamma(1,c,scale=s)*rq/(1-rq);  ## not used
  sigmasq <- c(var(res));
  return(list("beta"=beta,"res"=res,"e"=e,"sigmasq"=sigmasq,"rq"=rq,"y"=y));
}

ACSS_Gibbs_step=function(Y,X,paralist,auxlist,iterpara,sampleR2){
  n=auxlist$n;p=auxlist$p;XtXdiag=auxlist$XtXdiag;
  a=paralist$a;b=paralist$b;c=paralist$c;s=paralist$s;
  beta=iterpara$beta;res=iterpara$res;e=iterpara$e;sigmasq=iterpara$sigmasq;rq=iterpara$rq;y=iterpara$y;
  
  ### UPDATE q
  rq=GaussHyper(3*e/2+a,p-e+b+c,e/2+c,s*sum(beta*beta)/(2*sigmasq)-1);
  ### UPDATE y
  y <- rgamma(1, shape = c+(e/2), rate = (1-rq)/rq/s + sum(beta*beta)/(2*sigmasq));
  ### UPDATE beta
  for(j in 1:p)
  {
    C1 <- XtXdiag[j]+y;
    res <- res + beta[j]*X[,j];
    C2 <- sum(X[,j]*res);
    prob <- rq/(1-rq)*sqrt(y/C1)*exp(C2^2/(2*C1*sigmasq)); 
    prob <- 1/(1+prob);
    u <- runif(1);
    if(u < prob) 
    {beta[j] = 0;}
    else 
    {beta[j] = rnorm(1,C2/C1,sqrt(sigmasq/C1));}
    res <- res - beta[j]*X[,j];
  }
  e <- sum(beta!=0);
  ### UPDATE sigmasq
  sigmasq <- 1/rgamma(1,shape = (n+e)/2, rate = (sum(res*res) + y*sum(beta*beta))/2);
  
  return(list("beta"=beta,"res"=res,"e"=e,"sigmasq"=sigmasq,"rq"=rq,"y"=y));
}

INSS_Gibbs_step=function(Y,X,paralist,auxlist,iterpara,sampleR2){
  n=auxlist$n;p=auxlist$p;XtXdiag=auxlist$XtXdiag;
  a=paralist$a;b=paralist$b;c=paralist$c;s=paralist$s;
  beta=iterpara$beta;res=iterpara$res;e=iterpara$e;sigmasq=iterpara$sigmasq;rq=iterpara$rq;y=iterpara$y;
  
  ### UPDATE r
  rq=rbeta(1,a+e,b+p-e);
  ### UPDATE y
  zc=GaussHyper(e/2+a,b+c,e/2+c,s*sum(beta*beta)/(2*sigmasq)-1);
  y <- rgamma(1, shape = c+(e/2), rate = (1-zc)/zc/s + sum(beta*beta)/(2*sigmasq));
  ### UPDATE beta
  for(j in 1:p)
  {
    C1 <- XtXdiag[j]+y;
    res <- res + beta[j]*X[,j];
    C2 <- sum(X[,j]*res);
    prob <- rq/(1-rq)*sqrt(y/C1)*exp(C2^2/(2*C1*sigmasq)); 
    prob <- 1/(1+prob);
    u <- runif(1);
    if(u < prob) 
    {beta[j] = 0;}
    else 
    {beta[j] = rnorm(1,C2/C1,sqrt(sigmasq/C1));}
    res <- res - beta[j]*X[,j];
  }
  e <- sum(beta!=0);
  ### UPDATE sigmasq
  sigmasq <- 1/rgamma(1,shape = (n+e)/2, rate = (sum(res*res) + y*sum(beta*beta))/2);
  
  return(list("beta"=beta,"res"=res,"e"=e,"sigmasq"=sigmasq,"rq"=rq,"y"=y));
}

GLP_aux=function(X){
  # # Set Start
  # betan <- paste0("X", 1:ncol(X))
  # colnames(X) <- betan
  k <- dim(X)[2]
  Tn <- dim(X)[1]
  
  # Create Objects
  x <- c(seq(0, 0.1, 0.001), seq(0.11, 0.9, 0.01), seq(0.901, 1, 0.001))
  x_c <- x[-length(x)] + (diff(x) / 2)
  xy_c <- expand.grid(x_c, x_c)
  
  grid_q    <- xy_c$Var1
  grid_r2   <- xy_c$Var2
  grid_area <- apply(expand.grid(diff(x), diff(x)), 1, prod)
  grid_lq   <- log(grid_q)
  grid_l1q  <- log(1 - grid_q)
  grid_lr2  <- log(grid_r2)
  grid_l1r2 <- log(1 - grid_r2)
  grid_c1   <- k * grid_q * ((1 - grid_r2) / (2 * grid_r2))
  # grid_pr2q <- rep(0, length(grid_q))
  
  all_pos <- seq_along(grid_q)
  
  # dfr <- tibble(q      = double(S),
  #               r2     = double(S),
  #               z      = rep(list(double(k)), S),
  #               beta   = rep(list(double(k)), S),
  #               sigma2 = double(S))
  return(list("k"=k,"Tn"=Tn,"all_pos"=all_pos,"grid_area"=grid_area,"grid_q"=grid_q,"grid_r2"=grid_r2,
              "grid_lq"=grid_lq,"grid_l1q"=grid_l1q,"grid_lr2"=grid_lr2,"grid_l1r2"=grid_l1r2,"grid_c1"=grid_c1));
}

GLP_init=function(Y,X,paralist){
  lasso.mod <- HDCI::Lasso(X, Y, fix.lambda = FALSE)
  
  beta <- lasso.mod$beta
  sigma2 <- c(var(HDCI::mypredict(lasso.mod, X) - Y))
  z <- as.integer(beta != 0)
  if (sum(z) == 0) { ## to avoid failure when no variable is selected
    z <- rep(1, length(z))
  }
  
  return(list("beta"=beta,"z"=z,"r2"=NaN,"sigma2"=sigma2,"rq"=NaN,"y"=NaN));
}

GLP_Gibbs_step=function(Y,X,paralist,auxlist,iterpara,sampleR2){
  k=auxlist$k;Tn=auxlist$Tn;all_pos=auxlist$all_pos;grid_area=auxlist$grid_area;grid_q=auxlist$grid_q;grid_r2=auxlist$grid_r2;
  grid_lq=auxlist$grid_lq;grid_l1q=auxlist$grid_l1q;grid_lr2=auxlist$grid_lr2;grid_l1r2=auxlist$grid_l1r2;grid_c1=auxlist$grid_c1;
  
  a=paralist$a;b=paralist$b;A=paralist$A;B=paralist$B;
  
  beta=iterpara$beta;z=iterpara$z;sigma2=iterpara$sigma2;
  
  # Sample p(r2, q | Y, beta, sigma2, z)
  cterm <- c(t(beta) %*% diag(z) %*% beta)
  grid_pr2q <- pqr(sigma2, k, grid_lq, grid_l1q, grid_lr2, grid_l1r2, grid_c1, grid_area, cterm, z, a, b, A, B)
  
  pos <- sample(all_pos, 1, prob = grid_pr2q)
  q   <- grid_q[pos]
  r2  <- grid_r2[pos]
  
  # Sample p(z | Y, r2, q)
  #z <- c(refz(z, r2, q, y, X, Tn, k))
  
  for (j in sample(seq_along(z))){
    if (!(sum(z) == 1 & z[j] == 1)){
      zp <- z
      zp[j] <- 1 - zp[j]
      probzp <- pz_nphi(z, zp, r2, q, Y, X, Tn, k)
      z[j] <- sample(c(z[j], zp[j]), 1, prob = c(1, probzp))
    }
  }
  
  # sample p(sigma2 | y, r2, q, z)
  g2 <- (1 / (k * q)) * (r2 / (1 - r2))
  tauz <- sum(z)
  ytil <- Y
  Xtil <- X[, z == 1]
  Wtil <- t(Xtil) %*% Xtil + diag(tauz) / g2
  betatilhat <- solve(Wtil) %*% t(Xtil) %*% ytil
  
  sigma2 <- extraDistr::rinvgamma(1, Tn / 2, (t(ytil) %*% ytil - t(betatilhat) %*% (t(Xtil) %*% Xtil + diag(tauz) / g2) %*% betatilhat) / 2)
  
  # sample p(beta | y, sigma2, r2, q, z)
  beta <- double(k)
  # names(beta) <- betan
  beta[z == 1] <- MASS::mvrnorm(1, betatilhat, sigma2 * solve(t(Xtil) %*% Xtil + diag(tauz) / g2))
  
  res=NaN;
  if(sampleR2) res=Y-X%*%beta;
  
  return(list("beta"=beta,"z"=z,"r2"=r2,"sigma2"=sigma2,"rq"=q,"y"=q*k*(1-r2)/r2,"res"=res));
}


ACSS=list(init_fun=SS_init,aux_fun=SS_aux,step_fun=ACSS_Gibbs_step)
INSS=list(init_fun=SS_init,aux_fun=SS_aux,step_fun=INSS_Gibbs_step)
GLP=list(init_fun=GLP_init,aux_fun=GLP_aux,step_fun=GLP_Gibbs_step)

## add majorty voting function
Maj_Vot=function(reslist,sp_cr)
{
  sparbeta=apply(reslist$betamat,1 ,function(x) mean(x!=0)>sp_cr)
  betahat0=apply(reslist$betamat,1 ,function(x) mean(x[x!=0]))
  betahat=ifelse(sparbeta,betahat0,0)
  return(betahat)
}
