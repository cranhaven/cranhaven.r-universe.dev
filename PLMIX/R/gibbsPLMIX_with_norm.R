#/' gibbsPLMIX_with_norm
#/'
#/' @param pi_inv 
#/' @param K 
#/' @param G 
#/' @param init 
#/' @param n_iter 
#/' @param n_burn 
#/' @param hyper 
#/' @param centered_start 
#/'
#/' @return
#/' @export
#/'
#/' @examples
gibbsPLMIX_with_norm <- function(pi_inv,K,G,
                                 init=list(z=NULL,p=NULL),
                                 n_iter=1000,
                                 n_burn=500,
                                 hyper=list(shape0=matrix(1,nrow=G,ncol=K),rate0=rep(0.001,G),alpha0=rep(1,G)),
                                 centered_start=FALSE){
  
  cl=match.call()
  
  if(class(pi_inv)[1]!="top_ordering"){
    if(class(pi_inv)[1]=="RankData"){
      pi_inv=as.top_ordering(data=pi_inv)
    }
    if(class(pi_inv)[1]=="rankings"){
      pi_inv=as.top_ordering(data=pi_inv)
    }
    if(class(pi_inv)[1]=="matrix" | class(pi_inv)[1]=="data.frame"){
      pi_inv=as.top_ordering(data=pi_inv,format_input="ordering",aggr=FALSE)
    }
  }
  pi_inv <- fill_single_entries(data=pi_inv)
  N <- nrow(pi_inv)
  n_rank <-  howmanyranked(pi_inv)
  rho <- matrix(1:K,nrow=G,ncol=K,byrow=TRUE)
  
  if(is.null(init$z)){
    z <- binary_group_ind(class=sample(1:G,size=N,replace=TRUE),G=G)
  }else{
    z <- init$z
  }
  
  
  omega <- colMeans(z)
  
  if(is.null(init$p)){
    if(centered_start){
      
      print("CENTERED START !!")
      
      # omega <- rdirichlet(1,rep(1,G))
      mle1comp <- matrix(prop.table(table(factor(pi_inv[,1],levels=1:K))),nrow=1)
      p <- random_start(mlesupp=mle1comp, givenweights=omega)
      # p <- p/rowSums(p)
      
    }else{
      
      print("COMPLETELY RANDOM (uniform support, rescaled) START")                    
      
      p <- matrix(rgamma(n=G*K,shape=1,rate=1),nrow=G,ncol=K)
      
    }
    
  }else{
    
    p <- init$p
  }
  
  shape0 <- hyper$shape0
  rate0 <- hyper$rate0
  alpha0 <- hyper$alpha0
  
  u_bin <- umat(pi_inv=pi_inv)
  
  
  log_lik <- c(loglikPLMIX(p=p,ref_order=rho,weights=omega,pi_inv=pi_inv),
               rep(NA,n_iter))
  
  log_prior <- c(log(ddirichlet(omega,alpha0))+sum(dgamma(p,shape=shape0,rate=rate0,log=TRUE)),
                 rep(NA,n_iter))
  
  objective <- log_lik+log_prior
  
  Pi <- array(NA,dim=c(G,K,n_iter+1))
  Pi[,,1] <- p
  Zeta <- z
  Omega <- matrix(NA,nrow=n_iter+1,ncol=G)
  Omega[1,] <- omega
  
  
  for(l in 1:n_iter){
#    print(l)
    
    if(l%%500==0){
      print(paste("GIBBS iteration",l))
    }
    
    Omega[l+1,] <- rdirichlet(n=1,alpha=alpha0+colSums(Zeta))
    
    temprate <- CompRateYpartial(p=adrop(Pi[,,l,drop=FALSE],3),pi_inv=pi_inv,ref_order=rho,z=Zeta,n_rank=n_rank)
    Ypsilon <- SimYpsilon(rate=temprate,n_rank=n_rank)
    
    Pi[,,l+1] <- matrix(rgamma(n=G*K,shape=shape0+gammamat(u_bin=u_bin,z_hat=Zeta),
                               rate <- CompRateP(pi_inv=pi_inv, Y=Ypsilon, z=Zeta, u_bin=u_bin, n_rank=n_rank, rate0=rate0)),nrow=G,ncol=K)
    tempPi <- adrop(Pi[,,l+1,drop=FALSE],drop=3)
    Pi[,,l+1] <- tempPi/rowSums(tempPi)
    
    Zeta <- binary_group_ind(apply(CompProbZpartial(p=adrop(Pi[,,l+1,drop=FALSE],3),pi_inv=pi_inv,Y=Ypsilon, u_bin=u_bin,n_rank,omega=Omega[l+1,]),1,FUN=sample,x=1:G,replace=TRUE,size=1),G=G)
    
    log_lik[l+1] <- loglikPLMIX(p=adrop(Pi[,,l+1,drop=FALSE],3),ref_order=rho,weights=Omega[l+1,],
                                pi_inv=pi_inv)
    
    log_prior[l+1] <- log(ddirichlet(Omega[l+1,],alpha0))+sum(dgamma(adrop(Pi[,,l+1,drop=FALSE],3),shape=shape0,rate=rate0,log=TRUE))
    
    objective[l+1] <- log_lik[l+1]+log_prior[l+1]
    
  }
  
  log_lik <- log_lik[-c(1:(n_burn+1))]
  
  objective <- objective[-c(1:(n_burn+1))]
  
  Omega <- Omega[-c(1:(n_burn+1)),,drop=FALSE]
  colnames(Omega) <- paste0("w_",1:G)
  
  
  Pi <- array(apply(Pi,3,FUN=function(x)x/rowSums(x)),c(G,K,n_iter+1))	
  
  Pi=t(apply(Pi,3,c))[-c(1:(n_burn+1)),]
  colnames(Pi) <- paste0("p_",rep(1:G,K),rep(1:K,each=G))
  
  out=list(W=Omega,P=Pi,log_lik=log_lik,deviance=-2*log_lik,objective=objective,call=cl)
  
  class(out)="gsPLMIX"
  
  return(out)
  
}
