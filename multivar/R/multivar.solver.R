#' @export
multivar.solver <- function(lambda,model,K,d,p,T){
  
  maxiter <- 10000
  tol <- 10^(-12)
  rho <- 1
  N <- T-p
  Xk <- model$mat_Z
  Yk <- model$vec_Y
  
  inv_Xk <- array(0,dim=c((d^2*p),(d^2*p),K))
  XYk <- array(0,dim=c((d^2*p),K))
  for (k in 1:K){
    inv_Xk[,,k] <- solve(1/N*t(Xk[[k]])%*%Xk[[k]] + rho*diag(1,(d^2*p)))
    XYk[,k] <- 1/N*t(Xk[[k]])%*%Yk[[k]]
  }
  
  gamma_k_cur <- gamma_k_next <- array(0,c((d^2*p),K))
  alpha_0_cur <- alpha_0_next <- array(0,(d^2*p))
  alpha_k_cur <- alpha_k_next <- array(0,c((d^2*p),K))
  u_k_cur <- u_k_next <- array(0,c((d^2*p),K))
  
  gx_0 <- vector("numeric",maxiter)
  fx_k <- gx_k <- array(NA,dim=c(K,maxiter))
  func_val <- array(NA,dim=c(K,maxiter))
  r_abs <- r_rel <- array(NA,dim=c(K,maxiter))
  s_abs <- s_rel <- array(NA,dim=c(K,maxiter))
  rel_error <- array(NA,dim=c(K,maxiter))
  
  for (t in 1:maxiter){
    
    tmp_0 <- vector("numeric",(d^2*p))
    for (k in 1:K){
       gamma_k_next[,k] <- inv_Xk[,,k]%*%(XYk[,k] + rho*(alpha_0_cur + alpha_k_cur[,k] -  u_k_cur[,k]))
       
       tmp_k <- gamma_k_next[,k]-alpha_0_cur+u_k_cur[,k]
       alpha_k_next[,k] <- sign(tmp_k)*pmax(tmp_k -lambda[2]/rho,rep(0,(d^2*p)))
       
       tmp_0 <- tmp_0 + gamma_k_next[,k] - alpha_k_next[,k] + u_k_cur[,k]
    }
    alpha_0_next <- sign(tmp_0/K)*pmax(tmp_0/K - lambda[1]/rho,rep(0,(d^2*p)))
    
    for (k in 1:K){
      u_k_next[,k] <- u_k_cur[,k] + gamma_k_next[,k] - alpha_0_next - alpha_k_next[,k]
    }
    
    gx_0[t] <- lambda[1]*sum(abs(alpha_0_next)) 
    for (k in 1:K){
      fx_k[k,t] <- 1/N*norm(Yk[[k]]-Xk[[k]]%*%gamma_k_next[,k],type="2")^2
      gx_k[k,t] <- lambda[2]*sum(abs(alpha_k_next[,k]))
    }
    
    for (k in 1:K){
      r_abs[k,t] <- norm(gamma_k_next[,k] - alpha_k_next[,k] - alpha_0_next,type="2")
      r_rel[k,t] <- r_abs[k,t]/(sqrt(d^2*p)+max(norm(gamma_k_next[,k],type="2"),norm(alpha_k_next[,k]+alpha_0_next,type="2")))
      s_abs[k,t] <- rho*norm(u_k_next[,k]-u_k_cur[,k],type="2")
      s_rel[k,t] <- s_abs[k,t]/(sqrt(d^2*p)+rho*norm(u_k_next[,k],type="2"))
      rel_error[k,t] <- max(r_rel[k,t],s_rel[k,t])
    }
    # print(paste0(t,"th iteration has been completed.",
    #              " maximum relative error is ",max(rel_error[,t])))
    
    if ( max(rel_error[,t]) < tol ){
      #print("Converged")
      output <- list(alpha0 = alpha_0_next, alpha_K = alpha_k_next, 
                     rel_error = rel_error[,1:t], r_rel = r_rel[,1:t], s_rel = s_rel[,1:t])
      return(output)
    }
    
    gamma_k_cur <- gamma_k_next
    alpha_0_cur <- alpha_0_next
    alpha_k_cur <- alpha_k_next
    u_k_cur <- u_k_next
  
  }
  print("Exceed the maximum number of iterations")
  
  output <- list(alpha0 = alpha_0_next, alpha_K = alpha_k_next, 
                 rel_error = rel_error, r_rel = r_rel, s_rel = s_rel)
  return(output)
}