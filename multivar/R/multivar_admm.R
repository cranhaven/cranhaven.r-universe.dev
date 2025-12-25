#' @export
multivar_admm <- function(lambda1,lambda2,Z,Y,k,d,p,n){

  beta <- array(0,dim = c((d[1]),(d[1]*(k + 1) + 1), length(lambda1)*length(lambda2)))
  cnt <- 1
  for(lam1 in lambda1[,1]){
    for(lam2 in lambda2[,1]){ # lam1 <- lambda1[1]; lam2 <- lambda2[1]
  
       fit <- multivar.solver(lambda=c(lam1,lam2),model = list(mat_Z =Z, vec_Y=Y),k,d[[1]],p,n)
       
       #first_col <- seq(from = 1, to = (d[[1]]*k-d[[1]])+1, by = d[[1]])
       #final_col <- first_col + (d[[1]]-1)
       
       mats_unique <- list()
       for(jj in 1:ncol(fit$alpha_K)){
         mats_unique[[jj]] <- t(matrix(fit$alpha_K[,jj],d,d))
       }
       beta[,-1,cnt] <- cbind(t(matrix(fit$alpha0,d,d)), do.call("cbind",mats_unique))
       cnt <- cnt + 1
      
    }
  }
  
  
  return(beta)
}