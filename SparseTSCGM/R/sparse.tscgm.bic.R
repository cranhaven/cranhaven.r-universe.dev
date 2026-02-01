
sparse.tscgm.bic <-
         function(penalty=c("scad","lasso"),T=T, n=n, p=p, q=q, xty=xty, xtx=xtx, yty=yty,
            xtxt=xtxt, xtx2=xtx2, yty2=yty2, lam1=lam1, lam2=lam2,
            optimality = c("NULL", "bic","bic_ext","bic_mod","aic","gic"),
            setting=setting)
{       
	###Determining the tuning or penalty parameter
	 lam.vec.1 = lam1
	 lam.vec.2 = lam2

	lamR =length( lam.vec.1)*length(lam.vec.2)
	BICh = matrix(NA,lamR,1)
	lam1h  = matrix(NA,lamR,1)
	lam2h = matrix(NA,lamR,1)
	
  penalty = match.arg(penalty)

	uv <- 0
	for(u in 1:length(lam.vec.1)){
 	 for(v in 1:length(lam.vec.2)){
    	 uv <- uv+1
     	 outscad_s <- compute.sparse.tscgm(penalty=penalty, T=T, n=n, p=p, q=q, xty=xty, xtx=xtx, yty=yty,
          xtxt=xtxt, xtx2=xtx2, yty2=yty2, lam1=lam.vec.1[u], lam2=lam.vec.2[v],
          optimality = "NULL", setting=setting)
        PO = outscad_s$theta
     		PB = outscad_s$gamma
     		

      #  a = as.matrix(PO)
      #  ZeroIndex = which(a==0, arr.ind=TRUE) ## Select the path of zeros
      #  if(dim(ZeroIndex)[1] != 0) { #break("Stop")
      #     WS = (yty - t(xty) %*% PB - t(PB) %*% xty + t(PB) %*% xtx %*% PB)/(n*T)
      #    out4 = glasso(WS, rho = 0, zero = ZeroIndex) ##Compute un-penalized estimastion
      # }
      # else {
        WS = (yty - t(xty) %*% PB - t(PB) %*% xty + t(PB) %*% xtx %*% PB)/(n*T)
     #   out4 = glasso(WS, rho = 0) ##Compute un-penalized estimastion
     # }
      #lik1  = determinant( out4$wi)$modulus[1]
      #lik2 <- sum(diag( out4$wi %*% WS))##min
      lik1  = determinant(PO)$modulus[1]
      lik2 <- sum(diag( PO %*% WS))##min
      ###Number of nonzero entries
     	diag(PO)=0
    	pdO = sum(sum(PO !=0))
    	pdB = sum(sum(PB !=0))

      # pena1 = sum(sum(abs(wt*PO)))
      # pena2 =  sum(sum(abs(wt1*PB)))

      LLk <-  (n*T/2)*(lik1-lik2) #- pena1  -  pena2    #(n*T/2)*
      LLk0 <-  (n*T/2)*(-lik2)  #- pena2

    	###BIC caclulation
    	if(optimality == "bic"){ 
        BICh[uv,1] <- -2*LLk + (log(n*T))*(pdO/2 + q + pdB) 
        }
      else if(optimality == "bic_ext"){
        BICh[uv,1] <- -2*LLk + (log(n*T))*(pdO/2 + q +pdB) + (pdO/2 + q + pdB)*4*0.5*log(q+p)
        }
      else if(optimality == "bic_mod"){
        BICh[uv,1] <- -2*LLk + log(n*T)*(pdO/2 + q +pdB) *log(log(q + p))
        }
      else if(optimality == "aic") {
      	 BICh[uv,1] <- -2*LLk + 2*(pdO/2 + q +pdB)
         }
      else if(optimality == "gic"){
       BICh[uv,1] <- -2*LLk + log(log(n*T))*(pdO/2 + q +pdB) *log(q + p)
        }
    	lam1h[uv,1] <- lam.vec.1[u]
   	  lam2h[uv,1] <- lam.vec.2[v]
   }}
   res.scad <- cbind(lam1h, lam2h, BICh)
   ### Determining the minimum Bic and optimal penalty parameter
   #bicm <- min(res.scad[,3], na.rm = TRUE)
   #   for(rr in 1:lamR){
   #    if(res.scad[rr,3]==bicm){
   #       lam1.opt <- res.scad[rr,1]
   #       lam2.opt <- res.scad[rr,2]
   #    }
   #}
   
   bicid <- which.min(res.scad[,3])
   lam1.opt <- res.scad[bicid,1]
   lam2.opt <- res.scad[bicid,2]
   bicm <-  res.scad[bicid,3]
   
   tmp.out= compute.sparse.tscgm(penalty=penalty,T=T, n=n, p=p, q=q, xty=xty, xtx=xtx, yty=yty,
          xtxt=xtxt, xtx2=xtx2, yty2=yty2, lam1=lam1.opt, lam2=lam2.opt,
          optimality = "NULL", setting=setting)
   best.B = tmp.out$gamma
   best.theta = tmp.out$theta
	   #Level of Sparsity
  d.gamma <- tmp.out$gamma
  diag(d.gamma) <- 0
  s.gamma = sum( abs(d.gamma) > 0 )/(p^2)
  d.theta <- tmp.out$theta
  diag(d.theta) <- 0
  s.theta = (0.5*sum( abs(d.theta) > 0 ))/(0.5*q*(q-1))
  tun.ic <- res.scad
  lam1s <- lam.vec.1
  lam2s <- lam.vec.2
  min.ic <- bicm
  colnames(tun.ic) <- c("Lambda1", "Lambda2","IC")
   return(list(gamma=best.B, theta=best.theta, lam1.opt=lam1.opt, lam2.opt=lam2.opt,
    lam1.seq=lam1s, lam2.seq=lam2s, 
   min.ic=min.ic, tun.ic=tun.ic, s.gamma=s.gamma, s.theta=s.theta))
 }

