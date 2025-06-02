draw_lm_cov_latent_cont <- function(X1,X2,param="multilogit",Mu,Si,Be,Ga,fort=TRUE){

# Draw a sample from LM model with covariates
# X1    = design matrix for the initial probabilities (n x n.cov.)
# X2    = design matrix for the initial probabilities (n x TT-1 x n.cov.)
# param = type of parametrization for the transition probabilities:
#         multilogit = standard multinomial logit for every row of the transition matrix
#         difflogit  = multinomial logit based on the difference between two sets of parameters
# Mu = matrix of means (r x k)
# Si = common variance-covariance matrix
# Be = parameters on the initial probabilities (if start=2)
# Ga = parameters on the transition probabilities (if start=2)
# fort  = fortran use (FALSE for not use fortran)

  warning("draw_lm_cov_latent_cont function is no longer maintained. Please look at drawLMlatentcont function",call. = FALSE)


# Preliminaries
  	n = nrow(X2)
  	TT = dim(X2)[2]+1
    if(is.vector(Mu)){
    	r = 1
    	k = length(Mu)
    	Mu = matrix(Mu,r,k)
    Si = matrix(Si,r,r)
    }else{
    	r = nrow(Mu)
    	k = ncol(Mu)
    }

# Covariate structure and related matrices: initial probabilities
	if(is.vector(X1)) X1 = matrix(X1,n,1)
	nc1 = dim(X1)[2] # number of covariates on the initial probabilities
	if(k == 2){
		GBe = as.matrix(c(0,1))
	}else{
		GBe = diag(k); GBe = GBe[,-1]
	}
	out = aggr_data(X1)
	Xdis = out$data_dis
	if(nc1==1) Xdis = matrix(Xdis,length(Xdis),1)
	Xlab = out$label
	Xndis = max(Xlab)
	XXdis = array(0,c(k,(k-1)*(nc1+1),Xndis))
	for(i in 1:Xndis){
		xdis = c(1,Xdis[i,])
		XXdis[,,i] = GBe%*%(diag(k-1)%x%t(xdis))
	}

# for the transition probabilities
	if(is.matrix(X2)) X2 = array(X2,c(n,TT-1,1))
	nc2 = dim(X2)[3] # number of covariates on the transition probabilities
	Z = NULL
	for(t in 1:(TT-1)) Z = rbind(Z,X2[,t,])
	if(nc2==1) Z = as.vector(X2)
	out = aggr_data(Z); Zdis = out$data_dis; Zlab = out$label; Zndis = max(Zlab)
	if(param=="multilogit"){
		ZZdis = array(0,c(k,(k-1)*(nc2+1),Zndis,k))
		for(h in 1:k){
			if(k==2){
				if(h == 1) GGa = as.matrix(c(0,1)) else GGa = as.matrix(c(1,0))
			}else{
				GGa = diag(k); GGa = GGa[,-h]
			}
			for(i in 1:Zndis){
				zdis = c(1,Zdis[i,])
				ZZdis[,,i,h] = GGa%*%(diag(k-1)%x%t(zdis))
			}
		}
	}else if(param=="difflogit"){
		Zlab = (((Zlab-1)*k)%x%rep(1,k))+rep(1,n*(TT-1))%x%(1:k)
		ZZdis = array(0,c(k,k*(k-1)+(k-1)*nc2,Zndis*k))
		j = 0
		for(i in 1:Zndis){
			for(h in 1:k){
				j = j+1
				if(k==2){
					if(h == 1) GGa = as.matrix(c(0,1)) else GGa = as.matrix(c(1,0))
				}else{
					GGa = diag(k); GGa = GGa[,-h]
				}
				u = matrix(0,1,k); u[1,h] = 1
				U = diag(k); U[,h] = U[,h]-1
				U = U[,-1]
				ZZdis[,,j] = cbind(u%x%GGa,U%x%t(Zdis[i,]))
			}
		}
	}

# Draw data
	Y = array(0,c(n,TT,r))
	U = matrix(0,n,TT)

# first time occasion
    be = as.vector(Be)
    out = prob_multilogit(XXdis,be,Xlab,fort)
    Piv = out$P
    for(i in 1:n) U[i,1] = which(rmultinom(1,1,Piv[i,])==1)

# following time occasions
    if(param=="multilogit"){
    	  if(is.list(Ga)) stop("invalid mode (list) for Ga")
      Ga = matrix(Ga,(nc2+1)*(k-1),k)
      PIdis = array(0,c(Zndis,k,k)); PI = array(0,c(k,k,n,TT))
      for(h in 1:k){
        out = prob_multilogit(ZZdis[,,,h],Ga[,h],Zlab,fort)
        PI[h,,,2:TT] = array(as.vector(t(out$P)),c(1,k,n,TT-1))
      }
    }else if(param=="difflogit"){
    	  if(is.list(Ga)) Ga = c(as.vector(t(Ga[[1]])),as.vector(Ga[[2]]))
      if(length(Ga)!=k*(k-1)+(k-1)*nc2) stop("invalid dimensions for Ga")
      PI = array(0,c(k,k,n,TT))
      out = prob_multilogit(ZZdis,Ga,Zlab,fort)
      Tmp = array(out$P,c(k,n,TT-1,k))
	  PI[,,,2:TT] = aperm(Tmp,c(1,4,2,3))
    }
    for(i in 1:n) for(t in 2:TT){
      U[i,t] = which(rmultinom(1,1,PI[U[i,t-1],,i,t])==1)
    }

# draw response variables
    for(i in 1:n) for(t in 1:TT) Y[i,t,] = rmvnorm(1,Mu[,U[i,t]],Si)

# output
  if(r==1) Y = matrix(Y,n,TT)
  out = list(U=U,Y=Y)

}


