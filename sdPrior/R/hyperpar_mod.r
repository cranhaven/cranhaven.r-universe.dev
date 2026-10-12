#' Find Scale Parameter for modular regression



#' 

#' @param Z rows from the tensor product design matrix
#' @param K1 precision matrix1
#' @param K2 precision matrix2
#' @param A constraint matrix
#' @param c  threshold from eq. (8) in Klein & Kneib (2016)
#' @param alpha  probability parameter from eq. (8) in Klein & Kneib (2016)
#' @param omegaseq  sequence of weights for the anisotropy
#' @param omegaprob  prior probabilities for the weights
#' @param R  number of simulations
#' @param myseed  seed in case of simulation. default is 123.
#' @param thetaseq  possible sequence of thetas. default is NULL.
#' @param type  type of hyperprior for tau/tau^2; options: IG => IG(1,theta) for tau^2, SD => WE(0.5,theta) for tau^2, HN => HN(0,theta) for tau, U => U(0,theta) for tau, HC => HC(0,theta) for tau
#' @param lowrank default is FALSE. If TRUE a low rank approximation is used for Z with k columns.
#' @param k only used if lowrank=TRUE. specifies target rank of low rank approximation. Default is 5.
#' @param mc default is FALSE. only works im thetaseq is supplied. can parallel across thetaseq.
#' @param ncores default is 1. number of cores is mc=TRUE
#' @param truncate default is 1. If < 1 the lowrank approximation is based on on cumsum(values)/sum(values).
#' @return the optimal value for theta
#' @author Nadja Klein
#' @references Kneib, T., Klein, N., Lang, S. and Umlauf, N. (2017) Modular Regression - A Lego System for Building Structured Additive Distributional Regression Models with Tensor Product Interactions 
#' \emph{Working Paper}.
#' 
#'
#' @import MASS
#' @import mgcv
#' @import mvtnorm
#' @import doParallel
#' @import parallel
#' @export


hyperpar_mod <- function(Z, K1, K2, A, c=0.1, alpha=0.1,
                     omegaseq, omegaprob, R=10000, myseed=123,
                     thetaseq = NULL, type="IG",lowrank=FALSE,k=5,mc=FALSE,ncores=1,truncate=1)
  {
  # require("mvtnorm")
  # require("MASS")
	# require("mgcv")
  L1=dim(K1)[2]
	L2=dim(K2)[2]
	if(is.null(dim(K1)[2])){L1=1}
	if(is.null(dim(K2)[2])){L2=1}
	I1 <- diag(L1)
	I2=diag(L2)
	
		Zu <- uniquecombs(Z)
		ind <- attr(Zu,"index")
		Zorig=Z
		Z=Zu
		counts=as.data.frame(table(ind))
		counts=counts$Freq

	
	if(!is.null(K2))
		{
	
		if(type=="IG")
			{	
			
			Adim=dim(A)
			tAA=t(A)%*%A
			s=svd(((tAA) + t(tAA))/2)
			bA <- t(s$u[,round(s$d, 10)==0])
			iAbA=solve(rbind(A,bA))
			C=(iAbA[,1:Adim[1]])
			bC=(iAbA[,(Adim[1]+1):Adim[2]])
			Zunique=Z[!duplicated(Z,MARGIN=1),]
			bZ<-Zunique%*%bC
			
			nZ=dim(Z)[1]
			ztz=list()
			# K=list();
			Kinv=list();
			for(r in 1:length(omegaseq))
				{

				K <- t(bC)%*%(omegaseq[r]*(K1 %x% I2) + (1-omegaseq[r])*(I1 %x% K2))%*%bC

				Kinv <- ginv(K) # +omegaprob[r]*Kinv
				ztz[[r]]=diag(bZ%*%Kinv%*%t(bZ))
				}
				# ztz=diag(bZ%*%Kinv%*%t(bZ))
				
			marginal_df <- function(lambda, ztz,alpha,c)
				{
				df <- 2 
				mu <- 0
				res=0;
				for(r in 1:length(omegaseq))
					{
					sigma <- sqrt(ztz[[r]]*(lambda))
					res <- res+omegaprob[r]*sapply(sigma, FUN=function(x){pt((c-mu) / x, df = df) - pt((-c-mu) / x, df = df)} )
					}
				res <- sum(res*counts) - nZ + alpha
				return(res)
				}
			cat("Optimising theta\n")

			(bopt <- uniroot(f = marginal_df, interval = c(0.000001, upper = 100), 
											 ztz=ztz,alpha=alpha,c = c))
			res=bopt$root
			} else {
			


				simsup <- function(theta)
					{
					set.seed(myseed)

					tau2sample <- rep(0,R)
					if(type=="U")
						tau2sample <- runif(0,theta)
					else if(type=="SD")
						tau2sample <- rweibull(R, shape = 0.5, scale = theta)
					else if(type=="HN")
						tau2sample <- (rnorm(R, mean=0, sd=sqrt(theta)))^2
					else if (type=="HC")
						tau2sample <- (rgb2(R, shape1=1, scale=theta^2, shape2=0.5, shape3=0.5))^2
					else
						stop("please specify type in U,IG,HN,SD,HC")
					
					s <- list()
					m <- list()
					omegasample <- sample(1:length(omegaseq), size=R, replace=TRUE, prob=omegaprob)
					if(truncate<1)
						{
						Kinv <- list()
						multmat <- list()
						tr <- list()

						for(r in 1:length(omegaseq))
							{
							cat("r (prep.): ", r, "\n")
							K <- (omegaseq[r]*(K1 %x% I2) + (1-omegaseq[r])*(I1 %x% K2))
							Kinv[[r]] <- ginv((t(K)+K)/2)
							e <- eigen(Kinv[[r]], symmetric=TRUE)
							ecum <- cumsum(e$values)
							tr[[r]] <- length(which(ecum/sum(e$values)<truncate))
							multmat[[r]] <- e$vectors[,1:tr[[r]]]%*%diag(sqrt(e$values[1:tr[[r]]]))
							}
						supsample <- rep(0,R)

						for(r in 1:length(omegaseq))
							{
							cat("r (sim.): ", r, "\n")
							ind <- which(omegasample==r)

							betasample <- multmat[[r]]%*%matrix(rnorm(length(ind)*tr[[r]]), nrow=tr[[r]], ncol=length(ind))

							# correct for constraint
							V <- Kinv[[r]]%*%t(A)
							W <- A%*%V
							U <- ginv(W)%*%t(V)
							betasample <- betasample - t(U)%*%A%*%betasample

							fsample <- Z%*%betasample
							supsample[ind] <- apply(abs(fsample), 2, max)
							}	
						} else {
						for(r in 1:length(omegaseq))
							{
							ind1 <- which(omegasample==r)

							K <- (omegaseq[r]*(K1 %x% I2) + (1-omegaseq[r])*(I1 %x% K2))

							# determine bar{A1}
							e <- eigen(t(A)%*%A)
							ind2 <- which(round(e$values, 10)==0)
							Abar <- t(e$vectors[,ind2])
							C <- solve(rbind(A, Abar))
							C2 <- C[,-(1:nrow(A))]

							# determine new precision and design matrix
							Knew <- t(C2)%*%K%*%C2
							Znew <- Z%*%C2
							

							Kinv <- ginv((t(Knew)+Knew)/2)
							d <- dim(Kinv)[1]
							s[[r]] <- svd((Kinv + t(Kinv))/2)
							s[[r]]$d <- abs(zapsmall(s[[r]]$d))
							m[[r]] <- sum(s[[r]]$d > 0)
							s[[r]]$d <- s[[r]]$d[1:m[[r]]]
							s[[r]]$u <-s[[r]]$u[,1:m[[r]]]
							if(lowrank)
								{
								s[[r]]$d <- s[[r]]$d[1:k]
								s[[r]]$u <- s[[r]]$u[,1:k]
								m[[r]] <- k
								}
							}
						supsample <- rep(0,R)

						for(r in 1:length(omegaseq))
							{
							ind1 <- which(omegasample==r)
							
							x <- matrix(rnorm(length(ind1)*m[[r]]), ncol=m[[r]])
							betasample <- (apply(x,FUN=function(x){s[[r]]$u %*% diag(sqrt(s[[r]]$d))%*%matrix(x,ncol=1)},MARGIN=1))
		#					betasample <- t(rmvnorm(length(ind1), mean=rep(0, ncol(Znew)), sigma=Kinv))

							fsample <- Znew%*%betasample
							supsample[ind1] <- apply(abs(fsample), 2, max)
							}
						}
					supsample <- sqrt(tau2sample)*supsample

					return(supsample)
					}

				if(is.null(thetaseq))
					{
					optfn1 <- function(theta)
						{
						quantile(simsup(theta=theta), probs=alpha) - c
						}
					bopt <- uniroot(f=optfn1, interval=c(1e-8,upper=1e8), trace=1)
					bopt <- bopt$root
					res <- bopt
					}
				else
					{
					if(mc)
						{
						func <- function(theta)
							{
							try(res <- quantile(simsup(theta=thetaseq[theta]), probs=alpha) - c)
							res
							return(res)
							}
						tests <- mclapply(1:length(thetaseq),FUN=func,mc.cores=ncores)
						res <- unlist(tests)
						} else {
						res <- rep(0, length(thetaseq))
						for(theta in 1:length(thetaseq))
							{
							cat("theta: ", thetaseq[theta], "\n")
							res[theta] <- quantile(simsup(theta=thetaseq[theta]), probs=alpha) - c
							}
						}
				}
			}
		} else {
		
		if(type=="IG")
			{	
			Kinv=ginv(K1)
			ztz=diag(Z%*%Kinv%*%t(Z))
			nZ=dim(Z)[1]
			marginal_df <- function(lambda, ztz,alpha,c)
				{
				df <- 2 
				mu <- 0
				sigma <- sqrt(ztz*(lambda))
				res <- sapply(sigma, FUN=function(x){pt((c-mu) / x, df = df) - pt((-c-mu) / x, df = df)} )		
				res <- sum(res*counts) - nZ + alpha
				return(res)
				}
			cat("Optimising theta\n")

			(bopt <- uniroot(f = marginal_df, interval = c(0.000001, upper = 100), 
											 ztz=ztz,alpha=alpha,c = c))
			res=bopt$root
			} else {

			simsup <- function(theta)
				{
				set.seed(myseed)

				tau2sample <- rep(0,R)
				if(type=="U")
					tau2sample <- runif(0,theta)
				else if(type=="SD")
					tau2sample <- rweibull(R, shape = 0.5, scale = theta)
				else if(type=="HN")
					tau2sample <- (rnorm(R, mean=0, sd=sqrt(theta)))^2
				else if (type=="HC")
					tau2sample <- (rgb2(R, shape1=1, scale=theta^2, shape2=0.5, shape3=0.5))^2
				else
					stop("please specify type in U,IG,HN,SD,HC")

				Kinv <- ginv(K1)
					
				d <- dim(Kinv)[1]
				s <- svd((Kinv + t(Kinv))/2)
				s$d <- abs(zapsmall(s$d))
				m <- sum(s$d > 0)
				s$d <- s$d[1:m]
				s$u <-s$u[,1:m]
				if(lowrank)
					{
					s$d <- s$d[1:k]
					s$u <- s$u[,1:k]
					m <- k
					}
				
				x <- matrix(rnorm(m*R), nrow=m)
				# x <- rbind(x, matrix(0, nrow=d-m, ncol=R))
				betasample <- s$u %*% diag(sqrt(s$d)) %*% x
				fsample <- t(Z %*% betasample)
				
				fsample <-fsample * sqrt(tau2sample)
				fsample <- apply(abs(fsample), 2, max)
				return(fsample)
				}

			if(is.null(thetaseq))
				{
				optfn1 <- function(theta)
					{
					quantile(simsup(theta=theta), probs=alpha) - c
					}
				bopt <- uniroot(f=optfn1, interval=c(1e-8,upper=1e8), trace=1)
				bopt <- bopt$root
				res <- bopt
				} else{
				if(mc)
					{
					func <- function(theta)
						{
						try(res <- quantile(simsup(theta=thetaseq[theta]), probs=alpha) - c)
						res
						return(res)
						}
					tests <- mclapply(1:length(thetaseq),FUN=func,mc.cores=ncores)
					res <- unlist(tests)
					} else {
					res <- rep(0, length(thetaseq))
					for(theta in 1:length(thetaseq))
						{
						cat("theta: ", thetaseq[theta], "\n")
						res[theta] <- quantile(simsup(theta=thetaseq[theta]), probs=alpha) - c
						}
					}
				}
			}
		
		
		}
  return(res)
  }

