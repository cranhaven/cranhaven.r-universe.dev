#' @keywords internal
bcgam.fit <- function(y=y, xmat=xmat, shapes=shapes, zmat=zmat, nums=nums,
ks=ks, sps=sps, nloop=nloop, burnin=burnin, family=family, 
xmatnms=xmatnms, znms=znms)
{

n=length(y)

	if(is.null(zmat)==F){
		XZ=cbind(xmat,zmat)
		XZ_one=cbind(rep(1,n),XZ)
		X_one=cbind(rep(1,n),xmat)
		Z_one=cbind(rep(1,n),zmat)
	
	err_xz=tryCatch(solve(t(XZ)%*%XZ), error=function(e) NULL)
	err_xz_one=tryCatch(solve(t(XZ_one)%*%XZ_one), error=function (e) NULL)
	err_z_one=tryCatch(solve(t(Z_one)%*%Z_one), error=function (e) NULL)
	err_x_one=tryCatch(solve(t(X_one)%*%X_one), error=function (e) NULL)
	ind_z_one=1

	if(is.null(err_xz)==T){ stop("cols of X and Z are not linearly independent")}
	if(is.null(err_xz_one)==T){
		if(is.null(err_x_one)==T|is.null(err_z_one)==T){
			if(is.null(err_x_one)==T){stop("unitary vector is in the space spanned by cols of X")}else{ind_z_one=0} }
			else{stop("unitary vector is in the space spanned by cols of X and Z")}
		}
	}
	else{
		X_one=cbind(rep(1,n),xmat)
		
	err_x_one=tryCatch(solve(t(X_one)%*%X_one), error=function (e) NULL)
	ind_z_one=1

	if(is.null(err_x_one)==T){ stop("unitary vector is in the space spanned by cols of X")}
	}


	v=1:n*0+1	
	nz=length(zmat)/n
	if(nz!=round(nz)){nz=0}            
	if(nz==1){zmat=matrix(zmat,ncol=1)}

	
	xs<-NULL  
	L=length(xmat)/n
	tt<-vector("list",L)  
	center.delta<-vector("list",L) 
	delta<-NULL 
	s<-NULL 
	m<-NULL 
	vx<-NULL  
	sumvx<-NULL 
	incr<-NULL
	decr<-NULL
	pos_con<-NULL  
	zmatb<-NULL  
	znmsb<-NULL 	

for(i in 1:L){

		xs<-unique(sort(xmat[,i])) 
		n1<-length(xs)

if(length(ks[[i]])>1){
		tt[[i]]=cbind(ks[[i]])}


else{ if(nums[i]==0){k=trunc(4+n1^(1/7))} else{ if(nums[i]>0){k=nums[i]}  }
	if(sps[i]=="Q"){
		tt[[i]]<-quantile(xs, probs=seq(0,1,length=k))
		}
	else{if(sps[i]=="E"){
		xs=xmat[,i] 
		tt[[i]]=cbind(min(xs)+(max(xs)-min(xs))*(0:(k-1))/(k-1))	
				}}
	}
			

	if(shapes[i]==1){	delta.i=monincr(xmat[,i],tt[[i]])
				delta=rbind(delta,delta.i$sigma-delta.i$center.vector)
				m=c(m,length(delta.i$sigma)/n)
				center.delta[[i]]=delta.i$center.vector}

	if(shapes[i]==2){	delta.i=mondecr(xmat[,i],tt[[i]])
				delta=rbind(delta,delta.i$sigma-delta.i$center.vector) 
				m=c(m,length(delta.i$sigma)/n)
				center.delta[[i]]=delta.i$center.vector}

	if(shapes[i]==3){	delta.i=convex(xmat[,i],tt[[i]], pred.new=FALSE)
				delta=rbind( delta,delta.i$sigma-t(delta.i$x.mat%*%delta.i$center.vector) )  
				m=c(m,length(delta.i$sigma)/n)
				center.delta[[i]]=delta.i$center.vector}

	if(shapes[i]==4){	delta.i=concave(xmat[,i],tt[[i]], pred.new=FALSE)
				delta=rbind( delta,delta.i$sigma-t(delta.i$x.mat%*%delta.i$center.vector) ) 
				m=c(m,length(delta.i$sigma)/n)
				center.delta[[i]]=delta.i$center.vector}

	if(shapes[i]==5){	delta.i=incconvex(xmat[,i],tt[[i]])
				delta=rbind(delta,delta.i$sigma-delta.i$center.vector) 
				m=c(m,length(delta.i$sigma)/n)
				center.delta[[i]]=delta.i$center.vector}

	if(shapes[i]==7){	delta.i=incconcave(xmat[,i],tt[[i]])
				delta=rbind(delta,delta.i$sigma-delta.i$center.vector) 
				m=c(m,length(delta.i$sigma)/n)
				center.delta[[i]]=delta.i$center.vector}

	if(shapes[i]==6){	delta.i=decconvex(xmat[,i],tt[[i]])
				delta=rbind(delta,delta.i$sigma-delta.i$center.vector)
				m=c(m,length(delta.i$sigma)/n)
				center.delta[[i]]=delta.i$center.vector}

	if(shapes[i]==8){	delta.i=decconcave(xmat[,i],tt[[i]])
				delta=rbind(delta,delta.i$sigma-delta.i$center.vector) 
				m=c(m,length(delta.i$sigma)/n)
				center.delta[[i]]=delta.i$center.vector}

	if(shapes[i]==1|shapes[i]==5|shapes[i]==7){incr=c(incr,1)}else{incr=c(incr,0)}
	if(shapes[i]==2|shapes[i]==6|shapes[i]==8){decr=c(decr,1)}else{decr=c(decr,0)}


	vx=cbind(vx,xmat[,i])

	if(incr[i]==0 & decr[i]==0){
		pos_con=cbind(pos_con,i)
		zmatb=cbind(zmatb,vx[,i])
		znmsb=c(znmsb, xmatnms[i])
	}  	
	
	}


	if(ind_z_one==1){
		zmatb=cbind(v,zmat,zmatb)
		znmsb=c("(Intercept)",znms, znmsb)}
	else{zmatb=cbind(zmat,zmatb)
		znmsb=c(znms, znmsb)}

	p=length(zmatb)/n

	betalpha<-NULL
	M<-NULL
	P<-NULL
	deltaz<-NULL
	tau<-NULL


	if(family$family=="gaussian"){
	gausCode <- nimbleCode({
		for (j in 1:n){
		y[j] ~ dnorm(eta[j],tau)	
		eta[j]<-inprod(betalpha[1:(M+P)],deltaz[j,1:(M+P)])
		}

		tau ~ dgamma(0.1,0.1)		
		sigma <- 1/sqrt(tau)		
		
		for(m in 1:M){
		betalpha[m] ~ dgamma(0.01,0.01) }

		for(k in (M+1):(M+P)){
		betalpha[k] ~ dnorm(0,1.0E-4) }
		})
	
		delta.dat=t(delta)
		z.dat=zmatb
		deltaz.dat=cbind(delta.dat,z.dat)

	gausConsts <- list(n=n,M=sum(m), P=p, deltaz=deltaz.dat)

	gausData <- list(y=y)		
	
		beta.ini=rep(0.1,sum(m))
		alpha.ini=rep(1,p)
		betalpha.ini=c(beta.ini,alpha.ini)
		tau.ini=0.1

	gausInits <- list( betalpha=betalpha.ini,tau=tau.ini )

	gaus <- nimbleModel(code = gausCode, name = 'gaus', constants = gausConsts,
                    data = gausData, inits = gausInits)

	gausMonitor <- configureMCMC(gaus)
	gausMonitor$addMonitors(c('betalpha','sigma','eta'))
	gausMCMC <- buildMCMC(gausMonitor)

	Cgaus <- compileNimble(gaus)
	CgausMCMC <- compileNimble(gausMCMC, project = gaus)

	CgausMCMC$run(nloop)

	MCMCsamples <- as.matrix(CgausMCMC$mvSamples)

	 }

	else{ if(family$family=="binomial"){
		binoCode <- nimbleCode({
		for (j in 1:n){
		y[j]~dbern(p[j])
		p[j] <- expit(eta[j])
		eta[j] <- inprod(betalpha[1:(M+P)],deltaz[j,1:(M+P)])
		}

		for(m in 1:M){
		betalpha[m] ~ dgamma(0.01,0.01) }

		for(k in (M+1):(M+P)){
		betalpha[k] ~ dnorm(0,1.0E-4) }
		})
	
		delta.dat=t(delta)
		z.dat=zmatb
		deltaz.dat=cbind(delta.dat,z.dat)

	binoConsts <- list(n=n,M=sum(m), P=p, deltaz=deltaz.dat)

	binoData <- list(y=y)		
	
		beta.ini=rep(0.1,sum(m))
		alpha.ini=rep(1,p)
		betalpha.ini=c(beta.ini,alpha.ini)

	binoInits <- list( betalpha=betalpha.ini)

	bino <- nimbleModel(code = binoCode, name = 'bino', constants = binoConsts,
                    data = binoData, inits = binoInits)

	binoMonitor <- configureMCMC(bino)
	binoMonitor$addMonitors(c('betalpha','eta'))
	binoMCMC <- buildMCMC(binoMonitor)

	Cbino <- compileNimble(bino)
	CbinoMCMC <- compileNimble(binoMCMC, project = bino)

	CbinoMCMC$run(nloop)

	MCMCsamples <- as.matrix(CbinoMCMC$mvSamples)
	
	}

	else{ if(family$family=="poisson"){
		poisCode <- nimbleCode({
				for (j in 1:n){
		y[j]~dpois(p[j])
		p[j] <- exp(eta[j])
		eta[j]<-inprod(betalpha[1:(M+P)],deltaz[j,1:(M+P)])
		}

		for(m in 1:M){
		betalpha[m] ~ dgamma(0.01,0.01) }

		for(k in (M+1):(M+P)){
		betalpha[k] ~ dnorm(0,1.0E-4) }
		})
	
		delta.dat=t(delta)
		z.dat=zmatb
		deltaz.dat=cbind(delta.dat,z.dat)

	poisConsts <- list(n=n,M=sum(m), P=p, deltaz=deltaz.dat)

	poisData <- list(y=y)		
	
		beta.ini=rep(0.1,sum(m))
		alpha.ini=rep(1,p)
		betalpha.ini=c(beta.ini,alpha.ini)

	poisInits <- list( betalpha=betalpha.ini)

	pois <- nimbleModel(code = poisCode, name = 'pois', constants = poisConsts,
                    data = poisData, inits = poisInits)

	poisMonitor <- configureMCMC(pois)
	poisMonitor$addMonitors(c('betalpha','eta'))
	poisMCMC <- buildMCMC(poisMonitor)

	Cpois <- compileNimble(pois)
	CpoisMCMC <- compileNimble(poisMCMC, project = pois)

	CpoisMCMC$run(nloop)

	MCMCsamples <- as.matrix(CpoisMCMC$mvSamples)

	}
	else{ stop("not valid family") } } }

sims.list=MCMCsamples[(burnin+1):nloop,]

delta.t=t(delta)
zmat.b=zmatb
bigmat <- cbind(zmat.b, delta.t)

alpha.sims <- matrix( sims.list[,(sum(m)+1):(sum(m)+p)], ncol=p )
beta.sims <- matrix( sims.list[,1:sum(m)], ncol=sum(m) )
eta.sims <- matrix( sims.list[,(sum(m)+p+1):(sum(m)+p+n)], ncol=n )

coefs <- apply(sims.list[,c((sum(m)+1):(sum(m)+p), 1:sum(m))],2, mean)
sd.coefs <- apply(sims.list[,c((sum(m)+1):(sum(m)+p), 1:sum(m))],2, sd)
coefs.sims <- cbind(beta.sims, alpha.sims)

etahat <- apply(eta.sims, 2, "mean")

	if(family$family=="gaussian"){
		sigma.sims <-  matrix( sims.list[,sum(m)+p+n+1], ncol=1 )
		muhat <- etahat
		mu.sims <- eta.sims
	}

	if(family$family=="binomial"){
		muhat <- exp(etahat)/(1+exp(etahat))
		mu.sims <- exp(eta.sims)/(1+exp(eta.sims))
	}

	if(family$family=="poisson"){
		muhat <- exp(etahat)
		mu.sims <- exp(eta.sims)
	}

	ans <- list(alpha.sims=alpha.sims, beta.sims=beta.sims, mu.sims=mu.sims, eta.sims=eta.sims,
	delta=delta.t, zmat=zmat.b, znms=znmsb, bigmat=bigmat, coefs.sims=coefs.sims, coefs=coefs, sd.coefs=sd.coefs,
	muhat=muhat, etahat=etahat, knots=tt, ind_intercept=ind_z_one, center.delta=center.delta)

	if(family$family=="gaussian"){
		ans$sigma.sims <- sigma.sims}

ans
}


