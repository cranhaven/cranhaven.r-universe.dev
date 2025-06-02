bootstrap_lm_cov_latent_cont <- function(X1,X2,param="multilogit",Mu,Si,Be,Ga,B=100){

# preliminaries
	mMu = mSi = mBe = 0
	m2Mu = m2Si = m2Be = 0
	if(param=="multilogit"){
		mGa = 0
		m2Ga = 0
	}else if(param=="difflogit"){
		mGa = vector("list",2)
		m2Ga = vector("list",2)
		mGa[[1]] = matrix(0,dim(Ga[[1]]))
		mGa[[2]] = matrix(0,dim(Ga[[2]]))
		m2Ga[[1]] = matrix(0,dim(Ga[[1]]))
		m2Ga[[2]] = matrix(0,dim(Ga[[2]]))
	}

    if(is.vector(Mu)){
    	r =1
    	k = length(Mu)
    }else{
    	r = nrow(Mu)
    	k = ncol(Mu)
    }

  for (b in 1:B) {
    cat("boostrap sample n. ",b,"\n")
    out = draw_lm_cov_latent_cont(X1,X2,param,Mu,Si,Be,Ga)
    Yb = out$Y
    out = est_lm_cov_latent_cont(Yb,X1,X2,param=param,k=k)
    mMu = mMu + out$Mu/B
    mSi = mSi + out$Si/B
    mBe = mBe + out$Be/B
    if(param=="multilogit"){
   	 	mGa = mGa + out$Ga/B
    	m2Ga = m2Ga + out$Ga^2/B   	
    }else if(param=="difflogit"){
    	mGa[[1]] = mGa[[1]]+out$Ga[[1]]/B
    	mGa[[2]] = mGa[[2]]+out$Ga[[2]]/B
    	m2Ga[[1]] = m2Ga[[1]] + out$Ga[[1]]^2/B   	
      	m2Ga[[2]] = m2Ga[[2]] + out$Ga[[2]]^2/B   	
    }
    m2Mu = m2Mu + out$Mu^2/B
    m2Si = m2Si + out$Si^2/B
    m2Be = m2Be + out$Be^2/B
 
  }
  seMu = sqrt(m2Mu - mMu^2)
  seSi = sqrt(m2Si - mSi^2)
  seBe = sqrt(m2Be - mBe^2)
  if(param=="multilogit"){ 
  	seGa = sqrt(m2Ga - mGa^2)
  }else if(param=="difflogit"){
  	seGa = vector("list",2)
  	seGa[[1]] = sqrt(m2Ga[[1]] - mGa[[1]]^2)
  	seGa[[2]] = sqrt(m2Ga[[2]] - mGa[[2]]^2)	
  }
  out = list(mMu = mMu, mSi = mSi, mBe = mBe, mGa = mGa,
                 seMu = seMu, seSi = seSi, seBe = seBe, seGa = seGa)
  
}