RCreliability.ex<-function(z.main, r, z.rep, W=NULL, Y){
  nm <- nrow(z.main)
  nr <- length(r)
  n <- nm+nr
  t<-ncol(z.main)
  r <- c(rep(1, nm), r)
  z <- sapply(1:t, function(x)
    rbind(cbind(z.main[,x],matrix(NA,nrow = nm, ncol = max(r) - 1)),
          z.rep[[x]]), simplify = F)
  indicator <- c(rep(1, nm), rep(0, nr))
  meanz<-sapply(z, function(x) colMeans(x, na.rm = T))[1,]
  sdz<-sapply(z, function(x) apply(x, 2, function(y) sd(y,na.rm = T)))[1,]
  z.new <- sapply(1:t, function(x) (z[[x]]-meanz[x])/sdz[x], simplify = F)
  if(is.null(W)){

    zmain<- na.omit(sapply(z.new,function(y) y[indicator==1,]))
    Ymain<-Y

    #1. naive analysis
    fit1<-glm(Ymain~zmain,family = "binomial")
    beta.fit1<-(fit1$coefficients) #naive estimator
    var1<-vcov(fit1)#naive covariance matrix

    tab1<-summary(fit1)$coefficients
    tab1[,1:2] <- tab1[,1:2]/c(1,sdz)
    CI.low<-tab1[,1]-1.96*tab1[,2]
    CI.high<-tab1[,1]+1.96*tab1[,2]
    tab1<-cbind(tab1,exp(cbind(OR = tab1[, 1],CI.low,CI.high)))

    #2. on xhat
    zrep<-sapply(z.new,function(y) y[indicator==0,],simplify = F)
    zbar<-sapply(zrep,function(y) rowMeans(y,na.rm = T))#mean prevalence=0.503

    r0<-r[indicator==0]

    muz<-as.numeric(colSums(zmain)/nm)

    dif<-sapply(1:nm, function(x) zmain[x,]-muz)
    if(t == 1){
      sigmaz<-t(dif)%*%(dif)/nm
    }else{
      sigmaz<-dif%*%t(dif)/nm
    }

    diff<-as.matrix(na.omit(sapply(1:t,function(x) (zrep[[x]]-zbar[,x]))))

    if(t==1){
      sigma<-sum(sapply(1:nrow(diff),function(x) diff[x,]%*%t(diff[x,])))/sum(r0-1)
    }else{
      sigma<-matrix(rowSums(sapply(1:nrow(diff),function(x) diff[x,]%*%t(diff[x,]))),t)/sum(r0-1)
    }
    v12star<-sigmaz[1:t,]-sigma

    if(t==1){
      xhat<-apply(cbind(zmain),1,function(t) (v12star%*%solve(sigmaz)%*%t))
    }else{
      xhat<-t(apply(cbind(zmain),1,function(t) (v12star%*%solve(sigmaz)%*%t)))
    }
    sigmax<-sigmaz-sigma
    sigmawithin<-sigma
    icc<-sigmax%*%solve(sigmaz)

    fit2<-glm(Ymain~xhat,family = "binomial")
    beta.fit2<-fit2$coefficients
    var2<-sandwich(fit2)

    tab2<-summary(fit2)$coefficients
    tab2[,2]<-sqrt(diag(var2))
    tab2[,1:2] <- tab2[,1:2]/c(1,sdz)
    CI.low<-tab2[,1]-1.96*tab2[,2]
    CI.high<-tab2[,1]+1.96*tab2[,2]
    tab2<-cbind(tab2,exp(cbind(OR = tab2[, 1],CI.low,CI.high)))


    p<-as.vector(exp(beta.fit2 %*% t(cbind(1,xhat)))/(1+exp(beta.fit2 %*% t(cbind(1,xhat)))))
    sigmaz.inv<-solve(sigmaz)

    Z<-t(cbind(zmain))

    m<-matrix(0,nrow = t,ncol = t)
    c<-matrix(0,nrow = t,ncol = t)

    ##sigmaX
    ###diag
    ddv.x<-sapply(1:t,function(x){
      a<-rep(0,t)
      a[x]<-a[x]+1
      diag(a)
    },simplify = F)
    ddm.x<-sapply(1:t,function(x){
      a<-rep(0,t)
      a[x]<-a[x]+1
      diag(a)
    },simplify = F)
    db.x<-sapply(1:t,function(x) t((ddv.x[[x]]%*%sigmaz.inv-
                                      v12star%*%sigmaz.inv%*%ddm.x[[x]]%*%sigmaz.inv)%*%Z),simplify = F)

    ###off-diag
    if(t>1){odv.x<-sapply(1:(t-1),function(x) sapply(min((x+1),t):t,function(y){
      c[x,y]<-c[x,y]+1
      c[y,x]<-c[y,x]+1
      c
    },simplify = F),simplify = F)
    odm.x<-sapply(1:(t-1),function(x) sapply(min((x+1),t):t,function(y){
      m[x,y]<-m[x,y]+1
      m[y,x]<-m[y,x]+1
      m
    },simplify = F),simplify = F)
    ob.x<-sapply(1:(t-1),function(x) sapply(1:(t-x),function(y)
      t((odv.x[[x]][[y]]%*%sigmaz.inv-
           v12star%*%sigmaz.inv%*%odm.x[[x]][[y]]%*%sigmaz.inv)%*%Z),simplify = F),simplify = F)
    }

    ##sigma
    ###diag
    db.0<-sapply(1:t,function(x) t((ddv.x[[x]]%*%sigmaz.inv)%*%Z),simplify = F)
    #db.0<-sapply(1:t,function(x) t(Z) %*% -ddv.x[[x]]%*%sigmaz.inv,simplify = F)

    ###off-diag
    if(t>1){ob.0<-sapply(1:(t-1),function(x) sapply(1:(t-x),function(y)
      t((odv.x[[x]][[y]]%*%sigmaz.inv)%*%Z),simplify = F),simplify = F)}

    m<-(t)*(t+1)/2+(t*(t+1))/2 #number of the covariance estimates
    s<-t+1 #number of beta estimates

    b<-if(t>1){
      list(db.x,ob.x,db.0,ob.0)
    }else list(db.x,db.0)

    b<-sapply(1:m,function(x) matrix(unlist(b),nrow=nm)[,(t*x-t+1):(t*x)],simplify = F)

    d<-as.matrix(p*(1-p)%*%t(beta.fit2[2:(t+1)])) / nm

    bstar<-sapply(b,function(x) -rowSums(x*d))

    a<-matrix(NA,ncol=m,nrow=s)
    a[1,]<-colSums(bstar)
    a[2:(t+1),]<-t(apply(as.matrix(xhat), 2, function(x) colSums(x * bstar)))

    #Ai<-cbind(1,xhat)
    #Aii<-p*(1-p)*Ai/nm
    w <- diag(p * (1 - p))
    Astar <- t(cbind(1,xhat)) %*% w %*% cbind(1,xhat) /nm

    A<-rbind(cbind(diag(rep(-1,m)),
                   matrix(0,nrow=m,ncol=s)),cbind(a,Astar))
    A<-rbind(cbind(matrix(diag(rep(-1,t),nrow=t),t),matrix(0,nrow=t,ncol=m+s)),
             cbind(matrix(0,nrow=m+s,ncol=t),A))

    ###sigmas
    ###diag
    dmgi<-sapply(1:(t),function(x) (Z[x,]-muz[x])/nm)

    #dgi<-sapply(1:(t),function(x) (((Z[x,]-mu[x])^2-diag(sigmaz)[x]))/nm)

    dgi<-t(((Z-muz)^2-diag(sigmaz))/nm)
    ###off-diag
    if(t>1){ogi<-sapply(1:(t-1),function(x) sapply((x+1):(t), function(y)
      (((Z[x,]-muz[x])*((Z[y,]-muz[y])-sigmaz[x,y]))/nm)))}
    ###within variance
    ###diag
    dgi.0<-if(t==1){
      sapply(1:t,function(y) (rowSums((zrep[[y]]-zbar[,y])^2,na.rm = T)-sigma*(r0-1))/sum(r0-1))
    }else sapply(1:t,function(y) (rowSums((zrep[[y]]-zbar[,y])^2,na.rm = T)-diag(sigma)[y]*(r0-1))/sum(r0-1))
    ###off-diag
    if(t>1){ogi.0<-sapply(1:(t-1),function(x) sapply((x+1):t, function(y)
      (rowSums((zrep[[x]]-zbar[,x])*(zrep[[y]]-zbar[,y]),na.rm = T)-sigma[x,y]*(r0-1))/sum(r0-1)))
    }



    ###betas
    gi.beta<-cbind(1,xhat)*(Ymain-p) / nm

    gi.1<-if(t==1){
      cbind(dmgi,dgi,gi.beta)
    }else cbind(dmgi,dgi,matrix(unlist(ogi),nrow=nm),gi.beta)
    B1<-t(gi.1)%*%gi.1

    gi.2<-if(t==1){
      dgi.0
    }else cbind(dgi.0,matrix(unlist(ogi.0),nrow=nr))
    B2<-t(gi.2)%*%gi.2

    m1<-(t)*(t+1)/2
    m2<-t*(t+1)/2
    B<-rbind(cbind(B1[1:(t+m1),1:(t+m1)],matrix(0,t+m1,m2),B1[1:(t+m1),(t+m1+1):(t+m1+s)]),
             cbind(matrix(0,m2,t+m1),B2,matrix(0,m2,s)),
             cbind(B1[(t+m1+1):(t+m1+s),1:(t+m1)],matrix(0,s,m2),B1[(t+m1+1):(t+m1+s),(t+m1+1):(t+m1+s)]))

    cov2<-solve(A)%*%B%*%t(solve(A))
    var3<-cov2[(t+m+1):(t+m+s),(t+m+1):(t+m+s)]

    tab3<-summary(fit2)$coefficients
    tab3[,2]<-sqrt(diag(cov2)[(t+m+1):(t+m+s)])
    tab3[,1:2] <- tab3[,1:2]/c(1,sdz)
    CI.low<-tab3[,1]-1.96*tab3[,2]
    CI.high<-tab3[,1]+1.96*tab3[,2]
    tab3[,3]<-tab3[,1]/tab3[,2]
    tab3[,4]<-2*pnorm(tab3[,3],lower.tail=FALSE)
    tab3<-cbind(tab3,exp(cbind(OR = tab3[, 1],CI.low,CI.high)))
  }else{
    t<-length(z)
    q<-ncol(W)
    W <- matrix(W, nm)
    meanw<-colMeans(W)
    sdw<-apply(W, 2, function(y) sd(y))
    W.new = sapply(1:q, function(x) (W[,x]-meanw[x])/sdw[x])

    zmain<-na.omit(sapply(z.new,function(y) y[indicator==1,]))
    Wmain<-matrix(W.new,nm)
    Ymain<-Y[indicator==1]
    nm<-sum(indicator)
    nr<-n-nm

    #1. naive analysis
    fit1<-glm(Ymain~zmain+Wmain,family = "binomial")
    beta.fit1<-(fit1$coefficients) #naive estimator
    var1<-vcov(fit1) #naive covariance matrix

    tab1<-summary(fit1)$coefficients
    tab1[,1:2] <- tab1[,1:2]/c(1,sdz,sdw)
    CI.low<-tab1[,1]-1.96*tab1[,2]
    CI.high<-tab1[,1]+1.96*tab1[,2]
    tab1<-cbind(tab1,exp(cbind(OR = tab1[, 1],CI.low,CI.high)))

    #2. on xhat
    zrep<-sapply(z.new,function(y) y[indicator==0,],simplify = F)
    zbar<-sapply(zrep,function(y) rowMeans(y,na.rm = T))#mean prevalence=0.503

    r0<-r[indicator==0]
    v<-sum(r0)-sum(r0^2)/sum(r0)

    muz<-as.numeric(colSums(zmain)/nm)
    muw<-as.numeric(colSums(Wmain)/nm)

    dif<-rbind(sapply(1:nm, function(x) zmain[x,]-muz),
               sapply(1:nm, function(x) Wmain[x,]-muw))
    sigmaz<-dif%*%t(dif)/nm

    #sigmaz<-cov(cbind(zmain))*(nm-1)/nm


    diff<-as.matrix(na.omit(sapply(1:t,function(x) (zrep[[x]]-zbar[,x]))))

    if(t==1){
      sigma<-sum(sapply(1:nrow(diff),function(x) diff[x,]%*%t(diff[x,])))/sum(r0-1)
    }else{
      sigma<-matrix(rowSums(sapply(1:nrow(diff),function(x) diff[x,]%*%t(diff[x,]))),t)/sum(r0-1)
    }
    v12star<-sigmaz[1:t,]-cbind(sigma,matrix(0,ncol = q,nrow=t))

    if(t==1){
      xhat<-apply(cbind(zmain, Wmain),1,function(t) (v12star%*%solve(sigmaz)%*%t))
    }else{
      xhat<-t(apply(cbind(zmain, Wmain),1,function(t) (v12star%*%solve(sigmaz)%*%t)))
    }

    fit2<-glm(Ymain~xhat+Wmain,family = "binomial")
    beta.fit2<-fit2$coefficients
    var2<-sandwich(fit2)

    tab2<-summary(fit2)$coefficients
    tab2[,2]<-sqrt(diag(var2))
    tab2[,1:2] <- tab2[,1:2]/c(1,sdz,sdw)
    CI.low<-tab2[,1]-1.96*tab2[,2]
    CI.high<-tab2[,1]+1.96*tab2[,2]
    tab2<-cbind(tab2,exp(cbind(OR = tab2[, 1],CI.low,CI.high)))

    sigmax<-sigmaz-rbind(cbind(sigma,matrix(0,ncol = q,nrow=t)),matrix(0,ncol=t+q,nrow=q))
    sigmawithin<-rbind(cbind(sigma,matrix(0,ncol = q,nrow=t)),matrix(0,ncol=t+q,nrow=q))
    icc<-sigmax%*%solve(sigmaz)
    colnames(sigmax)<-colnames(var1)[-1]
    rownames(sigmax)<-rownames(var1)[-1]
    colnames(sigmawithin)<-colnames(var1)[-1]
    rownames(sigmawithin)<-rownames(var1)[-1]



    p<-as.vector(exp(beta.fit2 %*% t(cbind(1,xhat,Wmain)))/(1+exp(beta.fit2 %*% t(cbind(1,xhat,Wmain)))))
    sigmaz.inv<-solve(sigmaz)

    Z<-t(cbind(zmain,Wmain))

    m<-matrix(0,nrow = t+q,ncol = t+q)
    c<-matrix(0,nrow = t,ncol = t+q)

    ##sigmaX
    ###diag
    ddv.x<-sapply(1:t,function(x){
      a<-rep(0,t)
      a[x]<-a[x]+1
      cbind(diag(a),matrix(0,nrow = t,ncol = q))
    },simplify = F)
    ddm.x<-sapply(1:t,function(x){
      a<-rep(0,t+q)
      a[x]<-a[x]+1
      diag(a)
    },simplify = F)
    db.x<-sapply(1:t,function(x) t((ddv.x[[x]]%*%sigmaz.inv-
                                      v12star%*%sigmaz.inv%*%ddm.x[[x]]%*%sigmaz.inv)%*%Z),simplify = F)
    ###off-diag
    if(t>1){odv.x<-sapply(1:(t-1),function(x) sapply(min((x+1),t):t,function(y){
      c[x,y]<-c[x,y]+1
      c[y,x]<-c[y,x]+1
      c
    },simplify = F),simplify = F)
    odm.x<-sapply(1:(t-1),function(x) sapply(min((x+1),t):t,function(y){
      m[x,y]<-m[x,y]+1
      m[y,x]<-m[y,x]+1
      m
    },simplify = F),simplify = F)
    ob.x<-sapply(1:(t-1),function(x) sapply(1:(t-x),function(y)
      t((odv.x[[x]][[y]]%*%sigmaz.inv-
           v12star%*%sigmaz.inv%*%odm.x[[x]][[y]]%*%sigmaz.inv)%*%Z),simplify = F),simplify = F)
    }
    ##sigmaXW
    ##all off-diag
    odv.xw<-sapply(1:t,function(x) sapply(max((x+1),t+1):(t+q),function(y){
      c[x,y]<-c[x,y]+1
      c
    },simplify = F),simplify = F)
    odm.xw<-sapply(1:t,function(x) sapply(max((x+1),t+1):(t+q),function(y){
      m[x,y]<-m[x,y]+1
      m[y,x]<-m[y,x]+1
      m
    },simplify = F),simplify = F)
    ob.xw<-sapply(1:t,function(x) sapply(1:q,function(y)
      t((odv.xw[[x]][[y]]%*%sigmaz.inv-
           v12star%*%sigmaz.inv%*%odm.xw[[x]][[y]]%*%sigmaz.inv)%*%Z),simplify = F),simplify = F)

    ##sigmaW
    ###diag
    ddm.w<-sapply((t+1):(t+q),function(x){
      a<-rep(0,t+q)
      a[x]<-a[x]+1
      diag(a)
    },simplify = F)
    db.w<-sapply(1:q,function(x)
      t((-v12star%*%sigmaz.inv%*%ddm.w[[x]]%*%sigmaz.inv)%*%Z),simplify = F)

    ###off-diag
    if(q>1){odm.w<-sapply((t+1):(t+q-1),function(x) sapply(min((x+1),t+q):(t+q),function(y){
      m[x,y]<-m[x,y]+1
      m[y,x]<-m[y,x]+1
      m
    },simplify = F),simplify = F)
    ob.w<-sapply(1:(q-1),function(x) sapply(1:(q-x),function(y)
      t((-v12star%*%sigmaz.inv%*%odm.w[[x]][[y]]%*%sigmaz.inv)%*%Z),simplify = F),simplify = F)
    }
    ##sigma
    ###diag
    db.0<-sapply(1:t,function(x) t((-ddv.x[[x]]%*%sigmaz.inv)%*%Z),simplify = F)

    ###off-diag
    if(t>1){ob.0<-sapply(1:(t-1),function(x) sapply(1:(t-x),function(y)
      t((-odv.x[[x]][[y]]%*%sigmaz.inv)%*%Z),simplify = F),simplify = F)}

    m<-(t+q)*(t+q+1)/2+(t*(t+1))/2 #number of the covariance estimates
    s<-t+q+1 #number of beta estimates

    b<-if(t>1 & q>1){
      list(db.x,db.w,ob.x,ob.xw,ob.w,db.0,ob.0)
    }else if(q>1){
      list(db.x,db.w,ob.xw,ob.w,db.0)
    }else if(t>1){
      list(db.x,db.w,ob.x,ob.xw,db.0,ob.0)
    }else list(db.x,db.w,ob.xw,db.0)

    b<-sapply(1:m,function(x) matrix(unlist(b),nrow=nm)[,(t*x-t+1):(t*x)],simplify = F)

    d<-as.matrix(p*(1-p)%*%t(beta.fit2[2:(t+1)]))/nm

    bstar<-sapply(b,function(x) -rowSums(x*d))

    a<-matrix(NA,ncol=m,nrow=s)
    a[1,]<-colSums(bstar)
    a[2:(t+1),]<-t(apply(as.matrix(xhat), 2, function(x) colSums(x * bstar)))
    a[(t+2):(t+q+1),]<-t(apply(as.matrix(Wmain), 2, function(x) colSums(x * bstar)))


    w <- diag(p * (1 - p))
    Astar <- t(cbind(1,xhat, Wmain)) %*% w %*% cbind(1,xhat, Wmain) /nm

    A<-rbind(cbind(diag(rep(-1,m)),
                   matrix(0,nrow=m,ncol=s)),cbind(a,Astar))
    A<-rbind(cbind(diag(rep(-1,t+q),nrow=t+q),matrix(0,nrow=t+q,ncol=m+s)),
             cbind(matrix(0,nrow=m+s,ncol=t+q),A))

    ##mus
    mu<-c(muz,muw)
    dmgi<-sapply(1:(t+q),function(x) (Z[x,]-mu[x])/nm)
    ###sigmas
    ###diag
    dgi<-t(((Z-mu)^2-diag(sigmaz))/nm)    ###off-diag
    ogi<-sapply(1:(t+q-1),function(x) sapply((x+1):(t+q), function(y)
      ((Z[x,]-mu[x])*(Z[y,]-mu[y])-sigmaz[x,y])/nm))
    ###within variance
    ###diag
    dgi.0<-if(t==1){
      sapply(1:t,function(y) (rowSums((zrep[[y]]-zbar[,y])^2,na.rm = T)-sigma*(r0-1))/sum(r0-1))
    }else sapply(1:t,function(y) (rowSums((zrep[[y]]-zbar[,y])^2,na.rm = T)-diag(sigma)[y]*(r0-1))/sum(r0-1))
    ###off-diag
    if(t>1){ogi.0<-sapply(1:(t-1),function(x) sapply((x+1):t, function(y)
      (rowSums((zrep[[x]]-zbar[,x])*(zrep[[y]]-zbar[,y]),na.rm = T)-sigma[x,y]*(r0-1))/sum(r0-1)))
    }

    ###betas
    gi.beta<-cbind(1,xhat,Wmain)*(Ymain-p)/nm


    gi.1<-cbind(dmgi,dgi,matrix(unlist(ogi),nrow=nm),gi.beta)
    B1<-t(gi.1)%*%gi.1
    gi.2<-if(t==1){
      dgi.0
    }else cbind(dgi.0,matrix(unlist(ogi.0),nrow=nr))
    B2<-t(gi.2)%*%gi.2

    m1<-(t+q)*(t+q+1)/2
    m2<-t*(t+1)/2
    B<-rbind(cbind(B1[1:(t+q+m1),1:(t+q+m1)],matrix(0,t+q+m1,m2),B1[1:(t+q+m1),(t+q+m1+1):(t+q+m1+s)]),
             cbind(matrix(0,m2,t+q+m1),B2,matrix(0,m2,s)),
             cbind(B1[(t+q+m1+1):(t+q+m1+s),1:(t+q+m1)],matrix(0,s,m2),B1[(t+q+m1+1):(t+q+m1+s),(t+q+m1+1):(t+q+m1+s)]))

    cov2<-solve(A)%*%B%*%t(solve(A))
    var3<-cov2[(t+q+m+1):(t+q+m+s),(t+q+m+1):(t+q+m+s)]

    tab3<-summary(fit2)$coefficients
    tab3[,2]<-sqrt(diag(cov2)[(t+q+m+1):(t+q+m+s)])
    tab3[,1:2] <- tab3[,1:2]/c(1,sdz,sdw)
    CI.low<-tab3[,1]-1.96*tab3[,2]
    CI.high<-tab3[,1]+1.96*tab3[,2]
    tab3[,3]<-tab3[,1]/tab3[,2]
    tab3[,4]<-2*pnorm(tab3[,3],lower.tail=FALSE)
    tab3<-cbind(tab3,exp(cbind(OR = tab3[, 1],CI.low,CI.high)))
  }
  list("Naive estimates"=tab1[-1,],
       "Corrected estimates"=tab2[-1,],
       "Corrected estimates, taking into account the extra variation due to estimating the parameters in the measurement error model"=tab3[-1,])
}


