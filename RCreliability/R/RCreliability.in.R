
RCreliability.in<-function(r,z,W=NULL,Y){
  n <- length(r)
  t<-length(z)
  meanz<-sapply(z, function(x) colMeans(x, na.rm = T))[1,]
  sdz<-sapply(z, function(x) apply(x, 2, function(y) sd(y,na.rm = T)))[1,]
  z.new <- sapply(1:t, function(x) (z[[x]]-meanz[x])/sdz[x], simplify = F)

  if(is.null(W)){
    zbar<-sapply(z.new,function(y) rowMeans(y,na.rm = T))#mean prevalence=0.503
    #1. naive analysis
    fit1<-glm(Y~zbar,family = "binomial")
    beta.fit1<-fit1$coefficients #naive estimator
    var1<-vcov(fit1) #naive covariance matrix

    tab1<-summary(fit1)$coefficients
    tab1[,1:2] <- tab1[,1:2]/c(1,sdz)
    CI.low<-tab1[,1]-1.96*tab1[,2]
    CI.high<-tab1[,1]+1.96*tab1[,2]
    tab1<-cbind(tab1,exp(cbind(OR = tab1[, 1],CI.low,CI.high)))

    #2. on xhat
    muz<-as.numeric((r%*%zbar)/sum(r))
    v<-sum(r)-sum(r^2)/sum(r)

    dif<-sapply(1:n, function(x) zbar[x,]-muz)
    if(t == 1){
      sigmazstar<-t(dif)%*%(dif*r)/v
    }else{
      sigmazstar<-dif%*%t(dif*r)/v
    }
    diff<-as.matrix(na.omit(sapply(1:t,function(x) (z.new[[x]]-zbar[,x]))))
    if(t==1){
      sigma<-sum(sapply(1:nrow(diff),function(x) diff[x,]%*%t(diff[x,])))/sum(r-1)
    }else{
      sigma<-matrix(rowSums(sapply(1:nrow(diff),function(x) diff[x,]%*%t(diff[x,]))),t)/sum(r-1)
    }
    v12star<-sigmazstar-(n)*sigma/v

    sigmax<-sigmazstar-(n)*sigma/v
    sigmawithin<-sigma
    icc<-sigmax%*%solve(sigmazstar)

    if(t==1){
      sigmazhat<-t(sapply(r,function(x) sigmazstar-(n)*sigma/v+sigma/x))
    }else{
      sigmazhat<-sapply(r,function(x) sigmazstar-(n)*sigma/v+sigma/x)
    }


    if(t==1){
      xhat<-sapply(1:n,function(i) (v12star%*%solve(matrix(sigmazhat[,i],ncol=t))%*%zbar[i]))
    }else{
      xhat<-t(sapply(1:n,function(i) (v12star%*%solve(matrix(sigmazhat[,i],ncol=t))%*%zbar[i,])))
    }
    fit2<-glm(Y~xhat,family = "binomial")
    beta.fit2<-fit2$coefficients
    var2<-sandwich(fit2)

    tab2<-summary(fit2)$coefficients
    tab2[,2]<-sqrt(diag(var2))
    tab2[,1:2] <- tab2[,1:2]/c(1,sdz)
    CI.low<-tab2[,1]-1.96*tab2[,2]
    CI.high<-tab2[,1]+1.96*tab2[,2]
    tab2<-cbind(tab2,exp(cbind(OR = tab2[, 1],CI.low,CI.high)))

    p<-as.vector(exp(beta.fit2 %*% t(cbind(1,xhat)))/(1+exp(beta.fit2 %*% t(cbind(1,xhat)))))
    sigmaz.inv<-solve(sigmazstar)

    Z<-t(cbind(zbar))

    m<-matrix(0,nrow = t,ncol = t)
    c<-matrix(0,nrow = t,ncol = t)

    ### sigmax
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
    db.x<-sapply(1:t,function(x) t(sapply(1:n,function(y) (ddv.x[[x]]%*%solve(matrix(sigmazhat[,y],ncol=t))-
                                                             v12star%*%solve(matrix(sigmazhat[,y],ncol=t))%*%ddm.x[[x]]%*%solve(matrix(sigmazhat[,y],ncol=t)))%*%Z[,y])),simplify = F)

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
      t(sapply(1:n,function(u) (odv.x[[x]][[y]]%*%solve(matrix(sigmazhat[,u],ncol=t))-
                                  v12star%*%solve(matrix(sigmazhat[,u],ncol=t))%*%odm.x[[x]][[y]]%*%solve(matrix(sigmazhat[,u],ncol=t)))%*%Z[,u])),simplify = F),simplify = F)
    }
    ### sigma
    ###diag
    db.0<-sapply(1:t,function(x) t(sapply(1:n, function(u) t((-ddv.x[[x]]%*%solve(matrix(sigmazhat[,u],ncol=t)))%*%(Z/r)[,u]))),simplify = F)

    ###off-diag
    if(t>1){ob.0<-sapply(1:(t-1),function(x) sapply(1:(t-x),function(y)
      t(sapply(1:n,function(u) t((-odv.x[[x]][[y]]%*%solve(matrix(sigmazhat[,u],ncol=t)))%*%(Z/r)[,u]))),simplify = F),simplify = F)}

    m<-(t)*(t+1)/2+(t*(t+1))/2 #number of the covariance estimates
    s<-t+1 #number of beta estimates

    b<-if(t>1){
      list(db.x,ob.x,db.0,ob.0)
    }else list(db.x,db.0)

    b<-sapply(1:m,function(x) matrix(unlist(b),nrow=n)[,(t*x-t+1):(t*x)],simplify = F)

    d<-as.matrix(p*(1-p)%*%t(beta.fit2[2:(t+1)]))/n

    bstar<-sapply(b,function(x) -rowSums(x*d))

    a<-matrix(NA,ncol=m,nrow=s)
    a[1,]<-colSums(bstar)
    a[2:(t+1),]<-t(apply(as.matrix(xhat), 2, function(x) colSums(x * bstar)))

    w = diag(p * (1 - p))
    Astar <- t(cbind(1,xhat)) %*% w %*% cbind(1,xhat) /n


    A<-rbind(cbind(diag(rep(-1,m)),
                   matrix(0,nrow=m,ncol=s)),cbind(a,Astar))
    A<-rbind(cbind(matrix(diag(rep(-1,t),nrow=t),t),matrix(0,nrow=t,ncol=m+s)),
             cbind(matrix(0,nrow=m+s,ncol=t),A))




    dmgi<-sapply(1:(t),function(x) r*(((Z[x,])-muz[x]))/sum(r))
    ###sigmas
    ###diag
    dgi<-r*t(((Z-muz)^2-diag(sigmazstar))/v)
    ###off-diag
    if(t>1){ogi<-sapply(1:(t-1),function(x) sapply((x+1):(t), function(y)
      (r*((Z[x,]-muz[x])*((Z[y,]-muz[y])-sigmazstar[x,y]))/v)))}
    ###within variance
    ###diag
    dgi.0<-if(t==1){
      sapply(1:t,function(y) (rowSums((z.new[[y]]-zbar[,y])^2,na.rm = T)-sigma*(r-1))/sum(r-1))
    }else sapply(1:t,function(y) (rowSums((z.new[[y]]-zbar[,y])^2,na.rm = T)-diag(sigma)[y]*(r-1))/sum(r-1))
    ###off-diag
    if(t>1){ogi.0<-sapply(1:(t-1),function(x) sapply((x+1):t, function(y)
      (rowSums((z.new[[x]]-zbar[,x])*(z.new[[y]]-zbar[,y]),na.rm = T)-sigma[x,y]*(r-1))/sum(r-1)))
    }

    ###betas
    gi.beta<-cbind(1,xhat)*(Y-p) / n


    gi.1<-if(t==1){
      cbind(dmgi,dgi,gi.beta)
    }else cbind(dmgi,dgi,matrix(unlist(ogi),nrow=n),gi.beta)
    B1<-t(gi.1)%*%gi.1

    gi.2<-if(t==1){
      dgi.0
    }else cbind(dgi.0,matrix(unlist(ogi.0),nrow=n))
    B2<-t(gi.2)%*%gi.2

    m1<-(t)*(t+1)/2
    m2<-t*(t+1)/2
    B<-rbind(cbind(B1[1:(t+m1),1:(t+m1)],matrix(0,t+m1,m2),B1[1:(t+m1),(t+m1+1):(t+m1+s)]),
             cbind(matrix(0,m2,t+m1),B2,matrix(0,m2,s)),
             cbind(B1[(t+m1+1):(t+m1+s),1:(t+m1)],matrix(0,s,m2),B1[(t+m1+1):(t+m1+s),(t+m1+1):(t+m1+s)]))

    cov2<-solve(A)%*%B%*%t(solve(A))

    tab3<-summary(fit2)$coefficients
    tab3[,2]<-sqrt(diag(cov2)[(t+m+1):(t+m+s)])
    tab3[,1:2] <- tab3[,1:2]/c(1,sdz)
    CI.low<-tab3[,1]-1.96*tab3[,2]
    CI.high<-tab3[,1]+1.96*tab3[,2]
    tab3[,3]<-tab3[,1]/tab3[,2]
    tab3[,4]<-2*pnorm(tab3[,3],lower.tail=FALSE)
    tab3<-cbind(tab3,exp(cbind(OR = tab3[, 1],CI.low,CI.high)))
  }
  else{
    W<-matrix(W,n)

    t<-length(z)
    q<-ncol(W)
    meanw<-colMeans(W)
    sdw<-apply(W, 2, function(y) sd(y))
    W.new <- sapply(1:q, function(x) (W[,x]-meanw[x])/sdw[x])

    zbar<-sapply(z.new,function(y) rowMeans(y,na.rm = T))#mean prevalence=0.503

    #1. naive analysis
    fit1<-glm(Y~zbar+W.new,family = "binomial")
    beta.fit1<-fit1$coefficients #naive estimator
    var1<-vcov(fit1) #naive covariance matrix

    tab1<-summary(fit1)$coefficients
    tab1[,1:2] <- tab1[,1:2]/c(1,sdz,sdw)
    CI.low<-tab1[,1]-1.96*tab1[,2]
    CI.high<-tab1[,1]+1.96*tab1[,2]
    tab1<-cbind(tab1,exp(cbind(OR = tab1[, 1],CI.low,CI.high)))

    #2. on xhat
    muz<-as.numeric((r%*%zbar)/sum(r))
    muw<-as.numeric(colSums(W.new)/n)

    v<-sum(r)-sum(r^2)/sum(r)

    dif<-rbind(sapply(1:n, function(x) zbar[x,]-muz),
               sapply(1:n, function(x) W.new[x,]-muw))

    sigmazstar<-dif%*%t(dif*r)/v

    diff<-as.matrix(na.omit(sapply(1:t,function(x) (z.new[[x]]-zbar[,x]))))
    if(t==1){
      sigma<-sum(sapply(1:nrow(diff),function(x) diff[x,]%*%t(diff[x,])))/sum(r-1)
    }else{
      sigma<-matrix(rowSums(sapply(1:nrow(diff),function(x) diff[x,]%*%t(diff[x,]))),t)/sum(r-1)
    }
    v12star<-sigmazstar[1:t,]-cbind((n)*sigma/v,matrix(0,ncol = q,nrow=t))

    sigmax<-sigmazstar-rbind(cbind((n)*sigma/v,matrix(0,ncol = q,nrow=t)),matrix(0,ncol=t+q,nrow=q))
    sigmawithin<-rbind(cbind(sigma,matrix(0,ncol = q,nrow=t)),matrix(0,ncol=t+q,nrow=q))
    icc<-sigmax%*%solve(sigmazstar)
    colnames(sigmax)<-colnames(var1)[-1]
    rownames(sigmax)<-rownames(var1)[-1]
    colnames(sigmawithin)<-colnames(var1)[-1]
    rownames(sigmawithin)<-rownames(var1)[-1]
    colnames(icc)<-colnames(var1)[-1]
    rownames(icc)<-rownames(var1)[-1]

    sigmazhat<-sapply(r,function(x) sigmax+rbind(cbind(sigma/x,matrix(0,ncol = q,nrow=t)),matrix(0,ncol=t+q,nrow=q)))

    if(t==1){
      xhat<-sapply(1:n,function(i) (v12star%*%solve(matrix(sigmazhat[,i],ncol=t+q))%*%cbind(zbar,W.new)[i,]))
    }else{
      xhat<-t(sapply(1:n,function(i) (v12star%*%solve(matrix(sigmazhat[,i],ncol=t+q))%*%cbind(zbar,W.new)[i,])))
    }

    fit2<-glm(Y~xhat+W.new,family = "binomial")
    beta.fit2<-fit2$coefficients
    var2<-sandwich(fit2)

    tab2<-summary(fit2)$coefficients
    tab2[,2]<-sqrt(diag(var2))
    tab2[,1:2] <- tab2[,1:2]/c(1,sdz,sdw)
    CI.low<-tab2[,1]-1.96*tab2[,2]
    CI.high<-tab2[,1]+1.96*tab2[,2]
    tab2<-cbind(tab2,exp(cbind(OR = tab2[, 1],CI.low,CI.high)))

    p<-as.vector(exp(beta.fit2 %*% t(cbind(1,xhat,W.new)))/(1+exp(beta.fit2 %*% t(cbind(1,xhat,W.new)))))
    sigmaz.inv<-solve(sigmazstar)

    Z<-t(cbind(zbar,W.new))

    m<-matrix(0,nrow = t+q,ncol = t+q)
    c<-matrix(0,nrow = t,ncol = t+q)

    ### sigmax
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
    db.x<-sapply(1:t,function(x) t(sapply(1:n,function(y) (ddv.x[[x]]%*%solve(matrix(sigmazhat[,y],ncol=t+q))-
                                                             v12star%*%solve(matrix(sigmazhat[,y],ncol=t+q))%*%ddm.x[[x]]%*%solve(matrix(sigmazhat[,y],ncol=t+q)))%*%Z[,y])),simplify = F)

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
      t(sapply(1:n,function(u) (odv.x[[x]][[y]]%*%solve(matrix(sigmazhat[,u],ncol=t+q))-
                                  v12star%*%solve(matrix(sigmazhat[,u],ncol=t+q))%*%odm.x[[x]][[y]]%*%solve(matrix(sigmazhat[,u],ncol=t+q)))%*%Z[,u])),simplify = F),simplify = F)
    }
    ### sigmaxW
    ### all off-diag
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
      t(sapply(1:n,function(u) t((odv.xw[[x]][[y]]%*%solve(matrix(sigmazhat[,u],ncol=t+q))-
                                    v12star%*%solve(matrix(sigmazhat[,u],ncol=t+q))%*%odm.xw[[x]][[y]]%*%solve(matrix(sigmazhat[,u],ncol=t+q)))%*%Z[,u]))),simplify = F),simplify = F)

    ### sigmaW
    ###diag
    ddm.w<-sapply((t+1):(t+q),function(x){
      a<-rep(0,t+q)
      a[x]<-a[x]+1
      diag(a)
    },simplify = F)
    db.w<-sapply(1:q,function(x)
      t(sapply(1:n,function(u) t((-v12star%*%solve(matrix(sigmazhat[,u],ncol=t+q))%*%ddm.w[[x]]%*%solve(matrix(sigmazhat[,u],ncol=t+q)))%*%Z[,u]))),simplify = F)

    ###off-diag
    if(q>1){odm.w<-sapply((t+1):(t+q-1),function(x) sapply(min((x+1),t+q):(t+q),function(y){
      m[x,y]<-m[x,y]+1
      m[y,x]<-m[y,x]+1
      m
    },simplify = F),simplify = F)
    ob.w<-sapply(1:(q-1),function(x) sapply(1:(q-x),function(y)
      t(sapply(1:n,function(u) (-v12star%*%solve(matrix(sigmazhat[,u],ncol=t+q))%*%odm.w[[x]][[y]]%*%solve(matrix(sigmazhat[,u],ncol=t+q)))%*%Z[,u])),simplify = F),simplify = F)
    }
    ### sigma
    ###diag
    db.0<-sapply(1:t,function(x) t(sapply(1:n, function(u) t((-ddv.x[[x]]%*%solve(matrix(sigmazhat[,u],ncol=t+q)))%*%(Z/r)[,u]))),simplify = F)

    ###off-diag
    if(t>1){ob.0<-sapply(1:(t-1),function(x) sapply(1:(t-x),function(y)
      t(sapply(1:n,function(u) t((-odv.x[[x]][[y]]%*%solve(matrix(sigmazhat[,u],ncol=t+q)))%*%(Z/r)[,u]))),simplify = F),simplify = F)}

    m<-(t+q)*(t+q+1)/2+(t*(t+1))/2 #number of the covariance estimates
    s<-t+q+1 #number of beta estimates

    b<-if(t>1 & q>1){
      list(db.x,db.w,ob.x,ob.xw,ob.w,db.0,ob.0)
    }else if(q>1){
      list(db.x,db.w,ob.xw,ob.w,db.0)
    }else if(t>1){
      list(db.x,db.w,ob.x,ob.xw,db.0,ob.0)
    }else list(db.x,db.w,ob.xw,db.0)

    b<-sapply(1:m,function(x) matrix(unlist(b),nrow=n)[,(t*x-t+1):(t*x)],simplify = F)

    d<-as.matrix(p*(1-p)%*%t(beta.fit2[2:(t+1)]))/n

    bstar<-sapply(b,function(x) -rowSums(x*d))

    a<-matrix(NA,ncol=m,nrow=s)
    a[1,]<-colSums(bstar)
    a[2:(t+1),]<-t(apply(as.matrix(xhat), 2, function(x) colSums(x * bstar)))
    a[(t+2):(t+q+1),]<-t(apply(as.matrix(W.new), 2, function(x) colSums(x * bstar)))

    w <- diag(p * (1 - p))
    Astar <- t(cbind(1,xhat, W.new)) %*% w %*% cbind(1,xhat, W.new)/n


    A<-rbind(cbind(diag(rep(-1,m)),
                   matrix(0,nrow=m,ncol=s)),cbind(a,Astar))
    A<-rbind(cbind(diag(rep(-1,t+q),nrow=t+q),matrix(0,nrow=t+q,ncol=m+s)),
             cbind(matrix(0,nrow=m+s,ncol=t+q),A))

    ###sigmas
    ###diag
    mu<-c(muz,muw)
    dmgi<-sapply(1:(t+q),function(x) r*(Z[x,]-mu[x])/sum(r))

    dgi<-sapply(1:(t+q),function(x) (r*((Z[x,]-mu[x])^2-diag(sigmazstar)[x]))/v)
    ###off-diag
    ogi<-sapply(1:(t+q-1),function(x) sapply((x+1):(t+q), function(y)
      (r*((Z[x,]-mu[x])*(Z[y,]-mu[y])-sigmazstar[x,y]))/v))

    ###within variance
    ###diag
    dgi.0<-if(t==1){
      sapply(1:t,function(y) (rowSums((z.new[[y]]-zbar[,y])^2,na.rm = T)-sigma*(r-1))/sum(r-1))
    }else sapply(1:t,function(y) (rowSums((z.new[[y]]-zbar[,y])^2,na.rm = T)-diag(sigma)[y]*(r-1))/sum(r-1))
    ###off-diag
    if(t>1){ogi.0<-sapply(1:(t-1),function(x) sapply((x+1):t, function(y)
      (rowSums((z.new[[x]]-zbar[,x])*(z.new[[y]]-zbar[,y]),na.rm = T)-sigma[x,y]*(r-1))/sum(r-1)))
    }

    ###betas
    gi.beta<-cbind(1,xhat,W.new)*(Y-p)/n

    gi.1<-cbind(dmgi,dgi,matrix(unlist(ogi),nrow=n),gi.beta)
    B1<-crossprod(gi.1)

    gi.2<-if(t==1){
      dgi.0
    }else cbind(dgi.0,matrix(unlist(ogi.0),nrow=n))
    B2<-t(gi.2)%*%gi.2

    m1<-(t+q)*(t+q+1)/2
    m2<-t*(t+1)/2
    B<-rbind(cbind(B1[1:(t+q+m1),1:(t+q+m1)],matrix(0,t+q+m1,m2),B1[1:(t+q+m1),(t+q+m1+1):(t+q+m1+s)]),
             cbind(matrix(0,m2,t+q+m1),B2,matrix(0,m2,s)),
             cbind(B1[(t+q+m1+1):(t+q+m1+s),1:(t+q+m1)],matrix(0,s,m2),B1[(t+q+m1+1):(t+q+m1+s),(t+q+m1+1):(t+q+m1+s)]))

    cov2<-solve(A)%*%B%*%t(solve(A))

    CI.low<-beta.fit2-1.96*sqrt(diag(cov2)[(t+q+m+1):(t+q+m+s)])
    CI.high<-beta.fit2+1.96*sqrt(diag(cov2)[(t+q+m+1):(t+q+m+s)])
    coverage.xhat0<-log(1.5)<=CI.high & log(1.5)>=CI.low

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

