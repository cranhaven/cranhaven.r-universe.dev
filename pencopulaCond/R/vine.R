vine <- function(data,d=2,d2=2,D=4,D3=6,lambda=c(100,50),type="Rvine",order.Dvine=FALSE,m=2,cores=NULL,q=1,mod.cond=TRUE,
max.iter=51,fix.lambda=FALSE,RVM=NULL,cal.cond=FALSE,
id=NULL,test.ind=FALSE,test.cond=2,lambda.search=FALSE,lam1.vec=NULL,lam2.vec=NULL) {
  if(is.null(cores)) registerDoParallel() else registerDoParallel(cores=cores)
  mcoptions <- list(preschedule=FALSE)

  if(!is.matrix(data)) data <- as.matrix(data)
  U <- data
S <- seq(1:(dim(U)[2]))
N <-  dim(U)[1]
k <-  1
if(type=="Dvine") {
Dvine <- list()
SSi <-  list()
#order options
#pairwise AIC-order in the first level
help.env <- new.env()
assign("S",S,help.env)
assign("U",U,help.env)
assign("lambda",lambda,help.env)
assign("lambda.search",lambda.search,help.env)
assign("lam1.vec",lam1.vec,help.env)
assign("lam2.vec",lam2.vec,help.env)
assign("fix.lambda",fix.lambda,help.env)
assign("d",d,help.env)
assign("d2",d,help.env)
assign("D",D,help.env)
assign("max.iter",max.iter,help.env)
assign("base",base<-"B-spline",help.env)
assign("m",m,help.env)
assign("q",q,help.env)
assign("test.ind",test.ind,help.env)
dd <- (2**d)+1 # Anzahl Knoten
ddb <- dd # Anzahl Basisfunktionen
cond.dens<-0
D.struc<-1
######### independence model ###
if(test.ind) {
model.l<-pencopula(data=data[,c(1,2)],d=d,D=D,pen.order=m,base="B-spline",lambda=rep(lambda[1],2),max.iter=1,cond=FALSE,q=q,id=id,fix.lambda=fix.lambda)
assign("ck.val",get("ck.val.start",model.l),model.l)
assign("log.like",0,model.l)
assign("cAIC",-0.00000000001,model.l)
assign("AIC",0,model.l)
assign("f.hat.val",get("f.hat.val.start",model.l),model.l)
assign("indep",TRUE,model.l)
assign("indep.model",model.l,help.env)
assign("cond",FALSE,model.l)
rm(model.l)
}
#########

Index.basis.D <- matrix(NA,ddb^2,2)
Index.basis.D[,1] <- rep(seq(1,ddb),ddb)
Index.basis.D[,2] <- sort(Index.basis.D[,1])
if(order.Dvine) {
  assign("order.stat","cAIC",help.env)
  order.Dvine(help.env)
  U <- U[,get("order",help.env)]
  #org.data<-org.data[,get("order",help.env)]
}
else {
  assign("order",seq(1,length(S)),help.env)
}
for ( i in 1:length(S))
    {
      vine.knot <-  list(j1=S[i],j2=NULL,D=NULL,v=NULL, U = U[,S[i]] )
      SSi <- append(SSi, list(vine.knot))
    }
Dvine <- append(Dvine, list(SSi))
rm(SSi)

  level <-  1
  log.like <- AIC <- cAIC <- 0

if(order.Dvine) {
  pairs.fit <- get("pairs.fit",help.env)
  pairs.new <- get("pairs.new",help.env)
  Dvine <- append(Dvine, list(foreach(i=2:length(S),.combine=list,.multicombine=TRUE,.options.multicore=mcoptions) %do% {
    index.j1 <- i-1 #pairs.fit[i-1,1]
    index.j2 <- i #pairs.fit[i-1,2]
    rev<-FALSE
    for(kk in 1:dim(pairs.new)[1]) {
       if(all(pairs.new[kk,]==pairs.fit[i-1,])|all(pairs.new[kk,]==c(pairs.fit[i-1,2],pairs.fit[i-1,1]))) {
         model.l <- get("fit.level1",help.env)[[kk]]
         if(all(pairs.new[kk,]==c(pairs.fit[i-1,2],pairs.fit[i-1,1]))) rev<-TRUE
       }
    }
    #vine.knot <-  list(j1=index.j1,j2=index.j2,D=NULL,v=get("ck.val",model.l),U=U[,c(index.j1,index.j2)],log.like = get("log.like",model.l), AIC=get("AIC",model.l),cAIC=get("cAIC",model.l),lambda=get("lambda",model.l))
    vine.knot <-  list(j1=index.j1,j2=index.j2,D=NULL,v=get("ck.val",model.l),U=get("Y",model.l),log.like = get("log.like",model.l), AIC=get("AIC",model.l),cAIC=get("cAIC",model.l),lambda=get("lambda",model.l),cond=FALSE,f.hat=get("f.hat.val",model.l),indep=get("indep",model.l),rev=rev)
}))
  level <- 2
  SS <-  Dvine[[level]]
  nSS <- length(SS)
  while (nSS>1)
    {
     SSi <-  list()
     for (i in 2:nSS)
       {
         S1 <- c(SS[[i-1]]$j1, SS[[i-1]]$j2,SS[[i-1]]$D)
         S2 <- c(SS[[i]]$j1, SS[[i]]$j2,SS[[i]]$D)
         index1 <- rep(TRUE,length(S1))
         index2 <- rep(TRUE,length(S2))
         S3 <- c()
         for (j in 1:length(S1))
           {
             indexi <- S1[j]==S2
             if (sum(indexi)>0)
               {
                 S3 <- c(S3, S1[j])
                 index1[j] <- FALSE
                 index2[indexi] <- FALSE                
               }
           }
         if(!is.null(S3)) S3 <- sort(S3)
         S1 <- S1[index1]
         S2 <- S2[index2]
         vine.knot <-  list(j1=S1,j2=S2,D=S3,v=NULL, U = NULL, log.like = NULL, AIC = NULL)
         SSi <- append(SSi, list(vine.knot))
       }
     Dvine <- append(Dvine, list(SSi))
     level <- level + 1
     SS <-  Dvine[[level]]
     nSS <- length(SS)
   }
  level <- 2
}

if(!order.Dvine) {
  SS <-  Dvine[[level]]
  nSS <- length(SS)
  while (nSS>1)
    {
     SSi <-  list()
     for (i in 2:nSS)
       {
         S1 <- c(SS[[i-1]]$j1, SS[[i-1]]$j2,SS[[i-1]]$D)
         S2 <- c(SS[[i]]$j1, SS[[i]]$j2,SS[[i]]$D)
         index1 <- rep(TRUE,length(S1))
         index2 <- rep(TRUE,length(S2))
         S3 <- c()
         for (j in 1:length(S1))
           {
             indexi <- S1[j]==S2
             if (sum(indexi)>0)
               {
                 S3 <- c(S3, S1[j])
                 index1[j] <- FALSE
                 index2[indexi] <- FALSE                
               }
           }
         if(!is.null(S3)) S3 <- sort(S3)
         S1 <- S1[index1]
         S2 <- S2[index2]
         vine.knot <-  list(j1=S1,j2=S2,D=S3,v=NULL, U = NULL, log.like = NULL, AIC = NULL)
         SSi <- append(SSi, list(vine.knot))
       }
     Dvine <- append(Dvine, list(SSi))
     level <- level + 1
     SS <-  Dvine[[level]]
     nSS <- length(SS)
   }
  level <-  2
  Tree.l <- Dvine[[level]]
    Tree.l.temp <- foreach(i=1:length(Tree.l),.combine=list,.multicombine=TRUE,.options.multicore=mcoptions) %do%   {
      index.j1 <- Tree.l[[i]]$j1
      index.j2 <- Tree.l[[i]]$j2
      new.ind<-c()
        if(lambda.search) model.l<-lam.search(data=U[,c(index.j1,index.j2)],d=d,D=D,lam=lam1.vec,m=m,max.iter=max.iter,q=q,cond=FALSE,id=id,l.lam=2,fix.lambda=fix.lambda,test.ind=test.ind)
        if(!lambda.search) model.l<-pencopula(data=U[,c(index.j1,index.j2)],d=d,D=D,pen.order=m,base="B-spline",lambda=rep(lambda[1],2),max.iter=max.iter,q=q,id=id,cond=FALSE,fix.lambda=fix.lambda,test.ind=test.ind)
      list(j1=index.j1,j2=index.j2,D=NULL,v=get("ck.val",model.l),U=get("Y",model.l),log.like = get("log.like",model.l),AIC=get("AIC",model.l),cAIC=get("cAIC",model.l),lambda=get("lambda",model.l),cond=get("cond",model.l),f.hat=get("f.hat.val",model.l),indep=get("indep",model.l))
    }
  
if(length(S)>2) Dvine[[level]] <- Tree.l.temp
else Dvine[[level]] <- list(Tree.l.temp)
}

log.like.vec<-AIC.vec<-cAIC.vec<-c()
 if(level<length(S)) {
    for(i in 1:length(Dvine[[level]])) {
      log.like <- log.like+Dvine[[level]][[i]]$log.like
      log.like.vec[i]<-Dvine[[level]][[i]]$log.like
      AIC <- AIC+Dvine[[level]][[i]]$AIC
      AIC.vec[i]<-Dvine[[level]][[i]]$AIC
      cAIC <- cAIC+Dvine[[level]][[i]]$cAIC
      cAIC.vec[i]<-Dvine[[level]][[i]]$cAIC
    }
    count<-length(Dvine[[level]])
  }
  else {
    log.like <- log.like+Dvine[[level]][[1]]$log.like
    log.like.vec<-Dvine[[level]][[1]]$log.like
    AIC <- AIC+Dvine[[level]][[1]]$AIC
    AIC.vec<-Dvine[[level]][[1]]$AIC
    cAIC <- cAIC+Dvine[[level]][[1]]$cAIC
    cAIC.vec<-Dvine[[level]][[1]]$cAIC
    count<-1
  }

if(length(S)>2) {
  for(level in 3:length(S)) {
    print(paste("level= ",level,sep=""))
    Tree.l <- Dvine[[level]] # current tree in vine
    Tree.l1 <- Dvine[[level-1]] # previous tree in vine, one level up
    t.length <- length(Tree.l) 
    Tree.l.temp <- foreach(i=1:t.length,.combine=list,.multicombine=TRUE,.options.multicore=mcoptions) %do%  {
      UU <-  new.ind<-c()
      index <- list(c(Tree.l[[i]]$j1,Tree.l[[i]]$D),c(Tree.l[[i]]$j2,Tree.l[[i]]$D)) # list of involved indices in current knot, i.e {j1,D} and {j2,D}   
      D.index<-Tree.l[[i]]$D
      len.D<-length(D.index)
      p<-2+len.D 
      if(p>(2+D.struc)) p<-2+D.struc
      for(j in 1:2) # Loop over both index sets, i.e. {j1,D} and {j2,D}
        {
          indexi <- sort(index[[j]])
          index.ancestor <- c()
          for (ml in 1:length(Tree.l1))
            {
              index.ancestor <- c(index.ancestor, all(indexi==sort(c(Tree.l1[[ml]]$j1,Tree.l1[[ml]]$j2,Tree.l1[[ml]]$D))))
            }
          ancestor.knot <- Tree.l1[index.ancestor][[1]] # Ancestor knot of j-th Element in Index set, i.e. knot with indices {j1,D} for j=1 and {j2,D} for j=2, respectively.
          ord1<-c(1,2)
          #if(level==3&j==1) if(ancestor.knot$rev) ord1<-c(2,1)
          if(j==1&!ancestor.knot$cond) UU <-cbind(UU,hierarchbs.cond.cop(data=ancestor.knot$U,coef=ancestor.knot$v,intp=c(TRUE,FALSE)[ord1],d=d,D=D,p=2,q=q))
          if(j==1&ancestor.knot$cond) UU <-cbind(UU,hierarchbs.cond.cop(data=ancestor.knot$U,coef=ancestor.knot$v,intp=c(TRUE,FALSE,rep(FALSE,dim(ancestor.knot$U)[2]-2)),d=d2,D=D3,p=dim(ancestor.knot$U)[2],q=q))
          if(j==1) new.ind<-ancestor.knot$indep
          #if(j==2) new.ind<-c(new.ind,ancestor.knot$indep)
          ord1<-c(1,2)
          #if(level==3&j==2) if(ancestor.knot$rev) ord1<-c(2,1)
          if(j==2&!ancestor.knot$cond) UU <-cbind(UU,hierarchbs.cond.cop(data=ancestor.knot$U,coef=ancestor.knot$v,intp=c(FALSE,TRUE)[ord1],d=d,D=D,p=2,q=q))
          if(j==2&ancestor.knot$cond) UU <-cbind(UU,hierarchbs.cond.cop(data=ancestor.knot$U,coef=ancestor.knot$v,intp=c(FALSE,TRUE,rep(FALSE,dim(ancestor.knot$U)[2]-2)),d=d2,D=D3,p=dim(ancestor.knot$U)[2],q=q))
      }
      if(any(new.ind)) {
         model.l<-get("indep.model",help.env)
         cond<-FALSE
         prcomp.D<-NULL
      }
      else {
        if(!mod.cond) {       
          if(lambda.search) model.l<-lam.search(data=UU,d=d,D=D,lam=lam1.vec,m=m,max.iter=max.iter,q=q,cond=FALSE,id=id,l.lam=2,fix.lambda=fix.lambda,test.ind=test.ind)
          if(!lambda.search) model.l<-pencopula(data=UU,d=d,D=D,pen.order=m,base="B-spline",lambda=rep(lambda[1],2),max.iter=max.iter,q=q,id=id,cond=FALSE,fix.lambda=fix.lambda,test.ind=test.ind)
          cond<-FALSE
          prcomp.D<-NULL
        }
        if(mod.cond) {
         l.lam<-2+len.D
         if(l.lam>(2+D.struc)) l.lam<-2+D.struc
         if(len.D<=D.struc) {
           if(!cal.cond) {
               if(test.cond==1) pacotestOptions=pacotestset(testType='VI')
               if(test.cond==2) pacotestOptions=pacotestset(testType='ECORR')
               if(test.cond==2|test.cond==1) test.res<-pacotest(UU,U[,D.index],pacotestOptions)$pValue
             if(is.null(test.cond)) test.res<-0.051
             if(test.res<0.05) {
               UU<-cbind(UU,U[,D.index])
               colnames(UU)<-NULL
               cond<-TRUE
               prcomp.D<-NULL
               if(lambda.search) model.l<-lam.search(data=UU,d=d2,D=D3,lam=lam2.vec,m=m,max.iter=max.iter,q=q,cond=TRUE,id=id,l.lam=l.lam,fix.lambda=fix.lambda,test.ind=test.ind)
               if(!lambda.search) model.l<-pencopula(data=UU,d=d2,D=D3,pen.order=m,base="B-spline",lambda=rep(lambda[2],l.lam),max.iter=max.iter,cond=TRUE,q=q,id=id,fix.lambda=fix.lambda,test.ind=test.ind)
             }
             else {
               prcomp.D<-NULL
               cond <- FALSE
               if(lambda.search) model.l<-lam.search(data=UU,d=d,D=D,lam=lam1.vec,m=m,max.iter=max.iter,q=q,cond=FALSE,id=id,l.lam=2,fix.lambda=fix.lambda,test.ind=test.ind)
               if(!lambda.search) model.l<-pencopula(data=UU,d=d,D=D,pen.order=m,base="B-spline",lambda=rep(lambda[1],2),max.iter=max.iter,cond=FALSE,q=q,id=id,fix.lambda=fix.lambda,test.ind=test.ind)
             }
           } 
           if(cal.cond) {
             UU<-cbind(UU,U[,Tree.l[[i]]$D])
             prcomp.D<-NULL
             cond<-TRUE
             l.lam<-2+len.D
             if(l.lam>(2+D.struc)) l.lam<-2+D.struc
             D.struc.temp<-min(D.struc,len.D)
             if(lambda.search) model.l<-lam.search(data=UU,d=d2,D=D3,lam=lam2.vec,m=m,max.iter=max.iter,q=q,cond=TRUE,id=id,l.lam=l.lam,fix.lambda=fix.lambda,test.ind=test.ind) 
             if(!lambda.search) model.l<-pencopula(data=UU,d=d2,D=D3,pen.order=m,base="B-spline",lambda=rep(get("lambda",help.env)[2],l.lam),max.iter=max.iter,cond=TRUE,q=q,id=id,fix.lambda=fix.lambda,test.ind=test.ind)
           }
         }
         if(len.D>D.struc) {
             if(!cal.cond) {
               if(test.cond==1) pacotestOptions=pacotestset(testType='VI')
               if(test.cond==2) pacotestOptions=pacotestset(testType='ECORR')
               if(test.cond==2|test.cond==1) test.res<-pacotest(UU,U[,D.index],pacotestOptions)$pValue
               if(is.null(test.cond)) test.res <- 0.051
               if(test.res<0.05) {
                   data.distr<-cal.pca(help.env,val=get("data",help.env)[,D.index])
                   UU<-cbind(UU,data.distr$data.distr)
                   prcomp.D<-data.distr$prcomp.D
                   colnames(UU) <- NULL
                   cond<-TRUE
                        if(lambda.search) model.l<-lam.search(data=UU,d=d2,D=D3,lam=lam2.vec,m=m,max.iter=max.iter,q=q,cond=TRUE,id=id,l.lam=dim(UU)[2],fix.lambda=fix.lambda,test.ind=test.ind)
                   if(!lambda.search) model.l<-pencopula(data=UU,d=d2,D=D3,pen.order=m,base="B-spline",lambda=rep(get("lambda",help.env)[2],l.lam),max.iter=max.iter,cond=TRUE,q=q,id=id,fix.lambda=fix.lambda,test.ind=test.ind)
               }
               else {
                 prcomp.D<-NULL
                 cond <- FALSE
                 if(lambda.search) model.l<-lam.search(data=UU,d=d,D=D,lam=lam1.vec,m=m,max.iter=max.iter,q=q,cond=FALSE,id=id,l.lam=2,fix.lambda=fix.lambda,test.ind=test.ind)
                 if(!lambda.search) model.l<-pencopula(data=UU,d=d,D=D,pen.order=m,base="B-spline",lambda=rep(lambda[1],2),max.iter=max.iter,cond=FALSE,q=q,id=id,fix.lambda=fix.lambda,test.ind=test.ind)
                 }
               }
             if(cal.cond) {
                 data.distr<-cal.pca(help.env,val=get("data",help.env)[,D.index])
                 UU<-cbind(UU,data.distr$data.distr)
                 prcomp.D<-data.distr$prcomp.D
                 colnames(UU) <- c()
                 l.lam<-2+len.D
                 if(l.lam>(2+D.struc)) l.lam<-2+D.struc
                 cond<-TRUE
                 if(lambda.search) model.l<-lam.search(data=UU,d=d2,D=D3,lam=lam2.vec,m=m,max.iter=max.iter,q=q,cond=TRUE,id=id,l.lam=l.lam,fix.lambda=fix.lambda,test.ind=test.ind)
                 if(!lambda.search) model.l<-pencopula(data=UU,d=d2,D=D3,pen.order=m,base="B-spline",lambda=rep(get("lambda",help.env)[2],l.lam),max.iter=max.iter,cond=TRUE,q=q,id=id,fix.lambda=fix.lambda,test.ind=test.ind)
            }
         }
      }
      }
      list(j1=Tree.l[[i]]$j1,j2=Tree.l[[i]]$j2,D=Tree.l[[i]]$D,U=UU,v=get("ck.val",model.l),log.like = get("log.like",model.l),AIC=get("AIC",model.l),cAIC=get("cAIC",model.l),lambda=get("lambda",model.l),cond=cond,prcomp.D=prcomp.D,f.hat=get("f.hat.val",model.l),indep=get("indep",model.l))
    }
    if(level<length(S)) {
      for(i in 1:length(Tree.l.temp)) {
        log.like <- log.like+Tree.l.temp[[i]]$log.like
        log.like.vec[i+count]<-Tree.l.temp[[i]]$log.like
        AIC <- AIC+Tree.l.temp[[i]]$AIC 
        AIC.vec[i+count]<-Tree.l.temp[[i]]$AIC
        cAIC <- cAIC+Tree.l.temp[[i]]$cAIC
        cAIC.vec[i+count]<-Tree.l.temp[[i]]$cAIC
        cond.dens<-cond.dens+Tree.l.temp[[i]]$cond
      }
      count<-count+i
    }
    else {
      log.like <- log.like+Tree.l.temp$log.like
      log.like.vec[1+count]<-Tree.l.temp$log.like
      AIC <- AIC+Tree.l.temp$AIC
      AIC.vec[1+count]<-Tree.l.temp$AIC
      cAIC <- cAIC+Tree.l.temp$cAIC
      cAIC.vec[1+count]<-Tree.l.temp$cAIC
      cond.dens<-cond.dens+Tree.l.temp$cond
      Tree.l.temp <- list(Tree.l.temp)
    }
    Dvine[[level]] <- Tree.l.temp
  }
} 
  class(vine) <- "pencopulaCond"
return(list(Dvine=Dvine,log.like=log.like,log.like.vec=log.like.vec,AIC=AIC,AIC.vec=AIC.vec,cAIC=cAIC,cAIC.vec=cAIC.vec,d=d,d2=d2,D=D,D3=D3,order=get("order",help.env),S=S,N=N,q=q,no.cond.dens=cond.dens,D.struc=D.struc,type=type))
}


if(type=="Rvine") {
vine <- list()
SSi <-  list()

help.env <- new.env()
#if(is.null(lam.vec)&l.search) lam.vec<-c(10,seq(50,1550,by=250))
assign("lam1.vec1",lam1.vec,help.env)
assign("lam2.vec2",lam2.vec,help.env)
assign("fix.lambda",fix.lambda,help.env)
assign("lambda.search",lambda.search,help.env)
assign("test.ind",test.ind,help.env)
assign("test.cond",test.cond,help.env)
assign("S",S,help.env)
assign("max.iter",max.iter,help.env)
assign("p",length(S),help.env)
assign("U",U,help.env)
assign("lambda",lambda,help.env)
assign("base",base<-"B-spline",help.env)
assign("d",d,help.env)
assign("d2",d2,help.env)
assign("D",D,help.env)
assign("D3",D3,help.env)
assign("base",base,help.env)
assign("m",m,help.env)
assign("q",q,help.env)
dd <- (2**d)+1 # Anzahl Knoten
ddb <- dd # Anzahl Basisfunktionen
assign("order.stat","cAIC",help.env)
assign("selec","cAIC",help.env)
assign("id",id,help.env)
assign("test.ind",test.ind,help.env)
D.struc<-1
assign("D.struc",D.struc,help.env)
if(!is.null(RVM)) assign("RVM",reRVineMatrix(help.env),help.env)

order.vine(help.env,test.ind=test.ind)

for ( i in 1:length(S))
   {
     vine.knot <-  list(j1=S[i],j2=NULL,D=NULL,v=NULL, U = U[,S[i]],follow=c())
     SSi <- append(SSi, list(vine.knot))
   }
vine <- append(vine, list(SSi))
rm(SSi)
level <-  1
log.like.vec<-AIC.vec<-cAIC.vec<-c()
  log.like <- AIC <- cAIC <- cond.dens<-0
pairs.fit <- get("pairs.fit",help.env)
pairs.new <- get("pairs.new",help.env)

mcoptions<-list(preschedule=FALSE)
vine <- append(vine, list(foreach(i=2:length(S),.combine=list,.multicombine=TRUE,.options.multicore=mcoptions) %dopar% {
   index.j1 <- pairs.fit[i-1,1]
   index.j2 <- pairs.fit[i-1,2]
   if(get("selec",help.env)=="cAIC") {
     for(kk in 1:dim(pairs.new)[1]) {
        if(all(pairs.new[kk,]==pairs.fit[i-1,])) model.l <- get("fit.level2",help.env)[[kk]]
     }
   }
   if(get("selec",help.env)=="ken.tau") model.l <- get("fit.level2",help.env)[[i-1]]
   dim2<-length(model.l)
   vine.knot <-  list(j1=index.j1,j2=index.j2,D=NULL,v=model.l$v,U=U[,c(index.j1,index.j2)],log.like = model.l$log.like, AIC=model.l$AIC,cAIC=model.l$cAIC,lambda=model.l$lambda,f.hat=model.l$f.hat,Index.basis.D=model.l$Index.basis.D,ddb=model.l$ddb,cond=FALSE)
}))
foreach(i=2:length(S),.combine=list,.multicombine=TRUE,.options.multicore=mcoptions) %dopar% {
   index.j1 <- pairs.fit[i-1,1]
   index.j2 <- pairs.fit[i-1,2]
   vine[[level]][[index.j1]]$follow <- c(vine[[level]][[index.j1]]$follow,i-1)
   vine[[level]][[index.j2]]$follow <- c(vine[[level]][[index.j2]]$follow,i-1)
   vine[[level+1]][[i-1]]$before <- c(vine[[level]][[i-1]]$before,pairs.fit[i-1,1],pairs.fit[i-1,2])
}

rm(list=c("fit.level2"),envir=help.env)
level <-  2
assign("level",level,help.env)
for(i in 1:length(vine[[level]])) {
      log.like <- log.like+vine[[level]][[i]]$log.like
      log.like.vec[i]<-vine[[level]][[i]]$log.like
      AIC <- AIC+vine[[level]][[i]]$AIC
      AIC.vec[i]<-vine[[level]][[i]]$AIC
      cAIC <- cAIC+vine[[level]][[i]]$cAIC
      cAIC.vec[i]<-vine[[level]][[i]]$cAIC
}
    count<-length(vine[[level]])

assign("rest.indep",FALSE,help.env)
  for(level in 3:length(S)) {
    print(paste("level=",level,sep=""))
    assign("level",level,help.env)
    Tree.l1 <- vine[[level-1]]
      order.vine.level(tree=Tree.l1,help.env)
      pairs.fit <- get("pairs.fit",help.env)
      pairs.new <- get("pairs.new",help.env)
      vine <- append(vine, vinepart <- list(foreach(i=1:dim(pairs.fit)[1],.combine=list,.multicombine=TRUE,.options.multicore=mcoptions) %do%  {
         help.j1 <- c(Tree.l1[[pairs.fit[i,1]]]$j1,Tree.l1[[pairs.fit[i,1]]]$j2,Tree.l1[[pairs.fit[i,1]]]$D)
         help.j2 <- c(Tree.l1[[pairs.fit[i,2]]]$j1,Tree.l1[[pairs.fit[i,2]]]$j2,Tree.l1[[pairs.fit[i,2]]]$D)
         index.j1 <- help.j1[!help.j1%in%help.j2]#Vorgaenger
         index.j2 <- help.j2[!help.j2%in%help.j1]#
         D.help <- c(Tree.l1[[pairs.fit[i,1]]]$D,Tree.l1[[pairs.fit[i,2]]]$D)
         D <- sort(unique(c(help.j1[help.j1%in%help.j2],D.help)))
         if(get("selec",help.env)=="cAIC") {
           if(dim(pairs.new)[1]==1) model.l<-get(paste("fit.level",level,sep=""),help.env)
           if(dim(pairs.new)[1]>1) {
             for(kk in 1:dim(pairs.new)[1]) {
                if(all(pairs.new[kk,]==pairs.fit[i,])) model.l <- get(paste("fit.level",level,sep=""),help.env)[[kk]]
             }
           }
         }      
         if(get("selec",help.env)=="ken.tau") model.l <- get(paste("fit.level",level,sep=""),help.env)[[i]] 
         list(j1=index.j1,j2=index.j2,D=D,U=get("Y",model.l),v=get("ck.val",model.l),log.like = get("log.like",model.l), AIC=get("AIC",model.l),cAIC=get("cAIC",model.l),lambda=get("lambda",model.l),f.hat=get("f.hat.val",model.l),Index.basis.D=get("Index.basis.D",model.l),ddb=get("ddb",model.l),cond=get("cond",model.l),prcomp.D=get("prcomp.D",model.l),indep=get("indep",model.l))
      }))
      if(dim(pairs.new)[1]==1) {
         vine[[level]]<-list(vine[[level]])
         vinepart<-list(vinepart)
      }
      foreach(i=1:dim(pairs.fit)[1],.options.multicore=mcoptions,.multicombine=TRUE) %do% {
         index.j1 <- pairs.fit[i,1]
         index.j2 <- pairs.fit[i,2]
         vine[[level-1]][[index.j1]]$follow <- c(vine[[level-1]][[index.j1]]$follow,i)
         vine[[level-1]][[index.j2]]$follow <- c(vine[[level-1]][[index.j2]]$follow,i)
         vine[[level]][[i]]$before <- c(vine[[level]][[i]]$before,pairs.fit[i,1],pairs.fit[i,2])
      }
      add.VineMatrix(vinepart[[1]],help.env,level)
      for(i in 1:length(vinepart[[1]])) {
        log.like <- log.like+vinepart[[1]][[i]]$log.like
        log.like.vec[i+count]<-vinepart[[1]][[i]]$log.like
        AIC <- AIC+vinepart[[1]][[i]]$AIC 
        AIC.vec[i+count]<-vinepart[[1]][[i]]$AIC
        cAIC <- cAIC+vinepart[[1]][[i]]$cAIC
        cAIC.vec[i+count]<-vinepart[[1]][[i]]$cAIC
        cond.dens<-cond.dens+vinepart[[1]][[i]]$cond
      }
      count<-count+i
      rm(list=c(paste("fit.level",level,sep="")),pos=help.env)
      if(all(foreach(i=1:length(vine[[level]]),.combine=c) %do% vine[[level]][[i]]$indep)) assign("rest.indep",TRUE,help.env)
  }
  RVineMatrix(help.env)
  data.est <- rep(1,dim(data)[1])
  for(i in 2:length(S))  {
    for(j in 1:length(vine[[i]]))  {
      data.est <- data.est*vine[[i]][[j]]$f.hat
    }
  }
  class(vine) <- "pencopulaCond"
return(list(Dvine=vine,log.like=log.like,log.like.vec=log.like.vec,AIC=AIC,AIC.vec=AIC.vec,cAIC=cAIC,cAIC.vec=cAIC.vec,d=get("d",help.env),d2=d2,D=get("D",help.env),D3=D3,S=S,N=N,base=base,q=q,no.cond.dens=cond.dens,D.struc=D.struc,type=type,VineMatrix=get("VineMatrix",help.env)))
}
}

