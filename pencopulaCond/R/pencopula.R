pencopula <- function(data,d=3,D=d,q=1,base="B-spline",max.iter=20,test.ind=FALSE,
                    lambda=c(100,100),pen.order=2,data.frame=parent.frame(),cond=FALSE,fix.lambda=FALSE,id=NULL) {
  penden.env <- new.env()
  assign("D.struc",1,penden.env)
  assign("id",id,penden.env)
  assign("frame",data.frame,penden.env)
  assign("Y",data,penden.env)
  assign("vec2",vec2<-NULL,penden.env)
  assign("lam.equal",TRUE,penden.env)
  assign("fix.lambda",fix.lambda,penden.env)
  if(is.matrix(data)|is.data.frame(data)) den.form <-  pendenForm(penden.env)
  else stop(" 'data' is not a matrix or a data frame. Create 'data' column by column.")

  if(is.null(d)) d <- 3 # Anzahl Halbierungen des Intervals [0,1]
  if(is.null(D)) D <- d # max. Hierachiestufe

  if(base=="B-spline" & D<d) d <- D

  assign("D",D,penden.env)
  assign("d",d,penden.env)
  assign("pen.order",pen.order,penden.env)
  assign("alpha",alpha<-0,penden.env)
  assign("symmetric",symmetric<-TRUE,penden.env)
  assign("no",FALSE,penden.env)
  assign("max.iter",max.iter,penden.env)
  assign("base",base,penden.env)
  assign("cond",cond,penden.env)
  assign("adapt.grid",adapt.grid<-FALSE,penden.env)
  assign("plot.bsp",plot.bsp<-FALSE,penden.env)
  assign("try.lambda",FALSE,penden.env)
  assign("margin.normal",FALSE,penden.env)
  assign("ind.val",c(),penden.env)
  assign("n",dim(data)[1],penden.env)

  p <- get("p",penden.env)  #p Dimension der Kovariablen
  
  if(base=="B-spline") {
    dd <- (2**d)+1 # Anzahl Knoten
    ddb <- dd + (q-1) # Anzahl Basisfunktionen
    if(is.null(q)) q <- 1 # Grad des B spline
    assign("q",q,penden.env)
  }
  assign("dd",dd,penden.env)
  assign("ddb",ddb,penden.env)
  
  assign("D",D,penden.env) #max. Hierarchiestufe

  if(base=="B-spline") dimension <- c(rep(0,q+1),rep(1:d,2**(0:(d-1))))

  if(base=="B-spline") {
    #if(is.null(lambda)) lambda <- rep(10000,p)
    if(!is.null(lambda)) if(length(lambda)<p | length(lambda)>p) stop("length of lambda is wrong")
  }

  assign("lambda",as.vector(lambda),penden.env)
  assign("dimension",dimension,penden.env)
    
# Dimension gibt die Hierarchiestufe an, aus der der hierarchische B-spline berechnent wird
  
  ##################

  if(base=="B-spline") {  
  #D maximale Hierarchiestufe
    DIMENSION <- dimension
    Index.basis <- matrix(1:ddb)
    index.sparse <- DIMENSION <= D
    Index.basis.D <- matrix(Index.basis[index.sparse,])
    DIMENSION <- DIMENSION[index.sparse]
    
    for ( j in 2:p)
      {
                                        #print(j)
        DIMENSION.j <-  kronecker(matrix(1,ddb,1),DIMENSION) + kronecker( dimension, matrix(1, length(DIMENSION),1))
        Index.basis.plus.1 <- matrix(NA, dim(Index.basis.D)[1] * ddb , j)
        Index.basis.plus.1[,j] <- kronecker(matrix(1:ddb), matrix(1,dim(Index.basis.D)[1],1))
        Index.basis.plus.1[, 1:(j-1)] <-  kronecker(matrix(1, ddb,1),Index.basis.D)
        index.sparse <- DIMENSION.j <= D
        Index.basis.D <- Index.basis.plus.1[index.sparse,]
        DIMENSION <- DIMENSION.j[index.sparse]
      }
    DD <- dim(Index.basis.D)[1] # Dimension of sparse grid basis
    assign("DD",DD,penden.env) # DD Anzahl Koeffizienten
  }

  assign("Index.basis.D",Index.basis.D,penden.env)
  
  
  ###################

  # Matrix zur Erstellung der marginalen Spline Koeffizienten
  # 
  j <- 1
  T.marg <- array(NA, dim=c(ddb,DD,p))

  for ( j in 1:p)
    {
      for ( l in 1:ddb)
        {
          T.marg[l,,j] <- (Index.basis.D[,j] == l)+0
        }
    }
  assign("T.marg",T.marg,penden.env)

  ####################
 
  knots.start(penden.env)

  ####################

  if(cond&p==3) {
    T.marg2 <- array(NA, dim=c(ddb^2, DD,2))
    cond.par1 <- c(1, 3)
    cond.par2 <- c(2, 3)
    count <- 1
    ind.cal<-c()
    for (l in 1:ddb) {
     for (k in 1:ddb) {
       T.marg2[count, ,1] <- (Index.basis.D[,cond.par1[1]] == l & Index.basis.D[,cond.par1[2]] == k) + 0
       count <- count + 1
       }
    }
    count<-1
    for (l in 1:ddb) {
     for (k in 1:ddb) {
       T.marg2[count, ,2] <- (Index.basis.D[,cond.par2[1]] == l & Index.basis.D[,cond.par2[2]] == k) + 0
       count <- count + 1
       }
    }
    ind1<-ind2<-c()
    for(l in 1:(ddb^2)) if(all(T.marg2[l,,1]==0)) ind1<-c(ind1,l)
    for(l in 1:(ddb^2)) if(all(T.marg2[l,,2]==0)) ind2<-c(ind2,l)
    l.new <- ddb^2-length(ind1)
    if(!is.null(ind1)|!is.null(ind2)) {
          T.marg2.help<-array(NA,dim=c(l.new,DD,2)) 
          T.marg2.help[,,1]<-T.marg2[-ind1,,1]
          T.marg2.help[,,2]<-T.marg2[-ind2,,2]
          assign("T.marg2", T.marg2.help, penden.env)
          rm(T.marg2.help)
        }
     else assign("T.marg2", T.marg2, penden.env)
  }

  if(cond&p==4) {
    #browser()
    T.marg2 <- array(NA, dim=c(ddb^3, DD,2))
    cond.par1 <- c(1, 3, 4)
    cond.par2 <- c(2, 3, 4)
    count <- 1
    ind.cal<-c()
    for (l in 1:ddb) {
      for (k in 1:ddb) {
        for(mm in 1:ddb) {
          T.marg2[count, ,1] <- (Index.basis.D[,cond.par1[1]] == l & Index.basis.D[,cond.par1[2]] == k& Index.basis.D[,cond.par1[3]] == mm) + 0
          T.marg2[count, ,2] <- (Index.basis.D[,cond.par2[1]] == l & Index.basis.D[,cond.par2[2]] == k& Index.basis.D[,cond.par2[3]] == mm) + 0
          count <- count + 1
        }
      }
    }
    ind1<-ind2<-c()
    for(l in 1:(ddb^3)) {
       if(all(T.marg2[l,,1]==0)) ind1<-c(ind1,l)
       if(all(T.marg2[l,,2]==0)) ind2<-c(ind2,l)
    }
    l.new <- ddb^3-length(ind1)
    if(!is.null(ind1)|!is.null(ind2)) {
          T.marg2.help<-array(NA,dim=c(l.new,DD,2)) 
          T.marg2.help[,,1]<-T.marg2[-ind1,,1]
          T.marg2.help[,,2]<-T.marg2[-ind2,,2]
          assign("T.marg2", T.marg2.help, penden.env)
          rm(T.marg2.help)
        }
     else assign("T.marg2", T.marg2, penden.env)
  }
 
  #####################

  tilde.Psi.d <-  array(NA, dim=c(get("n",penden.env),ddb,p))
 
  for (j in 1:p)
    {
      tilde.Psi.d[,,j] <-  hierarch.bs(get("Y",penden.env)[,j], d = d, plot.bsp = plot.bsp,typ=3,penden.env,int=FALSE)$B.tilde
    }
  assign("tilde.Psi.d",tilde.Psi.d,penden.env)
  rm(tilde.Psi.d)
  assign("tilde.PSI.d.D",get("tilde.Psi.d",penden.env)[,Index.basis.D[,1],1],penden.env)
  for (j in 2:p)
    {
      assign("tilde.PSI.d.D",get("tilde.PSI.d.D",penden.env) * get("tilde.Psi.d",penden.env)[,Index.basis.D[,j],j],penden.env)
    }
  #assign("tilde.PSI.d.D",get("tilde.PSI.d.D",penden.env),penden.env)
  #####################
 
  #startwerte und gitter berechnen

  start.valgrid(penden.env)

  #####################################

  # Allgemein: Die Restriktionsmatrizen sind als Array aufgefasst:
  
  A <- array(NA, dim=c(get("ddb",penden.env),DD,p))

  for ( j in 1:p)
    {
      A[,,j] <- get("tilde.Psi.knots.d",penden.env) %*% T.marg[,,j]
    }
  assign("A.Restrict",A,penden.env)
  rm(A)
  if(cond){
  A2 <- array(NA, dim=c(dim(get("tilde.PSI.knots.D",penden.env))[1],DD,2))
  for ( j in 1:2)
    {
      A2[,,j] <- get("tilde.PSI.knots.D",penden.env) %*% get("T.marg2",penden.env)[,,j]
    }
#  #A2<-get("tilde.PSI.knots.D",penden.env) %*% T.marg2
  assign("A2.Restrict",A2,penden.env)
  rm(A2)
  }
  # Nun muss gelten A[,,j] %*% c = 1 fuer alle j

  #############################
  penalty.matrix(penden.env=penden.env)

  #############################
  ######### independence model ###
  if(test.ind) {
     model.l<-new.env()
     assign("lambda",lambda,model.l)
     assign("tilde.PSI.d.D",get("tilde.PSI.d.D",penden.env),model.l)
     assign("ck.val",get("ck.val.start",penden.env),model.l)
     assign("f.hat.val",get("tilde.PSI.d.D",penden.env)%*%get("ck.val.start",penden.env),model.l)
     assign("log.like",sum(sapply(get("f.hat.val",model.l),log)),model.l)
     assign("log.like.temp",get("log.like",model.l),model.l)
     assign("pen.log.like",get("log.like",model.l)-0.5*get("lambda",penden.env)[1]*(t(get("ck.val.start",penden.env))%*%get("DDD.sum",penden.env)%*%get("ck.val.start",penden.env)),model.l)
     assign("DDD.sum",get("DDD.sum",penden.env),model.l)
     assign("DDD",get("DDD",penden.env),model.l)
     assign("marg.log.like.temp",0,model.l)
     assign("marg.log.like",0,model.l)
     assign("ind.val",c(),model.l)
     assign("cond",FALSE,model.l)
     Derv1(model.l)
     Derv2(model.l)
     assign("n",get("n",penden.env),model.l)
     my.IC(model.l,temp=FALSE)
     assign("Y",get("Y",penden.env),model.l)
     assign("indep.model",model.l,penden.env)
  }
  ################################


  liste <- matrix(0,1,4+DD+p)
  lam <- coef <- c()
  for(i in 1:p) lam[i] <- paste("lambda.",i,sep="")
  for(j in 1:DD) coef[j] <- paste("b.",j,sep="")
  colnames(liste) <- c("pen.log.like","log.like","marg.log.like",lam,"cAIC",coef)
  help.str <- paste("d=",get("d",penden.env),"D=",get("D",penden.env),"lambda=",get("lambda",penden.env)[1],sep="")
  assign("help.str",help.str,penden.env)
  assign("new.liste",liste,penden.env)

  #if(gold) gold.cal(penden.env,lambda1=lambda[1],lambda2=lambda[2],eps=0.05,step.size=(lambda[2]-lambda[1])*0.05)

  calculate(penden.env)
  assign("indep",FALSE,penden.env)
  if(test.ind&(get("log.like",penden.env)/get("n",penden.env))<0.001) {
        #browser()
        assign("liste",get("liste",penden.env),model.l)
        assign("indep",TRUE,model.l)
        assign("lambda",get("lambda",penden.env),model.l)
        print("indep model selected")
        class(model.l) <- "pencopula"
        return(model.l)
  }
  
  class(penden.env) <- "pencopula"
  return(penden.env)
}
