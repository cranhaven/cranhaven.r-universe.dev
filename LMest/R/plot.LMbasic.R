plot.LMbasic<-function(x, what = c("modSel", "CondProb", "transitions","marginal"),verbose=interactive(),...)
  {

  object <- x 
  what <- match.arg(what,several.ok = TRUE)
  n <- object$n; TT <- object$TT;  k <- object$k
  d <- dim(object$Psi)
  nc <- d[1]
  if(length(d)==2) r <- d[2]
  else r <- d[3]


  if(k>1){
    Pmarg <- as.matrix(object$piv)
    for(t in 2:TT) Pmarg= cbind(Pmarg,t(object$Pi[,,t])%*%Pmarg[,t-1])
  }

  if(interactive() & length(what) > 1){
    title <- "LM model plots:"
    # present menu waiting user choice
    choice <- menu(what, graphics = FALSE, title = title)
    while(choice != 0)
    { if(what[choice] == "modSel"){
     # par(mar=c(5, 4, 4, 2) + 0.1,mfrow=c(1,1))
      par(mfrow=c(1,1))
      if(is.null(object$Aic)){
        object$Aic <- object$aic
        names(object$Aic) <- paste("k",k,sep = "=")
        object$Bic <- object$bic
        names(object$Bic) <- paste("k",k,sep = "=")
      }
      ylim1 =  min(c(object$Aic,object$Bic))
      ylim2 = max(c(object$Aic,object$Bic))*1.05
      kv <-as.numeric(substr(names(object$Bic),3,3))
      matplot(kv,cbind(object$Bic,object$Aic),type="b",pch=1:2,lty=1,col=c(2,4),xaxt="n",xlab="Number of states",ylab="",ylim = c(ylim1,ylim2))
      #lines(object$Aic,type="b",pch=2,col=4)
      axis(side=1,at=kv)
      legend("topright",legend=c("BIC","AIC"),col=c(2,4),lty=1,bty="n")

    }
      if(what[choice] == "CondProb"){
        title = "Conditional response probabilities"
        if(r==1){
          if(k==1){
            #par(mfrow=c(1,1),mar=c(4,4,2,0)+0.1)
            par(mfrow=c(1,1))
            pi.class <- matrix(NA,nrow=r,ncol=nc)
            pi.class[1,] <- object$Psi
            ds.plot <- data.frame(Items=as.vector(row(pi.class)),Categories=as.vector(col(pi.class)),value=as.vector(pi.class))
            tt =as.table(ds.plot$value)
            dimnames(tt)[[1]]=0:(nc-1)
            plot(tt, type = "h", col = "blue", lwd = 10,col.axis="blue",bty="n",main=title,ylab="Prob.Cond.",xlab="Item categories",ylim=c(0,1))
          }else{
            #par(mfrow=c(k,1),mar=c(4,4,2,0)+0.1)
            par(mfrow=c(k,1))
            for (u in 1:k) {
              title = paste("State ",u,": Initial probabilities = ",round(object$piv[u],3),sep="")
              pi.class <- matrix(NA,nrow=r,ncol=nc)
              pi.class[1,] <- object$Psi[,u,]
              ds.plot <- data.frame(Items=as.vector(row(pi.class)),Categories=as.vector(col(pi.class)),value=as.vector(pi.class))
              tt =as.table(ds.plot$value)
              dimnames(tt)[[1]]=0:(nc-1)
              plot(tt, type = "h", col = "blue", lwd = 10,col.axis="blue",bty="n",main=title,ylab="Prob.Cond.",xlab="Item categories",ylim=c(0,1))
            }
          }
        }else{
          if(k==1){
            #par(mfrow=c(1,1),mar=c(5,4,4,2)+0.1)
            par(mfrow=c(1,1))
            pi.class <- matrix(NA,nrow=r,ncol=nc)
            for (j in 1:r) pi.class[j,] <- object$Psi[,j]
            dimnames(pi.class) <- list(item=1:r,category=0:(nc-1))
            ds.plot <- data.frame(Items=as.vector(row(pi.class)),Categories=as.vector(col(pi.class)),value=as.vector(pi.class))
            vis <- scatterplot3d(ds.plot,type="h",lwd=7,pch=" ",x.ticklabs=1:r,y.ticklabs=colnames(pi.class),z.ticklabs = "",
                                 xlab="Items",zlab="Cond. prob.",main=title,lab=c(r-1,nc-1),zlim=c(0,1)
                                 ,mar=c(3,3,2,3), highlight.3d=FALSE,col.grid="lightblue",col.axis="lightblue",cex.main=1.5,angle=75,box=FALSE,color=4)
          }else{
            #par(mfrow=c(k,1),mar=c(5,4,4,2)+0.1)
            par(mfrow=c(k,1))
            for (u in 1:k) {
              title = paste("State ",u,": Initial probabilities = ",round(object$piv[u],3),sep="")
              pi.class <- matrix(NA,nrow=r,ncol=nc)
              for (j in 1:r) pi.class[j,] <- object$Psi[,u,j]
              dimnames(pi.class) <- list(item=1:r,category=0:(nc-1))
              ds.plot <- data.frame(Items=as.vector(row(pi.class)),Categories=as.vector(col(pi.class)),value=as.vector(pi.class))
              vis <- scatterplot3d(ds.plot,type="h",lwd=7,pch=" ",x.ticklabs=1:r,y.ticklabs=colnames(pi.class),z.ticklabs = "",
                                   xlab="Items",zlab="Cond. prob.",main=title,lab=c(r-1,nc-1),zlim=c(0,1)
                                   ,mar=c(3,3,2,3), highlight.3d=FALSE,col.grid="lightblue",col.axis="lightblue",cex.main=1.5,angle=75,box=FALSE,color=4)
            }
          }
       }
      }
      if(what[choice] == "transitions"){
        if(k==1){
          stop("Transition probabilities not available when k=1")
        }
        #par(mar=c(2,1,5,1),mfrow=c(1,1))
        par(mfrow=c(1,1))
        PM <- round(apply(object$Pi[,,2:TT],c(1,2),mean),2)
        plotmat(t(PM),relsize=0.7,box.col="lightblue",lwd = 1,
                box.lwd = 1,self.cex = 0.8,
                cex.txt = 0.8, box.size = 0.1,box.prop = 0.5,main="Averaged transition probabilities")

      }
      if(what[choice] == "marginal") {
        if(k==1){
          stop("Marginal distribution of the latent states not available when k=1")
        }
        #par(mar=c(5, 4, 4, 2) + 0.1,mfrow=c(1,1))
        par(mfrow=c(1,1))
        plot(1:TT,Pmarg[1,],ylim=c(0,1),xaxt="n",xlab="Time",ylab="Estimated marginal distribution",type="l",lwd=1)
        for(u in 2:k) lines(Pmarg[u,],col=u,lwd=1)
        leg = paste("state",1:k)
        axis(side=1,at=1:TT)
        legend(x="topright",leg,col=1:k,lwd=1,bty="n")
      }

      # re-present menu waiting user choice
      choice <- menu(what, graphics = FALSE, title = title)
    }
  }
  else
  { if(any(what == "modSel")){
    #par(mar=c(5, 4, 4, 2) + 0.1,mfrow=c(1,1))
    par(mfrow=c(1,1))
    if(is.null(object$Aic)){
      object$Aic <- object$aic
      names(object$Aic) <- paste("k",k,sep = "=")
      object$Bic <- object$bic
      names(object$Bic) <- paste("k",k,sep = "=")
    }
    ylim1 =  min(c(object$Aic,object$Bic))
    ylim2 = max(c(object$Aic,object$Bic))*1.05
    kv <-as.numeric(substr(names(object$Bic),3,3))
    matplot(kv,cbind(object$Bic,object$Aic),type="b",pch=1:2,lty=1,col=c(2,4),xaxt="n",xlab="Number of states",ylab="",ylim = c(ylim1,ylim2))
    #lines(object$Aic,type="b",pch=2,col=4)
    axis(side=1,at=kv)
    legend("topright",legend=c("BIC","AIC"),col=c(2,4),lty=1,bty="n")

  }
    if(any(what == "CondProb")){
      title = "Conditional response probabilities"
      if(r==1){
        if(k==1){
          #par(mfrow=c(1,1),mar=c(4,4,2,0)+0.1)
          par(mfrow=c(1,1))
          pi.class <- matrix(NA,nrow=r,ncol=nc)
          pi.class[1,] <- object$Psi
          ds.plot <- data.frame(Items=as.vector(row(pi.class)),Categories=as.vector(col(pi.class)),value=as.vector(pi.class))
          tt =as.table(ds.plot$value)
          dimnames(tt)[[1]]=0:(nc-1)
          plot(tt, type = "h", col = "blue", lwd = 10,col.axis="blue",bty="n",main=title,ylab="Prob.Cond.",xlab="Item categories",ylim=c(0,1))
        }else{
          #par(mfrow=c(k,1),mar=c(4,4,2,0)+0.1)
          par(mfrow=c(k,1))
          for (u in 1:k) {
            title = paste("State ",u,": Initial probabilities = ",round(object$piv[u],3),sep="")
            pi.class <- matrix(NA,nrow=r,ncol=nc)
            pi.class[1,] <- object$Psi[,u,]
            ds.plot <- data.frame(Items=as.vector(row(pi.class)),Categories=as.vector(col(pi.class)),value=as.vector(pi.class))
            tt =as.table(ds.plot$value)
            dimnames(tt)[[1]]=0:(nc-1)
            plot(tt, type = "h", col = "blue", lwd = 10,col.axis="blue",bty="n",main=title,ylab="Prob.Cond.",xlab="Item categories",ylim=c(0,1))
          }
        }
      }else{
        if(k==1){
          #par(mfrow=c(1,1),mar=c(5,4,4,2)+0.1)
          par(mfrow=c(1,1))
          pi.class <- matrix(NA,nrow=r,ncol=nc)
          for (j in 1:r) pi.class[j,] <- object$Psi[,j]
          dimnames(pi.class) <- list(item=1:r,category=0:(nc-1))
          ds.plot <- data.frame(Items=as.vector(row(pi.class)),Categories=as.vector(col(pi.class)),value=as.vector(pi.class))
          vis <- scatterplot3d(ds.plot,type="h",lwd=7,pch=" ",x.ticklabs=1:r,y.ticklabs=colnames(pi.class),z.ticklabs = "",
                               xlab="Items",zlab="Cond. prob.",main=title,lab=c(r-1,nc-1),zlim=c(0,1)
                               ,mar=c(3,3,2,3), highlight.3d=FALSE,col.grid="lightblue",col.axis="lightblue",cex.main=1.5,angle=75,box=FALSE,color=4)
        }else{
          #par(mfrow=c(k,1),mar=c(5,4,4,2)+0.1)
          par(mfrow=c(k,1))
          for (u in 1:k) {
            title = paste("State ",u,": Initial probabilities = ",round(object$piv[u],3),sep="")
            pi.class <- matrix(NA,nrow=r,ncol=nc)
            for (j in 1:r) pi.class[j,] <- object$Psi[,u,j]
            dimnames(pi.class) <- list(item=1:r,category=0:(nc-1))
            ds.plot <- data.frame(Items=as.vector(row(pi.class)),Categories=as.vector(col(pi.class)),value=as.vector(pi.class))
            vis <- scatterplot3d(ds.plot,type="h",lwd=7,pch=" ",x.ticklabs=1:r,y.ticklabs=colnames(pi.class),z.ticklabs = "",
                                 xlab="Items",zlab="Cond. prob.",main=title,lab=c(r-1,nc-1),zlim=c(0,1)
                                 ,mar=c(3,3,2,3), highlight.3d=FALSE,col.grid="lightblue",col.axis="lightblue",cex.main=1.5,angle=75,box=FALSE,color=4)
          }
        }
      }
    }
    if(any(what == "transitions")) {
      if(k==1){
        stop("Transition probabilities not available when k=1")
      }
      #par(mar=c(2,1,5,1),mfrow=c(1,1))
      par(mfrow=c(1,1))
      TT <- dim(object$Pi)[3]
      PM <- round(apply(object$Pi[,,2:TT],c(1,2),mean),2)
      plotmat(t(PM),relsize=0.7,box.col="lightblue",lwd = 1,
              box.lwd = 1,self.cex = 0.8,
              cex.txt = 0.8, box.size = 0.1,box.prop = 0.5,main="Averaged transition probabilities")
    }
  }
  if(any(what == "marginal")) {
    if(k==1){
      stop("Marginal distribution of the latent states not available when k=1")
    }
    #par(mar=c(5, 4, 4, 2) + 0.1,mfrow=c(1,1))
    par(mfrow=c(1,1))
    plot(1:TT,Pmarg[1,],ylim=c(0,1),xaxt="n",xlab="Time",ylab="Estimated marginal distribution",type="l",lwd=1)
    for(u in 2:k) lines(Pmarg[u,],col=u,lwd=1)
    axis(side=1,at=1:TT)
    leg = paste("state",1:k)
    legend(x="topright",leg,col=1:k,lwd=1,bty="n")
  }

}

