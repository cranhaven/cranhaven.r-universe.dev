plot.LMlatentcont<-function(x,what = c("modSel", "density", "transitions","marginal"),components=NULL,verbose=interactive(),...)
  {
  object <- x
  what <- match.arg(what,several.ok = TRUE)
  n <- object$n; TT <- object$TT; r <- dim(object$Mu)[1]; k <- object$k
  if(k==1) pmarg <- 1
  else{
    PMarg <- array(0,c(n,k,TT))
    PMarg[,,1] <- as.matrix(object$Piv)
    for(i in 1:n) for(t in 2:TT) PMarg[i,,t]= t(object$PI[,,i,t])%*%PMarg[i,,t-1]
    Pmarg <-apply(PMarg,c(2,3),mean)
    pmarg <- apply(PMarg,2,mean)
  }
  out <- object

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
      axis(side=1,at=kv)
      legend("topright",legend=c("BIC","AIC"),col=c(2,4),lty=1,bty="n")
    }
      if(what[choice] == "density"){
        #par(mar=c(5, 4, 4, 2) + 0.1,mfrow=c(1,1))
        par(mfrow=c(1,1))
        temp <- getResponses(data = object$data,formula =attributes(object)$responsesFormula)
        if(r==1){
          minY <- c(min(out$Mu)-2.5*sqrt(out$Si))
          maxY <- c(max(out$Mu)+2.5*sqrt(out$Si))
          y <- seq(minY,maxY,by=0.01)
          f <- pmarg[1]*dnorm(y,out$Mu[1],sqrt(out$Si))
          if(k>1) for(u in 2:k) f <- f+pmarg[u]*dnorm(y,out$Mu[u],sqrt(out$Si))
          plot(y,f,type="l",ylab = "density",xlab=colnames(temp$Y),main ="Density overall")
        }else{
          class(out) <- "Mclust"
          out$data <-temp$Y
          colnames(out$data) <- paste(colnames(temp$Y), " (overall)")
          if(r==2){
            minX <- min(out$Mu[1,])-2.5*sqrt(out$Si[1,1])
            maxX <- max(out$Mu[1,])+2.5*sqrt(out$Si[1,1])
            minY <- min(out$Mu[2,])-2.5*sqrt(out$Si[2,2])
            maxY <- max(out$Mu[2,])+2.5*sqrt(out$Si[2,2])
          }
          out$parameters$variance$d <- r
          out$parameters$variance$G <- k
          out$parameters$pro <- pmarg
          out$parameters$mean <- object$Mu
          out$modelName <- "EEE"
          out$parameters$variance$modelName <- "EEE"
          out$parameters$variance$sigma <- array(object$Si,c(r,r,k))
          out$parameters$variance$Sigma <- object$Si
          out$parameters$variance$cholSigma <- chol(object$Si)
          if(r==2) plot(out,what="density",xlim=c(minX,maxX),ylim=c(minY,maxY)) else plot(out,what="density")
        }
      }
      if(what[choice] == "transitions"){
        if(k==1){
          stop("Transition probabilities not available when k=1")
        }
        #par(mar=c(2,1,5,1),mfrow=c(1,1))
        par(mfrow=c(1,1))
        PM<-apply(object$PI[,,,2:TT],c(1,2),mean)
        PM <- round(diag(1/rowSums(PM))%*%PM,2)
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
        plot(1:TT,Pmarg[1,],ylim=c(0,1),xaxt="n",xlab="Time",ylab="Estimated averaged marginal distribution",type="l",lwd=1)
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
    axis(side=1,at=kv)
    legend("topright",legend=c("BIC","AIC"),col=c(2,4),lty=1,bty="n")
  }
    if(any(what == "density")){
      temp <- getResponses(data = object$data,formula =attributes(object)$responsesFormula)
      if(is.null(components)){
        #par(mar=c(5, 4, 4, 2) + 0.1,mfrow=c(1,1))
        par(mfrow=c(1,1))
        if(r==1){
          minY <- c(min(out$Mu)-2.5*sqrt(out$Si))
          maxY <- c(max(out$Mu)+2.5*sqrt(out$Si))
          y <- seq(minY,maxY,by=0.01)
          f <- pmarg[1]*dnorm(y,out$Mu[1],sqrt(out$Si))
          if(k>1) for(u in 2:k) f <- f+pmarg[u]*dnorm(y,out$Mu[u],sqrt(out$Si))
          plot(y,f,type="l",ylab = "density",xlab=colnames(temp$Y),main ="Density overall")
        }else{
          class(out) <- "Mclust"
          out$data <-temp$Y
          colnames(out$data) <- paste(colnames(temp$Y), " (overall)")
          if(r==2){
            minX <- min(out$Mu[1,])-2.5*sqrt(out$Si[1,1])
            maxX <- max(out$Mu[1,])+2.5*sqrt(out$Si[1,1])
            minY <- min(out$Mu[2,])-2.5*sqrt(out$Si[2,2])
            maxY <- max(out$Mu[2,])+2.5*sqrt(out$Si[2,2])
          }
          out$parameters$variance$d <- r
          out$parameters$variance$G <- k
          out$parameters$pro <- pmarg
          out$parameters$mean <- object$Mu
          out$modelName <- "EEE"
          out$parameters$variance$modelName <- "EEE"
          out$parameters$variance$sigma <- array(object$Si,c(r,r,k))
          out$parameters$variance$Sigma <- object$Si
          out$parameters$variance$cholSigma <- chol(object$Si)
          if(r==2) plot(out,what="density",xlim=c(minX,maxX),ylim=c(minY,maxY)) else plot(out,what="density")
        }
      }else{
        nk <- length(components)
        if(nk==1) #par(mar=c(5, 4, 4, 2) + 0.1,mfrow=c(1,1))
          par(mfrow=c(1,1))
        else{
          nr = round(nk/2)
          if((nk/2-nr)==0.5) nr = nr+1
          #par(mar=c(5, 4, 4, 2) + 0.1,mfrow=c(nr,2))
          par(mfrow=c(nr,2))
        }
        if(r==1){
          for(u in components){
            minY <- c(min(out$Mu[u])-2.5*sqrt(out$Si))
            maxY <- c(max(out$Mu[u])+2.5*sqrt(out$Si))
            y <- seq(minY,maxY,by=0.01)
            f <- dnorm(y,out$Mu[u],sqrt(out$Si))
            plot(y,f,type="l",ylab = "density",xlab=colnames(temp$Y),main =paste("Density component ",u),col=u)
          }
        }else{
          class(out) <- "Mclust"
          out$data <-temp$Y
          out$parameters$pro <- 1
          out$parameters$variance$G <- 1
          out$parameters$variance$d <- r
          out$modelName <- "XXX"
          out$parameters$variance$modelName <- "XXX"
          out$G <- 1
          out$parameters$variance$cholsigma <- chol(object$Si)
          out$parameters$variance$sigma <- array(object$Si,c(r,r,1))
          out$parameters$variance$Sigma <- object$Si
          out$parameters$variance$cholSigma <- chol(object$Si)
          for(u in components){
            if(r==2){
              minX <- min(out$Mu[1,u])-2.5*sqrt(out$Si[1,1])
              maxX <- max(out$Mu[1,u])+2.5*sqrt(out$Si[1,1])
              minY <- min(out$Mu[2,u])-2.5*sqrt(out$Si[2,2])
              maxY <- max(out$Mu[2,u])+2.5*sqrt(out$Si[2,2])
            }
            colnames(out$data) <- paste(colnames(temp$Y), " (component ",u,")",sep="")
            out$parameters$mean <- matrix(object$Mu[,u],r,1)
            if(r>2 & nk>1) dev.new()
            if(r==2) plot(out,what="density",col=u+1,xlim=c(minX,maxX),ylim=c(minY,maxY)) else plot(out,what="density",col=u+1)
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
      PM<-apply(object$PI[,,,2:TT],c(1,2),mean)
      PM = round(diag(1/rowSums(PM))%*%PM,2)
      plotmat(t(PM),relsize=0.7,box.col="lightblue",lwd = 1,
              box.lwd = 1,self.cex = 0.8,
              cex.txt = 0.8, box.size = 0.1,box.prop = 0.5,main="Averaged transition probabilities")

    }
    if(any(what  == "marginal")) {
      if(k==1){
        stop("Marginal distribution of the latent states not available when k=1")
      }
      #par(mar=c(5, 4, 4, 2) + 0.1,mfrow=c(1,1))
      par(mfrow=c(1,1))
      plot(1:TT,Pmarg[1,],ylim=c(0,1),xaxt="n",xlab="Time",ylab="Estimated averaged marginal distribution",type="l",lwd=1)
      for(u in 2:k) lines(Pmarg[u,],col=u,lwd=1)
      leg = paste("state",1:k)
      axis(side=1,at=1:TT)
      legend(x="topright",leg,col=1:k,lwd=1,bty="n")
    }

  }

}
