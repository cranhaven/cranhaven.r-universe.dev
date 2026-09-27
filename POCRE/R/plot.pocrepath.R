plot.pocrepath<-function(x, which=1:3, cex=.5, lwd=1, ...)
{
  # Plot lambda vs. beta, and lambda vs. #[beta!=0]
  if( any(which==1) )
    plotbetanzbeta(ppojb=x, cex=.5, lwd=1, ...)

  # Plot lambda vs. beta, and gamma vs. R^2
  if( any(which==2) )
    plotbetarsq(ppojb=x, cex=.5, lwd=1, ...)

  # Plot lambda vs. R^2, and gamma vs. #[beta!=0]
  if( any(which==3) )
    plotrsqnzbeta(ppojb=x, cex=.5, lwd=1, ...)
}



# Plot lambda vs. beta, and lambda vs. #[beta!=0]
plotbetanzbeta<-function(ppojb, cex=.5, lwd=1, ...)
{
  eps <- .Machine$double.eps
  nlambda <- length(ppojb)
  temp <- NULL
  for(i in 1:nlambda){
    temp <- c(temp,ppojb[[i]]$lambda)
  }
  lambda <- temp
  p <- dim(ppojb[[1]]$beta)[1]
  ny <- dim(ppojb[[1]]$beta)[2]
  nzbeta <-NULL

  plot <- ifelse(ny==1,1,2)

  pl<-list()
  ip<-list()

  betanzbeta<-function(k){
    beta<-matrix(0,p,nlambda)
    for(j in 1:nlambda){
      beta[,j] <- ppojb[[j]]$beta[,k]
      nzbeta[j] <- sum(abs(ppojb[[j]]$beta[,k])>eps)
    }

    beta_C <- matrix(t(beta),dim(beta)[1]*dim(beta)[2],1)
    lambda_C <- rep(lambda,dim(beta)[1])
    group_C <- seq(1,dim(beta)[1])
    group_C <- rep(group_C,each=dim(beta)[2])
    group_C <- as.character(group_C)
    plotdata <- data.frame(lambda_C,beta_C,group_C)
    pl<-ggplot(...) +
      geom_line(aes(x=lambda_C,y=beta_C,group=group_C),linetype = "dashed",color=group_C,size=lwd) +
      geom_point(aes(x=lambda_C,y=beta_C,group=group_C),color=group_C,shape=4,size=cex) +
      geom_line(aes(x=lambda,y=nzbeta/(max(nzbeta)/max(beta_C)),color='#[beta!=0]'),size=lwd) +
      geom_point(aes(x=lambda,y=nzbeta/(max(nzbeta)/max(beta_C)),color='#[beta!=0]'),size=cex) +
      labs(y="beta",x="lambda",title=paste('Y_',k,sep="")) +
      scale_y_continuous(sec.axis=sec_axis(~.*max(nzbeta)/max(beta_C),name="#[beta!=0]"))+
      scale_color_discrete(name=NULL)+
      theme(legend.justification=c(0,0),legend.position=c(0,0),plot.title = element_text(hjust=0.5))

    return(pl)
  }

  pp<-lapply(1:ny,betanzbeta)
  for(i in 1:ny) print(pp[[i]])
}


# Plot lambda vs. beta, and lambda vs. R^2
plotbetarsq<-function(ppojb, cex=.5, lwd=1, ...)
{
  eps <- .Machine$double.eps
  nlambda <- length(ppojb)
  temp <- NULL
  for(i in 1:nlambda){
    temp <- c(temp,ppojb[[i]]$lambda)
  }
  lambda <- temp
  p <- dim(ppojb[[1]]$beta)[1]
  ny <- dim(ppojb[[1]]$beta)[2]
  rsq <- NULL
  for(i in 1:nlambda){
    rsq <- cbind(rsq,ppojb[[i]]$rsq)
  }
  rsq <- t(matrix(rsq,ny,nlambda))

  plot <- ifelse(ny==1,1,2)

  betarsq<-function(k){
    beta <- matrix(0,p,nlambda)
    for(j in 1:nlambda){
      beta[,j] <- ppojb[[j]]$beta[,k]
    }
    temprsq <- (rsq[,k])
    beta_C <- matrix(t(beta),dim(beta)[1]*dim(beta)[2],1)
    lambda_C <- rep(lambda,dim(beta)[1])
    group_C <- seq(1,dim(beta)[1])
    group_C <- rep(group_C,each=dim(beta)[2])
    group_C <- as.character(group_C)

    plotdata <- data.frame(lambda_C,beta_C,group_C)
    pl<-ggplot(...) +
      geom_line(aes(x=lambda_C,y=beta_C,group=group_C),linetype = "dashed",color=group_C,size=lwd) +
      geom_point(aes(x=lambda_C,y=beta_C,group=group_C),color=group_C,shape=4,size=cex) +
      geom_line(aes(x=lambda,y=temprsq/(max(temprsq)/max(beta_C)),color='R^2'),size=lwd) +
      geom_point(aes(x=lambda,y=temprsq/(max(temprsq)/max(beta_C)),color='R^2'),size=cex) +
      labs(y = "beta", x = "lambda",title=paste('Y_', k,sep=""))+
      scale_y_continuous(sec.axis = sec_axis(~.*max(temprsq)/max(beta_C),name="R^2"))+
      scale_color_discrete(name=NULL)+
      theme(legend.justification=c(0,0),legend.position=c(0,0),plot.title = element_text(hjust=0.5))
    return(pl)
  }

  pp<-lapply(1:ny,betarsq)
  for(i in 1:ny) print(pp[[i]])
}



# Plot lambda vs. R^2, and lambda vs. #[beta!=0]
plotrsqnzbeta<-function(ppojb, cex=.5, lwd=1, ...)
{
  eps <- .Machine$double.eps
  nlambda <- length(ppojb)
  temp <- NULL
  for(i in 1:nlambda){
    temp <- c(temp,ppojb[[i]]$lambda)
  }
  lambda <- temp
  p <- dim(ppojb[[1]]$beta)[1]
  ny <- dim(ppojb[[1]]$beta)[2]
  nzbeta <- NULL

  plot <- ifelse(ny==1,1,2)

  rsq <- NULL
  for(i in 1:nlambda){
    rsq <- cbind(rsq,ppojb[[i]]$rsq)
  }
  rsq <- t(matrix(rsq,ny,nlambda))

  rsqnzbeta<-function(k){
    beta <- matrix(0,p,nlambda)
    for(j in 1:nlambda){
      nzbeta[j] <- sum(abs(ppojb[[j]]$beta[,k])>eps)
    }
    temprsq <-rsq[,k]
    pl<-ggplot(...) +
      geom_line(aes(x=lambda,y=temprsq,color='R^2'),size=lwd) +
      geom_point(aes(x=lambda,y=temprsq,color='R^2'),size=cex) +
      labs(title=paste('Y_',k,sep=""),y="R^2",x="lambda") +
      geom_line(aes(x=lambda,y=nzbeta*max(temprsq)/max(nzbeta),color='#[beta!=0]'),linetype = "dashed",size=lwd) +
      geom_point(aes(x=lambda,y=nzbeta*max(temprsq)/max(nzbeta),color='#[beta!=0]'),shape=4,size=cex) +
      scale_y_continuous(sec.axis = sec_axis(~.*max(nzbeta)/max(temprsq),name="#[beta!=0]"))+
      scale_color_discrete(name=NULL)+
      theme(legend.justification=c(0,0),legend.position=c(0,0),plot.title=element_text(hjust=0.5))

    return(pl)
  }

  pp<-lapply(1:ny,rsqnzbeta)
  for(i in 1:ny) print(pp[[i]])
}
