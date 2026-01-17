cap_design.base<-function(area,params,w,plot)
{
   labels<-matrix(NA,3,length(params))
   ranges<-list()
   title<-c()
   for(i in 1:length(params))
   {
      labels[1,i]<-(params[[i]])[[1]]$operation$label
      for (j in 1:length(area$operation[[3]]))
      {
         if((area$operation[[3]])[[j]]$operation$label==labels[1,i])
         {
            labels[2,i]<-j
            title<-c(title,(area$operation[[3]])[[j]]$operation$name)
         }
      }
      for (j in 1:length(area$operation[[8]]))
      {
         if((area$operation[[8]])[[j]]$operation$label==labels[1,i])
         {
            labels[3,i]<-j
            title<-c(title,(area$operation[[8]])[[j]]$operation$name)
         }
      }
      ranges[[i]]<-(params[[i]])[[2]]
   }
   f<-function(area,factor,labels)
   {
      for(i in 1:length(factor))
      {
         address<-labels[2:3,i]
         if(which(!is.na(address))==1) {area$operation$reservoirs[[address[1]]]$operation$geometry$capacity<-(area$operation[[3]])[[address[1]]]$operation$geometry$capacity*factor[i]}
         if(which(!is.na(address))==2) {area$operation$demands[[address[2]]]$operation$demandTS<-(area$operation[[8]])[[address[2]]]$operation$demandTS*factor[i]}
      }
      return(area)
   }
   g<-function(area,codes)
   {
      title<-c()
      for(j in 1:length(codes))
      {
         for (i in 1:length(area$operation$demands))
         {
            if(any(area$operation$demands[[i]]$operation$suppliers==codes[j]))
            {
               title<-c(title,area$operation$demands[[i]]$operation$name)
            }
         }
      }
      return(title)
   }

   grids<-as.matrix(expand.grid(ranges))
   colnames(grids)<-title
   titleDemand<-title[which(!is.na(labels[3,]))]
   titleReservoir<-title[which(!is.na(labels[2,]))]
   if(any(!is.na(labels[3,])))
   {
      payoff<-matrix(NA,nrow(grids),4*sum(!is.na(labels[3,]))+1)
      payofftitle<-c('Vul','Rel','Res','Sus')
      Title<-c()
      for(i in 1:sum(!is.na(labels[3,])))
      {
         Title<-c(Title,paste(titleDemand[i],'(',payofftitle,')',sep=''))
      }
      Title<-c(Title,'RSus')
      colnames(payoff)<-Title
   }
   if(all(!is.na(labels[2,])))
   {
      codes<-labels[1,which(!is.na(labels[2,]))]
      titleD<-g(area,codes)
      payofftitle<-c('Vul','Rel','Res','Sus')
      Title<-c()
      for(i in 1:length(titleD))
      {
         Title<-c(Title,paste(titleD[i],'(',payofftitle,')',sep=''))
      }
      Title<-c(Title,'RSus')
      payoff<-matrix(NA,nrow(grids),length(Title))
      colnames(payoff)<-Title
   }

   for(i in 1:nrow(grids))
   {
      factor<-grids[i,1:ncol(labels)]
      names(factor)<-NULL
      area<-f(area,factor,labels)
      simulated<-sim(area)
      rrv<-risk(simulated)
      if(any(!is.na(labels[3,])))
      {
         for(j in 1:sum(!is.na(labels[3,])))
         {
            payoffind<-rrv[,match(titleDemand[j],colnames(rrv))]
            names(payoffind)<-NULL
            payoff[i,grep(titleDemand[j],colnames(payoff))]<-c(payoffind,NA)
         }
      }
      if(all(!is.na(labels[2,])))
      {
         for(j in 1:length(titleD))
         {
            payoffind<-rrv[,match(titleD[j],colnames(rrv))]
            names(payoffind)<-NULL
            payoff[i,grep(titleD[j],colnames(payoff))]<-c(payoffind,NA)
         }
      }
   }
   for (i in 1:((ncol(payoff)-1)/4))
   {
      x<-payoff[,((i-1)*4+1):(4*i-1)]
      if(!all(x[,1]==0))
      {
         x[,1]<-1-(x[,1]/max(x[,1]))
      }
      if(!all(x[,1]==0))
      {
         x[,1]<-1-(x[,1]/max(x[,1]))
      }
      payoff[,i*4]<-ifelse(x[,1]==0,x[,2]*x[,3],x[,1]*x[,2]*x[,3])
   }
   if(all(is.na(w))){w<-rep(1/((ncol(payoff)-1)/4),((ncol(payoff)-1)/4))}
   payoff[,ncol(payoff)]<-apply(payoff[,seq(4,ncol(payoff),4),drop=FALSE]*w,1,sum)
   if(plot)
   {
      if(ncol(grids)>1)
      {
         expand.grid.unique <- function(x, y, include.equals=FALSE)
         {
            x <- unique(x); y <- unique(y)
            g <- function(i)
            {
               z <- setdiff(y, x[seq_len(i-include.equals)])
               if(length(z)) cbind(x[i], z, deparse.level=0)
            }
            do.call(rbind, lapply(seq_along(x), g))
         }
         vars<-expand.grid.unique(1:ncol(grids),1:ncol(grids))
         for (i in 1:nrow(vars))
         {
            if (nrow(vars)>1)
            {
               oask <- devAskNewPage(TRUE)
               on.exit(devAskNewPage(oask))
            }
            xlab<-paste(colnames(grids)[vars[i,1]],'_factor')
            ylab<-paste(colnames(grids)[vars[i,2]],'_factor')
            persp(cbind(grids[,vars[i,1]],grids[,vars[i,2]],payoff[,ncol(payoff)]),xlab=paste('\n\n',xlab,sep=''),ylab=paste('\n\n',ylab,sep=''),zlab=paste('\n\n','Relavive Sustainability',sep=''),phi=30,theta=45,col = "lightblue",ticktype = "detailed")->res
         }
      }
      if(ncol(grids)==1)
      {
         xlab=colnames(grids)
         ylab='Relavive Sustainability'
         plot(grids,payoff[,ncol(payoff)],col=2,lwd=2,type='l',xlab=xlab,ylab=ylab)
      }
   }
   cbind(grids,payoff)
}