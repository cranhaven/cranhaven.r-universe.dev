plot.TPidm <-
function(x,chosen.tr="ALL",col="black", ...){
    if(!inherits(x,"TPidm")) stop("'x' must be of class 'TPidm'.")
    times<-x$times
    s<-x$s
    t<-x$t
    CI<-x$CI
    
    if(chosen.tr[1]=="ALL") chosen.tr<- x$p.trans
    
    
    
    if(is.null(x$cov)){
      
      if(is.null(col)) col<-c("black","red")
      if(length(col)<2) col<-rep(col,length.out=2)
      
      # CI==TRUE
      if(CI==TRUE){
        
        all.probs<-x$all.probs[,,chosen.tr]
        
        # option 1: individual curves with CI
        if(length(chosen.tr)==1){
          tit<-paste("p",chosen.tr)
          timemax<-max(times)*1.04
          plot(times,all.probs[,"probs"],type="s",xlab="Time",ylab="Probability",ylim=c(0,1),col=col[1],xlim=c(0,timemax),cex.axis=0.7,cex.lab=0.7,main=tit, ...)
          lines(times,all.probs[,"lower"],type="s",lty=3,col=col[2])
          lines(times,all.probs[,"upper"],type="s",lty=3,col=col[2])
          
        }else if(length(chosen.tr)>1 & length(chosen.tr)<=3){
          N<-seq(1,length(chosen.tr),1)
          tpg<-layout(matrix(N,1,length(chosen.tr),byrow=TRUE))
          #layout.show(tpg)
          
          for(i in 1:length(chosen.tr)){
            tit<-paste("p",chosen.tr[[i]])
            timemax<-max(times)*1.04
            plot(times,all.probs[,"probs",chosen.tr[[i]]],type="s",xlab="Time",ylab="Probability",ylim=c(0,1),xlim=c(0,timemax),col=col[1],cex.axis=0.7,cex.lab=0.7,main=tit, ...)
            lines(times,all.probs[,"lower",chosen.tr[[i]]],type="s",lty=3,col=col[2])
            lines(times,all.probs[,"upper",chosen.tr[[i]]],type="s",lty=3,col=col[2]) 
          }
        } else {
          
          n.cols<-3
          n.rows<-ceiling(length(chosen.tr)/n.cols)
          N<-seq(1,length(chosen.tr),1)
          if(length(N)<n.cols*n.rows){
            m<-n.cols*n.rows-length(N)
            N<-c(N,rep(0,m))
          }
          tpg<-layout(matrix(N,n.rows,n.cols,byrow=TRUE))
          #layout.show(tpg)
          
          for(i in 1:length(chosen.tr)){
            tit<-paste("p",chosen.tr[[i]])
            timemax<-max(times)*1.04
            plot(times,all.probs[,"probs",chosen.tr[[i]]],type="s",xlab="Time",ylab="Probability",col=col[1],ylim=c(0,1),xlim=c(0,timemax),cex.axis=0.7,cex.lab=0.7,main=tit, ...)
            lines(times,all.probs[,"lower",chosen.tr[[i]]],type="s",lty=3,col=col[2])
            lines(times,all.probs[,"upper",chosen.tr[[i]]],type="s",lty=3,col=col[2]) 
          }
        }
        
        if(s==0){
          title("State Occupation Probabilities", outer=TRUE,  line=-1)
        } else{
          title("Transition Probabilities", outer=TRUE,  line=-1)
        }
        
      }else{
        
        
        all.probs<-x$all.probs[,,chosen.tr]
        
        # option 2: individual curves without CI
        
        if(length(chosen.tr)==1){
          tit<-paste("p",chosen.tr)
          timemax<-max(times)*1.04
          plot(times,all.probs,type="s",xlab="Time",ylab="Probability",ylim=c(0,1),xlim=c(0,timemax),col=col[1],cex.axis=0.7,cex.lab=0.7,main=tit, ...)
          
        }else if(length(chosen.tr)>1 & length(chosen.tr)<=3){
          N<-seq(1,length(chosen.tr),1)
          tpg<-layout(matrix(N,1,length(chosen.tr),byrow=TRUE))
          #layout.show(tpg)
          
          for(i in 1:length(chosen.tr)){
            tit<-paste("p",chosen.tr[[i]])
            timemax<-max(times)*1.04
            plot(times,all.probs[,chosen.tr[[i]]],type="s",xlab="Time",ylab="Probability",ylim=c(0,1),xlim=c(0,timemax),col=col[1],cex.axis=0.7,cex.lab=0.7,main=tit, ...)
            
          }
        } else {
          
          n.cols<-3
          n.rows<-ceiling(length(chosen.tr)/n.cols)
          N<-seq(1,length(chosen.tr),1)
          if(length(N)<n.cols*n.rows){
            m<-n.cols*n.rows-length(N)
            N<-c(N,rep(0,m))
          }
          tpg<-layout(matrix(N,n.rows,n.cols,byrow=TRUE))
          #layout.show(tpg)
          
          for(i in 1:length(chosen.tr)){
            tit<-paste("p",chosen.tr[[i]])
            timemax<-max(times)*1.04
            plot(times,all.probs[,chosen.tr[[i]]],type="s",xlab="Time",ylab="Probability",ylim=c(0,1),xlim=c(0,timemax),col=col[1],cex.axis=0.7,cex.lab=0.7,main=tit, ...)
            
          }
        }
        if(s==0){
          title("State Occupation Probabilities", outer=TRUE,  line=-1)
        } else{
          title("Transition Probabilities", outer=TRUE,  line=-1)
        }
        
      } # if cov=="null"
    } else {
      
      maxims<-c(NA)
      nnn<-length(x$times)
      for(iii in 1:nnn){
        maxims[iii]<-max(x$times[iii]$t)
      }
      timemax<-max(maxims)*1.04
      
      if(is.null(col)) col<-"black"
      n.cats<-length(names(x$all.probs))
      if(length(col)<n.cats) col<-rep(col,length.out=n.cats)
      
      # CI==TRUE
      if(CI==TRUE){
        nn<-length(names(x$all.probs)) 
        
        # option 1: individual curves with CI
        if(length(chosen.tr)==1){
          tit<-paste("p",chosen.tr)
          plot(x$times[1]$t,x$all.probs[1]$CI[,"probs",chosen.tr[[1]]],type="s",xlab="Time",ylab="Probability",ylim=c(0,1),xlim=c(0,timemax),col=col[1],cex.axis=0.7,cex.lab=0.7,main=tit, ...)
          lines(x$times[1]$t,x$all.probs[1]$CI[,"lower",chosen.tr[[1]]],type="s",lty=3,col=col[1])
          lines(x$times[1]$t,x$all.probs[1]$CI[,"upper",chosen.tr[[1]]],type="s",lty=3,col=col[1])
          for(j in 2:nn){
            lines(x$times[j]$t,x$all.probs[j]$CI[,"probs",chosen.tr[[1]]],type="s",col=col[j])
            lines(x$times[j]$t,x$all.probs[j]$CI[,"lower",chosen.tr[[1]]],type="s",lty=3,col=col[j])
            lines(x$times[j]$t,x$all.probs[j]$CI[,"upper",chosen.tr[[1]]],type="s",lty=3,col=col[j])
          }
          
          
        }else if(length(chosen.tr)>1 & length(chosen.tr)<=3){
          N<-seq(1,length(chosen.tr),1)
          tpg<-layout(matrix(N,1,length(chosen.tr),byrow=TRUE))
          #layout.show(tpg)
          
          for(i in 1:length(chosen.tr)){
            tit<-paste("p",chosen.tr[[i]])
            plot(x$times[1]$t,x$all.probs[1]$CI[,"probs",chosen.tr[[i]]],type="s",xlab="Time",ylab="Probability",col=col[1],ylim=c(0,1),xlim=c(0,timemax),cex.axis=0.7,cex.lab=0.7,main=tit, ...)
            lines(x$times[1]$t,x$all.probs[1]$CI[,"lower",chosen.tr[[i]]],type="s",lty=3,col=col[1])
            lines(x$times[1]$t,x$all.probs[1]$CI[,"upper",chosen.tr[[i]]],type="s",lty=3,col=col[1])
            for(j in 2:nn){
              lines(x$times[j]$t,x$all.probs[j]$CI[,"probs",chosen.tr[[i]]],type="s",col=col[j])
              lines(x$times[j]$t,x$all.probs[j]$CI[,"lower",chosen.tr[[i]]],type="s",lty=3,col=col[j])
              lines(x$times[j]$t,x$all.probs[j]$CI[,"upper",chosen.tr[[i]]],type="s",lty=3,col=col[j])
            }
          }
        } else {
          
          n.cols<-3
          n.rows<-ceiling(length(chosen.tr)/n.cols)
          N<-seq(1,length(chosen.tr),1)
          if(length(N)<n.cols*n.rows){
            m<-n.cols*n.rows-length(N)
            N<-c(N,rep(0,m))
          }
          tpg<-layout(matrix(N,n.rows,n.cols,byrow=TRUE))
          #layout.show(tpg)
          
          for(i in 1:length(chosen.tr)){
            tit<-paste("p",chosen.tr[[i]])
            plot(x$times[1]$t,x$all.probs[1]$CI[,"probs",chosen.tr[[i]]],type="s",xlab="Time",ylab="Probability",ylim=c(0,1),xlim=c(0,timemax),col=col[1],cex.axis=0.7,cex.lab=0.7,main=tit, ...)
            lines(x$times[1]$t,x$all.probs[1]$CI[,"lower",chosen.tr[[i]]],type="s",lty=3,col=col[1])
            lines(x$times[1]$t,x$all.probs[1]$CI[,"upper",chosen.tr[[i]]],type="s",lty=3,col=col[1])
            for(j in 2:nn){
              lines(x$times[j]$t,x$all.probs[j]$CI[,"probs",chosen.tr[[i]]],type="s",col=col[j])
              lines(x$times[j]$t,x$all.probs[j]$CI[,"lower",chosen.tr[[i]]],type="s",lty=3,col=col[j])
              lines(x$times[j]$t,x$all.probs[j]$CI[,"upper",chosen.tr[[i]]],type="s",lty=3,col=col[j])
            }
          }
        }
        
        if(s==0){
          title("State Occupation Probabilities", outer=TRUE,  line=-1)
        } else{
          title("Transition Probabilities", outer=TRUE,  line=-1)
        }
        
      }else{
        
        nn<-length(names(x$all.probs))
        
        # option 2: individual curves without CI
        
        if(length(chosen.tr)==1){
          tit<-paste("p",chosen.tr)
          plot(x$times[1]$t,x$all.probs[1]$probs[,,chosen.tr],type="s",xlab="Time",ylab="Probability",col=col[1],ylim=c(0,1),xlim=c(0,timemax),cex.axis=0.7,cex.lab=0.7,main=tit, ...)
          for(j in 2:nn){
            lines(x$times[j]$t,x$all.probs[j]$probs[,,chosen.tr],type="s",col=col[j])
          }
        }else if(length(chosen.tr)>1 & length(chosen.tr)<=3){
          N<-seq(1,length(chosen.tr),1)
          tpg<-layout(matrix(N,1,length(chosen.tr),byrow=TRUE))
          #layout.show(tpg)
          
          for(i in 1:length(chosen.tr)){
            tit<-paste("p",chosen.tr[[i]])
            plot(x$times[1]$t,x$all.probs[1]$probs[,,chosen.tr[[i]]],type="s",xlab="Time",ylab="Probability",ylim=c(0,1),xlim=c(0,timemax),col=col[1],cex.axis=0.7,cex.lab=0.7,main=tit, ...)
            for(j in 2:nn){
              lines(x$times[j]$t,x$all.probs[j]$probs[,,chosen.tr[[i]]],type="s",col=col[j])
            }
          }
        } else {
          
          nn<-length(names(x$all.probs))
          
          n.cols<-3
          n.rows<-ceiling(length(chosen.tr)/n.cols)
          N<-seq(1,length(chosen.tr),1)
          if(length(N)<n.cols*n.rows){
            m<-n.cols*n.rows-length(N)
            N<-c(N,rep(0,m))
          }
          tpg<-layout(matrix(N,n.rows,n.cols,byrow=TRUE))
          #layout.show(tpg)
          
          for(i in 1:length(chosen.tr)){
            tit<-paste("p",chosen.tr[[i]])
            plot(x$times[1]$t,x$all.probs[1]$probs[,,chosen.tr[[i]]],type="s",xlab="Time",ylab="Probability",ylim=c(0,1),xlim=c(0,timemax),col=col[1],cex.axis=0.7,cex.lab=0.7,main=tit, ...)
            for(j in 2:nn){
              lines(x$times[j]$t,x$all.probs[j]$probs[,,chosen.tr[[i]]],type="s",col=col[j])
            }
          }
        }
        if(s==0){
          title("State Occupation Probabilities", outer=TRUE,  line=-1)
        } else{
          title("Transition Probabilities", outer=TRUE,  line=-1)
        }       
      } # if CI==TRUE      
    }    
  }
