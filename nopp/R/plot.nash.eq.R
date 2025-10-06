plot.nash.eq <-
function(x,...){

   dots <- list(...)

   pos <- NULL
   if(!is.null(x$position))
    pos <- x$position

   idd <- which(names(dots) == "pos")
   if(length(idd)==1)
    pos <- dots[[idd]]

 #  print(str(pos)) 
   votes <- NULL
   if(!is.null(x$votes))
    votes <- x$votes
   ide <- which(names(dots) == "votes")
   if(length(ide)==1)
    votes <- dots[[ide]]
 #  print(str(votes)) 
 
 
      
      emn <- sort(x$basic$est)
      mP <- x$basic$mP
      
    dotchart(   sort(emn), main="Nash Equilibrium" , xlab="equilibrium position", pch=19)
    

    if(!is.null(pos)){
     idx <- match( names(emn) , names(pos))
     pos <- unlist(pos)[idx]
    # print(emn)
    # print(pos)
     avg <- mean(abs(pos-emn))
     plot(pos, emn, xlab="External", ylab="Nash",cex=0.5,
     main=sprintf("Average Absolute Distance = %.2f\nCorrelation = %.2f\n", avg, cor(emn,pos)))
     text(pos+.1, emn, names(emn), cex=0.7)
     abline(a=0, b=1, lty=3)
    }



 plot(emn, mP,type="h",axes=FALSE,
    xlab="equilibrium position", ylab="eq. vote share", ylim=c(0, max(mP+min(mP)*.2)), xlim=c(min(emn), max(emn)), main="Nash Equilibrium")
   text(emn, mP+min(mP)*.2,
   labels=sprintf("%.1f%%", mP*100),cex=0.7) # plot vote share on top of bar
 
    mtext(paste(names(emn), round(emn,2),sep="\n"),side=1,
 at= emn,padj=0.5,cex=0.7)





    if(!is.null(votes)){
     idx <- match( names(mP) , names(votes))
     votes <- unlist(votes)[idx]
     mm <- rbind(mP, votes)
     rownames(mm) <- c("Nash", "External")
     #     id <- order(x$basic$est)
     #  mm[,id]
     id <- order(x$basic$est,decreasing=TRUE)
     mm <- mm[, id]
     avg <- mean(abs(mm[1,]-mm[2,]))
     dotchart( mm, main=sprintf("Average Absolute Distance = %.2f%%\nCorrelation = %.2f\n", avg, cor(mP,votes)) , color=c("red","blue"), xlab="vote share", pch=19)
     
#     print(mm)
    }
  
  
  if(!is.null(x$boot)){
      emnB <- x$boot$est.mean
      eLB <- x$boot$est.LB
      eUB <- x$boot$est.UB
      
      id <- order(emnB)
      emnB <- emnB[id]
      eLB <- eLB[id]
      eUB <- eUB[id]
      dotchart(   emnB, main="Nash Equilibrium\n(Bootstrap)" , xlab="equilibrium position", xlim=c(min(eLB)*.9, max(eUB)*1.10) , pch=19)
      for (i in 1:length(emnB)){
          lines(x=c(eLB[i],eUB[i]), y=c(i,i),col="red")
      }

    mmP <- x$boot$mP.mean
    mPLB <- x$boot$mP.LB
    mPUB <- x$boot$mP.UB
    #    id <- order(mmP)
    id <- order(x$boot$est.mean)
    mmP <- mmP[id]
    mPLB <- mPLB[id]
    mPUB <- mPUB[id]
    dotchart(   mmP, main="Nash Equilibrium\n(Bootstrap)" , xlab="vote share", xlim=c(min(mPLB)*.9, max(mPUB)*1.10 ), pch=19)
    for (i in 1:length(mmP)){
        lines(x=c(mPLB[i],mPUB[i]), y=c(i,i),col="red")
    }
  }
  
  if(!is.null(x$MC)){
      emnB <- x$MC$est.mean
      eLB <- x$MC$est.LB
      eUB <- x$MC$est.UB
      
      id <- order(emnB)
      emnB <- emnB[id]
      eLB <- eLB[id]
      eUB <- eUB[id]
      dotchart(   emnB, main="Nash Equilibrium\n(Monte Carlo)" , xlab="equilibrium position", xlim=c(min(eLB)*.9, max(eUB)*1.10), pch=19 )
      for (i in 1:length(emnB)){
          lines(x=c(eLB[i],eUB[i]), y=c(i,i),col="red")
      }
      
      mmP <- x$MC$mP.mean
      mPLB <- x$MC$mP.LB
      mPUB <- x$MC$mP.UB
      #    id <- order(mmP)
      id <- order(x$MC$est.mean)
      mmP <- mmP[id]
      mPLB <- mPLB[id]
      mPUB <- mPUB[id]
      dotchart(   mmP, main="Nash Equilibrium\n(Monte Carlo)" , xlab="vote share", xlim=c(min(mPLB)*.9, max(mPUB)*1.10 ), pch=19)
      for (i in 1:length(mmP)){
          lines(x=c(mPLB[i],mPUB[i]), y=c(i,i),col="red")
      }
  }
  

  
   #   dotchart(   sort(emn), main="Nash Equilibrium" , xlab="equilibrium position")
  
  
  
  #plot(emn, x$boot$mP.mean,type="h",axes=FALSE,
  #  xlab="equilibrium position", ylab="eq. vote share", ylim=c(0, max(mP+min(mP)*.2)), xlim=c(min(emn-esd),max(emn+esd) ), main="Bootstrap Nash Equilibrium")
  
  #    mtext(paste(names(emn), round(emn,2),sep="\n"),side=1,
  # at= emn,padj=0.5,cex=0.7)
 #  text(emn, mP+min(mP)*.2,
 #   labels=sprintf("%.1f%%", mP*100),cex=0.7)
 #    n <- length(emn)
 #   for(i in 1:length(emn)){
 #   sd <- esd[i]
 #   mu <- emn[i]
 #       xx <- c(mu-sd, mu-sd, mu+sd, mu+sd)
 #       yy <- c(0, mP[i]/(2*sd), mP[i]/(2*sd), 0)
 #       polygon(xx,yy, col=rainbow(n, alpha=0.1)[i] )
 # #  lines(  c(x$basic$est[i],   x$basic$est[i], mu), c(0, x$basic$mP[i], mP[i]), lty=3)
 #
 #}
 #   plot(emn, x$basic$est, xlab="Bootstrap Nash", ylab="Nash",cex=0.5)
 #   text(emn+.1, x$basic$est, names(emn), cex=0.7)
 #   abline(a=0, b=1, lty=3)
   


#  if(!is.null(x$MC)){
#   emn <- x$MC$est.mean
#   esd <- x$MC$est.sd
#   mP <- x$MC$mP.mean
#   plot(emn, x$MC$mP.mean,type="h",axes=FALSE,
#    xlab="equilibrium position", ylab="eq. vote share",
#    ylim=c(0, max(mP+min(mP)*.2)),
#    xlim=c(min(emn-esd),max(emn+esd) ), main="Monte Carlo Nash Equilibrium")
# mtext(paste(names(emn), round(emn,2),sep="\n"),side=1,
# at= emn,padj=0.5,cex=0.7)
#    text(emn, mP+min(mP)*.2,
#     labels=sprintf("%.1f%%", mP*100),cex=0.7)
#     n <- length(emn)
#    for(i in 1:length(emn)){
#    sd <- esd[i]
#    mu <- emn[i]
## lines(  c(x$basic$est[i],   x$basic$est[i], mu), c(0, x$basic$mP[i], mP[i]), lty=3)#
#
#        xx <- c(mu-sd, mu, mu+sd, mu-sd)
#        yy <- c(0, mP[i], 0, 0)
#        polygon(xx,yy, col=rainbow(n, alpha=0.1)[i] )
#    }
#  plot(emn, x$basic$est, xlab="Monte Carlo Nash", ylab="Nash",cex=0.5)
#    text(emn+.1, x$basic$est, names(emn), cex=0.7)
#    abline(a=0, b=1, lty=3)
#
#  }
  
    
}
