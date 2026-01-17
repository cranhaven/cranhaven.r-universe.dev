rippl<-function(discharge,target,plot=TRUE)
{
   if(missing(discharge)){stop('discharge vector is missing!')}
   if(missing(target)){stop('target vector is missing!')}
   if(length(target)<length(discharge))
   {
      target<-rep(target,floor(length(discharge)/length(target))+1)[1:length(discharge)]
   }
   if(length(target)>length(discharge))
   {
      target<-target[1:length(discharge)]
   }
   deficit<-cumsum(discharge-target)
   ind_min<-which(diff(sign(diff(c(0,deficit))))== 2)
   ind_max<-which(diff(sign(diff(c(0,deficit))))==-2)
   if((deficit[length(deficit)]-deficit[length(deficit)-1])>0)
   {
      ind_max<-c(ind_max,length(deficit))
   }else{
      ind_min<-c(ind_min,length(deficit))
   }
   if((deficit[1]-deficit[2])>0)
   {
      ind_max<-c(1,ind_max)
   }else{
      ind_min<-c(1,ind_min)
   }
   ind_min<-ind_min[which(duplicated(ind_min)==FALSE)]
   ind_max<-ind_max[which(duplicated(ind_max)==FALSE)]
   cap<-rep(NA,length(ind_min))
   for(i in length(ind_min):2)
   {
      m1<-deficit[ind_min[i]]
      m2<-deficit[ind_min[i-1]]
      M1<-deficit[ind_max[which((ind_max<ind_min[i])*(ind_max>ind_min[i-1])==1)]]
      cap[i]<-M1-m1
      if(plot)
      {
         plot(deficit,typ='o',xlab='Time index',ylab=expression(sum(discharge[t]-target[t])))
         points(ind_min[i]  ,m1,pch=19,col=2)
         points(ind_min[i-1],m2,pch=19,col=2)
         points(ind_max[which((ind_max<ind_min[i])*(ind_max>ind_min[i-1])==1)],M1,pch=19,col=3)
         abline(h=m1,col=2)
         abline(h=M1,col=3)
         text(x=round(length(discharge)*0.1),y=M1-cap[i]/2,label=paste('capacity:',round(cap[i],2)))
         Sys.sleep(1)
      }
   }
   id_cap<-which(cap==max(cap,na.rm=TRUE))
   plot(deficit,typ='o',xlab='Time index',ylab=expression(sum(discharge[t]-target[t])))
   m1<-deficit[ind_min[id_cap]]
   m2<-deficit[ind_min[id_cap-1]]
   M1<-deficit[ind_max[which((ind_max<ind_min[id_cap])*(ind_max>ind_min[id_cap-1])==1)]]
   if(plot)
   {
      points(ind_min[id_cap],m1,pch=19,col=2)
      points(ind_min[id_cap-1],m2,pch=19,col=2)
      points(ind_max[which((ind_max<ind_min[id_cap])*(ind_max>ind_min[id_cap-1])==1)],M1,pch=19,col=3)
      abline(h=m1,col=2)
      abline(h=M1,col=3)
      text(x=25,y=M1-cap[i]/2,label=paste('calculated capacity:',round(max(cap,na.rm=TRUE),2)),col=2)
   }
   return(cap[id_cap])
}