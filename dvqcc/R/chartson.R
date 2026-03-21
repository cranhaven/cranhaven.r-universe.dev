# charts calculation (online)

chartson=function(varest,type,plot=plot){



  C=varest$C
  I=varest$I
  #n=length(varest$qt)+1
  n=length(varest$Lim_T2)

  Inew=dim(varest$tnew)[1]



  beyond.limits=list()
  time.to.first.detection=c()
  arl=c()
  artl=c()
  s=c()
  perc=c()


  if(type=="W.var"){

    for(i in 1:Inew){

      #arl[i]=1/(sum(varest$wnew[i,]>varest$qwt)/(n-1))
      arl[i]=1/(sum(varest$wnew[i,]>varest$Lim_W)/(n))
      beyond.limits[[i]]=which(varest$wnew[i,]>varest$Lim_W)
      perc[i]=1/arl[i]
      #time.to.first.detection[i]=beyond.limits[[i]][1]
      #artl[i]=time.to.first.detection[i]/(n-1)

      if (length(beyond.limits[[i]])==0){

        arl[i]=NA
        beyond.limits[[i]]=NA
        perc[i]=0

      } else {

        arl[i]=arl[i]
        beyond.limits[[i]]=beyond.limits[[i]]
        perc[i]=perc[i]


      }

      k=0


      if (length(beyond.limits[[i]])>2){

        for (j in 1:(length(beyond.limits[[i]])-2)){

          if (beyond.limits[[i]][j+1]-beyond.limits[[i]][j]==1 & beyond.limits[[i]][j+2]-beyond.limits[[i]][j+1]==1){
            k=k+1
            s[k]<-beyond.limits[[i]][j]
          }

        }

        if(k!=0) { time.to.first.detection[i]<-s[1]} else {time.to.first.detection[i]=NA}



        artl[i]=time.to.first.detection[i]/(n-1)
        artl[i]=time.to.first.detection[i]/(n)

      } else {

        time.to.first.detection[i]=NA

        artl[i]=NA

        }



    }

  }else{
    for(i in 1:Inew){

      arl[i]=1/(sum(varest$tnew[i,]>varest$Lim_T2)/(n))
      #arl[i]=1/(sum(varest$tnew[i,]>varest$qt)/(n-1))
      beyond.limits[[i]]=which(varest$tnew[i,]>varest$Lim_T2)
      perc[i]=1/arl[i]


      if (length(beyond.limits[[i]])==0){

        arl[i]=NA
        beyond.limits[[i]]=NA
        perc[i]=0

      } else {

        arl[i]=arl[i]
        beyond.limits[[i]]=beyond.limits[[i]]
        perc[i]=perc[i]


      }


      k=0


      if (length(beyond.limits[[i]])>2 ){

        for (j in 1:(length(beyond.limits[[i]])-2)){

          if (beyond.limits[[i]][j+1]-beyond.limits[[i]][j]==1 & beyond.limits[[i]][j+2]-beyond.limits[[i]][j+1]==1){
            k=k+1
            s[k]<-beyond.limits[[i]][j]
          }

        }

        if(k!=0) { time.to.first.detection[i]<-s[1]} else {time.to.first.detection[i]=NA}



        #artl[i]=time.to.first.detection[i]/(n-1)
        artl[i]=time.to.first.detection[i]/(n)

      } else {

        time.to.first.detection[i]=NA

        artl[i]=NA

        }

    }
  }




  if(plot==TRUE){
    if(type=="T2.var"){

      vt2=varest$Lim_T2

      df=data.frame(t=(1:(n)),vt2,tnew=t(varest$tnew))
     # df=data.frame(t=(1:(n-1)),qt=varest$qt,tnew=t(varest$tnew))
      go <- ggplot(df, aes(x=t,y=vt2))+geom_line()
      #go<-go+ggtitle("T2.var scores of Dynamic VAR coefficients")
      go<-go+geom_hline(yintercept = 0)
      go<-go+xlab("time-instants")+ylab("control limits and T2.var scores")

      for(i in 1:Inew){
        df2=df
        df2$vc=1*(df[,i+2]>df$vt2)+1
        g <- go + geom_point(aes(color=as.factor(df2$vc)),y=df2[,(i+2)],x=df2[,1])+ylim(c(0,max(max(df$vt2),max(df[,(i+2)]))+10))
        g<-g+ggtitle(paste("T2.var scores / new batch", i))
        g<-g+guides(color = FALSE)
        g<-g+scale_color_manual(values=c("blue","red"))
        plot(g)

        # print(paste(" new batch number", i ,sep="  .....  "))
       # print(g)
       # readline("Press ENTER to next batch")
      }

    }else{

      vw=varest$Lim_W

      df=data.frame(t=(1:(n)),vw,wnew=t(varest$wnew))
      #df=data.frame(t=(1:(n-1)),qw=varest$qwt,wnew=t(varest$wnew))
      go <- ggplot(df, aes(x=t,y=vw))+geom_line()
      #go<-go+ggtitle("W.var scores of Dynamic VAR coefficients")
      go<-go+xlab("time-instants")+ylab("control limits and W scores")
      go<-go+geom_hline(yintercept = 0)

      for(i in 1:Inew){
        df2=df
        df2$vc=2*(df[,i+2]>df$vw)
        g <- go + geom_point(aes(color=as.factor(df2$vc)),y=df2[,(i+2)],x=df2[,1])+ylim(c(0,(max(df$vw)+10)))
        g<-g+ggtitle(paste("W.var scores / new batch", i))
        g<-g+guides(color = FALSE)
        g<-g+scale_color_manual(values=c("blue","red"))
        plot(g)

         ##print("new batch number")
        #print(paste(" new batch number", i ,sep="  .....  "))
        #warning( "new batch number", i ,sep="  ....."  )
        #print(g)
        #readline("Press ENTER to next batch")
      }


    }

     if (Inew > 1) {message("Warning...Press the arrow in the output screen to backward visualization of the previous charts")}

  } # plot TRUE

 # for ( i in 1:Inew){

   # if (is.na(beyond.limits[[i]])) {beyond.limits[[i]]=NULL} else {beyond.limits[[i]]=beyond.limits[[i]]}}




   return(list(beyond.limits=beyond.limits,time.to.first.detection=time.to.first.detection,perc=perc,arl=arl,artl=artl))


}






