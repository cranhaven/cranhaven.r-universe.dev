# charts calculation

chartsoff=function(varest,newdata=NULL,confidence.level,type,covvar,plot=plot){

  C=dim(varest$vec.phis)[2]
  I=varest$I
  n=varest$n
  Inew=varest$Inew

  cc=c()

  beyond.limits=c()
    #arl_0=c()


  if(type == "T2.var"){
    # T^2 chart

    if(!is.null(newdata)){


     # arl_1=c()

      if (covvar=="empirical"){
        #hotellingn=mqcc(varest$vec.phis,
                        #type = "T2.single",newdata=varest$vec.phis.new,
                       # center=colMeans(varest$vec.phis),
                       # cov=varest$vec.cov.empirical,limits=c(0,(C*(I-1)/(I-C))*qf(confidence.level,C,(I-C))), plot=T)


        T2=c()
       for (i in 1:I) {
        T2[i]=mahalanobis(varest$vec.phis[i,],center=colMeans(varest$vec.phis),cov= varest$vec.cov.empirical,inverted=FALSE)
       }

        #mahalanobis(Cb_js[i2,],center=meanT[[j]],cov= covT[[j]],inverted=FALSE)

        T2new=c()
        for (i in 1:Inew) {
          T2new[i]=mahalanobis(varest$vec.phis.new[i,],center=colMeans(varest$vec.phis),cov= varest$vec.cov.empirical,inverted=FALSE)
        }





      } # empirical
      else{


        #hotellingn=mqcc(varest$vec.phis,
         #                   type = "T2.single",newdata=varest$vec.phis.new,
          #                  center=colMeans(varest$vec.phis),
           #                 cov=varest$vec.cov.theoretical,limits=c(0,(C*(I-1)/(I-C))*qf(confidence.level,C,(I-C))), plot=plot)

      T2=c()
      for (i in 1:I) {
        T2[i]=mahalanobis(varest$vec.phis[i,],center=colMeans(varest$vec.phis),cov= varest$vec.cov.theoretical,inverted=FALSE)
      }

      T2new=c()
      for (i in 1:Inew) {
        T2new[i]=mahalanobis(varest$vec.phis.new[i,],center=colMeans(varest$vec.phis),cov= varest$vec.cov.theoretical,inverted=FALSE)
      }


            } # Theoretical

      vT=c(T2,T2new)

      line=(C*(I-1)/(I-C))*qf(confidence.level,C,(I-C))

       cc=which(vT>line)

    #if (!is.null(cc)){beyond.limits=cc }  else { beyond.limits=NULL }
       if (length(cc)>0) {beyond.limits=cc }  else { beyond.limits=NA }

       cc_0=c()
       cc_0=which(T2>line)

       #if (!is.null(cc_0)){arl_0=I/sum(T2>line)} else {arl_0=NA}
       if (length(cc_0)>0) {

         arl_0=I/sum(T2>line)
         perc_0=1/arl_0

       } else {

         arl_0=NA

       perc_0=0}

       cc_1=c()
       cc_1=which(T2new>line)
       #cc_1=which(cc[which(T2new>line)])


       #if (!is.null(cc_1)) {arl_1=Inew/sum(T2new>line)} else {arl_1=NA}
      if (length(cc_1)>0) {

        arl_1=Inew/sum(T2new>line)
        perc_1=1/arl_1

      } else {

        arl_1=NA
        perc_1=0}

        if(plot==TRUE){
        #line=(C*(I-1)/(I-C))*qf(confidence.level,C,(I-C))
        #vT=c(T2,T2new)
        vc=(vT>line)*1
        df=data.frame(i=1:(I+Inew),vT=vT,vc=vc)
        go<-ggplot(df, aes(x=i,y=vT))+geom_point()
        go<-go+ggtitle("T2.var scores")
        #go<-go+geom_hline(yintercept = 0)
        go<-go+geom_vline(xintercept = I,linetype="dashed",size=0.6)
        go<-go+xlab("batches")+ylab("control limits and T2.var scores")
        go<-go+geom_hline(yintercept = line)
        go<-go+geom_point(aes(color=as.factor(vc)),y=df[,2],x=df[,1])+ylim(c(0,max(max(df$vT),line)+5))
        go<-go+guides(color = FALSE)
        go<-go+scale_color_manual(values=c("blue","red"))
        print(go)  }


      return(list(beyond.limits=beyond.limits,LimT2=line,perc_ref=perc_0,arl_ref=arl_0,perc_new=perc_1,arl_new=arl_1))

    } # newdata
    else{


      if (covvar=="empirical"){
        #hotellingn=mqcc(varest$vec.phis,
                        #type = "T2.single",
                       # center=colMeans(varest$vec.phis),
                      #  cov=varest$vec.cov.empirical,limits=c(0,(C*(I-1)/(I-C))*qf(confidence.level,C,(I-C))), plot=plot)



        T2=c()
        for (i in 1:I) {
          T2[i]=mahalanobis(varest$vec.phis[i,],center=colMeans(varest$vec.phis),cov= varest$vec.cov.empirical,inverted=FALSE)
        }






      }else{


        #hotellingn=mqcc(varest$vec.phis,
                          #  type = "T2.single",
                          #  center=colMeans(varest$vec.phis),
                          #  cov=varest$vec.cov.theoretical,limits=c(0,(C*(I-1)/(I-C))*qf(confidence.level,C,(I-C))), plot=plot)

      T2=c()
      for (i in 1:I) {
        T2[i]=mahalanobis(varest$vec.phis[i,],center=colMeans(varest$vec.phis),cov= varest$vec.cov.theoretical,inverted=FALSE)
      }





      }


      line=(C*(I-1)/(I-C))*qf(confidence.level,C,(I-C))
      cc=which(T2>line)
      #arl_0=1/(sum(T2>line)/(I))

      #if (!is.null(cc)){beyond.limits=cc}  else { beyond.limits=NULL}

      if (length(cc)>0) {beyond.limits=cc}  else { beyond.limits=NA}

      #if (!is.null(cc)){ arl_0=1/(sum(T2>line)/(I))} else {arl_0=NA}

     if (length(cc)>0) {

       arl_0=1/(sum(T2>line)/(I))
       perc_0=1/arl_0
     } else {

       arl_0=NA
       perc_0=0
       }



      if(plot==TRUE){
      #line=(C*(I-1)/(I-C))*qf(confidence.level,C,(I-C))
      vT=T2
      vc=(vT>line)*1
      df=data.frame(i=1:I,vT=vT,vc=vc)
      go<-ggplot(df, aes(x=i,y=vT))+geom_point()
      go<-go+ggtitle("T2.var scores")
      #go<-go+geom_hline(yintercept = 0)
      #go<-go+geom_vline(xintercept = I,linetype="dashed",size=0.6)
      go<-go+xlab("batches")+ylab("control limits and T2.var scores")
      go<-go+geom_hline(yintercept = line)
      go<-go+geom_point(aes(color=as.factor(vc)),y=df[,2],x=df[,1])+ylim(c(0,max(max(df$vT),line)+5))
      go<-go+guides(color = FALSE)
      go<-go+scale_color_manual(values=c("blue","red"))
      print(go) }


      return(list(beyond.limits=beyond.limits,LimT2=line,perc_ref=perc_0,arl_ref=arl_0))

      } #only data
  } # T2.var
    else{

    if(!is.null(newdata)){
    # W chart
     # Inew=varest$Inew

     # arl_1=c()

    if (covvar=="empirical"){
    W=c()
    for(i in 1:I){
      W[i]<--C*(n-1)+C*(n-1)*log((n-1))-(n-1)*log(det(((n-1)-1)*varest$cov.B1[[i]])/det(varest$vec.cov.empirical))+sum(diag(solve(varest$vec.cov.empirical)%*%(((n-1)-1)*varest$cov.B1[[i]])))
    }
    Wnew=c()
    for(i in 1:Inew){
      Wnew[i]<--C*(n-1)+C*(n-1)*log((n-1))-(n-1)*log(det(((n-1)-1)*varest$cov.B1new[[i]])/det(varest$vec.cov.empirical))+sum(diag(solve(varest$vec.cov.empirical)%*%(((n-1)-1)*varest$cov.B1new[[i]])))
    }

    # wplotn=qcc(W,
    #            type = "xbar.one", newdata= Wnew,
    #            center=0.5,
    #            #limits=c(0,qchisq(0.95,C*(C+1)/2)), plot=T)
    #            limits=c(40,quantile(W,confidence.level)), plot=plot)



    }else{
      W=c()  ## cov teorica
      for(i in 1:I){
        W[i]<--C*(n-1)+C*(n-1)*log((n-1))-(n-1)*log(det(((n-1)-1)*varest$cov.B1[[i]])/det(varest$vec.cov.theoretical))+sum(diag(solve(varest$vec.cov.theoretical)%*%(((n-1)-1)*varest$cov.B1[[i]])))
      }
      Wnew=c()
      for(i in 1:Inew){
        Wnew[i]<--C*(n-1)+C*(n-1)*log((n-1))-(n-1)*log(det(((n-1)-1)*varest$cov.B1new[[i]])/det(varest$vec.cov.theoretical))+sum(diag(solve(varest$vec.cov.theoretical)%*%(((n-1)-1)*varest$cov.B1new[[i]])))
      }


    }

      vW=c(W,Wnew)
      line=quantile(W,confidence.level)

       cc=which(vW>line)
      #arl_0=1/(sum(W>line)/(I))
      #arl_1=1/(sum(Wnew>line)/(Inew))

      #if (!is.null(cc)){beyond.limits=cc }  else { beyond.limits=NULL }
      if (length(cc)>0) {beyond.limits=cc }  else { beyond.limits=NA }

       cc_0=c()
       cc_0=which(W>line)

      #if (!is.null(cc_0)){ arl_0=1/(sum(W>line)/(I))} else {arl_0=NA}
       if (length(cc_0)>0) {

         arl_0=1/(sum(W>line)/(I))
         perc_0=1/arl_0

       } else {
           arl_0=NA
           perc_0=0}

       cc_1=c()
       cc_1=which(Wnew>line)
       #cc_1=which(cc[which(Wnew>line)])

      #if (!is.null(cc_1)) {arl_1=Inew/sum(Wnew>line)} else {arl_1=NA}
      if (length(cc_1)>0) {

        arl_1=Inew/sum(Wnew>line)

        perc_1=1/arl_1

      } else {arl_1=NA

      perc_1=0}


      if(plot==TRUE){
      #line=quantile(W,confidence.level)
      #vW=c(W,Wnew)
      vc=(vW>line)*1
      df=data.frame(i=1:(I+Inew),vW=vW,vc=vc)
      go<-ggplot(df, aes(x=i,y=vW))+geom_point()
      go<-go+ggtitle("W.var scores")
      #go<-go+geom_hline(yintercept = 0)
      go<-go+geom_vline(xintercept = I,linetype="dashed",size=0.6)
      go<-go+xlab("batches")+ylab("control limits and W.var scores")
      go<-go+geom_hline(yintercept = line)
      go<-go+geom_point(aes(color=as.factor(vc)),y=df[,2],x=df[,1])+ylim(c(0,max(max(df$vW),line)+5))
      go<-go+guides(color = FALSE)
      go<-go+scale_color_manual(values=c("blue","red"))
      print(go)  }

      return(list(beyond.limits=beyond.limits,LimW=line,perc_ref=perc_0,arl_ref=arl_0,perc_new=perc_1,arl_new=arl_1))

    } else {
      if (covvar=="empirical"){

      W=c()
      for(i in 1:I){
        W[i]<--C*(n-1)+C*(n-1)*log((n-1))-(n-1)*log(det(((n-1)-1)*varest$cov.B1[[i]])/det(varest$vec.cov.empirical))+sum(diag(solve(varest$vec.cov.empirical)%*%(((n-1)-1)*varest$cov.B1[[i]])))
      }


    }else{
      W=c()
      for(i in 1:I){
        W[i]<--C*(n-1)+C*(n-1)*log((n-1))-(n-1)*log(det(((n-1)-1)*varest$cov.B1[[i]])/det(varest$vec.cov.theoretical))+sum(diag(solve(varest$vec.cov.theoretical)%*%(((n-1)-1)*varest$cov.B1[[i]])))
      }

    }


      line=quantile(W,confidence.level)
      cc=which(W>line)
      #arl_0=1/(sum(W>line)/(I))
      #cc=which(vW>line)
      #arl_0=1/(sum(W>line)/(I))
      #arl_1=1/(sum(Wnew>line)/(Inew))

      #if (!is.null(cc)){beyond.limits=cc }  else { beyond.limits=NULL }
      if (length(cc)>0) {beyond.limits=cc }  else { beyond.limits=NA }

      #if (!is.null(cc)){ arl_0=1/(sum(W>line)/(I))} else {arl_0=NA}

      if (length(cc)>0) {

        arl_0=1/(sum(W>line)/(I))
        perc_0=1/arl_0

      } else {arl_0=NA

      perc_0=0}

      if(plot==TRUE){
      line=quantile(W,confidence.level)
      vW=W
      vc=(vW>line)*1
      df=data.frame(i=1:I,vW=vW,vc=vc)
      go<-ggplot(df, aes(x=i,y=vW))+geom_point()
      go<-go+ggtitle("W.var scores")
      go<-go+xlab("batches")+ylab("control limits and W.var scores")
      go<-go+geom_hline(yintercept = line)
      go<-go+geom_point(aes(color=as.factor(vc)),y=df[,2],x=df[,1])+ylim(c(0,max(max(df$vW),line)+5))
      go<-go+guides(color = FALSE)
      go<-go+scale_color_manual(values=c("blue","red"))
      print(go)}

      return(list(beyond.limits=beyond.limits,LimW=line, perc_ref=perc_0,arl_ref=arl_0))

    }


  } # W.var
}


