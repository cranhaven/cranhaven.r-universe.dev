
MMsimu=function(m,x,z,tt,nj,beta,sigmae,D,phi,struc,typeModel,percCensu,nivel.Censu,cens.type,nu){
  Arp=length(phi)
  if(is.null(x)){x<-matrix(runif(sum(nj)*length(beta),-1,1),sum(nj),length(beta));rx="yes"}
  if(is.null(z)){z<-matrix(runif(sum(nj)*dim(D)[1],-1,1),sum(nj),dim(D)[1]);rz="yes"}
  y<-matrix(0,sum(nj),1)
  if(typeModel=="Normal"){
    if(struc=="ARp"){
      for(i in 1:m){
        tt1=tt[(sum(nj[1:i-1])+1) : (sum(nj[1:i]))]
        n=length(tt1)
        ome2=MatArpJ(phi,tt1,sigmae)
        errorp=as.vector(LaplacesDemon::rmvn(n=1,mu=rep(0,n), Sigma=ome2))
        b<-LaplacesDemon::rmvn(n=1,mu=rep(0,dim(D)[1]), Sigma=D)
        y[(sum(nj[1:i-1])+1) : (sum(nj[1:i])),]=x[(sum(nj[1:i-1])+1) : (sum(nj[1:i])),]%*%beta+z[(sum(nj[1:i-1])+1) : (sum(nj[1:i])),]%*%t(b)+errorp
      }
    } 
    if(struc!="ARp"){
      for(i in 1:m){
        tt1=tt[(sum(nj[1:i-1])+1) : (sum(nj[1:i]))]
        n=length(tt1)
        if(struc!="UNC")ome2=MatDec(tt1,phi[1],phi[2],struc)
        if(struc=="UNC")ome2=MatDec(tt1,phi[1],phi[2],struc)
        Omegai <- sigmae*ome2
        errorp=as.vector(LaplacesDemon::rmvn(n=1,mu=rep(0,n), Sigma=Omegai ))
        b<-LaplacesDemon::rmvn(n=1,mu=rep(0,dim(D)[1]), Sigma=D)
        y[(sum(nj[1:i-1])+1) : (sum(nj[1:i])),]=x[(sum(nj[1:i-1])+1) : (sum(nj[1:i])),]%*%beta+z[(sum(nj[1:i-1])+1) : (sum(nj[1:i])),]%*%t(b)+errorp
      }
    } 
    
  }
  if(typeModel=="Student"){
    if(struc=="ARp"){
      for(i in 1:m)
      {
        
        tt1 <- tt[(sum(nj[1:i-1])+1) : (sum(nj[1:i]))]
        n=length(tt1)
        ome2=MatArpJ(phi,tt1,sigmae)      
        errorp=as.vector(LaplacesDemon::rmvt(n=1, mu=rep(0,n), S=ome2, df=nu))
        b <-LaplacesDemon::rmvt(n=1, mu=rep(0,dim(D)[1]), S=D, df= nu)
        y[(sum(nj[1:i-1])+1) : (sum(nj[1:i])),]=x[(sum(nj[1:i-1])+1) : (sum(nj[1:i])),]%*%beta+z[(sum(nj[1:i-1])+1) : (sum(nj[1:i])),]%*%t(b)+errorp
      }}
    if(struc!="ARp"){
      for(i in 1:m)
      {
        
        tt1 <- tt[(sum(nj[1:i-1])+1) : (sum(nj[1:i]))]
        n=length(tt1)
        ome2 <- MatDec(tt1,phi[1],phi[2],struc)
        Omegai <- sigmae*ome2
        b <-LaplacesDemon::rmvt(n = 1,mu=rep(0,dim(D)[1]),S =D,df = nu)
        errorp <- as.vector(LaplacesDemon::rmvt(n = 1,mu =  rep(0,nj[i]),S=Omegai,df = nu))
        y[(sum(nj[1:i-1])+1) : (sum(nj[1:i])),]=x[(sum(nj[1:i-1])+1) : (sum(nj[1:i])),]%*%beta+z[(sum(nj[1:i-1])+1) : (sum(nj[1:i])),]%*%t(b)+errorp
      }
          }
  }
  
  yy=y
  y_cc=y
  cc=rep(0,length(y_cc))
  
  if(!is.null(percCensu))
    {
    if(percCensu!=0)
    {
      if(cens.type=="left") {
        aa=sort(y, decreasing = FALSE)
        bb=aa[1:(percCensu*sum(nj))]
        cutof<-bb[percCensu*sum(nj)]
        cc=matrix(1,sum(nj),1)*(y< cutof)
        y[cc==1]=cutof
        y_cc=y
        }
      
      if(cens.type=="right") {
        aa=sort(y, decreasing = TRUE)
        bb=aa[1:(percCensu*sum(nj))]
        cutof<-bb[percCensu*sum(nj)]
        cc=matrix(1,sum(nj),1)*(y> cutof)
        y[cc==1]=cutof
        y_cc=y
        }
      
      if(cens.type=="interval") {
        aa=sort(y, decreasing = FALSE)
        bbi=aa[1:(percCensu*sum(nj)*0.5)]
        aa=sort(y, decreasing = TRUE)
        bbs=aa[1:(percCensu*sum(nj)*0.5)]
        cutofi<-bbi[percCensu*sum(nj)*0.5]
        cutofs<-bbs[percCensu*sum(nj)*0.5]
        cci=matrix(1,sum(nj),1)*(y< cutofi)
        y[cci==1]=cutofi
        ccs=matrix(1,sum(nj),1)*(y>cutofs)
        y[ccs==1]=cutofs
        y_cc=y
        cc=cci+ccs
        }
    }
    }
  
  if(!is.null(nivel.Censu))
    {
    if(length(nivel.Censu)==1)
    {  
    
        if(cens.type=="left") {
          cutof<-nivel.Censu
          cc=matrix(1,sum(nj),1)*(y< cutof)
          y[cc==1]=cutof
          y_cc=y
          }
        
        if(cens.type=="right") {
          cutof<-nivel.Censu
          cc=matrix(1,sum(nj),1)*(y> cutof)
          y[cc==1]=cutof
          y_cc=y
          }
      
    }
    
    if(length(nivel.Censu)>1)
    {
      if(cens.type=="interval") {
         cutofi<-nivel.Censu[1]
        cutofs<-nivel.Censu[2]
        cci=matrix(1,sum(nj),1)*(y< cutofi)
        y[cci==1]=cutofi
        ccs=matrix(1,sum(nj),1)*(y>cutofs)
        y[ccs==1]=cutofs
        y_cc=y
        cc=cci+ccs
      }
    }
    }
  
return(list(cc=cc, y_cc=y_cc))
}


