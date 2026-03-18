cal.pca<-function(help.env,val,prcp=NULL) {
    kk<-o<-1
    dat.norm<-foreach(kk=1:dim(val)[2],.combine=cbind) %do% {
       rank(pnorm(val[,kk],0,1))/(dim(val)[1]+1)
    }         
    if(is.null(prcp)) prcomp.D <- prcomp(dat.norm,scale=FALSE)
    else prcomp.D<-prcp
    dat2<-dat.norm%*%prcomp.D$rotation
    data.distr<-foreach(o=1:get("D.struc",help.env),.combine=cbind) %do% {
      round(rank(dat2[,o])/(dim(val)[1]+1),10)
    }
return(list(data.distr=data.distr,prcomp.D=prcomp.D))
}
