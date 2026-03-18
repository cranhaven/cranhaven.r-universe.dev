estploidy <-
function(alphas=NA,het=NA,depth=NA,train=FALSE,pl=NA,set=NA,nclasses=2,ids=NA,pcs=1:2){
    ## create data structure
    ## regress het. on depth
    outH<-lm(het ~ depth)
    H<-outH$residuals
    Nind<-length(alphas)
    Nprops<-dim(alphas[[1]])[1]
    ## extrac point estimates for allelic proportions
    ad<-matrix(NA,nrow=Nind,ncol=Nprops)
    for(i in 1:Nind){
        ad[i,]<-alphas[[i]][,3]
    }
    datamatrix<-cbind(H,ad)
    
    ## pca
    pcout<-prcomp(datamatrix,center=TRUE,scale=TRUE)
    
    ## kmeans and LDA without training set
    if(train==FALSE){
        ## kmeans clustering, using pcs 
        kout<-kmeans(x=pcout$x[,pcs],centers=nclasses)
        ## DA for classification
        lout<-lda(kout$cluster ~ pcout$x[,pcs],CV=TRUE,prior=rep(1/nclasses,nclasses))
        pp<-cbind(ids,lout$posterior)
    }
    
    ## LDA with training set
    if(train==TRUE){
        df<-data.frame(pl=pl,pcout$x[,pcs])
        ltrn<-lda(pl ~ ., df,subset=set,prior=rep(1/nclasses,nclasses))
        lout<-predict(object=ltrn,newdata=df[-set,])
        pp<-lout$posterior
    }
    
    out<-list(pp=pp,pcwghts=pcout$rotation,pcscrs=pcout$x)
    out
}
