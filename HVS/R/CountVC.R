count.indep<-function(G,cutoff,method="exact",samp.size=1000){
### Counts the number of independent sets in the graph G at the given cut-off
### G should be a square matrix, representing a graph
    n<-as.integer(dim(G)[1])
    nsamp<-as.integer(samp.size)
    if(dim(G)[1]!=dim(G)[2]){
        stop("Graphs should be represented by a square matrix.")
    }
    ans<-rep(0,length(method))
    answer<-vector("double",1)
    for(i in seq_along(method)){
        if(method[i]=="exact"){
            returnval<-.C(count_indep,answer,n,G,cutoff,PACKAGE="HVS")
        }else if(method[i]=="importance_sample"){
            returnval<-.C(count_indep_import,answer,n,G,cutoff,nsamp,PACKAGE="HVS")
        }else{
            warning(paste("Unknown method: \"",method[i],"\" returning 0."),sep="")
            returnval<-0
        }
        ans[i]<-returnval[[1]]
        names(ans)[i]<-method[i]
    }
    return(ans)
}

count.vc<-count.indep
### there are the same number of vertex covers and independent sets.


select.cutoff<-function(G,batchsize=100,nbatch=100,level=0.05,threshold=NULL){
### Finds the value of c such that the number of vertex covers is
### equal to the target function. Currently the target function and
### level are fixed. Need to add flexibility to change this.  G should
### be a square matrix, representing a graph
    
    n<-as.integer(dim(G)[1])
    bs<-as.integer(batchsize)
    nb<-as.integer(nbatch)
    if(dim(G)[1]!=dim(G)[2]){
        stop("Graphs should be represented by a square matrix.")
    }
    answer<-vector("double",1+bs*nb*4)
    if(is.null(threshold)){
### Set threshold 
        K<-n*(1+log(n))+n*(n-1)/2*(log(4)-log(3))/log(2)*(1+log(n)-log((log(4)-log(3))/log(2)))
        threshold<-K/level
    }else if(threshold=="PRDS"){
        K<-n+n*(n-1)/2*(log(4)-log(3))/log(2)
        threshold<-K/level
    }   
##    returnval<-.C('set_level',answer,level,n)
    
    returnval<-.C('select_cutoff',answer,n,G,bs,nb,threshold)
    answer<-returnval[[1]]
    prob<-answer[1]

    siz<-list("x"=c(answer[1+seq_len(nb*bs)],Inf),"y"=c(answer[3*nb*bs+1+seq_len(nb*bs)],n),"data"=c(answer[2*nb*bs+1+seq_len(nb*bs)],n))
    class(siz)<-"isotonic"

    ### upper triangular elements of G
    pvals<-G[rep(1,n)%*%t(seq_len(n))<=seq_len(n)%*%t(rep(1,n))] 
    ord<-order(pvals)
    selected<-pvals<=prob
    sizes<-predict(siz,newdata=pvals[ord])
    
    ans<-list("level"=level,"pv"=pvals[ord],"sizing"=sizes,"rejected"=selected,
              "cut.off.slope"=threshold,"pv.cut.off"=prob,"independent.sets"=siz)
    class(ans)<-c("HVS","GLSUP")
    
    return(ans)
}


