count.indep.hyper<-function(H,method="exact",samp.size=1000){
### Counts the number of independent sets in the hypergraph H
### H has the following stucture:
### The first element gives the number of vertices.
### The second element is a list of edges.
### Each edge consist of a list of vertex numbers.
    nv<-as.integer(H$vertices)
    nsamp<-as.integer(samp.size)
    ne<-as.integer(length(H$edges))
    orders<-as.integer(lapply(H$edges,length))
    edges<-as.integer(unlist(H$edges)-1) # in c++ indices start at 0.
    ans<-rep(0,length(method))
    answer<-vector("double",1)
    for(i in seq_along(method)){
        if(method[i]=="exact"){
            returnval<-.C(count_indep_hyper_exact,answer,nv,ne,orders,edges,PACKAGE="HVS")
        }else if(method[i]=="approx"){
            returnval<-.C(count_indep_hyper_approx,answer,nv,ne,orders,edges,nsamp,PACKAGE="HVS")
        }else{
            warning(paste("Unknown method: \"",method[i],"\" returning 0."),sep="")
            returnval<-0
        }
        ans[i]<-returnval[[1]]
        names(ans)[i]<-method[i]
    }
    return(ans)
}

count.vc.hyper<-count.indep.hyper
### there are the same number of vertex covers and independent sets.
