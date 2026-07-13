### Auxilliary functions for performing isotonic regression.
### These are ad-hoc, and not designed for general use.

convex.hull<-function(x,y){
### Calculates the upper convex hull of a set of points with
### coordinates (x[i],y[i]). Assumes x is sorted.
### returns a vector of indices of points in the upper convex hull. 

    n<-length(x)
    xd<-as.double(x)
    yd<-as.double(y)
    answer<-vector("integer",n+1)
    returnval<-.C(find_convex_hull,answer,n,xd,yd,PACKAGE="HVS")
    answer<-returnval[[1]]
    ans<-answer[seq_len(answer[1])+1]+1
    return(ans)
}


isotonic.regression<-function(x1,y1,x2=NULL,y2=NULL){
### Performs decreasing isotonic regression on the combined samples
### x1, y1 and x2 y2. Th
    n1<-as.integer(length(x1))
    n2<-as.integer(length(x2))
    xconcat<-as.double(c(x1,x2))
    yconcat<-as.double(c(y1,y2))
    answer<-vector("double",n1+n2)
    returnval<-.C(isotonic_regression,answer,n1,n2,xconcat,yconcat,PACKAGE="HVS")
    y<-returnval[[1]]
    answer<-list("x"=sort.int(c(x1,x2,Inf),method="auto"),"y"=c(y,0),"data"=c(yconcat,NA))
    class(answer)<-"isotonic"
    return(answer)
}


predict.isotonic<-function(object,newdata,...){
### Extrapolates all values to the first value of the isotonic
### function with larger value. 
### Assume object$x is sorted and ends with Inf
    if(is.null(newdata)){
        return(object$y)
    }
    n<-length(newdata)
    m<-length(object$x)
    ord<-order(newdata)
    ord.inv<-rep(0,length(newdata))
    ord.inv[ord]<-seq_along(ord) #inverse permutation
    nds<-newdata[ord]
    ans<-rep(0,n)
    j<-1
    for(i in seq_len(n)){
        while(newdata[i]>object$x[j]){
            j=j+1
        }
        ans[i]<-object$y[j]
    }
    return(ans)
}


plot.isotonic<-function(x,...){
### plots both the original data and the isotonic regression value.
    args<-list(...)
    argn<-names(args)

    if(!("xlab" %in% argn)){
        args$xlab<-"x"
    }
    
    if(!("ylab" %in% argn)){
        args$ylab<-"y"
    }



    y<-x$y
    data<-x$data
    n<-length(y)
    x<-x$x

    graphics::plot(c(rep(x[-n],each=2),x[n]),c(y[1],rep(y[-1],each=2)),type='l',args)
    graphics::points(x,data,args)
}
