toVector <- function(x){
    if (any(is.data.frame(x),is.matrix(x))){
        for (i in 1:ncol(x)) {
            if (i==1) {
                x.i=as.character(x[,i])
            }else{
                x.i=c(x.i,as.character(x[,i]))
            }
        }
       return(x.i) 
    }else if(is.list(x)){
        x=unlist(x)
        names(x)=NULL
        return(x)
    }else{
        x
    }
}
