find.match <-
function(a, unacceptable=4)
{
    find.min.pos<-function(d)
    {
        pos<-which(d==min(d))[1]
        pos.x<-pos %% nrow(d)
        if(pos.x == 0) pos.x<-nrow(d)
        pos.y<-floor((pos-1)/nrow(d)+1)
        pos<-c(pos.x, pos.y)
        return(pos)
    }
    
    b<-a*0
    if(ncol(a) == 1)
    {
        sel<-which(a[,1]==min(a[,1]))[1]
        if(a[sel,1] <= unacceptable) b[sel,1]<-1
    }else if(nrow(a)==1){
        sel<-which(a[1,]==min(a[1,]))[1]
        if(a[1,sel] <= unacceptable) b[1,sel]<-1
    }else{
        p<-find.min.pos(a)
        while(a[p[1],p[2]] <= unacceptable)
        {
            b[p[1],p[2]]<-1
            a[p[1],]<-1e10
            a[,p[2]]<-1e10
            p<-find.min.pos(a)
        }
    }
    return(b)
}
