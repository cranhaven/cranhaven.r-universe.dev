rm.ridge <-
function(x,y2, bw)
{
    sel<-which(y2<quantile(y2, 0.75))
    max.x.sel<-max(x[sel])
    min.x.sel<-min(x[sel])
    
    in.sel<-which(x>=min.x.sel & x<=max.x.sel)
    over.sel<-which(x>max.x.sel)
    under.sel<-which(x<min.x.sel)
    
    
    this.s<-ksmooth(x[sel],y2[sel],x.points=x[in.sel],kernel="normal",bandwidth=bw)
    if(sum(is.na(this.s$y))>0) return(y2)
    
    y2[in.sel]<-y2[in.sel]-this.s$y
    y2[over.sel]<-y2[over.sel]-this.s$y[which(this.s$x==max(this.s$x))[1]]
    y2[under.sel]<-y2[under.sel]-this.s$y[which(this.s$x==min(this.s$x))[1]]
    
    y2[y2<0]<-0
    return(y2)
}
