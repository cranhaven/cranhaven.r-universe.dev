help.func <- function(i,x.seq,n,parent.knot,first.data,second.data,third.data=NULL,intp,d,D,p,cond,q,ord=NULL) {
 if(p==2) {
   if(!is.null(ord)) Yt <- cbind(rep(second.data[i],length(x.seq)),x.seq)[,ord] else Yt <- cbind(rep(second.data[i],length(x.seq)),x.seq)
   func <- hierarchbs.cond.cop(data=Yt,coef=parent.knot$v,intp=intp,d=d,D=D,p=p,cond=cond,q=q)
   func[func<0]<-0
   func[func>1]<-1
   return(x.seq[min(which(round(func,10)>=first.data[i]))])
 }
 if(p==3) {
   if(!is.null(ord)) Yt <- cbind(rep(second.data[i],length(x.seq)),x.seq,rep(third.data[i],length(x.seq)))[,c(ord,3)] else Yt <- cbind(rep(second.data[i],length(x.seq)),x.seq,rep(third.data[i],length(x.seq)))
   func <- hierarchbs.cond.cop(data=Yt,coef=parent.knot$v,intp=intp,d=d,D=D,p=p,cond=cond,q=q)
   func[func<0]<-0
   func[func>1]<-1
   return(x.seq[min(which(round(func,10)>=first.data[i]))])
 }

}
