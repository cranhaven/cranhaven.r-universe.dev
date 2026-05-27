fdr.thresh <-
function(y.sample,fdr,alpha=0.1){
  thr_list<-unique(sort(fdr(y.sample)))
  th.fdr<-NULL
  fdp0<-0
  for(i in 1:length(thr_list)){
    rej_tags<-(fdr(y.sample)<thr_list[i])  
    fdp<-ifelse(sum(rej_tags)==0,0,sum(rej_tags*fdr(y.sample))/sum(rej_tags))
    if(fdp>alpha){
      if(i>1){
        th.fdr<-thr_list[i-1]
      }else{
        th.fdr<-NA
      }
      break;
    }else{
      fdp0<-fdp
    }
  }
  if(is.null(th.fdr)){
    th.fdr<-max(thr_list)
  }
  out<-list(th.fdr=th.fdr,fdp=fdp0)
  return(out)
}
