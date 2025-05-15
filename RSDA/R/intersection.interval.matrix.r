intersection.interval.matrix<-function(interval.matrix.1,interval.matrix.2)
{
  if(interval.matrix.1$M == interval.matrix.2$M)
  {
      Intersection.df<-matrix(rep(NA,interval.matrix.1$N*interval.matrix.2$N*(2*interval.matrix.2$M+2)),ncol = 2*interval.matrix.2$M+2)
      Intersection.df<-as.data.frame(Intersection.df)
      seq.min<-seq(from = 1,to = 2*interval.matrix.2$M,by = 2)
      seq.max<-seq(from = 2,to = 2*interval.matrix.2$M,by = 2)
      indx.row<-1
      for(i in 1:interval.matrix.1$N)
      {
        ind.1<-interval.matrix.1$sym.obj.names[i]
        for(j in 1:interval.matrix.2$N)
        {
          ind.2<-interval.matrix.2$sym.obj.names[j]
          for(h in 1:interval.matrix.2$M)
          {
            col.act<-c(seq.min[h],seq.max[h])
            intersection.tmp<-intersection.interval(interval.matrix.1$data[i,col.act],interval.matrix.2$data[j,col.act])
            Intersection.df[indx.row,1]<-ind.1
            Intersection.df[indx.row,2]<-ind.2
            col.act.t<-col.act+2
            Intersection.df[indx.row,col.act.t[1]]<-intersection.tmp[1]
            Intersection.df[indx.row,col.act.t[2]]<-intersection.tmp[2]
          }
          indx.row<-indx.row+1
        }
      }
  }
  else{
    print("Matrix will have the same number of columns")
  }
  return(Intersection.df)
}
