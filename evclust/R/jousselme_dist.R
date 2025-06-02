# Jousselme distance between pairwise mass functions. used by credal_RI.
jousselme_dist<-function(me1,m11,m01,me2,m12,m02){
  
  m1<-c(me1,m11,m01,1-me1-m11-m01)
  m2<-c(me2,m12,m02,1-me2-m12-m02)
  J<-matrix(c(1,0,0,0,0,1,0,0.5,0,0,1,0.5,0,0.5,0.5,1),4,4)
  dm<-m1-m2
  d<-sqrt(0.5*t(dm)%*%J%*%dm)
  return(d)
}
