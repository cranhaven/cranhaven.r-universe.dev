# Belief distance between pairwise mass functions. Used by credal_RI
Bel_dist<-function(me1,m11,m01,me2,m12,m02){
  bel1<-c(m11,m01,1-me1)
  bel2<-c(m12,m02,1-me2)
  d<-sum(abs(bel1-bel2))/2
  return(d)
}