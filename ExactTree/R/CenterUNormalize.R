CenterUNormalize<-function(A){

  M<-(repmat(colMeans(A),n=dim(A)[1],m=1))
  B<-A-M
  S<-(repmat(sqrt(colSums(B^2)),n=dim(A)[1],m=1))
  B<-B/S

return(B)
}
