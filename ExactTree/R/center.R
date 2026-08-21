Center<-function(A){
  #library(pracma)

  if(NCOL(A)==1){
    B<-A-(repmat(mean(A),n=NROW(A),m=1))

  }else{
    B<-A-(repmat(colMeans(A),n=dim(A)[1],m=1))
  }

  return(B)
}
