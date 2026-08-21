GetOP<-function(I,Splits){
# returns the ordered partition assignment for an ordered partitioning
# defined by Splits
dimensions<-c()
dimensions[1]<-NROW(Splits)
dimensions[2]<-NCOL(Splits)
k_1<-max(dimensions) #check if dim or length
s<-1
A<-c()
for(ki in 1:k_1){
  A[I[s:Splits[ki]]]<-ki
  s<-Splits[ki]+1
}

A[I[s:length(I)]]<-k_1+1

return(A)
}
