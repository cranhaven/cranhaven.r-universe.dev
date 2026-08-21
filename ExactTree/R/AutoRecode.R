AutoRecode<-function(V){
# recode in ordered categories
C<-sort(V)
I<-order(V)
n<-NROW(V)
O<-I
a<-1
O[I[1]]<-a
M<-c()
M[1]<-V[I[1]]
for(i in 2:n){
  if(V[I[i]]!=V[I[i-1]]){
    #if ((V(I(i))-V(I(i-1)))>0.49)
      a<-a+1
      M[a]<-V[I[i]]
  }
  O[I[i]]<-a
}

return(list(O=O, M=M))

}
