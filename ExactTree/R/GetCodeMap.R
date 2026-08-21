GetCodeMap<-function(O,V){
# recode in ordered categories
M<-matrix(0,nrow = max(O), ncol=1)
for(id in min(O):max(O)){
  I<-which(O==id)
  M[id]<-min(V[I])
}
return(M)
}
