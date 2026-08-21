Freq<-function(X){
#create edges
#library(pracma)
minX<-min(X)
maxX<-max(X)
edges<-minX:maxX

f<-histc(X,edges) #from pracma

return(list(f=f,edges=edges))

}
