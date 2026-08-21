getFreqY<-function(Y){
#gives frequencies of classes in Y, Y should be autorecoded (i.e. 1,...)
#create edges
#library(pracma)
minY<-min(Y)
maxY<-max(Y)
edges<-minY:maxY
f<-histc(Y,edges)$cnt

return(f)
}
