GetPMisclassification<-function(Y){
nc<-NCOL(Y)
y<-matrix(0,nrow = nc, ncol = 1)
Y<-matrix(Y, nrow = NROW(Y), ncol = nc)
for(d in 1:nc){
  hf<-0
  O<-sort(Y[,d])#O should be a vector
  while(length(O)>0){
    c<-min(O)
    I<-which(O>c)
    f<-length(O)-length(I)
    if(f>hf){
      hf<-f
      y[d]<-c
    }
    O<-O[I]
  }
}

return(y)

}

