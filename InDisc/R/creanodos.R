creanodos<-function(li,ls,nin){
  # li -> inferior limit
  # ls -> superior limit
  # nin -> number of nodes

  d<-ls-li
  x<-matrix(0,nin,1)
  y<-matrix(0,nin,1)
  x[1]<-li
  nr<-nin-1
  amp<-d/nr
  #ordnor -> (1./sqrt(2.*pi)).*exp(-0.5.*x.*x)
  y[1]<-((1./sqrt(2*pi))*exp(-0.5*x[1]*x[1]))*amp
  for (j in 1:nr){
    x[j+1]<-x[j]+amp
    y[j+1]<-((1./sqrt(2*pi))*exp(-0.5*x[j+1]*x[j+1]))*amp
  }
  nodos=cbind(x,y)
  return(nodos)

}
