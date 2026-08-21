# BootsTrapIndex<-function(BootSize,SampleSize,WithDraw){
#
# if(nargs()<3){
#   #met teruglegging
#   I<-1+trunc(runif(BootSize)*SampleSize)
# }else{
#   #zonder teruglegging
#   #library(pracma)
#   P<-randperm(SampleSize)
#   I<-P[1:BootSize]
# }
#
# #I<-matrix(I, ncol=1)
#
# return(I)
# }
