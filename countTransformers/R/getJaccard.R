# getJaccard<-function(mem1,mem2)
# {
#   I <- length(intersect(mem1, mem2))
#   S <- I/(length(mem1)+length(mem2)-I)
#   
#   return(S)
# }

getJaccard<-function(cl1,cl2)
{
  d=sum(cl1==1 & cl2==1, na.rm=TRUE)
  b=sum(cl1==1 & cl2==0, na.rm=TRUE)
  myc=sum(cl1==0 & cl2==1, na.rm=TRUE)
  
  res=d/(b+myc+d)
  return(res)
}