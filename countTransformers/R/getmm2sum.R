# created  on Jan 12, 2019
#  (1) calculate sum of subject-specific squared difference
#      between sample mean and sample median

getmm2sum<-function(mat)
{
  md=apply(mat, 2, median, na.rm=TRUE)
  me=apply(mat, 2, mean, na.rm=TRUE)
  mm2sum=sum((md-me)^2, na.rm=TRUE)
  return(mm2sum)
}


