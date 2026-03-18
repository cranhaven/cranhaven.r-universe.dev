FV_pre_mood=function(data,years=10){

  n=years
  m=n+2
  momenti=rep(NA,m)
  U=1+data
  u=mean(U)
  for (i in 1:m) momenti[i]=moment(U,
  central = FALSE, absolute = FALSE, order =i)
  final_value=((momenti[n+1]-u)/(u-1))-
    ((momenti[n+2]-u*momenti[n+1]-momenti[2]+u^2)/
       ((u-1)^2))+
    ((momenti[n+1]-u)/((u-1)^3))*(momenti[2]-u^2)
  return(final_value)
}
