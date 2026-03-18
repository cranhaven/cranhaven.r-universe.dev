PV_post_mood_pm=function(data,years=10){
  n=years
  m=2*n+2
  momenti=rep(NA,m)
  U=1+data
  u=mean(U)
  for (i in 1:m) momenti[i]=moment(U,
  central = FALSE, absolute = FALSE, order =i)
  PV=((momenti[n]-1)/(momenti[n+1]-momenti[n]))-
    ((momenti[2*n+1]-momenti[n]*momenti[n+1]-momenti[2*n]+(momenti[n])^2)/((momenti[n+1]-momenti[n])^2))+
    ((momenti[n]-1)/((momenti[n+1]-momenti[n])^3))*
    (momenti[2*n+2]-(momenti[n+1])^2+momenti[2*n]-(momenti[n])^2-2*((momenti[2*n+1])-momenti[n]*momenti[n+1]))
   return(PV)
}
