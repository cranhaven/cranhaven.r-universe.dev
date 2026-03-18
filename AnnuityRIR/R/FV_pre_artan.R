FV_pre_artan=function(data,years=10){
sigma2=function(x){var(x) * (length(x) - 1)/length(x)}
  U=1+data
  u1=mean(U)
  var=sigma2(U)
  u2=sqrt(var+u1^2)
  u_min=1+min(data)
  u_max=1+max(data)
  d=(u_min+u_max)/2
  a=(u_max-u_min)/pi
  b=tan(x=((u2-d)/a))-tan(x=((u1-d)/a))
  c=tan(x=((u1-d)/a))-b
  appo=rep(NA,years)
  for(i in 1:years) {appo[i]=(a*atan(x=(b*i+c))+d)^i}
  final_value=sum(appo)
  return(final_value)
}
