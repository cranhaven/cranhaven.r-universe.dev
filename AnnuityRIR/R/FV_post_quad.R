FV_post_quad=function(data,years=10){
  n=years
  u=mean(data)
  u2=mean(data^2)
  final_value=n+(n*(n-1)/2)*u+(n*(n-1)/4)*(1+(2*n-1)/3)*u2
  return(final_value)
}


