PV_post_cubic=function(data,years=10){

  #not on U but X
  
  n=years
  u=mean(data)
  u2=mean(data^2)
  u3=mean(data^3)

  # qudratic approx PV=n-(n*(n+1)/2)*u+(n*(n+1)/4)*(1+(2*n+1)/3)*u2

  PV=n-(n*(n+1)/2)*u+(n*(n+1)/4)*(1+(2*n+1)/3)*u2-(n*(n+1)*u3/6)*(1+(2*n+1)/2+n*(n+1)/4)

  return(PV)
}


