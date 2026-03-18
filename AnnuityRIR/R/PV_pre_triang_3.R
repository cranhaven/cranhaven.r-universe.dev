PV_pre_triang_3=function(data,years=10){

  r=years
  app=rep(NA,r)
  for (i in 1:r) app[i]=triangular_moments_3_U(data,i)
  somma_momenti=1+sum(app[3:(r-1)])+triangular_moments_dis_U(data,1)+triangular_moments_dis_U(data,2)
  return(somma_momenti)

}
