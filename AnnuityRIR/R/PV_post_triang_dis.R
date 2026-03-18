PV_post_triang_dis=function(data,years=10){
  app=rep(NA,years)
    for(i in 1:years) app[i]=triangular_moments_dis_U(data,i)
    PV=sum(app)
    return(PV)
}


