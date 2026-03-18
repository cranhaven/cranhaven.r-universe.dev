PV_pre_mood_nm=function(data,years=10){

  n=years
  U=1+data
  u=mean(U)

  u_2=moment(U,
             central = FALSE,
             absolute = FALSE,
             order = 2)

  u_n=moment(U,
             central = FALSE,
             absolute = FALSE,
             order = -n)

  u_n_1=moment(U,
               central = FALSE,
               absolute = FALSE,
               order = -(n-1))

  u_n_2=moment(U,
               central = FALSE,
               absolute = FALSE,
               order = -(n-2))


  FV=(u-u_n_1)/(u-1)-(u_2-u^2-u_n_2+u*u_n_1)/(u-1)^2+(u-u_n_1)*(u_2-u^2)/(u-1)^3

  return(FV)
}
