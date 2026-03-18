variance_post_mood_nm=function(data,years=10){

  n=years
U=1+data
u=mean(U)

u2=moment(U,
           central = FALSE,
           absolute = FALSE,
           order = 2)

un=moment(U,
           central = FALSE,
           absolute = FALSE,
           order = -n)

u2n=moment(U,
           central = FALSE,
           absolute = FALSE,
           order = -2*n)

un1=moment(U,
             central = FALSE,
             absolute = FALSE,
             order = -(n-1))

un2=moment(U,
           central = FALSE,
           absolute = FALSE,
           order = -(n-2))

u2n1=moment(U,
           central = FALSE,
           absolute = FALSE,
           order = -2*(n-1))



ex=1-un
ey=u-1
vx=u2n-un^2
vy=u2-u^2
covxy=-un1+un*u

var_nm=((ex/ey)^2)*((vx/(ex)^2)+(vy/(ey)^2)-(2*covxy/(ex*ey)))


return(var_nm)

}








