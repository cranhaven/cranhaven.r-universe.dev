PV_post_exact=function(data,years=10){
X=data
U=1+X
n=years
a=rep(NA,n)
for (i in 1:n) a[i]=moment(U, central = FALSE, absolute = FALSE, order =-i)
PV=sum(a)
return(PV)
}

