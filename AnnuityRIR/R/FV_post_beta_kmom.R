FV_post_beta_kmom=function(data,years=10){


nor=function(x){(x-min(x))/(max(x)-min(x))}
data2=nor(data)

app=EnvStats::ebeta(data2, method = "mme")
c=app$parameters[1]
d=app$parameters[2]
a=min(data)
b=max(data)

#print(paste("The shape parameters are equal to",round(c,3),"and",round(d,3)))

mean_beta=function(c,d){c/(c+d)}
media=mean_beta(c,d)

var_beta=function(c,d){c*d/((c+d)^2*(c+d+1))}
var_beta(c,d)

#print(paste("The mean of the standard beta distribution is equal to",round(mean_beta(app$parameters[1],app$parameters[2]),3)))
#print(paste("The variance of the standard beta distribution is equal to",round(var_beta(app$parameters[1],app$parameters[2]),3)))

n=years

fattoriale_crescente=function(n,f){n*factorial(x=n+f-1)/factorial(x=n)}

momento_beta=function(c,d,f){fattoriale_crescente(c,f)/fattoriale_crescente((c+d),f)}

momento_nn_normalizzato=function(n,a,b,c,d){
  for(k in 0:n) {   m=actuar::mbeta(n-k, c, d)
  moment_nn_norm=sum(
    choose(n,k)*(a^k)*((b-a)^(n-k))*m
  ) }
  return(moment_nn_norm)}


sum_n_moments_non_norm_beta_pag_post=function(n,a,b,c,d){  app2=rep(NA,n)
for (i in 0:(n-1)) app2[i+1]=momento_nn_normalizzato(i,a,b,c,d)
app2[2:n]=app2[2:n]+1
return(sum(app2))}

sum_n_moments_non_norm_beta_pag_post(n,a,b,c,d)
}





