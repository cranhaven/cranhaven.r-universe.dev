norm_test_jb=function(data){
  x=data
sigma2=function(x){var(x) * (length(x) - 1)/length(x)}


a=tseries::jarque.bera.test(data)
m=mean(data)
n=sqrt(sigma2(data))
if (a$p.value>0.05) print(paste("Interest Rates are normally distributed with mean", round(m,5), "and standard deviation",round(n,5))) else print("Interest Rates are not normally distributed; you can try using the beta distribution")

#layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
par(mfrow=c(1,2))
hist(data,10)
curve(dnorm(x, mean=mean(data), sd=sd(data)), add=TRUE, col="red")
plot(density(data),col="green")
#boxplot(data,main="Box Plot")

}
