beta_parameters=function(data){


nor=function(x){(x-min(x))/(max(x)-min(x))}
data2=nor(data)

app=EnvStats::ebeta(data2, method = "mme")
c=app$parameters[1]
d=app$parameters[2]
a=min(data)
b=max(data)

print(paste("The shape parameters are equal to",round(c,3),"and",round(d,3)))

mean_beta=function(c,d){c/(c+d)}
media=mean_beta(c,d)

var_beta=function(c,d){c*d/((c+d)^2*(c+d+1))}
var_beta(c,d)

print(paste("The mean of the standard beta distribution is equal to",round(mean_beta(app$parameters[1],app$parameters[2]),3)))
print(paste("The variance of the standard beta distribution is equal to",round(var_beta(app$parameters[1],app$parameters[2]),3)))

# from the "stat" package
pl.beta <- function(a,b, asp = if(isLim) 1, ylim = if(isLim) c(0,1.1)) {
  if(isLim <- a == 0 || b == 0 || a == Inf || b == Inf) {
    eps <- 1e-10
    x <- c(0, eps, (1:7)/16, 1/2+c(-eps,0,eps), (9:15)/16, 1-eps, 1)
  } else {
    x <- seq(0, 1, length = 1025)
  }
  fx <- cbind(dbeta(x, a,b), pbeta(x, a,b), qbeta(x, a,b))
  f <- fx; f[fx == Inf] <- 1e100
  matplot(x, f, ylab="", type="l", ylim=ylim, asp=asp,
          main = sprintf("Beta Distribution", a,b))
  abline(0,1,     col="gray", lty=3)
  abline(h = 0:1, col="gray", lty=3)
  legend("top", paste0(c("d","p","q"), "beta(x, a,b)"),
         col=1:3, lty=1:3, bty = "n")
  invisible(cbind(x, fx))
}


par(mfrow=c(1, 3))
hist(data2,10, main="Histogram of Data",xlab="Interest Rates")
plot(density(data2), main="Kernel density estimation")
pl.beta(c,d)
}










