plot_FVs_post=function(data,years=10,lwd=1.5,lty1=1, lty2=2,lty3=3){

art=rep(NA,years)
lin=rep(NA,years)
moo=rep(NA,years)

for (i in 1:years) {
  art[i]=FV_post_artan(data,i)
  lin[i]=FV_post_quad(data,i)
  moo[i]=FV_post_mood(data,i)
}

result=cbind(art,lin,moo)
result

plot(art,type="l",col="red",lwd=lwd, lty=lty1, xlim=c(0,years),ylim=c(0,max(result)),xlab="Time", ylab="Final Expexted Value",main="Final Expected Value (annuity-immediate)")
par(new=TRUE)
plot(lin,type="l",col="black",lwd=lwd, lty=lty2, xlim=c(0,years),ylim=c(0,max(result)),xlab="",ylab="")
par(new=TRUE)
plot(moo,type="l",col="green",lwd=lwd, lty=lty3, xlim=c(0,years),ylim=c(0,max(result)),xlab="",ylab="")
legend("topleft",c("Tetraparametric","Quadratic","Mood et al."), lwd=lwd, lty=c(lty1,lty2,lty3),
 y.intersp = 0.65, col=c("red","black","green"),  xjust = 1, merge = TRUE)


}
